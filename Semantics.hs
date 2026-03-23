module Semantics where

import Kernel
import qualified Data.Map.Strict as Map
import Data.List
import Data.Char (isAlpha, isDigit, isLower)
import Debug.Trace
import Control.Monad.Reader
import Control.Monad
import Control.Monad.State.Strict hiding (State)
import qualified Control.Monad.State.Strict as S
import Control.Monad.IO.Class
import qualified Data.Set as Set
import Data.IORef (IORef)

{---- Oz Semantics Types ----------------}

-- Configuration (Immutable)
data Config = Config {
    quantum :: ExtInt,    -- Quantum for round-robin
    debugMode :: Bool,    -- Enable verbose tracing
    statsRef :: Maybe (IORef (Maybe State)) -- Reference for signal handling
}

-- The Interpreter Monad Stack
type InterpM = ReaderT Config (StateT State IO)

type SLoc = Int                            -- Store locations
type Name = Int                            -- Names
type Env = [(Var,SLoc)]                    -- Environments
type RecVal = (Atom, [(Feature,SLoc)])     -- Record values (label, feats/vals)
type Clos = ([Var], [Stmt], Env)           -- Closures (args, body, cenv)
type WatchMap = Map.Map SLoc (SLoc, String) -- Watched locations: loc -> (parentLoc, label)

-- Primitive operations (Lifted to InterpM)
type PrimFn = [(SLoc, SVal)] -> State -> InterpM (Either String (State, [SVal]))

-- The type stored in SVal for primitives
type PrimOp = State -> [SLoc] -> InterpM ExecResult

-- Store values
data SVal = SUnbound                       -- Unbound
          | SInt Integer                   -- Integer value
          | SFloat Float                   -- Floating-point value
          | SRec RecVal                    -- Record value
          | SProc Clos                     -- Procedure value
          | SPrimOp PrimOp                 -- Primitive (built-in) operation
          | SName Name                     -- Name value
          | SCell Name                     -- Cell name (-> mutable store)
          | SThunk [SLoc]                   -- Thunk references (triggers)
          | SReadOnly SLoc                 -- Read-Only view

data BindResult = BSuccess SAS [SLoc] [String] [([SLoc], SLoc)] [SLoc]
                | BFail 
                | BSuspend SLoc
                | BNeed [SLoc] SLoc -- trigger procs, needed variable

instance Show SVal where
  show SUnbound = "Unbound"
  show (SInt n) = show n
  show (SFloat f) = show f
  show (SRec rl) = show_Srec_Pure rl
  show (SProc clos) = "proc" ++ show clos
  show (SPrimOp op) = "Primitive Operation"
  show (SName n) = "Name "++(show n)
  show (SCell n) = "Cell "++(show n)
  show (SThunk ss) = "Thunk "++(show ss)
  show (SReadOnly s) = "ReadOnly "++(show s)

show_Srec_Pure :: RecVal -> String
show_Srec_Pure (a,[]) = if needsQuote a then "'" ++ a ++ "'" else a
show_Srec_Pure (a,fvs) = (if needsQuote a then "'" ++ a ++ "'" else a) ++ l2s "(" " " ")" (map show_pair fvs)
  where
    show_pair (FeatInt i, v) = show i ++ ":" ++ show v
    show_pair (FeatAtom a, v) = (if needsQuote a then "'" ++ a ++ "'" else a) ++ ":" ++ show v

needsQuote :: String -> Bool
needsQuote "" = True
needsQuote (c:cs) = not (isLower c && all isIdChar cs)
  where
    isIdChar c = isAlpha c || isDigit c || c == '_'

type Stack = [(Stmt, Env)]                 -- Execution stack of semantic stmts
type Thread = (Int, Stack)                 -- Thread (thread #, stack)
type SAS = [([SLoc],SVal)]                 -- Single-assignment store
type MUT = [(Name,SLoc)]                   -- Mutable store

data State = State {
  front :: [Thread],                       -- Queue of threads, front
  back :: [Thread],                        -- Queue of threads, reversed back

  sas :: SAS,                              -- Single-assignment store
  muts :: MUT,                             -- Mutable store 
  watches :: WatchMap,                     -- Watched variables
  suspended :: Map.Map SLoc [Thread],      -- Threads suspended on variables

  newlocs :: [Int],                        -- List of new SLocs (subject to GC)
  newname :: Int,                          -- Next available name
  newthread :: Int,                        -- Next available thread number

  -- Statistics
  steps :: Int,                            -- Total statements executed
  maxThreads :: Int                        -- Peak number of active/suspended threads
}

-- Extended integers (for quantum)
data ExtInt = Finite Int | Infinity
  deriving (Eq, Show)

-- Execution results
data ExecResult = EError String            -- Error with message
                | ESuspend SLoc            -- Statement suspends on specific SLoc
                | ENeed [SLoc] SLoc        -- Need thunk values
                | ENext State              -- Steps to a new state
                | EYield Stack             -- Yields execution with new stack

tplSec x y = (y,x)

spawnThunks :: [([SLoc], SLoc)] -> State -> State
spawnThunks [] s = s
spawnThunks ((triggers, needId):rest) s = 
    let s' = foldl (\currS triggerLoc -> 
                      case slookup triggerLoc (sas currS) of
                         Just (_, SProc (pArgs, pBody, pCe)) | length pArgs == 1 ->
                             let tnNew = newthread currS
                                 eNew = (head pArgs, needId) : pCe
                                 stNew = map (tplSec eNew) pBody
                             in currS{back = (tnNew, stNew) : back currS, newthread = tnNew + 1}
                         _ -> currS
                   ) s triggers
    in spawnThunks rest s'

wakeThreads :: [SLoc] -> State -> State
wakeThreads [] s = s
wakeThreads (l:ls) s = 
    let (ws, suspended') = Map.updateLookupWithKey (\_ _ -> Nothing) l (suspended s)
        s' = case ws of
            Just threads -> s{back = threads ++ back s, suspended = suspended'}
            Nothing -> s{suspended = suspended'}
    in wakeThreads ls s'

data SchedResult = SDone                   -- Program terminates successfully
                 | SError String           -- Error with message

{---- Store Logic ----------------}

bind :: SAS -> SLoc -> SLoc -> BindResult
bind sas id1 id2 = binds sas [(id1,id2)] [] [] [] [] where
  binds :: SAS -> [(SLoc,SLoc)] -> [SLoc] -> [String] -> [([SLoc], SLoc)] -> [SLoc] -> BindResult
  binds sas [] modified msgs triggered determined = BSuccess sas modified msgs triggered determined
  binds sas ((id1,id2):ids) modified msgs triggered determined
    | any (\(vg,sv) -> elem id1 vg && elem id2 vg) sas = binds sas ids modified msgs triggered determined
    | otherwise =
      let (vg1,sv1,sas1) = sremove sas id1
          (vg2,sv2,sas2) = sremove sas1 id2
          vg = vg1 ++ vg2
          modified' = vg ++ modified
          
          sas' = (vg,sv1):sas2
          match n1 n2 = if n1 == n2 then binds sas' ids modified' msgs triggered determined else BFail
          
          -- A location group is newly determined if it goes from (SUnbound | SThunk) to (Value | SReadOnly)
          newlyDet = if isUnbound sv1 && not (isUnbound sv2) then vg
                     else if isUnbound sv2 && not (isUnbound sv1) then vg
                     else []
          determined' = newlyDet ++ determined
          
      in case (sv1,sv2) of
        (SUnbound, SThunk ts) -> binds ((vg,sv2):sas2) ids modified' msgs triggered determined'
        (SUnbound, _) -> binds ((vg,sv2):sas2) ids modified' msgs triggered determined'
        (SThunk ts, SUnbound) -> binds ((vg,sv1):sas2) ids modified' msgs triggered determined'
        (_, SUnbound) -> binds sas' ids modified' msgs triggered determined'
        (SThunk ts1, SThunk ts2) -> binds ((vg, SThunk (ts1++ts2)):sas2) ids modified' msgs triggered determined'
        (SThunk ts, val) -> binds ((vg, val):sas2) ids modified' msgs ((ts, id1):triggered) determined'
        (val, SThunk ts) -> binds ((vg, val):sas2) ids modified' msgs ((ts, id2):triggered) determined'
        (SReadOnly r1, SReadOnly r2) -> binds sas ((r1,r2):ids) modified msgs triggered determined
        (SReadOnly r, val) -> checkRO r id2 sas ids modified msgs triggered determined
        (val, SReadOnly r) -> checkRO r id1 sas ids modified msgs triggered determined
        (SInt n1, SInt n2) -> match n1 n2
        (SFloat f1, SFloat f2) -> match f1 f2
        (SRec (l1,fvs1), SRec (l2,fvs2)) ->
          let (fs1,ids1) = unzip fvs1
              (fs2,ids2) = unzip fvs2
          in if l1 == l2 && fs1 == fs2 then binds sas' (zip ids1 ids2 ++ ids) modified' msgs triggered determined
             else BFail
        (SName n1, SName n2) -> match n1 n2
        (SCell n1, SCell n2) -> match n1 n2
        _ -> BFail

  checkRO :: SLoc -> SLoc -> SAS -> [(SLoc,SLoc)] -> [SLoc] -> [String] -> [([SLoc], SLoc)] -> [SLoc] -> BindResult
  checkRO r targetId sas ids modified msgs triggered determined = 
     case slookup r sas of
         Nothing -> BFail
         Just (_, SUnbound) -> BSuspend r
         Just (_, SThunk ts) -> BNeed ts r
         Just (_, SReadOnly r2) -> checkRO r2 targetId sas ids modified msgs triggered determined
         Just (_, val2) -> 
             case bind sas r targetId of
                BSuccess sas' modified' newMsgs newTriggered newDet -> 
                    binds sas' ids (modified ++ modified') (msgs ++ newMsgs) (triggered ++ newTriggered) (determined ++ newDet)
                res -> res

  isUnbound SUnbound = True
  isUnbound (SThunk _) = True
  isUnbound (SReadOnly _) = False -- Treated as value
  isUnbound _ = False

-- Notify watchers AFTER the store has been updated
notifyWatchers :: SAS -> WatchMap -> [SLoc] -> (WatchMap, [String])
notifyWatchers sas w modifiedIds =
      let 
          -- Filter to find if any of the modified IDs are being watched
          watched = filter (`Map.member` w) modifiedIds
      in if null watched 
         then (w, [])
         else foldl (\(currW, currMsgs) id -> 
         case Map.lookup id currW of
            Nothing -> (currW, currMsgs)
            Just (parentId, label) ->
               -- We use 'sas' which is ALREADY UPDATED
               case slookup parentId sas of
                 Just (_, rootVal) ->
                   if valIsUnbound rootVal then (currW, currMsgs)
                   else
                     -- Valid update. Now look up the CHILD that triggered it
                     case slookup id sas of
                        Just (_, childVal) -> 
                           if valIsUnbound childVal 
                           then (currW, currMsgs) -- Child still unbound (e.g. unif of two unbound vars)
                           else 
                               let 
                                   msg = label ++ ": " ++ formatPretty sas (Just parentId) rootVal
                                   -- Register watchers for the NEW child value, pointing back to Root
                                   newW = addChildrenDeep currW childVal parentId label sas 
                               in (Map.delete id newW, currMsgs ++ [msg])
                        Nothing -> (Map.delete id currW, currMsgs)
                 Nothing -> (Map.delete id currW, currMsgs)
      ) (w, []) watched

valIsUnbound SUnbound = True
valIsUnbound (SThunk _) = False -- User wants to see thunks ($)
valIsUnbound _ = False

-- Add children of a record/list to the watch map
-- Add children of a record/list to the watch map (Recursive, Cycle-Safe)
-- Note: We now propagate the ROOT ID and ROOT LABEL to all descendants.
-- We do NOT append paths (like .1) anymore, because we decided to always 
-- print from the Root, so the label should identify the Root.
addChildrenDeep :: WatchMap -> SVal -> SLoc -> String -> SAS -> WatchMap
addChildrenDeep w rootVal rootId rootLabel sas = 
    execState (traverseVal rootVal) w
  where
    traverseVal :: SVal -> S.State WatchMap ()
    traverseVal val = case val of
        SRec (lab, fvs) -> 
            let visitChild (feat, childLoc) = do
                  currW <- get
                  if Map.member childLoc currW 
                  then return () -- Already watched/visited
                  else do
                      -- Always use the Root Label
                      let childLabel = rootLabel 
                      put (Map.insert childLoc (rootId, childLabel) currW)
                      -- Recurse if the child is bound
                      case slookup childLoc sas of
                          Just (_, childVal) -> traverseVal childVal
                          Nothing -> return ()
            in mapM_ visitChild fvs
        _ -> return ()

-- Analysis for Rational Trees: Find true cycles (back-edges)
analyzeGraph :: SAS -> SVal -> Map.Map SLoc Int
analyzeGraph sas rootVal = execState (visit Set.empty rootVal) Map.empty
  where
    visit :: Set.Set SLoc -> SVal -> S.State (Map.Map SLoc Int) ()
    visit path val = case val of
       SRec (_, fvs) -> 
         let checkChild (_, loc) = do
               if Set.member loc path 
               then do
                   -- Found a True back-edge (Cycle)
                   cycles <- get
                   put (Map.insert loc 1 cycles)
               else do
                   -- No cycle yet, recursively traverse
                   case slookup loc sas of
                      Just (_, v) -> visit (Set.insert loc path) v
                      Nothing -> return ()
         in mapM_ checkChild fvs
       _ -> return ()


formatPretty :: SAS -> Maybe SLoc -> SVal -> String
formatPretty sas mbRoot rootVal = 
    evalState (do
        prefix <- case mbRoot of
           Just r -> 
              -- Check if r is aliased to any shared node
              let alias = foldr (\s acc -> case acc of 
                                  Just _ -> acc
                                  Nothing -> 
                                     -- Check alias: do they resolve to same entry?
                                     case (slookup r sas, slookup s sas) of
                                        (Just (l1,_), Just (l2,_)) | l1 == l2 -> Just s
                                        _ -> Nothing
                                ) Nothing (Set.toList cycles)
              in case alias of
                   Just s -> do
                      let rId = 1
                      (visited, shared, next) <- get
                      put (Map.insert s rId visited, shared, rId + 1)
                      return $ "(C" ++ show rId ++ ") "
                   Nothing -> return ""
           _ -> return ""
        s <- format rootVal
        return (prefix ++ s)
    ) (Map.empty, cycles, 1)
  where
    -- 1. Analyze for sharing
    cycles = Map.keysSet (analyzeGraph sas rootVal)

    -- State: (Visited Set, Next ID)
    -- We map SLoc -> Int (The C number) if it's cyclic.
    
    format :: SVal -> S.State (Map.Map SLoc Int, Set.Set SLoc, Int) String
    format val = case val of
      SUnbound -> return "_"
      SInt n -> return $ show n
      SFloat f -> return $ show f
      SProc _ -> return "<Proc>"
      SName n -> return $ "<Name " ++ show n ++ ">"
      SCell n -> return $ "<Cell " ++ show n ++ ">"
      SThunk _ -> return "$"
      SReadOnly _ -> return "<ReadOnly>"
      SPrimOp _ -> return "<Prim>"
      SRec (lab, fvs) -> do
         formatRec lab fvs

    needsParens :: SVal -> Bool
    needsParens (SRec (lab, fvs)) = (lab == "|" && length fvs == 2) || (lab == "#" && isTupleFeatures fvs)
    needsParens _ = False

    formatLoc :: SLoc -> S.State (Map.Map SLoc Int, Set.Set SLoc, Int) String
    formatLoc loc = do
        (visited, cyclic, nextId) <- get
        let val = case slookup loc sas of
                    Just (_, v) -> v
                    Nothing -> SUnbound
        let wrapIf s = if needsParens val then "(" ++ s ++ ")" else s
        
        case Map.lookup loc visited of
            Just cId -> 
                -- Already visited - if it's in a cycle, print the back-edge reference
                if loc `Set.member` cyclic 
                then return $ "C" ++ show cId
                else return "..." -- Cut off infinite recursion on non-cycle duplicates (e.g. DAG printing deeper than needed, though true DAG should just print inline if we didn't visit it *on this path*. For pretty printing, we just print it if it's not a cycle, but wait...
                                  -- actually, if it's not a CYCLE back-edge, we SHOULD print it! But we need to make sure we don't loop. 
                                  -- Since analyzeGraph identified true back-edges, we ONLY cut off if it's a back-edge. 
            Nothing -> do
                -- First visit. If it's cyclic, assign an ID and mark visited
                if loc `Set.member` cyclic 
                then do
                     let cId = nextId
                     put (Map.insert loc cId visited, cyclic, nextId + 1)
                     valStr <- format val
                     return $ "(C" ++ show cId ++ ") " ++ wrapIf valStr
                else do
                     -- Not cyclic, just format directly without caching ID
                     -- But we MUST add it to a "currently visiting" path to prevent infinite loops if analyzeGraph failed,
                     -- though analyzeGraph is accurate.
                     put (Map.insert loc 0 visited, cyclic, nextId) -- 0 means visited but no cycle ID
                     valStr <- format val
                     -- Pop it off so sibling paths can print it inline (DAG sharing)
                     (v2, c2, n2) <- get
                     put (Map.delete loc v2, c2, n2)
                     return $ wrapIf valStr

    formatRec :: String -> [(Feature, SLoc)] -> S.State (Map.Map SLoc Int, Set.Set SLoc, Int) String
    formatRec lab fvs = 
        if lab == "|" && length fvs == 2 
        then do -- List Sugar
            let hLoc = lookup (FeatInt 1) fvs
                tLoc = lookup (FeatInt 2) fvs
            case (hLoc, tLoc) of
                 (Just h, Just t) -> do
                    hStr <- formatLoc h
                    tStr <- formatListTail t
                    return $ hStr ++ " | " ++ tStr
                 _ -> formatRecBase lab fvs
        else if lab == "#" && isTupleFeatures fvs
        then do -- Tuple Sugar
             -- Sort fvs by key just in case, though isTupleFeatures should ensure validity
             let sortedFvs = sortBy (\(a,_) (b,_) -> compare a b) fvs
             args <- mapM (\(_, loc) -> formatLoc loc) sortedFvs
             return $ l2s "" " # " "" args
        else formatRecBase lab fvs

    isTupleFeatures :: [(Feature, SLoc)] -> Bool
    isTupleFeatures fvs = 
        let keys = map fst fvs
            sortedKeys = sort keys
            expectedKeys = map FeatInt [1 .. fromIntegral (length fvs)]
        in sortedKeys == expectedKeys

    formatRecBase lab fvs = do
        let formatArg (f, loc) = do
              s <- formatLoc loc
              return s
        args <- mapM formatArg fvs
        return $ show_Srec (lab, zip (map fst fvs) args) -- Re-use show_Srec outputting strings

    formatListTail :: SLoc -> S.State (Map.Map SLoc Int, Set.Set SLoc, Int) String
    formatListTail loc = do
        (visited, cyclic, nextId) <- get
        -- Check if this list tail is a cycle point
        if loc `Set.member` cyclic
        then formatLoc loc -- Will print C_N or C_N=...
        else case slookup loc sas of
           Just (_, SRec ("|", fvs)) -> do
               let h = lookup (FeatInt 1) fvs
                   t = lookup (FeatInt 2) fvs
               case (h, t) of
                   (Just h', Just t') -> do
                       hStr <- formatLoc h'
                       tTail <- formatListTail t'
                       return $ hStr ++ " | " ++ tTail
                   _ -> formatLoc loc
           Just (_, SRec ("nil", [])) -> return "nil"
           Just (_, SUnbound) -> return "_"
           Just (_, v) -> formatLoc loc -- Dotted pair
           Nothing -> return "_"

-- Modified helper to print SRec with pre-computed strings
show_Srec :: (String, [(Feature, String)]) -> String
show_Srec (a,[]) = if needsQuote a then "'" ++ a ++ "'" else a
show_Srec (a,fvs) = (if needsQuote a then "'" ++ a ++ "'" else a) ++ l2s "(" " " ")" (map show_pair fvs)
  where
    show_pair (FeatInt i, v) = show i ++ ":" ++ v
    show_pair (FeatAtom a, v) = (if needsQuote a then "'" ++ a ++ "'" else a) ++ ":" ++ v

{---- Store Operations ----------------}

-- Lookup store location in single-assignment store
slookup :: SLoc -> SAS -> Maybe ([SLoc],SVal)
slookup id [] = Nothing
slookup id ((xs,sv):sas) | elem id xs = Just (xs,sv)
                         | otherwise = slookup id sas

-- Update single-assignment store with new or replaced binding
supdate :: SAS -> SLoc -> SVal -> SAS
supdate [] id val = [([id],val)]
supdate ((vg,sv):sas) id val | elem id vg = (vg,val):sas
                             | otherwise = (vg,sv):supdate sas id val

-- Update mutable store with replaced binding
mupdate :: MUT -> Name -> SLoc -> MUT
mupdate ((n,id):muts) n0 id0 | n0 == n = (n0,id0):muts
                             | otherwise = (n,id):mupdate muts n0 id0

-- Remove and return an existing binding group from a single-assignment store
sremove :: SAS -> SLoc -> ([SLoc],SVal,SAS)
sremove ((vg,sv):sas) id
  | elem id vg = (vg,sv,sas)
  | otherwise = let (vg',sv',sas') = sremove sas id
                in (vg',sv', (vg,sv):sas')

{---- Helpers ----------------}

-- Check for duplicates
dups :: Eq a => [a] -> Bool
dups [] = False
dups (x:xs) = elem x xs || dups xs


-- Check variables
check_vars :: Env -> [Var] -> Either String [SLoc]
check_vars e vs = cv e vs [] [] False where
  cv :: Env -> [Var] -> [String] -> [SLoc] -> Bool -> Either String [SLoc]
  cv e [] msgs ids err =
    if err then Left (l2s "" "\n" "" $ reverse msgs) else Right (reverse ids)
  cv e (v:vs) msgs ids err = case lookup v e of
    Just id -> cv e vs msgs (id:ids) err
    Nothing -> cv e vs (("Variable " ++ v ++ " undeclared") : msgs) ids True

-- Create store value
create_val :: Env -> PVal -> Either String SVal
create_val e (PInt n) = Right (SInt n)
create_val e (PFloat f) = Right (SFloat f)
create_val e (PRec (lab,fvs)) =
  let fvs' = sortBy (\(a,_) (b,_) -> compare a b) fvs
      (fs',vs') = unzip fvs'
  in case check_vars e vs' of
    Left msg -> Left msg
    Right ids -> Right (SRec (lab, zip fs' ids))
create_val e pv@(PProc xs ms) =
  let frees = fvar_pval pv
  in case check_vars e frees of
    Left msg -> Left msg
    Right ids -> Right (SProc (xs, ms, zip frees ids))
