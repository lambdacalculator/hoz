module Primitives where

import Semantics
import Kernel
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (sortBy, nub)
import Control.Monad.IO.Class
import Control.Monad.State.Strict hiding (State)

{---- Global Registry ----------------}

data PrimMeta = PrimMeta 
  { pmName :: String
  , pmVal :: SVal
  , pmSym :: Maybe String
  , pmDoc :: String 
  }

primitives :: [PrimMeta]
primitives = 
  [ PrimMeta "Plus_" prim_plus (Just "+") "{Plus_ +N1 +N2 -Res} binds Res to N1 + N2 (Int+Int->Int, else Float)"
  , PrimMeta "Minus_" prim_minus (Just "-") "{Minus_ +N1 +N2 -Res} binds Res to N1 - N2"
  , PrimMeta "Multiply_" prim_multiply (Just "*") "{Multiply_ +N1 +N2 -Res} binds Res to N1 * N2"
  , PrimMeta "Div_" prim_div (Just "div") "{Div_ +N1 +N2 -Res} binds Res to N1 div N2 (Integer only)"
  , PrimMeta "Mod_" prim_mod (Just "mod") "{Mod_ +N1 +N2 -Res} binds Res to N1 mod N2 (Integer only 2nd arg)"
  , PrimMeta "EQ_" prim_eq (Just "==") "{EQ_ +V1 +V2 -Bool} binds Bool to (V1 == V2)"
  , PrimMeta "NotEQ_" prim_notEq (Just "\\=") "{NotEQ_ +V1 +V2 -Bool} binds Bool to (V1 \\= V2)"
  , PrimMeta "GT_" prim_intGT (Just ">") "{GT_ +N1 +N2 -Bool} binds Bool to (N1 > N2)"
  , PrimMeta "GTE_" prim_intGTE (Just ">=") "{GTE_ +N1 +N2 -Bool} binds Bool to (N1 >= N2)"
  , PrimMeta "LT_" prim_intLT (Just "<") "{LT_ +N1 +N2 -Bool} binds Bool to (N1 < N2)"
  , PrimMeta "LTE_" prim_intLTE (Just "=<") "{LTE_ +N1 +N2 -Bool} binds Bool to (N1 =< N2)"
  , PrimMeta "RecordAccess_" prim_recordAccess (Just ".") "{RecordAccess_ +Rec +Feat -Val} returns the value of feature Feat in record Rec (e.g. Rec.Feat)"
  , PrimMeta "NewCell" prim_newCell Nothing "{NewCell +Val -Cell} creates a new cell with initial value Val"
  , PrimMeta "Exchange" prim_exchange Nothing "{Exchange +Cell -Old +New} atomically updates Cell to New and returns Old"
  , PrimMeta "ByNeed" prim_byNeed Nothing "{ByNeed +Proc -Val} creates a lazy suspension that executes Proc to compute Val"
  , PrimMeta "Browse" prim_browse Nothing "{Browse +Val} prints information about Val to the console"
  , PrimMeta "IsDet" prim_isDet Nothing "{IsDet +Val ?Bool} returns true if Val is determined, false if unbound"
  , PrimMeta "NewName" prim_newName Nothing "{NewName ?Val} binds Val to a new unique name"
  , PrimMeta "ReadOnly" prim_readOnly (Just "!!") "{ReadOnly +X ?Y} binds Y to a read-only view of X"
  , PrimMeta "IsNumber" prim_isNumber Nothing "{IsNumber +Val ?Bool} returns true if Val is a number (Int or Float)"
  , PrimMeta "IsProcedure" prim_isProcedure Nothing "{IsProcedure +Val ?Bool} returns true if Val is a procedure"
  , PrimMeta "FloatDiv_" prim_divide (Just "/") "{FloatDiv_ +N1 +N2 -Res} binds Res to N1 / N2 (Float)"
  , PrimMeta "Delay" (SPrimOp prim_delay) Nothing "{Delay +N} suspends execution for N abstract steps"
  ]

-- Derived Exports
primitiveNames :: [String]
primitiveNames = map pmName primitives

globalEnv :: Env
globalEnv = zip primitiveNames [-1, -2 ..]

globalStore :: Map Int SVal
globalStore = Map.fromList $ zip [-1, -2 ..] (map pmVal primitives)

lookupGlobal :: Int -> Maybe SVal
lookupGlobal i = Map.lookup i globalStore

lookupPrimDoc :: String -> (Maybe String, String)
lookupPrimDoc name = 
    case filter (\p -> pmName p == name) primitives of
       (p:_) -> (pmSym p, pmDoc p)
       [] -> (Nothing, "No documentation available")

{---- Builder Infrastructure ----------------}

data Strictness = Strict | NonStrict | Output

lookupMixed :: SLoc -> State -> Maybe ([SLoc], SVal)
lookupMixed id s
  | id < 0 = fmap (\v -> ([id], v)) (lookupGlobal id)
  | otherwise = slookup id (sas s)

-- | interpOp
-- Handles strictness, suspension, and output binding.
interpOp :: String 
         -> [Strictness] 
         -> PrimFn -- Monadic implementation
         -> SVal
interpOp name mode impl = SPrimOp $ \s args ->
  if length args /= length mode 
  then return $ EError $ name ++ " expected " ++ show (length mode) ++ " arguments, got " ++ show (length args)
  else 
    let 
      -- Pass 1: Resolve Inputs
      resolveArgs [] [] locs vals s = 
        -- Call Implementation
        let inputs = filterInputs mode (zip (reverse locs) (reverse vals))
        in do
             res <- impl inputs s
             case res of
               Left err -> return $ EError err
               Right (s', results) -> bindOutputs args mode results s'

      resolveArgs (m:ms) (id:ids) locs vals s = case m of
        Output -> resolveArgs ms ids (id:locs) (SUnbound:vals) s
        NonStrict -> 
           case lookupMixed id s of
             Just (_, v) -> resolveArgs ms ids (id:locs) (v:vals) s
             Nothing -> resolveArgs ms ids (id:locs) (SUnbound:vals) s
        Strict ->
           let resolveStrict currId = 
                 case lookupMixed currId s of
                   Nothing -> return $ EError $ "Impossible: store missing id " ++ show currId
                   Just (_, SUnbound) -> return $ ESuspend currId
                   Just (_, SThunk tid) -> return $ ENeed tid currId
                   Just (_, SReadOnly r) -> resolveStrict r
                   Just (_, val) -> resolveArgs ms ids (currId:locs) (val:vals) s
           in resolveStrict id
      
      resolveArgs _ _ _ _ _ = return $ EError "Argument length mismatch during resolution"

      filterInputs [] _ = []
      filterInputs (Output:ms) (_:xs) = filterInputs ms xs
      filterInputs (_:ms) (x:xs) = x : filterInputs ms xs

      -- Pass 2: Bind Outputs
      bindOutputs :: [SLoc] -> [Strictness] -> [SVal] -> State -> InterpM ExecResult
      bindOutputs _ [] [] s = return $ ENext s
      bindOutputs (id:ids) (m:ms) outs s = case m of
        Output -> case outs of
          [] -> return $ EError $ "Available outputs exhausted for " ++ name
          (v:rs) -> 
              if id < 0 then return $ EError "Cannot bind global"
              else case slookup id (sas s) of
                 Just (_, SUnbound) -> 
                   do
                     let sas' = supdate (sas s) id v
                     let (w', msgs) = notifyWatchers sas' (watches s) [id]
                     liftIO $ mapM_ putStrLn msgs
                     -- Determining an output can resume threads waiting on that variable.
                     let s' = wakeThreads [id] (spawnThunks [] s{sas = sas', watches = w'})
                     bindOutputs ids ms rs s'
                 Just (vg, SThunk ts) -> 
                     let sas' = supdate (sas s) id v
                         (w', msgs) = notifyWatchers sas' (watches s) [id]
                     in do
                         liftIO $ mapM_ putStrLn msgs
                         let s' = wakeThreads [id] (spawnThunks [(ts, id)] s{sas = sas', watches = w'})
                         bindOutputs ids ms rs s'
                 Just _ ->
                    let id2:rest = newlocs s
                        sas' = ([id2],v) : (sas s)
                    in case bind sas' id id2 of
                         BFail -> return $ EError $ "Can't unify output result in " ++ name
                         BSuccess sas'' modifiedIds _ triggered det -> do
                             let (w', msgs) = notifyWatchers sas'' (watches s) modifiedIds
                             liftIO $ mapM_ putStrLn msgs
                             let s' = wakeThreads det (spawnThunks triggered s{sas=sas'', newlocs=rest, watches=w'})
                             bindOutputs ids ms rs s'
                         BSuspend id' -> return $ ESuspend id'
        _ -> bindOutputs ids ms outs s
      bindOutputs _ _ _ _ = return $ EError "Output binding mismatch"

    in resolveArgs mode args [] [] s

traceMsgs :: [String] -> InterpM a -> InterpM a
traceMsgs [] action = action
traceMsgs (m:ms) action = do
    liftIO $ putStrLn m
    traceMsgs ms action

{---- Implementations ----------------}

-- {Delay N}
-- {Delay N}
prim_delay :: PrimOp
prim_delay s args = case args of
  [nLoc] -> case slookup nLoc (sas s) of
     Just (_, SInt n) -> 
        if n <= 0 then return $ ENext s 
        else case front s of
             -- Current thread is at head of 'front'
             (tn, st):ths -> do
                 let 
                     currentEnv = case st of
                        (_, e):_ -> e
                        [] -> globalEnv

                     newId:rest = newlocs s
                     sas' = ([newId], SInt (n-1)) : sas s
                     
                     -- Bind a temporary name for the argument
                     argName = "<SystemDelayVal>"
                     env' = (argName, newId) : currentEnv
                     
                     stNew = (Apply "Delay" [argName], env') : st
                 
                 -- Update state with new allocations (preserving queues for now)
                 put s{sas=sas', newlocs=rest}
                 
                 -- Yield with the new stack
                 return $ EYield stNew 

             [] -> return $ ENext s -- Should not happen
     _ -> return $ EError "Delay expects integer"
  _ -> return $ EError "Delay args"

prim_browse = interpOp "Browse" [NonStrict] $ \args s ->
  case args of
    [(loc, val)] -> do
        let label = "_" ++ show loc
        -- Output immediate value
        liftIO $ putStrLn (label ++ ": " ++ formatPretty (sas s) (Just loc) val)
        
        -- Add to watchlist
        -- 1. Watch the root location itself (in case it is unbound or aliased later)
        -- We map loc -> (loc, label) so it acts as its own parent for label propagation?
        -- Actually, use a distinctive parent (-1) or just loc.
        let w = watches s
        let w1 = Map.insert loc (loc, label) w
        
        -- 2. Watch children deeply
        let w2 = addChildrenDeep w1 val loc label (sas s)
        
        return $ Right (s{watches=w2}, [])
    _ -> return $ Left "Args"

mixedOp :: (Integer -> Integer -> Integer) -> (Float -> Float -> Float) -> String -> PrimFn
mixedOp opI opF name args s = 
  return $ case args of
    [(_, SInt n1), (_, SInt n2)] -> Right (s, [SInt (opI n1 n2)])
    [(_, SInt n1), (_, SFloat f2)] -> Right (s, [SFloat (opF (fromIntegral n1) f2)])
    [(_, SFloat f1), (_, SInt n2)] -> Right (s, [SFloat (opF f1 (fromIntegral n2))])
    [(_, SFloat f1), (_, SFloat f2)] -> Right (s, [SFloat (opF f1 f2)])
    [(_, v1), (_, v2)] -> Left $ "Type error in " ++ name ++ ": " ++ show v1 ++ ", " ++ show v2

prim_plus = interpOp "Plus_" [Strict, Strict, Output] (mixedOp (+) (+) "Plus_")
prim_minus = interpOp "Minus_" [Strict, Strict, Output] (mixedOp (-) (-) "Minus_")
prim_multiply = interpOp "Multiply_" [Strict, Strict, Output] (mixedOp (*) (*) "Multiply_")

prim_div = interpOp "Div_" [Strict, Strict, Output] $ \args s ->
  return $ case args of
    [(_, SInt x), (_, SInt y)] -> 
       if y == 0 then Left "Division by zero"
       else Right (s, [SInt (div x y)])
    _ -> Left "Div_ requires two Integers"

prim_mod = interpOp "Mod_" [Strict, Strict, Output] $ \args s ->
  return $ case args of
    [(_, SInt x), (_, SInt y)] -> 
       if y == 0 then Left "Modulo by zero"
       else Right (s, [SInt (mod x y)])
    [(_, _), (_, _)] -> Left "Mod_ requires two Integers"

prim_intGT = interpOp "GT_" [Strict, Strict, Output] $ \args s ->
  return $ case args of
    [(_, SInt x), (_, SInt y)] -> Right (s, [boolVal (x > y)])
    [(_, SFloat x), (_, SFloat y)] -> Right (s, [boolVal (x > y)])
    _ -> Left "Type error GT"

prim_intGTE = interpOp "GTE_" [Strict, Strict, Output] $ \args s ->
  return $ case args of
    [(_, SInt x), (_, SInt y)] -> Right (s, [boolVal (x >= y)])
    [(_, SFloat x), (_, SFloat y)] -> Right (s, [boolVal (x >= y)])
    _ -> Left "Type error GTE"

prim_intLT = interpOp "LT_" [Strict, Strict, Output] $ \args s ->
  return $ case args of
    [(_, SInt x), (_, SInt y)] -> Right (s, [boolVal (x < y)])
    [(_, SFloat x), (_, SFloat y)] -> Right (s, [boolVal (x < y)])
    _ -> Left "Type error LT"

prim_intLTE = interpOp "LTE_" [Strict, Strict, Output] $ \args s ->
  return $ case args of
    [(_, SInt x), (_, SInt y)] -> Right (s, [boolVal (x <= y)])
    [(_, SFloat x), (_, SFloat y)] -> Right (s, [boolVal (x <= y)])
    _ -> Left "Type error LTE"

boolVal :: Bool -> SVal
boolVal b = SRec (if b then "true" else "false", [])

-- Deep Equality (simplified structural for now)
prim_eq = interpOp "EQ_" [Strict, Strict, Output] $ \args s ->
  -- TODO: Full structural equality
  return $ case args of
    [(_, v1), (_, v2)] -> Right (s, [boolVal (show v1 == show v2)]) 
    _ -> Left "Eq check"

prim_notEq = interpOp "NotEQ_" [Strict, Strict, Output] $ \args s ->
  -- TODO: Full structural equality
  return $ case args of
    [(_, v1), (_, v2)] -> Right (s, [boolVal (show v1 /= show v2)]) 
    _ -> Left "NotEq check"

-- {NewCell InitVal Cell}
prim_newCell = interpOp "NewCell" [Strict, Output] $ \args s ->
  return $ case args of
    [(idVal, _)] -> 
       let n = newname s
       in Right (s { newname = n + 1, muts = (n, idVal) : muts s }, [SCell n])
    _ -> Left "NewCell args"

-- {Exchange Cell Old New}
prim_exchange = interpOp "Exchange" [Strict, Output, NonStrict] $ \args s ->
  return $ case args of
    [(_, SCell n), (idNew, _)] ->
      case lookup n (muts s) of
        Nothing -> Left "Cell not found"
        Just oldId ->
           -- Lookup old value to separate it
           case lookupMixed oldId s of
             Nothing -> Left "Old value store missing" -- Possible?
             Just (_, oldVal) ->
                -- Update cell to point to New
                let muts' = mupdate (muts s) n idNew
                in Right (s{muts=muts'}, [oldVal])
    _ -> Left "Exchange Type Error"

-- {ByNeed Proc Res}
prim_byNeed = interpOp "ByNeed" [Strict, Output] $ \args s ->
  return $ case args of
    [(pid, SProc _)] -> 
       -- Create thunk pointing to Proc
       Right (s, [SThunk [pid]])
    _ -> Left "ByNeed must take a proc"


-- {IsDet Val Res} -> Res = true if det, false if unbound
-- {IsDet Val Res} -> Res = true if det, false if unbound
prim_isDet = interpOp "IsDet" [NonStrict, Output] $ \[(l1,v1)] s ->
   return $ case v1 of
     SUnbound -> Right (s, [SRec ("false", [])]) -- Not det
     SThunk _ -> Right (s, [SRec ("false", [])]) -- Suspended is not Det
     _ -> Right (s, [SRec ("true", [])])

prim_isNumber = interpOp "IsNumber" [Strict, Output] $ \[(l1,v1)] s ->
  return $ case v1 of
    SInt _ -> Right (s, [SRec ("true", [])])
    SFloat _ -> Right (s, [SRec ("true", [])])
    _ -> Right (s, [SRec ("false", [])])

prim_isProcedure = interpOp "IsProcedure" [Strict, Output] $ \[(l1,v1)] s ->
  return $ case v1 of
    SProc _ -> Right (s, [SRec ("true", [])])
    _ -> Right (s, [SRec ("false", [])])


-- {NewName Name}
prim_newName = interpOp "NewName" [Output] $ \args s ->
   return $ let n = newname s
   in Right (s { newname = n + 1 }, [SName n])

-- {ReadOnly Val Res}
prim_readOnly = interpOp "ReadOnly" [NonStrict, Output] $ \args s ->
  return $ case args of
     [(loc, _)] -> Right (s, [SReadOnly loc])
     _ -> Left "ReadOnly constraint"




prim_recordAccess :: SVal
prim_recordAccess = interpOp "RecordAccess" [Strict, Strict, Output] $ \[(ra, r), (fa, f)] s -> 
  return $ let 
    lookupField feat fs = case lookup feat fs of
         Just loc -> case slookup loc (sas s) of
               Just (_, val) -> Right (s, [val])
               Nothing -> Left "Impossible: Record field location missing"
         Nothing -> Left $ "Missing Feature '" ++ show feat ++ "' in record"
  in case r of
    SRec (lab, fields) -> case f of
        SRec (atom, []) -> lookupField (FeatAtom atom) fields
        SInt n -> lookupField (FeatInt n) fields
        _ -> Left $ "Record Feature must be an Atom or Integer, got: " ++ show f
    _ -> Left "Record Access expects a Record"

prim_divide :: SVal
prim_divide = interpOp "Divide" [Strict, Strict, Output] impl_divide

impl_divide :: PrimFn
impl_divide args s = return $ case args of
  [(_, SFloat n1), (_, SFloat n2)] -> 
    if n2 == 0 
    then Left "Division by zero"
    else Right (s, [SFloat (n1 / n2)])
  [(_, SInt n1), (_, SInt n2)] ->
    if n2 == 0 then Left "Division by zero" else
    let r = (fromInteger n1) / (fromInteger n2)
    in Right (s, [SFloat r])
  [(_, SInt n1), (_, SFloat n2)] ->
    if n2 == 0 then Left "Division by zero" else
    let r = (fromInteger n1) / n2
    in Right (s, [SFloat r])
  [(_, SFloat n1), (_, SInt n2)] ->
    if n2 == 0 then Left "Division by zero" else
    let r = n1 / (fromInteger n2)
    in Right (s, [SFloat r])
  _ -> Left "Divide expects two numbers"
