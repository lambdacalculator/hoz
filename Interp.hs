module Interp where

import Kernel
import Semantics
import Primitives
import Data.List
import Debug.Trace
import qualified Data.Map.Strict as Map
import Control.Monad.Reader
import Control.Monad
import Control.Monad.State.Strict hiding (State)
import Control.Monad.IO.Class
import Data.IORef (IORef, writeIORef)

{---- Oz Semantics ----------------}
-- Types defined in Semantics.hs

-- Output current state
output_state :: Env -> MUT -> DirectiveType -> InterpM ()

output_state e muts (Store) = do
    s <- get
    liftIO $ putStrLn ("Store ({var group}: value):\n"++(store2string (sas s)))
output_state e muts (Full) = do
    s <- get
    liftIO $ putStrLn ("Store ({var group}: value):\n" ++ store2string (sas s) ++ "\n\n"++
        "Mutable Store: " ++ (mut2string muts) ++ "\n\n" ++
        "Current Environment : "++(env2string e)++"\n\n" ++
        "Stack : "++ (if null (getSt s) then "[]" else show (getSt s)) ++ "\n")
  where getSt s = if (null.head.front) s then [] else map (\(x,y)->show x) $ (snd.head.front) s
output_state e muts (Stack) = do
    s <- get
    liftIO $ putStrLn ("Stack : "++ (if null (getSt s) then "[]" else show (getSt s)) ++ "\n")
  where getSt s = if (null.head.front) s then [] else map (\(x,y)->show x) $ (snd.head.front) s
output_state e muts (Globals) = 
    liftIO $ putStrLn ("Global Environment (alphabetical):\n" ++ concatMap showGlobal (sortBy (\(n1,_) (n2,_) -> compare n1 n2) Primitives.globalEnv))
  where
    showGlobal (name, idx) = 
      let (sym, doc) = Primitives.lookupPrimDoc name
          symStr = case sym of
                     Just s -> " (" ++ s ++ ")"
                     Nothing -> ""
      in name ++ symStr ++ " --> " ++ show idx ++ ": " ++ doc ++ "\n"
output_state e muts (Mutable) = do
    liftIO $ putStrLn ("Mutable Store: " ++ (mut2string muts) ++ "\n")

-- Check state invariants
state_check :: State -> Bool
state_check s@State{front = front, back = back, sas = sas, muts = muts,
                    newname = newname, newthread = newthread} =
  let thnos = map fst $ front ++ back        -- thread numbers
      slocs = concatMap fst sas              -- SAS locations
      svals = map snd sas                    -- SAS values
      mutnames = map fst muts                -- cell names
      env_ids = concatMap (map snd) $        -- ids from enviroments ...
                 map snd (concatMap snd $ front ++ back)  -- in stacks
                 ++ [ce | SProc (_,_,ce) <- svals]        -- in closures
      rec_ids = concat [map snd fvs | SRec (_,fvs) <- svals] -- ids from recs
      thunk_ids = concat [ids | SThunk ids <- svals]  -- ids from thunks
      sas_names = [n | SName n <- svals]     -- names from SAS
      sas_cells = [n | SCell n <- svals]     -- cell names from SAS
      nodups = not . dups
  in all (\tn -> 0 < tn && tn < newthread) thnos && nodups thnos
     && all (not . null) (map fst sas) 
     && nodups slocs
     && all (\n -> 0 < n && n < newname) mutnames && nodups mutnames
     && all (`elem` slocs) (filter (>=0) (env_ids ++ rec_ids ++ thunk_ids))
     && all (`elem` mutnames) sas_cells && null (intersect sas_names sas_cells)
     && all sorted [map fst fvs | SRec (l,fvs) <- svals] 

sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [x] = True
sorted (x:y:xs) = x <= y && sorted (y:xs)


-- Execute a single statement
exec :: Stmt -> Env -> InterpM ExecResult
exec Skip e = do
    s <- get
    return $ ENext s



exec (Directive skt) e = do
    s <- get
    output_state e (muts s) skt
    return $ ENext s


exec (If v1 ms1 ms2) e = do
  s <- get
  case check_vars e [v1] of
    Left msg -> return $ EError msg
    Right [id1] -> resolveIf id1 s
  where
    resolveIf currId s =
       case lookupMixed currId s of
         Nothing -> return $ EError $ "Impossible: store missing id " ++ show currId
         Just (_, SUnbound) -> return $ ESuspend currId
         Just (_, SThunk tid) -> return $ ENeed tid currId
         Just (_, SReadOnly r) -> resolveIf r s
         Just (_, SRec ("true", _)) -> 
             let (tn,st):ths = front s
                 st' = map (tplSec e) ms1 ++ st
             in return $ ENext s{front = (tn,st'):ths}
         Just (_, SRec ("false", _)) -> 
             let (tn,st):ths = front s
                 st' = map (tplSec e) ms2 ++ st
             in return $ ENext s{front = (tn,st'):ths}
         Just _ -> return $ EError "If condition not boolean"

exec (Case v1 lit ms1 ms2) e = do
  s <- get
  case check_vars e [v1] of
    Left msg -> return $ EError msg
    Right [id1] -> resolveCase id1 s
  where
    resolveCase currId s =
       case lookupMixed currId s of
         Nothing -> return $ EError $ "Impossible: store missing id " ++ show currId
         Just (_, SUnbound) -> return $ ESuspend currId
         Just (_, SThunk tid) -> return $ ENeed tid currId
         Just (_, SReadOnly r) -> resolveCase r s
         Just (_, val) ->
           case matchVal val lit of
             Nothing -> 
                let (tn,st):ths = front s
                    st' = map (tplSec e) ms2 ++ st
                in return $ ENext s{front = (tn,st'):ths}
             Just bindings ->
                let vars = map fst bindings
                in if dups vars
                   then return $ EError "Non-linear pattern variables"
                   else 
                      let (tn,st):ths = front s
                          e' = bindings ++ e
                          st' = map (tplSec e') ms1 ++ st
                      in return $ ENext s{front = (tn,st'):ths}

exec (Thread ms) e = do
  s <- get
  let tn = newthread s
  return $ ENext s{back = (tn, map (tplSec e) ms) : back s, newthread = tn+1}

exec (Local vs ms) e = do
  s <- get
  if null vs then return $ EError "No variables in local declaration"
  else if dups vs then return $ EError "Duplicate variables in local declaration"
  else do
      let (ids, rest) = splitAt (length vs) (newlocs s)
          e' = zip vs ids ++ e
          (tn,st):ths = front s
          st' = map (tplSec e') ms ++ st
          sas' = map (\id -> ([id],SUnbound)) ids ++ sas s
      return $ ENext s{front = (tn,st'):ths, sas = sas', newlocs = rest}

exec (EqVar v1 v2) e = do
  s <- get
  case check_vars e [v1,v2] of
    Left msg -> return $ EError msg
    Right [id1,id2] -> 
      case bind (sas s) id1 id2 of
        BFail -> return $ EError $ "Can't unify " ++ show id1 ++ " and " ++ show id2
        BSuccess sas' modifiedIds _ triggered det -> 
            let (w', msgs) = notifyWatchers sas' (watches s) modifiedIds
                s' = wakeThreads det (spawnThunks triggered s{sas = sas', watches = w'})
            in traceMsgs msgs (return $ ENext s')
        BSuspend id -> return $ ESuspend id
        BNeed ts loc -> return $ ENeed ts loc

exec (EqVal v1 pv) e = do
  s <- get
  case check_vars e [v1] of
    Left msg -> return $ EError msg
    Right [id1] -> 
      case create_val e pv of
        Left msg -> return $ EError msg
        Right sv -> 
          case slookup id1 (sas s) of
            Nothing -> return $ EError $ "Impossible: store missing id " ++ show id1
            Just (_,SUnbound) -> 
              let sas' = supdate (sas s) id1 sv
                  (w', msgs) = notifyWatchers sas' (watches s) [id1]
                  -- Directly determining a variable must wake threads suspended on it.
                  s' = wakeThreads [id1] (spawnThunks [] s{sas = sas', watches = w'})
              in traceMsgs msgs (return $ ENext s')
            Just (vg, SThunk ts) ->
              let sas' = supdate (sas s) id1 sv
                  (w', msgs) = notifyWatchers sas' (watches s) [id1]
                  -- A thunk transitioning to a value triggers threads waiting on it, and its own thunk procedure
                  s' = wakeThreads [id1] (spawnThunks [(ts, id1)] s{sas = sas', watches = w'})
              in traceMsgs msgs (return $ ENext s')
            Just _ ->
              let id2:rest = newlocs s
              in case bind (([id2],sv):sas s) id1 id2 of
                   BFail ->
                     return $ EError $ "Can't unify " ++ v1 ++ " in value creation"
                   BSuccess sas'' modifiedIds _ triggered det ->
                     let (w', msgs) = notifyWatchers sas'' (watches s) modifiedIds
                         s' = wakeThreads det (spawnThunks triggered s{sas = sas'', newlocs = rest, watches = w'})
                     in traceMsgs msgs (return $ ENext s')
                   BSuspend id -> return $ ESuspend id
                   BNeed ts loc -> return $ ENeed ts loc

exec (Apply v1 vs) e = do
  s <- get
  case check_vars e (v1:vs) of
    Left msg -> return $ EError msg
    Right (id1:ids) -> 
      case lookupMixed id1 s of -- Use Mixed lookup
        Nothing -> return $ EError $ "Impossible: store missing id " ++ show id1
        Just (_,SUnbound) -> return $ ESuspend id1
        Just (_,SThunk ts) -> return $ ENeed ts id1
        Just (_,SProc (args,body,ce)) ->
          if length args /= length vs then return $ EError "Wrong number of arguments"
          else 
            let (tn,st):ths = front s
                e' = zip args ids ++ ce
                st' = map (tplSec e') body ++ st
            in return $ ENext s{front = (tn,st'):ths}
        Just (_,SPrimOp f) -> f s ids
        Just _ -> return $ EError $ "Variable " ++ v1 ++ " not bound to procedure"

exec (TryCatch [] v1 ms2) e = do
    s <- get
    return $ ENext s
exec (TryCatch ms1 v1 ms2) e = do
  s <- get
  let (tn,st):ths = front s
      st' = map (tplSec e) ms1 ++ (TryCatch [] v1 ms2,e) : st
  return $ ENext s{front = (tn,st'):ths}

exec (Raise v1) e = do
    s <- get
    unwind s (snd.head.front $ s) 
  where
    unwind :: State -> Stack -> InterpM ExecResult
    unwind s [] = return $ EError "Uncaught exception"
    unwind s ((TryCatch [] v2 ms2, e2):st) =
      case check_vars e [v1] of
        Left msg -> return $ EError msg
        Right [id1] -> do
          let (tn,_):ths = front s
              e' = (v2, id1) : e2
              st' = map (tplSec e') ms2 ++ st
          return $ ENext s{front = (tn,st'):ths}
    unwind s (_:st) = unwind s st


-- Scheduler

-- Scheduler
-- n: current quantum counter
-- progress: did any thread execute in the current pass?

sched :: Int -> Bool -> InterpM SchedResult
sched n progress = do
  cfg <- ask
  s <- get
  -- Update maxThreads
  let currThreads = length (front s) + length (back s) + (sum $ map length $ Map.elems (suspended s))
  let s_with_stats = s{maxThreads = max (maxThreads s) currThreads}
  
  -- Update statsRef for signal handling
  case statsRef cfg of
    Just ref -> liftIO $ writeIORef ref (Just s_with_stats)
    Nothing -> return ()
  
  case (front s_with_stats, back s_with_stats) of
      ([], []) -> do
          put s_with_stats
          if Map.null (suspended s_with_stats)
          then return SDone
          else return $ SError "Deadlock"
      ([], b) -> 
          do
             -- Start next pass over runnable threads.
             put s_with_stats{front=reverse b, back=[]}
             sched n False
      ((tn, st):ths, _) ->
          case st of
             [] -> do
                put s_with_stats{front=ths}
                sched n True -- Thread terminates (Progress)
             (stm, env):st' -> do
                -- Increment steps
                let s_final = s_with_stats{steps = steps s_with_stats + 1}
                put s_final{front=(tn, st'):ths} -- Pop statement before execution
                res <- exec stm env
                let sched_loop s_after = do
                       put s_after
                       case res of
                          EError msg -> return $ SError msg
                          ESuspend v -> do
                               -- Move thread to suspended map
                               let suspended' = Map.insertWith (++) v [(tn, st)] (suspended s_after)
                               put s_after{front=ths, suspended=suspended'}
                               sched n progress
                          ENeed triggers needId -> do
                               -- Trigger lazy evaluation
                               let resolveNeed currId = 
                                     case slookup currId (sas s_after) of
                                        Just (_, SThunk _) -> do
                                             let sas' = supdate (sas s_after) currId SUnbound
                                             let sTemp = s_after{sas=sas'}
                                             let s_spawned = spawnThunks [(triggers, currId)] sTemp
                                             let suspended' = Map.insertWith (++) currId [(tn, st)] (suspended s_spawned)
                                             put s_spawned{front=ths, suspended=suspended'}
                                             sched n True
                                        Just (_, SReadOnly r) -> resolveNeed r
                                        _ -> do
                                             let suspended' = Map.insertWith (++) currId [(tn, st)] (suspended s_after)
                                             put s_after{front=ths, suspended=suspended'}
                                             sched n progress
                               resolveNeed needId
                          EYield newStack -> do
                               -- Yield execution
                               -- Thread is voluntarily giving up slice (and may have modified stack)
                               -- Move to back with progress = True
                               put s_after{front=ths, back=(tn, newStack):back s_after}
                               sched n True
                          ENext s_next -> do
                              put s_next
                              case quantum cfg of
                                Infinity -> sched n True -- Progress made!
                                Finite k -> 
                                   if n >= k 
                                   then do
                                       -- Yield
                                       case front s_next of
                                         [] -> sched 1 True -- Progress made (executed step)
                                         (tn', stNew):thsNew -> do
                                            put s_next{front=thsNew, back=(tn', stNew):back s_next}
                                            sched 1 True -- Progress made (executed step)
                                   else sched (n+1) True -- Progress made!
                get >>= sched_loop

-- Helper entry points for GHCI
mkState ss = State{front = [(1,map (\x -> (x,e)) ss)], back = [], sas = [], muts = [], watches = Map.empty, suspended = Map.empty, newlocs = [1..], newname = 1, newthread = 2, steps = 0, maxThreads = 1}
  where e = Primitives.globalEnv

reportStats :: State -> IO ()
reportStats s = do
  putStrLn "\nExecution Statistics:"
  putStrLn $ "  - Statements executed:  " ++ show (steps s)
  
  when (maxThreads s > 1) $
     putStrLn $ "  - Peak threads:         " ++ show (maxThreads s)
  
  let finalThreads = length (front s) + length (back s)
  when (finalThreads > 0) $
      putStrLn $ "  - Final active threads: " ++ show finalThreads
  
  let suspCount = sum $ map length $ Map.elems (suspended s)
  when (suspCount > 0) $
      putStrLn $ "  - Suspended threads:    " ++ show suspCount
  
  putStrLn $ "  - Store size (SAS):     " ++ show (length (sas s)) ++ " entries"
  
  when (length (muts s) > 0) $
      putStrLn $ "  - Mutable cells (MUT):  " ++ show (length (muts s)) ++ " entries"

oz :: [Stmt] -> Maybe (IORef (Maybe State)) -> IO (SchedResult, State)
oz ss ref = runStateT (runReaderT (sched 1 False) (Config Infinity False ref)) (mkState ss)

toz :: ExtInt -> [Stmt] -> Maybe (IORef (Maybe State)) -> IO (SchedResult, State)
toz q ss ref = runStateT (runReaderT (sched 1 False) (Config q False ref)) (mkState ss)


-- Debugging Utils (Keep helper printers)

store2string :: SAS -> String
store2string [] = "Empty"
store2string [((x:xs),sv)] = "{"++(show x)++(concat(map (\x->","++(show x)) xs))++"}: "++(show sv)
store2string (((x:xs),sv):sas) = "{"++(show x)++(concat(map (\x->","++(show x)) xs))++"}: "++(show sv)++"\n"++(store2string sas)

env2string :: Env -> String
env2string [] = "Empty"
env2string e = 
    let valid = filter (\(_, idx) -> idx >= 0) e
    in if null valid then "Empty" 
       else "(" ++ intercalate ", " (map (\(v,i) -> show v ++ " -> " ++ show i) valid) ++ ")"

mut2string :: MUT -> String
mut2string [] = "Empty"
mut2string ((v1,s1):e) = "("++(show v1)++" -> "++(show s1)++(concat(map (\(x,y)->", "++(show x)++" -> "++(show y)) e))++")"

matchVal :: SVal -> PatLit -> Maybe [(Var, SLoc)]
matchVal (SInt i) (LInt j) | i == j = Just []
matchVal (SFloat f) (LFloat g) | f == g = Just []
matchVal (SRec (lab1, fvs1)) (LRec (lab2, fvs2))
  | lab1 /= lab2 = Nothing
  | length fvs1 /= length fvs2 = Nothing
  | otherwise = 
      let fvs2' = sortBy (\(a,_) (b,_) -> compare a b) fvs2
          (fs2, vs2) = unzip fvs2'
          (fs1, vs1) = unzip fvs1
      in if fs1 == fs2 then Just (zip vs2 vs1) else Nothing
matchVal _ _ = Nothing

hasDups :: PatLit -> Bool
hasDups (LRec (_, fvs)) = dups (map snd fvs)
hasDups _ = False
