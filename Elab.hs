-- Elab.hs
-- Exports a function that elaborates (converts) full to kernel.
-- Imports: kernel.hs, full.hs

module Elab where

import Kernel
import Full
import Data.List (union, (\\))

-- Input: list of statements from full, integer for creating new unique names
-- Output: list of statements in the kernel syntax

-- Naming. Integer cnt is passed recursively through function calls, being
-- incremented whenever a new name is created. Naming is done individually:
--    e.g., exID = "E" ++ show (cnt+1) ++ "_"
-- or in a list:
--    e.g., exIDs = ["E" ++ show (cnt+i) ++ "_"| i<-[1..(length exps)]]
-- Expressions use E_, NewCell use C_, ByNeed B_, Patterns use P_,
-- SetCell use C_ and G_ (unused variable in exchange)
-- Expressions are renamed by default so " if X then ..."
-- becomes :
--    local EXU1 in
--      EXU1 = X
--      if EXU1 then ...
--  Special case for unecessary renaming of identifiers is not handled


-- Recursively move through statements, using pattern matching to distinguish
-- statement types.
elab :: [FStmt] -> Int -> ([Stmt], Int)
elab [] cnt = ([], cnt)
elab (f:fs) cnt = (elabFront ++ elabRest, cnt'')
  where
    (elabFront, cnt') = elabS f cnt
    (elabRest, cnt'') = elab fs cnt'

elabS :: FStmt -> Int -> ([Stmt], Int)
elabS f cnt = case f of
      FBasicSkip -> ([Skip], cnt)
      FDirective dirType -> ([Directive (sTrans dirType)], cnt)
      FThread inStmt -> ([Thread (fst $ inStmtH cnt inStmt)], cnt)
      FLocal inStmt -> inStmtH cnt inStmt
      FEq (PtVar x) (Ex ex) -> (expH cnt ex x, cnt)
      FEq patt (Ex (EVar v)) -> (pattBinds ++ [EqVar mainPattVar v], cnt')
        where
          (introVars, pattVars, pattBinds, mainPattVar, cnt') = pattH cnt patt
      FEq patt (Ex ex) -> (fullK, cnt'' + 1)
        where
          (introVars, pattVars, pattBinds, mainPattVar, cnt') = pattH cnt patt
          exStmts = expH cnt' ex mainPattVar
          fullK = if null pattVars then pattBinds ++ exStmts else [Local pattVars (pattBinds ++ exStmts)]
          cnt'' = cnt' -- exStmts doesn't return count, expH needs fixing? No, expH uses relative offset? 
          -- Wait, expH uses `cnt + length pattVars` in original.
          -- expH generates fresh vars using offsets. It doesn't thread/return new count.
          -- This is separate issue. For now assuming expH uses `cnt'` base is safer than `cnt+len`.
      FEq (PtVar x) (Pr params body) -> (kStmts, cnt')
        where
           -- Process Args
           (procArgs, procLocals, procBinds, cnt') = elaborateArgs params cnt
           
           -- Process Body
           (bodyStmts, _) = inStmtH cnt' body
           
           -- Wrap bindings + body in Local scope of pattern vars
           fullBody = procBinds ++ bodyStmts
           procBody = if null procLocals then fullBody else [Local procLocals fullBody]
           
           kStmts = [EqVal x (PProc procArgs procBody)]
           
      FEq (PtVar x) (Fn params body) -> fullK
        where
          exID = "E" ++ show (cnt+1) ++ "_"
          (procArgs, procLocals, procBinds, cnt') = elaborateArgs params (cnt+1)
          
          -- Function Body Elaboration (Expression)
          (bodyStmts, _) = inExpH body cnt' exID
          
          fullBody = procBinds ++ bodyStmts
          procBody = if null procLocals then fullBody else [Local procLocals fullBody]
          
          proc = PProc (procArgs ++ [exID]) procBody
          fullK = ([EqVal x proc], cnt')


      FIf (EVar exID) inStmt [] mElse -> ([ifStmt], cnt)
        where
          (inStmts,_) = inStmtH cnt inStmt
          oElse = case mElse of
            Nothing -> [Skip]
            Just (inStmt) -> fst $ inStmtH cnt inStmt
          ifStmt = If exID inStmts oElse
      FIf ex inStmt [] mElse -> (fullK, cnt + 1)
        where
          exID = "E" ++ show (cnt+1) ++ "_"
          exStmts = expH (cnt + 1) ex exID
          (inStmts,_) = inStmtH cnt inStmt
          oElse = case mElse of
            Nothing -> [Skip]
            Just (inStmt) -> fst $ inStmtH cnt inStmt
          ifStmt = If exID inStmts oElse
          fullK = [Local [exID] (exStmts ++ [ifStmt])]
      FIf (EVar exID) inStmt ((ex2,inStmt2):elseIfs) mElse -> ([ifStmt], cnt)
        where
          (inStmts,_) = inStmtH cnt inStmt
          (nestStmts,_) = elab [(FIf ex2 inStmt2 elseIfs mElse)] (cnt+1)
          ifStmt = If exID inStmts nestStmts
      FIf ex inStmt ((ex2,inStmt2):elseIfs) mElse -> (fullK, cnt + 1)
        where
          exID = "E" ++ show (cnt+1) ++ "_"
          exStmts = expH (cnt+1) ex exID
          (inStmts,_) = inStmtH cnt inStmt
          (nestStmts,_) = elab [(FIf ex2 inStmt2 elseIfs mElse)] (cnt+1)
          ifStmt = If exID inStmts nestStmts
          fullK = [Local [exID] (exStmts ++ [ifStmt])]
      -- Simple Case: FCase Var Patt (Maybe Exp) Stmt [] Else
      FCase (EVar exID) patt guard inStmt [] mElse -> (fullK, cnt')
        where
          (introVars, pattVars, pattBinds, mainPattVar, cnt') = pattH cnt patt
          
          (thenBodyStmts, _) = inStmtH cnt inStmt
          oElse = case mElse of
            Nothing -> [Skip]
            Just (inStmt) -> fst $ inStmtH cnt inStmt
          
          finalSuccessBranch = case guard of
             Nothing -> thenBodyStmts
             Just g -> 
                 let gID = "D" ++ show cnt ++ "_"
                     gStmts = expH (cnt+1) g gID
                 in [Local [gID] (gStmts ++ [If gID thenBodyStmts oElse])]

          -- Optimization for nested case duplication
          (elseBlock, elseDef) = case reverse pattBinds of
               (_:ps) -> abstractBlock (length ps) oElse cnt "F"
               _ -> (oElse, Nothing)

          caseInner = case reverse pattBinds of
            (EqVal _ (PRec x)):ps -> 
               let recLit = x
                   pattBinds' = ps
                   nestedCases = buildNestedCases pattBinds' finalSuccessBranch elseBlock
               in [Case exID (LRec recLit) nestedCases elseBlock]
            (EqVal _ (PInt x)):ps -> 
               let lit = x
                   pattBinds' = ps
                   nestedCases = buildNestedCases pattBinds' finalSuccessBranch elseBlock
               in [Case exID (LInt lit) nestedCases elseBlock]
            (EqVal _ (PFloat x)):ps -> 
               let lit = x
                   pattBinds' = ps
                   nestedCases = buildNestedCases pattBinds' finalSuccessBranch elseBlock
               in [Case exID (LFloat lit) nestedCases elseBlock]
            [] -> 
               [EqVar exID mainPattVar] ++ finalSuccessBranch
            _ -> error "case called on non-supported pattern structure"
          
          -- Determine scope vars
          isCase = case reverse pattBinds of 
             (EqVal _ (PRec _)):_ -> True
             (EqVal _ (PInt _)):_ -> True
             (EqVal _ (PFloat _)):_ -> True
             _ -> False
          scopeVars = if isCase then introVars else introVars ++ pattVars
          
          (finalScope, finalBody) = case elseDef of
               Just def -> (scopeVars ++ ["F"++show cnt], [def]++caseInner)
               Nothing -> (scopeVars, caseInner)

          fullK = if null finalScope 
                  then finalBody 
                  else [Local finalScope finalBody]

          -- Helper duplicated for now
          buildNestedCases :: [Stmt] -> [Stmt] -> [Stmt] -> [Stmt]
          buildNestedCases [] successBranch _ = successBranch
          buildNestedCases ((EqVal var (PRec lit)):rest) successBranch failBranch =
            [Case var (LRec lit) (buildNestedCases rest successBranch failBranch) failBranch]
          buildNestedCases ((EqVal var (PInt lit)):rest) successBranch failBranch =
            [Case var (LInt lit) (buildNestedCases rest successBranch failBranch) failBranch]
          buildNestedCases ((EqVal var (PFloat lit)):rest) successBranch failBranch =
            [Case var (LFloat lit) (buildNestedCases rest successBranch failBranch) failBranch]
          buildNestedCases _ _ _ = error "Impossible: literal not supported"

      -- Recursive Case: FCase Var Patt Guard Stmt ((P2,G2,S2):ps) Else
      FCase (EVar exID) patt guard inStmt ((patt2, g2, inStmt2):opts) mElse -> (fullK, cnt')
        where
          (introVars, pattVars, pattBinds, mainPattVar, cnt') = pattH cnt patt
          
          -- Identify if this pattern supports Kernel Case optimization
          (maybeLit, pattBinds') = case reverse pattBinds of
            (EqVal _ (PRec x)):ps -> (Just (LRec x), ps)
            (EqVal _ (PInt x)):ps -> (Just (LInt x), ps)
            (EqVal _ (PFloat x)):ps -> (Just (LFloat x), ps)
            _ -> (Nothing, pattBinds) -- Not a kernel case structure (e.g. Var/Anon)

          (thenBodyStmts, _) = inStmtH cnt inStmt

          -- Continuation (the Else branch)
          (oElse, cntWithElse) = elab [(FCase (EVar exID) patt2 g2 inStmt2 opts mElse)] cnt'

          -- Success Branch Logic
          finalSuccessBranch = case guard of
             Nothing -> thenBodyStmts
             Just g -> 
                 let gID = "D" ++ show cnt ++ "_"
                     gStmts = expH (cnt+1) g gID
                 in [Local [gID] (gStmts ++ [If gID thenBodyStmts oElse])]

          -- Optimization for nested case duplication
          (elseBlock, elseDef) = case maybeLit of
               Just _ -> abstractBlock (length pattBinds') oElse cnt "F"
               Nothing -> (oElse, Nothing)

          -- Main Instruction Generation
          kBlock = case maybeLit of
            Just lit -> 
              let nestedCases = buildNestedCases pattBinds' finalSuccessBranch elseBlock
              in [Case exID lit nestedCases elseBlock]
            Nothing -> 
              pattBinds ++ finalSuccessBranch
              
          -- Determine scope vars
          scopeVars = case maybeLit of
              Just _ -> introVars
              Nothing -> introVars ++ pattVars
          
          (finalScope, finalBody) = case elseDef of
               Just def -> (scopeVars ++ ["F"++show cnt], [def]++kBlock)
               Nothing -> (scopeVars, kBlock)

          fullK = if null finalScope 
                   then finalBody 
                   else [Local finalScope finalBody]
          buildNestedCases :: [Stmt] -> [Stmt] -> [Stmt] -> [Stmt]
          buildNestedCases [] successBranch _ = successBranch
          buildNestedCases ((EqVal var (PRec lit)):rest) successBranch failBranch =
            [Case var (LRec lit) (buildNestedCases rest successBranch failBranch) failBranch]
          buildNestedCases ((EqVal var (PInt lit)):rest) successBranch failBranch =
            [Case var (LInt lit) (buildNestedCases rest successBranch failBranch) failBranch]
          buildNestedCases ((EqVal var (PFloat lit)):rest) successBranch failBranch =
            [Case var (LFloat lit) (buildNestedCases rest successBranch failBranch) failBranch]
          buildNestedCases _ _ _ = error "Impossible: literal not supported"

      -- Handle Expression Subject (FCase Exp ...) by hoisting (wrapping in Local)
      FCase ex patt guard inStmt opts mElse -> (fullK, cnt + 1)
        where
          exID = "E" ++ show (cnt+1) ++ "_"
          exStmts = expH (cnt+1) ex exID
          -- Recurse into variable version
          (coreStmts, _) = elabS (FCase (EVar exID) patt guard inStmt opts mElse) (cnt+1)
          fullK = [Local [exID] (exStmts ++ coreStmts)]
      FTryCatch s1 catches finally -> 
        case finally of
          Just sFin -> 
            -- Pattern: try ... finally SFin
            -- Elab to: local FinProc in proc {FinProc} SFin end try ... catch E then {FinProc} raise E end end {FinProc} end
            let finBase = "F"
                excID = "X" ++ show (cnt+2) ++ "_"
                (finStmtsRaw, _) = inStmtH (cnt+3) sFin
                finStmts = if null finStmtsRaw then [Skip] else finStmtsRaw
                
                -- Optimization Check (N=2)
                (finBlock, finDef) = abstractBlock 2 finStmts (cnt+1) finBase
                
                -- Inner Try
                (innerStmts, cntInner) = elabS (FTryCatch s1 catches Nothing) (cnt+3)
                
                catchBlock = finBlock ++ [Raise excID]
                mainSeq = [TryCatch innerStmts excID catchBlock] ++ finBlock
                
                fullK = case finDef of
                   Just def -> [Local [finBase ++ show (cnt+1), excID] (def : mainSeq)]
                   Nothing -> [Local [excID] mainSeq]
                
            in (fullK, cntInner) -- Use propagated count
            
          Nothing ->
            -- Pattern: try ... catch ... end (No finally)
            if null catches then inStmtH cnt s1 -- Empty catch list = just run body
            else
               let excID = "X" ++ show (cnt+1) ++ "_"
                   (bodyStmts, cnt') = inStmtH (cnt+1) s1
                   
                   -- Convert catches to Case statement on excID
                   -- clauses: [(Patt, InStmt)]
                   ((p1, s1_catch):rest) = catches
                   
                   -- Convert rest to Case options format: (Patt, Maybe Exp, InStmt)
                   opts = map (\(p, s) -> (p, Nothing, s)) rest
                   
                   -- Else branch re-raises
                   reRaise = (Nothing, [FRaise (EVar excID)])
                   
                   -- Construct FCase
                   fCase = FCase (EVar excID) p1 Nothing s1_catch opts (Just reRaise)
                   
                   -- Elaborate the Case
                   (catchBody, cntFinal) = elabS fCase cnt'
                   
               in ([TryCatch bodyStmts excID catchBody], cntFinal)
      FRaise e -> 
        let eID = "E" ++ show (cnt+1) ++ "_"
            eStmts = expH (cnt+1) e eID
        in ([Local [eID] (eStmts ++ [Raise eID])], cnt+1)
      FSetCell id1 ex -> (fullK, cnt + 1)
        where
          exID = "C" ++ show (cnt+1) ++ "_"
          id2 = "G" ++ show (cnt+1) ++ "_"
          exStmts = expH (cnt+1) ex exID
          fullK = [Local [exID,id2] (exStmts ++ [Apply "Exchange" [id1, id2, exID]])]
      FApply procID params -> (fullK, cnt + length (concat newVars))
        where
          (exIDs, exStmts, newVars) = unzip3 $ map (elabP cnt) (zip [1..] params)
          kStmt = Apply procID exIDs
          fullK = if null (concat newVars) then concat exStmts ++ [kStmt]
            else [Local (concat newVars) (concat exStmts ++ [kStmt])]


sTrans :: Full.DirectiveType -> Kernel.DirectiveType
sTrans FStore = Store
sTrans FFull = Full
sTrans FStack = Stack
sTrans FGlobals = Globals
sTrans FMutable = Mutable

inStmtH :: Int -> InStmt -> ([Stmt], Int)
inStmtH cnt (Just decPart ,fstmts) = decPartH decPart cnt (elab fstmts cnt)
inStmtH cnt (Nothing, fstmts) = elab fstmts cnt

decPartH :: DecPart -> Int -> ([Stmt], Int) -> ([Stmt], Int)
decPartH ([], binds) cnt (ss, ss_cnt) = bindsH binds cnt ss
decPartH (ids, binds) cnt (ss, ss_cnt) = ([Local ids bindStmts], cnt')
  where
    (bindStmts, cnt') = bindsH binds cnt ss

bindsH :: [(Patt, Exp)] -> Int -> [Stmt] -> ([Stmt], Int)
bindsH [] cnt ss = (ss, cnt)
bindsH ((patt, EVar v):binds) cnt ss =
    let (introVars, pattVars, pattBinds, mainPattVar, cnt') = pattH cnt patt
        vars = introVars ++ pattVars
        bindStmts = pattBinds ++ [EqVar mainPattVar v]
        (recBinds, cnt'') = bindsH binds cnt' ss
        fullB = if null vars
                then bindStmts ++ recBinds
                else [Local vars (bindStmts ++ recBinds)]
    in (fullB, cnt')
bindsH ((patt,ex):binds) cnt ss =
    let (introVars, pattVars, pattBinds, mainPattVar, cnt') = pattH cnt patt
        expBinds = expH cnt' ex mainPattVar
        vars = introVars ++ pattVars
        bindStmts = pattBinds ++ expBinds
        (recBinds, cnt'') = bindsH binds (cnt'+1) ss -- Crude approximation since expH doesn't return count
        fullB = if null vars
                then bindStmts ++ recBinds
                else [Local vars (bindStmts ++ recBinds)]
    in (fullB, cnt')

inExpH :: InExp -> Int-> String -> ([Stmt], Int)
inExpH (Just decPart, fstmts, ex) cnt expID
  = let (stmts, cnt') = elab fstmts cnt
        (binds, cnt'') = decPartH decPart cnt' (stmts ++ expH cnt' ex expID, cnt')
    in (binds, cnt'')
inExpH (Nothing, fstmts, ex) cnt expID
  = let (stmts, cnt') = elab fstmts cnt
    in (stmts ++ expH cnt' ex expID, cnt')

opPairs :: [(String,String)]
opPairs = [("+","Plus_"),("-","Minus_"),("*","Multiply_"),("/","FloatDiv_"),("<","LT_"),(">","GT_"),
  ("div","Div_"),("mod","Mod_"),("==","EQ_"),("\\=","NotEQ_"),("=<","LTE_"),(">=","GTE_"), (".", "RecordAccess_")]

expH :: Int -> Exp -> String -> [Stmt]
expH cnt ex id2bind = case ex of
  ENum (FPInt x) -> [EqVal id2bind (PInt x)]
  ENum (FPFloat x) -> [EqVal id2bind (PFloat x)]
  EVar exID -> [EqVar id2bind exID]
  EList [] -> [EqVal id2bind (PRec ("nil",[]))]
  EList exps -> [Local exIDs (exStmts ++ listStmts)]
    where
      exIDs = ["E" ++ show (cnt+i) ++ "_"| i<-[1..(length exps)]]
      exStmts = concat $ map (\(a,b) -> expH (cnt + length exps) a b) $ zip exps exIDs
      listStmts = expH (cnt + length exps) (listbuild (head exIDs) (tail exIDs)) id2bind
  ERcd label featNexps -> fullK
    where
      (feats, exps) = unzip featNexps
      (exIDs, exStmts, newVars) = unzip3 $ map (elabP cnt) (zip [1..] exps)
      lRcd = PRec (label, zip (map transFeat feats) exIDs)
      bnd = EqVal id2bind lRcd
      fullK = if null (concat newVars) then concat exStmts ++ [bnd]
        else [Local (concat newVars) (concat exStmts ++ [bnd])]
  EParen ex1 ex2 op
    | op == "#" -> expH cnt (ERcd "'#'" [(Full.FeatInt 1,ex1),(Full.FeatInt 2,ex2)]) id2bind
    | op == "|" -> expH cnt (ERcd "'|'" [(Full.FeatInt 1,ex1),(Full.FeatInt 2,ex2)]) id2bind
    | otherwise -> 
        let opName = case lookup op opPairs of Just n -> n; Nothing -> op 
        in case (ex1, ex2) of
          (EVar v1, EVar v2) -> [Apply opName [v1, v2, id2bind]]
          (ex1', EVar v2) -> 
              let ex1ID = "E" ++ show (cnt+1) ++ "_"
                  ex1Stmts = expH (cnt+2) ex1' ex1ID
              in [Local [ex1ID] (ex1Stmts ++ [Apply opName [ex1ID, v2, id2bind]])]
          (EVar v1, ex2') ->
              let ex2ID = "E" ++ show (cnt+1) ++ "_"
                  ex2Stmts = expH (cnt+2) ex2' ex2ID
              in [Local [ex2ID] (ex2Stmts ++ [Apply opName [v1, ex2ID, id2bind]])]
          (ex1', ex2') ->
              let (ex1ID,ex2ID) = ("E" ++ show (cnt+1) ++ "_","E" ++ show (cnt+2) ++ "_")
              
                  (ex1Stmts, ex2Stmts) = (expH (cnt+3) ex1' ex1ID, expH (cnt+3) ex2' ex2ID)
              in [Local [ex1ID, ex2ID] (ex1Stmts ++ ex2Stmts ++ [Apply opName [ex1ID, ex2ID, id2bind]])]
  EFunApp funID params -> fullK
    where
      (exIDs, exStmts, newVars) = unzip3 $ map (elabP cnt) (zip [1..] params)
      -- Fix Argument Order for Exchange in Function Context
      kStmt = case (funID, exIDs) of
        ("Exchange", [c, n]) -> Apply "Exchange" [c, id2bind, n] -- {Exchange C Old New}
        ("NewCell", [v]) -> Apply "NewCell" [v, id2bind] -- {NewCell Val Res}
        _ -> Apply funID (exIDs++[id2bind])
      fullK = if null (concat newVars) then concat exStmts ++ [kStmt]
        else [Local (concat newVars) (concat exStmts ++ [kStmt])]
  EFun params inExp -> [EqVal id2bind (PProc pParams pBody)]
    where
      exID = "E" ++ show (cnt+1) ++ "_"
      (procArgs, procLocals, procBinds, cnt') = elaborateArgs params (cnt+1)
      (bodyStmts, _) = inExpH inExp cnt' exID
      pParams = procArgs ++ [exID]
      
      fullBody = procBinds ++ bodyStmts
      pBody = if null procLocals then fullBody else [Local procLocals fullBody]
      
  EProc params inStmt -> [EqVal id2bind (PProc procArgs pBody)]
    where
       (procArgs, procLocals, procBinds, cnt') = elaborateArgs params cnt
       (bodyStmts, _) = inStmtH cnt' inStmt
       
       fullBody = procBinds ++ bodyStmts
       pBody = if null procLocals then fullBody else [Local procLocals fullBody]
  ERaise e ->
    let eID = "E" ++ show (cnt+1) ++ "_"
        eStmts = expH (cnt+1) e eID
    in [Local [eID] (eStmts ++ [Raise eID])]
  EAnon -> 
    let freshVar = "N" ++ show (cnt+1) ++ "_"
    in [Local [freshVar] [EqVar id2bind freshVar]]
  ELocal inExp -> fst $ inExpH inExp cnt id2bind
  EIf (EVar exID) inExp [] mElse -> fullK
    where
      (inExps,_) = inExpH inExp (cnt+1) id2bind
      oElse = case mElse of
        Nothing -> [Skip]
        Just (inExp) -> fst $ inExpH inExp (cnt+1) id2bind
      ifStmt = If exID inExps oElse
      fullK = [ifStmt]
  EIf ex inExp [] mElse -> fullK
    where
      exID = "E" ++ show (cnt+1) ++ "_"
      exStmts = expH (cnt+1) ex exID
      (inExps,_) = inExpH inExp (cnt+1) id2bind
      oElse = case mElse of
        Nothing -> [Skip]
        Just (inExp) -> fst $ inExpH inExp (cnt+1) id2bind
      ifStmt = If exID inExps oElse
      fullK = [Local [exID] (exStmts ++ [ifStmt])]
  EIf (EVar exID) inExp ((ex2,inExp2):elseIfs) mElse -> fullK
    where
      (inExps,_) = inExpH inExp (cnt+1) id2bind
      nestStmts = expH (cnt+1) (EIf ex2 inExp2 elseIfs mElse) id2bind
      ifStmt = If exID inExps nestStmts
      fullK = [ifStmt]
  EIf ex inExp ((ex2,inExp2):elseIfs) mElse -> fullK
    where
      exID = "E" ++ show (cnt+1) ++ "_"
      exStmts = expH (cnt+1) ex exID
      (inExps,_) = inExpH inExp (cnt+1) id2bind
      nestStmts = expH (cnt+1) (EIf ex2 inExp2 elseIfs mElse) id2bind
      ifStmt = If exID inExps nestStmts
      fullK = [Local [exID] (exStmts ++ [ifStmt])]
  ECase (EVar exID) patt guard inExp [] mElse -> caseStmts
    where
      (p1, p2, p3, p4, cnt') = pattH cnt patt
      -- Identify if this pattern supports Kernel Case optimization
      (maybeLit, p3') = case reverse p3 of
        (EqVal _ (PRec x)):ps -> (Just (LRec x), ps)
        (EqVal _ (PInt x)):ps -> (Just (LInt x), ps)
        (EqVal _ (PFloat x)):ps -> (Just (LFloat x), ps)
        _ -> (Nothing, p3) -- var/anon/other

      (caseBody,_) = inExpH inExp cnt id2bind
      
      -- Helper to wrap body in Local if pattern introduces variables (like PTU for Anon)
      -- p4 is the main pattern variable bound (e.g. PTU1 for Anon, or explicit var)
      -- p1 is introVars (fresh vars), p2 is pattVars (bound vars).
      -- For PtAnon, p2 has PTU. We need to declare it.
      -- Also need to declare any other vars from pattern.
      
      -- Determine scope vars
      scopeVars = case maybeLit of
          Just _ -> p1 -- introVars only
          Nothing -> p1 ++ p2 -- intro + pattVars
      
      wrappedCaseBody = if null scopeVars then caseBody else [Local scopeVars caseBody]

      oElse = case mElse of
        Nothing -> [Skip] 
        Just (inExp) -> fst $ inExpH inExp cnt' id2bind
      
      finalSuccessBranch = case guard of
             Nothing -> wrappedCaseBody
             Just g -> 
                 let gID = "D" ++ show cnt' ++ "_"
                     gStmts = expH (cnt'+1) g gID
                 in [Local [gID] (gStmts ++ [If gID wrappedCaseBody oElse])]
      
      -- Main Instruction Generation
      (elseBlock, elseDef) = case maybeLit of
           Just _ -> abstractBlock (length p3') oElse cnt "F"
           Nothing -> (oElse, Nothing)

      kBlock = case maybeLit of
        Just lit -> 
           let nestedCases = buildNestedCases p3' finalSuccessBranch elseBlock
           in [Case exID lit nestedCases elseBlock]
        Nothing -> 
           p3 ++ finalSuccessBranch
           
      caseStmts = case elseDef of
          Just def -> [Local ["F"++show cnt] ([def]++kBlock)]
          Nothing -> kBlock
      buildNestedCases :: [Stmt] -> [Stmt] -> [Stmt] -> [Stmt]
      buildNestedCases [] successBranch _ = successBranch
      buildNestedCases ((EqVal var (PRec lit)):rest) successBranch failBranch =
        [Case var (LRec lit) (buildNestedCases rest successBranch failBranch) failBranch]
      buildNestedCases _ _ _ = error "Impossible: pattBind not a record"
  
  ECase (EVar exID) patt guard inExp ((patt2, g2, inExp2):opts) mElse -> caseStmts
    where
      (p1, p2, p3, p4, cnt') = pattH cnt patt
      
      -- Identify if this pattern supports Kernel Case optimization
      (maybeLit, p3') = case reverse p3 of
        (EqVal _ (PRec x)):ps -> (Just (LRec x), ps)
        (EqVal _ (PInt x)):ps -> (Just (LInt x), ps)
        (EqVal _ (PFloat x)):ps -> (Just (LFloat x), ps)
        _ -> (Nothing, p3) -- Not a kernel case structure (e.g. Var/Anon)

      (caseBody,_) = inExpH inExp cnt id2bind
      
      oElse = expH cnt' (ECase (EVar exID) patt2 g2 inExp2 opts mElse) id2bind
      
      finalSuccessBranch = case guard of
             Nothing -> caseBody
             Just g -> 
                 let gID = "D" ++ show cnt' ++ "_"
                     gStmts = expH (cnt'+1) g gID
                 in [Local [gID] (gStmts ++ [If gID caseBody oElse])]

      -- Main Instruction Generation
      (elseBlock, elseDef) = case maybeLit of
           Just _ -> abstractBlock (length p3') oElse cnt "F"
           Nothing -> (oElse, Nothing)

      kBlock = case maybeLit of
        Just lit -> 
           let nestedCases = buildNestedCases p3' finalSuccessBranch elseBlock
           in [Case exID lit nestedCases elseBlock]
        Nothing -> 
           p3 ++ finalSuccessBranch
           
      -- Determine scope vars: If generating Kernel Case, it binds pattVars.
      -- If desugaring, we must declare them.
      scopeVars = case maybeLit of
          Just _ -> p1
          Nothing -> p1 ++ p2
          
      (finalScope, finalBody) = case elseDef of
           Just def -> (scopeVars ++ ["F"++show cnt], [def]++kBlock)
           Nothing -> (scopeVars, kBlock)
           
      caseStmts = if null finalScope 
                   then finalBody 
                   else [Local finalScope finalBody]
      buildNestedCases :: [Stmt] -> [Stmt] -> [Stmt] -> [Stmt]
      buildNestedCases [] successBranch _ = successBranch
      buildNestedCases ((EqVal var (PRec lit)):rest) successBranch failBranch =
        [Case var (LRec lit) (buildNestedCases rest successBranch failBranch) failBranch]
      buildNestedCases _ _ _ = error "Impossible: pattBind not a record"

  ECase ex patt guard inExp opts mElse -> fullK
    where
      exID = "E" ++ show (cnt+1) ++ "_"
      exStmts = expH (cnt+1) ex exID
      (coreStmts) = expH (cnt+1) (ECase (EVar exID) patt guard inExp opts mElse) id2bind 
      fullK = [Local [exID] (exStmts ++ coreStmts)]
  EAndThen e1 e2 -> expH cnt (EIf e1 (Nothing, [], e2) [] (Just (Nothing, [], ERcd "false" []))) id2bind
  EOrElse e1 e2 -> expH cnt (EIf e1 (Nothing, [], ERcd "true" []) [] (Just (Nothing, [], e2))) id2bind
  EThread inExp -> [Thread (fst $ inExpH inExp cnt id2bind)]
  EAtCell id1 -> [Apply "Exchange" [id1, id2bind, id2bind]] -- Assumes id1 is a var name. Parser enforces.
  ERO ex -> case ex of
      EVar v -> [Apply "ReadOnly" [v, id2bind]]
      _ -> let exID = "E" ++ show (cnt+1) ++ "_"
               exStmts = expH (cnt+1) ex exID
           in [Local [exID] (exStmts ++ [Apply "ReadOnly" [exID, id2bind]])]

pattH :: Int -> Patt -> ([String], [String], [Stmt], String, Int)
pattH cnt patt = case patt of
  PtVar v -> ([], [v], [], v, cnt)
  PtAnon -> ([], ["N"++show cnt++"_"], [], "N"++show cnt++"_", cnt+1)
  PtNum (FPInt n) -> ([], ["P"++show cnt++"_"], [EqVal ("P"++show cnt++"_") (PInt n)], "P"++show cnt++"_", cnt+1)
  PtNum (FPFloat n) -> ([], ["P"++show cnt++"_"], [EqVal ("P"++show cnt++"_") (PFloat n)], "P"++show cnt++"_", cnt+1)
  PtList [] -> pattH cnt (PtRec "nil" [])
  PtList (p:ps) -> pattH cnt (PtRec "|" [(Full.FeatInt 1, p), (Full.FeatInt 2, PtList ps)])
  PtParen p1 p2 op -> (p11++p21 ,p12++p22++["P"++show cnt'++"_"] ,p13++p23++[bnd], "P"++show cnt'++"_", cnt'+1)
    where
      (p11, p12, p13, p14, c1) = pattH cnt p1
      (p21, p22, p23, p24, cnt') = pattH c1 p2
      pRcd = PRec ([op], [(Kernel.FeatInt 1,p14),(Kernel.FeatInt 2,p24)])
      bnd = EqVal ("P"++show cnt'++"_") pRcd
  PtRec label featNpatts -> (concat p1s, concat p2s++["P"++show cnt'++"_"], concat p3s++[bnd], "P"++show cnt'++"_", cnt'+1)
    where
      (feats, patts) = unzip featNpatts
      
      -- Thread cnt through children
      processChildren :: Int -> [Patt] -> ([([String],[String],[Stmt],String)], Int)
      processChildren c [] = ([], c)
      processChildren c (p:ps) = 
          let (a,b,d,e, c') = pattH c p
              (rest, finalC) = processChildren c' ps
          in ((a,b,d,e):rest, finalC)

      (results, cnt') = processChildren cnt patts
      (p1s, p2s, p3s, p4s) = unZip4 results
      

      
      lRcd = PRec (label, zip (map transFeat feats) p4s)
      bnd = EqVal ("P"++show cnt'++"_") lRcd

listbuild :: String -> [String] -> Exp
listbuild h [] = ERcd "|" [(Full.FeatInt 1,EVar h),(Full.FeatInt 2,ERcd "nil" [])]
listbuild h (t:ts) = ERcd "|" [(Full.FeatInt 1,EVar h),(Full.FeatInt 2,listbuild t ts)]

-- Translation helper
transFeat :: FFeature -> Feature
transFeat (Full.FeatInt i) = Kernel.FeatInt i
transFeat (Full.FeatAtom a) = Kernel.FeatAtom a

unZip4 :: [(a, b, c, d)] -> ([a], [b], [c], [d])
unZip4 [] = ([],[],[],[])
unZip4 ((a,b,c,d):l) = (a:as,b:bs,c:cs,d:ds)
  where
    (as,bs,cs,ds) = unZip4 l


elabP :: Int -> (Int, Exp) -> (String, [Stmt], [String])
elabP cnt (i, EVar v) = (v, [], [])
elabP cnt (i, ex) = (exID, exStmts, [exID])
  where
    exID = "E" ++ show (cnt+i) ++ "_"
    exStmts = expH (cnt+i) ex exID

-- Helper to elaborate arguments:
-- Input: [Patt]
-- Output: ([Var] args, [Var] locals, [Stmt] binds, Int newCnt)
elaborateArgs :: [Patt] -> Int -> ([Var], [Var], [Stmt], Int)
elaborateArgs [] c = ([], [], [], c)
elaborateArgs (p:ps) c = 
  let (v, newLocals, newBinds, nc) = case p of
        PtVar v -> (v, [], [], c) -- Optimization/Idempotence
        _ -> let argVar = "A" ++ show c ++ "_"
                 (intro, pVars, pBinds, mainP, nextC) = pattH (c+1) p
                 binds = pBinds ++ [EqVar mainP argVar]
                 -- We return the generated locals and statements, NOT wrapped in Local
                 -- intro is for temp vars in pattern match, pVars are the pattern vars (K)
             in (argVar, intro++pVars, binds, nextC) 
      (vs, restLocals, restBinds, finalC) = elaborateArgs ps nc
  in (v:vs, newLocals ++ restLocals, newBinds ++ restBinds, finalC)


-- Heuristic Optimization Helpers

stmtSize :: [Stmt] -> Int
stmtSize stmts = sum (map sSize stmts)
  where
    sSize :: Stmt -> Int
    sSize (Local _ s) = 1 + stmtSize s
    sSize (Thread s) = 1 + stmtSize s
    sSize (If _ s1 s2) = 1 + stmtSize s1 + stmtSize s2
    sSize (Case _ _ s1 s2) = 1 + stmtSize s1 + stmtSize s2
    sSize (TryCatch s1 _ s2) = 1 + stmtSize s1 + stmtSize s2
    sSize (EqVal _ (PProc _ s)) = 1 + stmtSize s
    sSize (EqVal _ _) = 1
    sSize (EqVar _ _) = 1
    sSize (Apply _ _) = 1
    sSize (Raise _) = 1
    sSize Skip = 1
    sSize (Directive _) = 0

-- Returns (blockToUse, optionalDefinition)
-- If optimization applies: returns ([Apply name], Just (EqVal name (PProc [] originalBlock)))
-- If not: returns (originalBlock, Nothing)
abstractBlock :: Int -> [Stmt] -> Int -> String -> ([Stmt], Maybe Stmt)
abstractBlock duplicationCount block cnt nameBase =
    let size = stmtSize block
        -- Abstract if S + 1 < N*(S-1) (from discussion: S > 3 for N=2)
        -- Simplified: If size > 3 or (N is large and size > 1)
        doAbstract = if duplicationCount >= 5 then size > 1 -- High duplication, aggressive optimization
                     else size > 3                          -- Low duplication (finally), conservative
    in if doAbstract
       then let procName = nameBase ++ show cnt
                def = EqVal procName (PProc [] block)
            in ([Apply procName []], Just def)
       else (block, Nothing)
