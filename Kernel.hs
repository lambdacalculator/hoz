-- kernel.hs (forced recompile)
-- Defines the kernel syntax data type, along with some basic syntactic support
--   like free-variable determination and program output
-- Imports: 

module Kernel (Var, Atom, Feature(..), RecLit, PatLit(..), PVal(..), Stmt(..), DirectiveType(..), fvar_pval, fvar_stmt, fvar_stmts, showStmts, l2s, forceRecompile) where

import Data.List

import Data.Char (isLower, isAlpha)


{---- Oz Kernel Syntax ----------------------

In this simplified Oz syntax, record labels and features are atoms (strings),
where we use "1", "2", etc., for integer features and "true" and "false" for
Boolean features, sequences of statements are represented by lists, and the
skip statement doubles as a state-output statement.  -}

type Var    = String                       -- Program variables
type Atom   = String                       -- Atoms
data Feature = FeatInt Integer | FeatAtom Atom
  deriving (Eq, Ord, Show)
type RecLit = (Atom, [(Feature,Var)])      -- Record literals (label, feat/vars)

data PatLit = LRec RecLit | LInt Integer | LFloat Float
  deriving (Eq)

instance Show PatLit where
  show (LRec r) = show_rec r
  show (LInt i) = show i
  show (LFloat f) = show f

-- Program values
data PVal = PInt Integer                   -- Integer literals
          | PFloat Float                   -- Floating-point literals
          | PRec RecLit                    -- Record literals
          | PProc [Var] [Stmt]             -- Procedures (args, body)
         
-- Statements
data Stmt = Directive DirectiveType          -- Empty statement (optional output)
          | Skip                           -- Skip statement
          | Thread [Stmt]                  -- Thread introduction
          | Local [Var] [Stmt]             -- Variable introduction (multiple)
          | EqVar Var Var                  -- Variable-variable binding
          | EqVal Var PVal                 -- Variable-value binding
          | If Var [Stmt] [Stmt]           -- Conditional statement
          | Case Var PatLit [Stmt] [Stmt]  -- Pattern matching
          | Apply Var [Var]                -- Procedure application
          | TryCatch [Stmt] Var [Stmt]     -- Exception handling: try/catch
          | Raise Var                      -- Exception handling: raise

data DirectiveType = Store                      -- Skip and display store
              | Full                       -- Skip and display environ + store
              | Stack                      -- Skip and display stack
              | Globals                    -- Skip and display globals
              | Mutable                    -- Skip and display mutable store
              | Environ                    -- Skip and display current environment

-- Free variables in program values and statements (without duplicates)
-- Global primitives are included in free variables (captured from global env)

fvar_pval :: PVal -> [Var]
fvar_pval (PInt _) = []
fvar_pval (PFloat _) = []
fvar_pval (PRec (lab,fvs)) = nub $ map snd fvs
fvar_pval (PProc vs ms) = fvar_stmts ms \\ vs

fvar_stmt :: Stmt -> [Var]
fvar_stmt (Directive _) = []
fvar_stmt Skip = []
fvar_stmt (Thread ms) = fvar_stmts ms
fvar_stmt (Local vs ms) = fvar_stmts ms \\ vs
fvar_stmt (EqVar v1 v2) = nub [v1,v2]
fvar_stmt (EqVal v1 pv) = union [v1] (fvar_pval pv)
fvar_stmt (If v1 ms1 ms2) = union (fvar_stmts ms1) (v1:fvar_stmts ms2)
fvar_stmt (Case v1 lit ms1 ms2) =
  union (fvar_stmts ms1 \\ fvar_lit lit) (v1:fvar_stmts ms2)
  where
    fvar_lit (LRec (_, fvs)) = map snd fvs
    fvar_lit _ = []

fvar_stmt (Apply v1 vs) = nub (v1:vs)
fvar_stmt (TryCatch ms1 v1 ms2) = union (fvar_stmts ms1) (v1:fvar_stmts ms2)
fvar_stmt (Raise v1) = [v1]

fvar_stmts :: [Stmt] -> [Var]
fvar_stmts ms = foldl' (\xs stm -> union xs (fvar_stmt stm)) [] ms


{---- Program output ----------------}

-- Convert list of strings to a string, using delimiters
l2s :: String -> String -> String -> [String] -> String
l2s beg mid end [] = beg ++ end
l2s beg mid end [xs] = beg ++ xs ++ end
l2s beg mid end (xs:yss) = beg ++ xs ++ concatMap (mid++) yss ++ end

show_rec :: RecLit -> String
show_rec (a,fvs) = 
  let a' = if needsQuote a then "'" ++ a ++ "'" else a
  in a' ++ l2s "(" " " ")" (map show_pair fvs)
  where
    show_pair (FeatInt i, v) = show i ++ ":" ++ v
    show_pair (FeatAtom a, v) = (if needsQuote a then "'" ++ a ++ "'" else a) ++ ":" ++ v
    needsQuote "" = True
    needsQuote (c:cs) = not (isLower c && all isIdChar cs)
    isIdChar c = isAlpha c || isDigit c || c == '_'
    isDigit c = c >= '0' && c <= '9'

indent :: String -> String
indent = unlines . map ("  "++) . lines

showStmts :: [Stmt] -> String
showStmts stmts = unlines (map show stmts)

instance Show PVal where
  show (PInt n) = show n
  show (PFloat f) = show f
  show (PRec rl) = show_rec rl
  show (PProc vs stmts) = "proc {$ " ++ unwords vs ++ "}\n" ++ indent (showStmts stmts) ++ "end"

instance Show Stmt where
  show Skip = "skip"
  show (Directive Store) = "skip" -- Store not supported in parser yet, map to skip
  show (Directive Full)  = "%show full"
  show (Directive Stack) = "%show stack"
  show (Directive Globals) = "%show globals"
  show (Directive Mutable) = "%show mutable"
  show (Directive Environ) = "%show env"
  show (Thread stmts) = "thread\n" ++ indent (showStmts stmts) ++ "end"
  show (Local vs stmts) = "local " ++ unwords vs ++ " in\n" ++ indent (showStmts stmts) ++ "end"
  show (EqVar v1 v2) = v1 ++ " = " ++ v2
  show (EqVal v val)       = v ++ " = " ++ show val
  show (If v s1 s2)        = "if " ++ v ++ " then " ++ showStmts s1 ++ " else " ++ showStmts s2 ++ " end"
  show (Case v l s1 s2)    = "case " ++ v ++ " of " ++ show l ++ " then " ++ showStmts s1 ++ " else " ++ showStmts s2 ++ " end"

  show (Apply f ps) = "{" ++ f ++ " " ++ l2s "" " " "" ps ++ "}"
  show (TryCatch trys v catch) = "try\n" ++ indent (showStmts trys) ++ "catch " ++ v ++ " then\n" ++ indent (showStmts catch) ++ "end"
  show (Raise v) = "raise " ++ v


forceRecompile :: ()
forceRecompile = ()
