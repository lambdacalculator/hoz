-- full.hs
-- Defines the full syntax data type
-- Imports:

module Full where

type FVar = String
type FAtom = String
data FFeature = FeatInt Integer | FeatAtom String
  deriving (Show, Eq, Ord)

data FPNum = FPInt Integer | FPFloat Float
  deriving (Show, Eq)

data FStmt
  = FDirective DirectiveType
  | FThread InStmt
  | FLocal InStmt
  | FEq Patt FBindRHS
  | FIf Exp InStmt [ElseIf] (Maybe InStmt)
  | FCase Exp Patt (Maybe Exp) InStmt [(Patt, Maybe Exp, InStmt)] (Maybe InStmt)
  | FApply String [Exp]
  | FSetCell FVar Exp
  | FDerefBind Exp Exp
  | FTryCatch InStmt [(Patt, InStmt)] (Maybe InStmt)
  | FRaise Exp
  | FBasicSkip
  deriving (Show, Eq)

data FBindRHS = Ex Exp | Pr [Patt] InStmt | Fn [Patt] InExp
  deriving (Show, Eq)

type InStmt = (Maybe DecPart, [FStmt])
type InExp = (Maybe DecPart, [FStmt], Exp)
type ElseIf = (Exp, InStmt)
type ElseIfExp = (Exp, InExp)
type DecPart = ([FVar], [(Patt, Exp)])

data Exp
  = ENum FPNum
  | EVar FVar
  | EList [Exp]
  | EParen Exp Exp String
  | ERcd FAtom [(FFeature, Exp)]
  | EFun [Patt] InExp
  | EProc [Patt] InStmt
  | ELocal InExp
  | EIf Exp InExp [ElseIfExp] (Maybe InExp)
  | ECase Exp Patt (Maybe Exp) InExp [(Patt, Maybe Exp, InExp)] (Maybe InExp)
  | EAndThen Exp Exp
  | EOrElse Exp Exp
  | EThread InExp
  | EAtCell Exp
  | EFunApp String [Exp]
  | ERO Exp
  | ERaise Exp
  | EAnon
  deriving (Show, Eq)

data Patt
  = PtVar FVar
  | PtList [Patt]
  | PtParen Patt Patt Char
  | PtRec FAtom [(FFeature, Patt)]
  | PtNum FPNum
  | PtAnon
  deriving (Show, Eq)

data DirectiveType
  = FStore 
  | FFull 
  | FStack
  | FGlobals
  | FMutable
  deriving (Show, Eq)
