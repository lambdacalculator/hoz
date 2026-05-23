{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
-- parser.hs
-- Exports a parsing function that takes a read file
--  as input and parse it into the full syntax, producing either a syntax
--  tree or a list of error-message lines as output
-- Imports: full.hs

module Parser where

import Full
import Kernel (Stmt(..), PVal(..), PatLit(..), DirectiveType(..))
import qualified Kernel
import Control.Applicative hiding (many, some)
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Combinators.Expr
import Data.Functor (($>))
import Data.Text (Text)
import Data.Void
import Data.Char (isSpace)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NE
import Debug.Trace
import Text.Megaparsec.Debug
import System.IO


type Parser = Parsec Void String

-- Using state so we can store a string that determines which paradigm
-- we are using for parsing based on user input.
type ParserT a = ParsecT Void String (State String) a




-- statements choice to determine which statement parsers are being included
-- All statement parsers
pStatement :: ParserT [FStmt]
pStatement = some (notFollowedBy pTerminator >> pSingleStatement)

pSingleStatement :: ParserT FStmt
pSingleStatement = choice
  [ pSkip, pDerefBind, pBind, pLocal, pIf, pCase, pFunDef, pProcDef, pThread, pSetCell, pProcApp, pShowDirective, pTry, pRaise, pIllegalStatement ]
  <?> "Statement"

pIllegalStatement :: ParserT FStmt
pIllegalStatement = do
    e <- pExp
    startPos <- getSourcePos
    -- If we parsed an expression but expected a statement:
    fail $ "Expression used as statement at " ++ sourcePosPretty startPos ++ 
           ". Did you mean to bind it (Var = ...), use a procedure '{...}', or discard it?"

-- This is the top-level parser for the whole file.
pStatementF :: ParserT [FStmt]
pStatementF = between sc eof pStatement


pSkip :: ParserT FStmt
pSkip = do
  rwordSC "skip"
  return $ FBasicSkip

pShowDirective :: ParserT FStmt
pShowDirective = do
  char '%'
  string "show" <* sc
  choice [pShowFull, pShowStack, pShowGlobals, pShowMutable]
  where
    pShowFull = do (FDirective FFull) <$ rwordSC "full"
    pShowStack = do (FDirective FStack) <$ rwordSC "stack"
    pShowGlobals = do (FDirective FGlobals) <$ rwordSC "globals"
    pShowMutable = do (FDirective FMutable) <$ rwordSC "mutable"

-- Since pShowDirective starts with '%', it might conflict with sc if sc eats comments.
-- My change to sc ensures it doesn't eat "%show".
-- However, sc is called *after* every token. 
-- So if I have "skip %show full",
-- pSkip parses "skip", calls sc. 
-- sc sees "%show", validates it is NOT a comment. consumes space.
-- next token is '%'.
-- So pShowDirective must parse '%' then "show".
-- BUT, `string "show"` above assumes we already consumed '%'?
-- No. `pShowDirective` is a statement parser.
-- It must parse the WHOLE thing.
-- So `char '%' >> string "show"` is correct.


withErrorContext :: String -> ParserT a -> ParserT a
withErrorContext name p = do
  pos <- getSourcePos
  let ctx = "block '" ++ name ++ "'"
  region (addCtx ctx) p
  where
    addCtx c (TrivialError pos us es) = TrivialError pos us (Set.insert (Label $ NE.fromList c) es)
    addCtx _ e = e

pLocal :: ParserT FStmt
pLocal = withErrorContext "local" $ do
  rwordSC "local"
  -- fail after this point
  inStmt <- pInStatement
  rwordSC "end"
  return $ FLocal inStmt -- the variable introduction is stored in inStmt

pDerefBind :: ParserT FStmt
pDerefBind = do
  cellExpr <- try $ do charSC '@' *> pExp <* charSC '='
  valExpr <- pExp
  return $ FDerefBind cellExpr valExpr

pBind :: ParserT FStmt
pBind = do
  id1 <- try $ do pPattern <* charSC '=' -- make atomic using do/try
  -- fail after this point
  ex <- pExp
  return $ FEq id1 (Ex ex)

pIf :: ParserT FStmt
pIf = withErrorContext "if" $ do
  rwordSC "if"
  -- Fail after this point
  exp1 <- pExp
  rwordSC "then"
  inStm <- pInStatement
  nestedElseIf <- many pElseIF
  optElseCheck <- optional (rwordSC "else") -- optional else statement
  optElse <- case optElseCheck of
    (Just s) -> do Just <$> pInStatement
    Nothing  -> do Nothing <$ sc
  rwordSC "end"
  return $ FIf exp1 inStm nestedElseIf optElse
  where
    pElseIF = do
      rwordSC "elseif"
      exp1 <- pExp
      rwordSC "then"
      inStm <- pInStatement
      return (exp1, inStm)

pCase :: ParserT FStmt
pCase = withErrorContext "case" $ do
  rwordSC "case"
  -- Fail after this point
  ex <- pExp
  rwordSC "of"
  patt <- pPattern
  guard <- optional (rwordSC "andthen" >> pExp)
  rwordSC "then"
  inStmt <- pInStatement
  nestedCase <- many pNestCase
  optElseCheck <- optional (rwordSC "else")
  optElse <- case optElseCheck of
    Just s -> do Just <$> pInStatement
    Nothing  -> do Nothing <$ sc
  rwordSC "end"
  return $ FCase ex patt guard inStmt nestedCase optElse
  where
    pNestCase = do
      string "[]" <* sc
      patt <- pPattern
      guard <- optional (rwordSC "andthen" >> pExp)
      rwordSC "then"
      inStmt <- pInStatement
      return (patt, guard, inStmt)

pProcApp :: ParserT FStmt
pProcApp = do
  charSC '{'
  -- Fail after this point
  procID <- pIdentifierSC
  params <- many (try (notFollowedBy (char '}') >> pExp))
  charSC '}'
  return $ FApply procID params

-- read a proc definition and convert it to the FEq type
pProcDef :: ParserT FStmt
pProcDef = withErrorContext "proc" $ do
  rwordSC "proc"
  -- Fail after this point
  charSC '{'
  procID <- pIdentifierSC
  params <- many pPattern
  charSC '}'
  body <- pInStatement
  rwordSC "end"
  return $ FEq (PtVar procID) $ Pr params body

-- read a func definition and convert it to the FEq type
pFunDef :: ParserT FStmt
pFunDef = withErrorContext "fun" $ do
  rwordSC "fun"
  lazy <- optional (rwordSC "lazy")
  charSC '{'
  funID <- pIdentifierSC
  params <- many pPattern
  charSC '}'
  body <- pInExpression
  rwordSC "end"
  case lazy of
    Just _ -> return $ FEq (PtVar funID) $ Fn params (Nothing, [], EFunApp "ByNeed" [EFun [] body])
    Nothing -> return $ FEq (PtVar funID) $ Fn params body

pThread :: ParserT FStmt
pThread = withErrorContext "thread" $ do
  rwordSC "thread"
  -- Fail after this point
  inStmt <- pInStatement
  rwordSC "end"
  return $ FThread inStmt

pSetCell :: ParserT FStmt
pSetCell = do
  id1 <- try $ pIdentifierSC <* stringSC ":=" -- make atomic using try
  ex <- pExp
  return $ FSetCell id1 ex

pTry :: ParserT FStmt
pTry = do
  rwordSC "try"
  trys <- pInStatement
  catches <- optional $ do
      rwordSC "catch"
      first <- pClause
      rest <- many pNestClause
      return (first:rest)
  finally <- optional (rwordSC "finally" >> pInStatement)
  rwordSC "end"
  let catches' = case catches of Just c -> c; Nothing -> []
  return $ FTryCatch trys catches' finally
  where
    pClause = do
      pat <- pPattern
      rwordSC "then"
      stmt <- pInStatement
      return (pat, stmt)
    pNestClause = do
      _ <- stringSC "[]"
      pClause

pRaise :: ParserT FStmt
pRaise = do
  rwordSC "raise"
  e <- pExp
  rwordSC "end"
  return $ FRaise e


-- InStatements
data DecChoice = Bnd (Patt, Exp) | Vr FVar

-- Helper for splitting declaration choices
splitDecChoice :: [DecChoice] -> ([FVar], [(Patt, Exp)])
splitDecChoice [] = ([],[])
splitDecChoice ((Bnd b):l) = let (as,bs) = splitDecChoice l in (as,b:bs)
splitDecChoice ((Vr v):l) = let (as,bs) = splitDecChoice l in (v:as,bs)

-- The parser for a single declaration, now at the top level
pDecl :: ParserT DecChoice
pDecl = try pNamedProcDecl
    <|> try pNamedFunDecl
    <|> (Bnd <$> try ((,) <$> pPattern <* charSC '=' <*> pExp))
    <|> (Vr <$> pIdentifierSC)
  where
    pNamedProcDecl = withErrorContext "proc" $ do
      rwordSC "proc"
      charSC '{'
      procID <- pIdentifierSC
      params <- many pPattern
      charSC '}'
      body <- pInStatement
      rwordSC "end"
      return $ Bnd (PtVar procID, EProc params body)

    pNamedFunDecl = withErrorContext "fun" $ do
      rwordSC "fun"
      lazy <- optional (rwordSC "lazy")
      charSC '{'
      funID <- pIdentifierSC
      params <- many pPattern
      charSC '}'
      body <- pInExpression
      rwordSC "end"
      case lazy of
        Just _ -> return $ Bnd (PtVar funID, EFun params (Nothing, [], EFunApp "ByNeed" [EFun [] body]))
        Nothing -> return $ Bnd (PtVar funID, EFun params body)

-- The pDecls function is now much simpler
pDecls :: ParserT ([FVar], [(Patt, Exp)])
pDecls = do
  choices <- many pDecl
  return $ splitDecChoice choices

-- Your pInStatement, with the fix, will now compile successfully
pInStatement :: ParserT InStmt
pInStatement = pExStatements <|> ((Nothing,) <$> pStatement)
  where
    pExStatements = try $ do
      decls <- some pDecl
      hasIn <- optional (rwordSC "in")
      case hasIn of
        Just _ -> do
           stmts <- pStatement
           return (Just (splitDecChoice decls), stmts)
        Nothing -> do
           if any isStrictDecl decls
           then fail "Missing 'in' keyword after variable declaration(s)"
           else fail "Backtrack"

isStrictDecl :: DecChoice -> Bool
isStrictDecl (Vr _) = True
isStrictDecl _ = False

-- Expressions



pBody :: ParserT ([FStmt], Exp)
pBody =
      try (do
        stmt <- head <$> pStatementA
        (rest_stmts, ex) <- pBody
        return (stmt : rest_stmts, ex)
      )
      <|>
      (do
        ex <- pExp
        lookAhead pTerminator
        return ([], ex)
      )

pInExpression :: ParserT InExp
pInExpression = pExExpression <|> pSimpleExpression
  where
    pExExpression = try $ do
      (vars, binds) <- pDecls
      rwordSC "in"
      (stmts, ex) <- pBody
      return (Just (vars, binds), stmts, ex)
    pSimpleExpression = do
      (stmts, ex) <- pBody
      return (Nothing, stmts, ex)

pStatementA :: ParserT [FStmt]
pStatementA = do
  s <- pSingleStatement <?> "Statement"
  return [s]


rbos :: [Char]
rbos = [ '+', '-', '*', '/', '|', '<', '>' ]

opPairs :: [(String,String)]
opPairs = [("+","Plus_"),("-","Minus_"),("*","Multiply_"),("/","Divide"),("div","Div_"),("mod","Mod_"),("==","EQ_"),("\\=","NotEQ_"),("=<","LTE_"),(">=","GTE_")]

rBinOp :: ParserT String
rBinOp = (:[]) <$> satisfy (\x-> elem x rbos)
  <|> string "div"
  <|> string "mod"
  <|> string "=="
  <|> string "=<"
  <|> string "\\="
  <|> string ">="

-- remove Maybe constructors
optStmt :: [Maybe FStmt] -> [FStmt]
optStmt ((Just s):ss) = s:(optStmt ss)
optStmt (Nothing:ss) = []

pExp :: ParserT Exp
pExp = pBoolean

pBoolean :: ParserT Exp
pBoolean = do
  e1 <- pRelational
  choice
    [ do rwordSC "andthen"; e2 <- pBoolean; return $ EAndThen e1 e2
    , do rwordSC "orelse"; e2 <- pBoolean; return $ EOrElse e1 e2
    , return e1
    ]

pRelational :: ParserT Exp
pRelational = do
  e1 <- pListOp
  choice 
    [ do
        op <- choice [ string "==", string "\\=", string "=<", string "<", string ">=", string ">" ]
        sc
        e2 <- pListOp
        return $ EParen e1 e2 op
    , return e1
    ]

pListOp :: ParserT Exp
pListOp = do
  node <- pTuple
  option node $ do
     op <- (charSC '|')
     rhs <- pListOp
     return $ ERcd "|" [(FeatInt 1, node), (FeatInt 2, rhs)]

pTuple :: ParserT Exp
pTuple = do
  nodes <- sepBy1 pArithmetic (charSC '#')
  case nodes of
    [n] -> return n
    ns -> return $ ERcd "#" (zip (map FeatInt [1..]) ns)

pArithmetic :: ParserT Exp
pArithmetic = makeExprParser pDerefTerm operatorTable

pDerefTerm :: ParserT Exp
pDerefTerm = (symbol "@" *> (EAtCell <$> pDerefTerm))
         <|> (symbol "!!" *> (ERO <$> pDerefTerm))
         <|> pDotTerm

-- DOT is left associative, but binds tighter than * / + etc.
-- We handle it manually to enforce strict feature parsing (no floats on RHS)
pDotTerm :: ParserT Exp
pDotTerm = do
  t <- pTerm
  feats <- many (try (char '.' >> pStrictFeature))
  return $ foldl (\acc f -> EParen acc f "RecordAccess_") t feats

pStrictFeature :: ParserT Exp
pStrictFeature = 
      (ENum . FPInt <$> pStrictInt)
  <|> (do a <- pAtom <* sc; return $ ERcd a [])
  <|> (EVar <$> pIdentifierSC <* sc) -- sc handled in pIdentifierSC? Yes.
  <|> (pParenExp <* sc)
  <?> "Feature (Int, Atom, Var, or (Exp))"

pStrictInt :: ParserT Integer
pStrictInt = do
    i <- some digitChar
    sc 
    return (read i)


operatorTable :: [[Operator (ParsecT Void String (State String)) Exp]]
operatorTable =
  [ [ prefix "~" (\e -> EParen (ENum (FPInt 0)) e "Minus_") ]
  , [ binary "*" "*"
    , binary "/" "/"
    , binaryRW "div" "div"
    , binaryRW "mod" "mod"
    ]
  , [ binary "+" "+"
    , binary "-" "-"
    ]
  ]
  where
    binary name prim = InfixL (symbol name $> (\x y -> EParen x y prim))
    binaryRW name prim = InfixL (rword name $> (\x y -> EParen x y prim))
    prefix name f = Prefix (symbol name $> f)

pTerm :: ParserT Exp
pTerm = choice
    [ pENum, pFunApp, pEVar, pListLiteral, pRecordLiteral, pAtomLiteral, pParenExp, pLocalE, pIfE, pCaseE, pFunE, pProcE, pThreadE, pRaiseE, pAnon ]
    <?> "Term"

-- Helper for pTerm
pParenExp :: ParserT Exp
pParenExp = parens pExp

pListLiteral :: ParserT Exp
pListLiteral = do
    charSC '['
    elems <- some pExp
    charSC ']'
    return $ EList elems

-- Helper for Record Sugar logic
data RItem a = RExplicit FFeature a | RImplicit a

processRecordItems :: [RItem a] -> [(FFeature, a)]
processRecordItems items =
  let
      -- 1. Identify all explicit integer keys used
      explicitKeys :: [Integer]
      explicitKeys = [k | RExplicit (FeatInt k) _ <- items]
      
      -- 2. Assign keys
      assign :: Integer -> [RItem a] -> [(FFeature, a)]
      assign n [] = []
      assign n (i:is) = 
         case i of
           RExplicit k v -> (k,v) : assign n is
           RImplicit v -> 
              let nextN = validN n explicitKeys
              in (FeatInt nextN, v) : assign (nextN + 1) is

      validN :: Integer -> [Integer] -> Integer
      validN n used | n `elem` used = validN (n+1) used
                    | otherwise = n
      
      isDigit c = c >= '0' && c <= '9'

  in assign 1 items


pRecordLiteral :: ParserT Exp
pRecordLiteral = do
    label <- try $ do (pAtom <|> stringSC "#") <* charSC '('
    items <- many pRecItem
    charSC ')'
    let finalItems = processRecordItems items
    return $ ERcd label finalItems
  where
    pRecItem = try (do
         f <- pFeatureSC
         charSC ':'
         e <- pExp
         return $ RExplicit f e)
       <|> (RImplicit <$> pExp)


pENum :: ParserT Exp
pENum = do ENum <$> pNum <* sc

pEVar :: ParserT Exp
pEVar = do EVar <$> pIdentifierSC

pAnon :: ParserT Exp
pAnon = do
  _ <- stringSC "_"
  return EAnon

parens :: ParserT a -> ParserT a
parens = between (symbol "(") (symbol ")")

pAtomLiteral :: ParserT Exp
pAtomLiteral = do
  label <- pAtom <* sc
  return $ ERcd label []

pFunE :: ParserT Exp
pFunE = withErrorContext "fun" $ do
  rwordSC "fun"
  lazy <- optional (rwordSC "lazy")
  charSC '{'
  charSC '$'
  params <- many pPattern
  charSC '}'
  body <- pInExpression
  rwordSC "end"
  case lazy of
      Just _ -> return $ EFunApp "ByNeed" [EFun params body]
      Nothing -> return $ EFun params body

pProcE :: ParserT Exp
pProcE = withErrorContext "proc" $ do
  rwordSC "proc"
  charSC '{'
  charSC '$'
  params <- many pPattern
  charSC '}'
  body <- pInStatement
  rwordSC "end"
  return $ EProc params body

pLocalE :: ParserT Exp
pLocalE = withErrorContext "local" $ do
  rwordSC "local"
  -- fail after this point
  inExp <- pInExpression
  rwordSC "end"
  return $ ELocal inExp

pIfE :: ParserT Exp
pIfE = withErrorContext "if" $ do
  rwordSC "if"
  -- Fail after this point
  exp1 <- pExp
  rwordSC "then"
  inExp <- pInExpression
  nestedElseIf <- many pElseIF
  optElseCheck <- optional (rwordSC "else") -- optional else statement
  optElse <- case optElseCheck of
    (Just s) -> do Just <$> pInExpression
    Nothing  -> do Nothing <$ sc
  rwordSC "end"
  return $ EIf exp1 inExp nestedElseIf optElse
  where
    pElseIF = do
      rwordSC "elseif"
      exp1 <- pExp
      rwordSC "then"
      inExp <- pInExpression
      return (exp1, inExp)

pCaseE :: ParserT Exp
pCaseE = withErrorContext "case" $ do
  rwordSC "case"
  -- Fail after this point
  ex <- pExp
  rwordSC "of"
  patt <- pPattern
  guard <- optional (rwordSC "andthen" >> pExp)
  rwordSC "then"
  inExp <- pInExpression
  nestedCase <- many pNestCase
  optElseCheck <- optional (rwordSC "else")
  optElse <- case optElseCheck of
    (Just s) -> do Just <$> pInExpression
    Nothing  -> do Nothing <$ sc
  rwordSC "end"
  return $ ECase ex patt guard inExp nestedCase optElse
  where
    pNestCase = do
      try (string "[]" <* sc)
      patt <- pPattern -- Update to be more versatile
      guard <- optional (rwordSC "andthen" >> pExp)
      rwordSC "then"
      inExp <- pInExpression
      return (patt, guard, inExp)

pThreadE :: ParserT Exp
pThreadE = withErrorContext "thread" $ do
  rwordSC "thread"
  -- Fail after this point
  inExp <- pInExpression
  rwordSC "end"
  return $ EThread inExp
  
pRaiseE :: ParserT Exp
pRaiseE = do
  rwordSC "raise"
  e <- pExp
  rwordSC "end"
  return $ ERaise e

pFunApp :: ParserT Exp
pFunApp = do
  charSC '{'
  funID <- pIdentifierSC
  params <- many (try (notFollowedBy (char '}') >> pExp))
  charSC '}'
  return $ EFunApp funID params





-- Patterns

rcbos :: [Char]
rcbos = ['#','|']

rcBinOp :: ParserT Char
rcBinOp = satisfy (\x-> elem x rcbos)

pPattern :: ParserT Patt
pPattern = pPatListOp

pPatListOp :: ParserT Patt
pPatListOp = do
  node <- pPatTuple
  option node $ do
     op <- (charSC '|')
     rhs <- pPatListOp
     return $ PtParen node rhs '|'

pPatTuple :: ParserT Patt
pPatTuple = do
  nodes <- sepBy1 pPatTerm (charSC '#')
  case nodes of
    [n] -> return n
    ns -> return $ PtRec "#" (zip (map FeatInt [1..]) ns)

pPatTerm :: ParserT Patt
pPatTerm = choice
  [ try (string "_" <* sc) >> return PtAnon
  , PtVar <$> pIdentifierSC
  , pPattList
  , pPattRcd
  , pPAtom
  , pPNum
  , parens pPattern
  ] <?> "Pattern"
  where
    pPattList = do
      charSC '['
      elems <- many pPattern
      charSC ']'
      return $ PtList elems
    pPattRcd = do
      label <- try $ do (pAtom <|> stringSC "#") <* charSC '('
      items <- many pRecPattItem
      charSC ')'
      let finalItems = processRecordItems items
      return $ PtRec label finalItems

    pRecPattItem = try (do
         f <- pFeatureSC
         charSC ':'
         p <- pPattern
         return $ RExplicit f p)
       <|> (RImplicit <$> pPattern)

    pPAtom = do
      label <- pAtom <* sc
      return $ PtRec label []
    pPNum = PtNum <$> pNum <* sc


-- Basic helper functions for parsing: white space consumer and values

-- MarkKaprov tutorial (parsing a simple imperative language)
-- space consumer
-- space consumer
sc :: ParserT ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = try (string "%" >> notFollowedBy (string "show") >> L.skipLineComment "")
    blockCmnt = L.skipBlockComment "/*" "*/"

-- wrap lexeme so it parses white space after it
lexeme :: ParserT a -> ParserT a
lexeme = L.lexeme sc

-- parse string and whitespace after
symbol :: String -> ParserT String
symbol = L.symbol sc

-- char wrapped with space consumer
charSC :: Char -> ParserT Char
charSC c = char c <* sc

stringSC :: String -> ParserT String
stringSC s = string s <* sc

-- reserved word parsers followed by at least one space
rword :: String -> ParserT String
rword w = (lexeme . try) (string w <* notFollowedBy alphaNumChar)

rwordSC :: String -> ParserT String
rwordSC w = rword w <* sc

-- reserved words
rws :: [String]
rws = ["skip","local","in","end","case","of","if","else","then","proc","elseif","Garb","thread","fun","andthen","orelse","try","catch","raise","lazy","div","mod","finally"]

-- parsing an identifier
-- starts with upper case letter, cannot be a reserved word
-- no underscore, no quote names
pIdentifier :: ParserT String
pIdentifier = try (p >>= check)
  where
    p = do
        _ <- optional (char '?')
        c <- upperChar
        rest <- many (alphaNumChar <|> char '_')
        return (c:rest)
    check x = if x `elem` rws
          then fail $ "keyword " ++ show x ++ " cannot be an identifier"
          else return x

pIdentifierSC :: ParserT String
pIdentifierSC = pIdentifier <* sc <?> "Identifier" -- expected identifier

-- parsing an atom
-- starts with lower case letter, cannot be reserved word
-- OR
-- starts with single quote, ends with single quote
pAtom :: ParserT String
pAtom = try ( (p >>= check) <|> pSingleQuotes) <?> "Atom"
  where
    p = (:) <$> lowerChar <*> many (alphaNumChar <|> char '_')
    check x = if x `elem` rws
          then fail $ "keyword " ++ show x ++ " cannot be an atom"
          else return x
    pSingleQuotes = do
      x <- singleQuotes
      return x
    singleQuotes = between (char '\'') (char '\'') (many (satisfy (not . (== '\''))))

-- A parser that looks for keywords that end a statement block without consuming them.
pTerminator :: ParserT ()
pTerminator = lookAhead (void (rword "end") <|>
                         void (rword "else") <|>
                         void (rword "elseif") <|>
                         void (string "[]")) -- for case statements

-- A helper to parse exactly one statement.



-- https://mmhaskell.com/parsing-4 for number example

pNum :: ParserT FPNum
pNum = (FPFloat <$> pFloat
  <|> FPInt <$> pInteger
  <|> pNegative)  <?> "Number"
  where
    pFloat :: ParserT Float
    pFloat = try $ do
      whole <- many digitChar
      void (char '.')
      fractional <- many digitChar
      when (null fractional && null whole) $ fail "Not a number"
      -- If it's a dot access like X.1, we might have consumed the dot.
      -- But pFloat expects optional fractional. 
      -- The issue is "read" crashes if it's not a valid float string.
      -- 1. -> 1.0 (handled)
      -- .1 -> 0.1 (handled)
      -- But if we have "X.1", "pFloat" shouldn't match at the dot!
      -- pFloat is called by pNum. pNum shouldn't match ".1" if it's an operator?
      -- Actually, Oz floats must start with digit. ".1" is not a float? "0.1" is.
      -- If "whole" is empty, we must check if it's a valid float start.
      when (null whole) $ fail "Floats must start with digit"
      
      let fStr = if null fractional then whole ++ ".0" else whole ++ "." ++ fractional
      let num = read fStr
      return num
    pInteger :: ParserT Integer
    pInteger = try $ do
      int <- some digitChar
      let num = read int
      return num
    pNegative :: ParserT FPNum
    pNegative = try $ do
      void (char '~')
      num <- many digitChar
      dec <- (optional . try) (char '.')
      case dec of
        Just _ -> do
          fractional <- many digitChar
          when (null num && null fractional) $ fail "Invalid negative number"
          let fStr = if null fractional then num ++ ".0" else num ++ "." ++ fractional
          let fStr' = if null num then "0" ++ fStr else fStr
          let fnum = read fStr'
          return $ FPFloat (-1 * fnum)
        Nothing -> do
          when (null num) $ fail "Not a number"
          return $ FPInt (-1* (read num))


pLiteral :: ParserT String
pLiteral = pAtom <?> "Literal"

pFeatureSC :: ParserT FFeature
pFeatureSC = do (pFeature <* sc) <?> "Feature"

pFeature :: ParserT FFeature
pFeature =
  (FeatAtom <$> try pAtom) <|>
  do
    n1 <- digitChar
    num <- many digitChar
    return $ FeatInt (read (n1:num))

-- Kernel Syntax Parsers -----------------------------------------------------

-- Top-level parser for Kernel Program
pKernelProgram :: ParserT [Stmt]
pKernelProgram = between sc eof (some pKernelStmt)

-- Strict Kernel Statement Parser
pKernelStmt :: ParserT Stmt
pKernelStmt = choice
  [ pKShowDirective, pKSkip, pKLocal, pKIf, pKCase, pKThread, pKProcDef, pKAppOrBind, pKTry, pKRaise, pKIllegal ]
  <?> "Kernel Statement"

-- Custom error for non-kernel constructs
pKIllegal :: ParserT Stmt
pKIllegal = do
  startPos <- getSourcePos
  -- Peek at the next token to catch common sugar
  void (lookAhead (try (pExp))) -- If it looks like an expression...
  fail $ "Kernel Syntax Error at " ++ sourcePosPretty startPos ++
         ": Expressions and nesting are not allowed in Kernel syntax."

pKSkip :: ParserT Stmt
pKSkip = do
  rwordSC "skip"
  return Skip

pKShowDirective :: ParserT Stmt
pKShowDirective = do
  charSC '%'
  rwordSC "show"
  choice
    [ rwordSC "store"   >> return (Directive Store)
    , rwordSC "full"    >> return (Directive Full)
    , rwordSC "stack"   >> return (Directive Stack)
    , rwordSC "globals" >> return (Directive Globals)
    , rwordSC "mutable" >> return (Directive Mutable)
    ]

pKLocal :: ParserT Stmt
pKLocal = withErrorContext "local" $ do
  rwordSC "local"
  vars <- some pIdentifierSC
  rwordSC "in"
  stmts <- some pKernelStmt
  rwordSC "end"
  return $ Local vars stmts

pKIf :: ParserT Stmt
pKIf = withErrorContext "if" $ do
  rwordSC "if"
  v <- pIdentifierSC
  rwordSC "then"
  s1 <- some pKernelStmt
  rwordSC "else"
  s2 <- some pKernelStmt
  rwordSC "end"
  return $ If v s1 s2

pKCase :: ParserT Stmt
pKCase = withErrorContext "case" $ do
  rwordSC "case"
  v <- pIdentifierSC
  rwordSC "of"
  pat <- pKPatLit
  rwordSC "then"
  s1 <- some pKernelStmt
  rwordSC "else"
  s2 <- some pKernelStmt
  rwordSC "end"
  return $ Case v pat s1 s2

pKPatLit :: ParserT PatLit
pKPatLit = choice
  [ LInt <$> (try pInteger)
  , LFloat <$> (try pFloat)
  , pKPatRec
  ] <?> "Kernel Pattern Literal"
  where
    pInteger = do i <- some digitChar; sc; return (read i)
    pFloat = do n <- some digitChar; char '.'; f <- some digitChar; sc; return (read (n ++ "." ++ f))
    pKPatRec = do
        lbl <- pAtom <* sc
        args <- option [] $ do
            charSC '('
            pairs <- many (try (do f <- pFeatureSC; charSC ':'; v <- pIdentifierSC; return (transKFeat f,v)))
            charSC ')'
            return pairs
        return $ LRec (lbl, args)
    
    transKFeat (FeatInt i) = Kernel.FeatInt i
    transKFeat (FeatAtom a) = Kernel.FeatAtom a

pKThread :: ParserT Stmt
pKThread = withErrorContext "thread" $ do
  rwordSC "thread"
  stmts <- some pKernelStmt
  rwordSC "end"
  return $ Thread stmts

pKSetCell :: ParserT Stmt
pKSetCell = try $ do
  v1 <- pIdentifierSC
  stringSC ":="
  v2 <- pIdentifierSC
  return $ Apply "Exchange" [v1, v2, v2] -- Syntactic Sugar in Kernel? Wait. 
  -- Checking Kernel.hs: There isn't a "SetCell" Stmt. It typically maps to Exchange or Assign?
  -- Kernel.hs says: Apply Var [Var]. 
  -- Actually, in our Kernel.hs `SetCell` isn't a datatype. 
  -- Full.hs has FSetCell. Kernel.hs has no cell primitives, they are just Apply or Primitives.
  -- But wait, checking Kernel.hs again.
  -- data Stmt = ... | Apply Var [Var]
  -- There is no specific SetCell in Kernel.hs types. It is usually a Primitive Apply.
  -- `Assign` or `Exchange`. 
  -- But standard Kernel Syntax in texts usually allows `X := Y` as sugar for Exchange?
  -- No, if it's strict Kernel, it should be `{Assign C V}`.
  -- Removing `pKSetCell` and treating `:=` as illegal or just not supporting it if not in Stmt.
  -- BUT: The User's prompt implies "kernel syntax" might be the "idempotent image".
  -- If `elab` converts `X := Y` to `{Assign X Y}`, then Kernel parser should parse `{Assign X Y}`.
  -- So I will REMOVE `pKSetCell` and rely on `pKAppOrBind`.

pKProcDef :: ParserT Stmt
pKProcDef = withErrorContext "proc" $ do
  rwordSC "proc"
  charSC '{'
  v <- pIdentifierSC
  args <- many pIdentifierSC
  charSC '}'
  stmts <- some pKernelStmt
  rwordSC "end"
  return $ EqVal v (PProc args stmts)



pKAppOrBind :: ParserT Stmt
pKAppOrBind = do
  -- Could be `X = Y`, `X = V`, or `{X Y Z}`.
  choice
    [ do -- {App ...}
        charSC '{'
        v <- pIdentifierSC
        args <- many pIdentifierSC
        charSC '}'
        return $ Apply v args
    , do -- Bind
        v1 <- pIdentifierSC
        charSC '='
        choice
          [ try $ do v2 <- pIdentifierSC; return $ EqVar v1 v2
          , do val <- pKPVal; return $ EqVal v1 val
          ]
    ]

pKPVal :: ParserT PVal
pKPVal = choice
  [ PInt <$> (try pInteger)
  , PFloat <$> (try pFloat)
  , pKValRec
  -- PProc handled in top-level proc statement for `X = proc ...`?
  -- Kernel.hs says `EqVal Var PVal`, and `PVal` includes `PProc`.
  -- Syntax: `X = proc {$ ...} ... end`
  , do 
      rwordSC "proc"
      charSC '{'
      charSC '$'
      args <- many pIdentifierSC
      charSC '}'
      stmts <- some pKernelStmt
      rwordSC "end"
      return $ PProc args stmts
  ] <?> "Kernel Value"
  where
    pInteger = do i <- some digitChar; sc; return (read i)
    pFloat = do n <- some digitChar; char '.'; f <- some digitChar; sc; return (read (n ++ "." ++ f))
    pKValRec = do
        lbl <- pAtom <* sc
        args <- option [] $ do
            charSC '('
            pairs <- many (try (do f <- pFeatureSC; charSC ':'; v <- pIdentifierSC; return (transKFeat f,v)))
            charSC ')'
            return pairs
        return $ PRec (lbl, args)
    
    transKFeat (FeatInt i) = Kernel.FeatInt i
    transKFeat (FeatAtom a) = Kernel.FeatAtom a

pKTry :: ParserT Stmt
pKTry = do
  rwordSC "try"
  s1 <- some pKernelStmt
  rwordSC "catch"
  v <- pIdentifierSC
  rwordSC "then"
  s2 <- some pKernelStmt
  rwordSC "end"
  return $ TryCatch s1 v s2

pKRaise :: ParserT Stmt
pKRaise = do
  rwordSC "raise"
  v <- pIdentifierSC
  return $ Raise v
