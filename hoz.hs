-- hoz.hs
--
-- One-time setup for students:
-- Recommended: Run `cabal repl` to start GHCi with all dependencies loaded.
--
-- Manual Setup (if not using Cabal):
--   cabal install --lib megaparsec mtl text parser-combinators
--   Then in GHCi: :set -package parser-combinators
--
-- This file is the main entry point pipeline: parser -> elab -> interp
--
-- Main executable that pipelines parser.hs -> elab.hs -> interp.hs
-- Imports: interp.hs, parser.hs, elab.hs (other modules for running the parser)

module Hoz where 

import Interp
import Parser
import Elab
import Kernel
import Semantics
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Control.Monad
import Control.Monad.State.Strict hiding (State)
import Debug.Trace
import System.Environment
import System.Exit
import qualified Data.List as L
import Control.Exception (catch, AsyncException(UserInterrupt), throwIO)
import Data.IORef

import System.IO (hSetBuffering, stderr, stdout, BufferMode(..))

main :: IO ()
main = do
  hSetBuffering stderr NoBuffering
  hSetBuffering stdout NoBuffering
  args <- getArgs
  statsRef <- newIORef Nothing
  
  let handleInterrupt :: AsyncException -> IO ()
      handleInterrupt UserInterrupt = do
        ms <- readIORef statsRef
        case ms of
          Just s -> reportStats s
          Nothing -> putStrLn "\nInterrupted before execution started."
        exitFailure
      handleInterrupt e = throwIO e

  catch (runHoz args statsRef) handleInterrupt

runHoz :: [String] -> IORef (Maybe State) -> IO ()
runHoz args statsRef = case parseArgs args of
    Just (opts, [inF], mbOut) -> do
       let q = case lookup "quantum" opts of
                 Just n -> Finite (read n)
                 Nothing -> Infinity
       let outF = case mbOut of
                    Just f -> Just f
                    Nothing -> defaultOutFile inF
       
       let check = case lookup "check" opts of
                     Just "true" -> True
                     _ -> False
       
       case outF of
         Just f -> 
            if isKernelMode opts 
            then runKernelT q check inF f statsRef
            else runFullT q check inF f statsRef
         Nothing -> 
            if isKernelMode opts
            then runKernelOnlyT q check inF statsRef
            else runOnlyT q check inF statsRef
    
    Just (opts, [inF, outF], _) -> do 
       let q = case lookup "quantum" opts of
                 Just n -> Finite (read n)
                 Nothing -> Infinity
       let check = case lookup "check" opts of
                     Just "true" -> True
                     _ -> False
       if isKernelMode opts
       then runKernelT q check inF outF statsRef
       else runFullT q check inF outF statsRef

    _ -> do
      putStrLn "Usage: hoz [-q quantum] [-k|--kernel] [-c|--check] [-h|--help] <input.oz> [output.ozk]"
      putStrLn "  -q <n>       : Set quantum for preemptive round-robin scheduling (default: infinity)"
      putStrLn "  -k / --kernel: Enforce Strict Kernel Syntax (bypasses elaboration)"
      putStrLn "  -c / --check : Run state invariant checks after execution"
      putStrLn "  -h / --help  : Print this help message"
      if "-h" `elem` args || "--help" `elem` args
        then exitSuccess
        else exitFailure

isKernelMode :: [(String, String)] -> Bool
isKernelMode opts = case lookup "kernel" opts of
                      Just "true" -> True
                      _ -> False

-- Determine default output file. Returns Nothing if no output should be generated (.ozk input or stdin).
defaultOutFile :: String -> Maybe String
defaultOutFile fname
  | fname == "-" = Nothing
  | ".ozk" `L.isSuffixOf` fname = Nothing
  | otherwise = Just (stripExt fname ++ ".ozk")
  
stripExt :: String -> String
stripExt s = if '.' `elem` s
             then reverse $ tail $ dropWhile (/= '.') $ reverse s
             else s

parseArgs :: [String] -> Maybe ([(String, String)], [String], Maybe String)
parseArgs [] = Just ([], [], Nothing)
parseArgs ("-q":n:rest) = case parseArgs rest of
  Just (opts, files, out) -> Just (("quantum", n):opts, files, out)
  Nothing -> Nothing
-- Also support --quantum
parseArgs ("--quantum":n:rest) = case parseArgs rest of
  Just (opts, files, out) -> Just (("quantum", n):opts, files, out)
  Nothing -> Nothing

-- Support -k / --kernel
parseArgs ("-k":rest) = case parseArgs rest of
  Just (opts, files, out) -> Just (("kernel", "true"):opts, files, out)
  Nothing -> Nothing
parseArgs ("--kernel":rest) = case parseArgs rest of
  Just (opts, files, out) -> Just (("kernel", "true"):opts, files, out)
  Nothing -> Nothing

-- Support -c / --check
parseArgs ("-c":rest) = case parseArgs rest of
  Just (opts, files, out) -> Just (("check", "true"):opts, files, out)
  Nothing -> Nothing
parseArgs ("--check":rest) = case parseArgs rest of
  Just (opts, files, out) -> Just (("check", "true"):opts, files, out)
  Nothing -> Nothing

parseArgs ("-h":_) = Nothing
parseArgs ("--help":_) = Nothing
parseArgs ("-":rest) = case parseArgs rest of
   Just (opts, files, out) -> Just (opts, "-":files, out)
   Nothing -> Nothing
parseArgs (('-':_):_) = Nothing

parseArgs (x:rest) = case parseArgs rest of
   Just (opts, files, out) -> Just (opts, x:files, out)
   Nothing -> Nothing

-- Run without writing output file
runOnlyT :: ExtInt -> Bool -> String -> IORef (Maybe State) -> IO ()
runOnlyT q check inF ref = do  
  input <- readInput inF
  let (s,n) = runState (runParserT pStatementF "" input) ""
  case s of
    (Left bundle) -> putStr (errorBundlePretty bundle)
    (Right xs) -> do 
      let (elabData, _) = elab xs 0

      res <- toz_p q check elabData ref
      case res of
        Left err -> putStrLn $ "Execution Error: " ++ err
        Right _ -> return ()

runFull :: String -> String -> IO ()
runFull inF outF = do 
  input <- readInput inF
  statsRef <- newIORef Nothing
  let (s,n) = runState (runParserT pStatementF "" input) "" -- set state with empty string
  case s of
    (Left bundle) -> putStr (errorBundlePretty bundle)
    (Right xs) -> do 
      let (elabData, _) = elab xs 0
      writeFile outF (showStmts elabData)
      res <- oz_p elabData statsRef
      case res of
        Left err -> putStrLn $ "Execution Error: " ++ err
        Right _ -> return ()

runFullT :: ExtInt -> Bool -> String -> String -> IORef (Maybe State) -> IO ()
runFullT q check inF outF ref = do  
  input <- readInput inF
  let (s,n) = runState (runParserT pStatementF "" input) ""  -- set state with empty string
  case s of
    (Left bundle) -> putStr (errorBundlePretty bundle)
    (Right xs) -> do 
      let (elabData, _) = elab xs 0
      writeFile outF (showStmts elabData)

      res <- toz_p q check elabData ref
      case res of
        Left err -> putStrLn $ "Execution Error: " ++ err
        Right _ -> return ()

-- Kernel Execution Modes
runKernelT :: ExtInt -> Bool -> String -> String -> IORef (Maybe State) -> IO ()
runKernelT q check inF outF ref = do
  input <- readInput inF
  let (s,n) = runState (runParserT pKernelProgram "" input) ""
  case s of
    (Left bundle) -> putStr (errorBundlePretty bundle)
    (Right stmts) -> do
       writeFile outF (showStmts stmts) -- Still write output? Yes, maybe just pretty printed.
       res <- toz_p q check stmts ref
       case res of
         Left err -> putStrLn $ "Execution Error: " ++ err
         Right _ -> return ()

runKernelOnlyT :: ExtInt -> Bool -> String -> IORef (Maybe State) -> IO ()
runKernelOnlyT q check inF ref = do
  input <- readInput inF
  let (s,n) = runState (runParserT pKernelProgram "" input) ""
  case s of
    (Left bundle) -> putStr (errorBundlePretty bundle)
    (Right stmts) -> do
       res <- toz_p q check stmts ref
       case res of
         Left err -> putStrLn $ "Execution Error: " ++ err
         Right _ -> return ()

readInput :: String -> IO String
readInput "-" = getContents
readInput f = readFile f

-- Example execution
-- ghci> :load hoz.hs
-- hoz> runFull "declarative" "inFile.txt" "outFile.txt"
-- hoz> runFullT (Finite 3) "declarative" "inFile.txt" "outFile.txt"


-- choice to determine which statement parsers are being included
  -- Parsing option input:
  --"functional" -> funcStmts
  --"declarative" -> declStmts
  --"declarative threaded" -> declThreadStmts
  --"stateful" -> stateStmts
  --"stateful threaded" -> stateThreadStmts
  
  --  funcStmts = [pSkip, pBind, pLocal, pIf, pCase, pFunDef]
  --  declStmts = funcStmts ++ [pProcApp, pProcDef]
  --  declThreadStmts = declStmts ++ [pThread, pByNeed]
  --  stateStmts = declStmts ++ [pExchange, pNewCell, pSetCell]
  --  stateThreadStmts = stateStmts ++ [pThread, pByNeed]

-- Input: parsing option, input text file, output text file
-- Output: Parsing error Or 
--         Program ouput to terminal and elaborated kernel syntax to out file


-- Display errors for either non-threaded or threaded option for the interpreter
oz_p :: [Stmt] -> IORef (Maybe State) -> IO (Either String ())
oz_p s ref = do
  (res, finalState) <- oz s (Just ref)
  reportStats finalState
  case res of
    SDone -> return $ Right ()
    SError st -> return $ Left st

toz_p :: ExtInt -> Bool -> [Stmt] -> IORef (Maybe State) -> IO (Either String ())
toz_p q check s ref = do
  (res, finalState) <- toz q s (Just ref)
  reportStats finalState
  when check $ do
      let valid = state_check finalState
      putStrLn $ "State Invariant Check: " ++ show valid
  case res of
    SDone -> return $ Right ()
    SError st -> return $ Left st
