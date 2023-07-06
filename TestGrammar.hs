-- File generated by the BNF Converter (bnfc 2.9.4.1).

-- | Program to test parser.

module Main where

import Prelude
  ( ($), (.)
  , Either(..)
  , Int, (>)
  , String, (++), concat, unlines
  , Show, show
  , IO, (>>), (>>=), mapM_, putStrLn
  , FilePath
  , getContents, readFile, print
  )
import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import Control.Monad      ( when )

import AbsGrammar   (P)
import LexGrammar   ( Token, mkPosToken )
import ParGrammar   ( pP, myLexer )
import PrintGrammar ( Print, printTree )
import SkelGrammar  ()
import TypeChecker ( parseTree) -- includere Type Checker
import Env

type Err        = Either String
type ParseFun a = [Token] -> Err a
type Verbosity  = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

--runFile :: (Print a, Show a) => Verbosity -> ParseFun a -> FilePath -> IO ()
runFile :: (Show env, Show infType) =>Verbosity -> ParseFun (P env infType) -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

--run :: (Print a, Show a) => Verbosity -> ParseFun a -> String -> IO ()
run ::  (Show env, Show infType) => Verbosity -> ParseFun (P env infType)-> String -> IO ()
run v p s =
  case p ts of
    Left err -> do
      putStrLn "\nParse              Failed...\n"
      putStrV v "Tokens:"
      mapM_ (putStrV v . showPosToken . mkPosToken) ts
      putStrLn err
      exitFailure
    Right tree -> do
      putStrLn "\nParse Successful!"
      -- showTree v tree
      putStrLn "\nTYPE CHECKING STARTING..."
      let (env, errors, annotatedTree) = parseTree tree defaultEnv []
      putStrLn "\nThe environment is:"
      print env
      putStrLn "\nThe errors/warnings are :"
      print errors
      putStrLn "\nThe tree is:"
      print tree
      putStrLn "\nThe annotated tree is:"
      print annotatedTree
  where
  ts = myLexer s
  showPosToken ((l,c),t) = concat [ show l, ":", show c, "\t", show t ]

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree = do
  putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
  putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    []         -> getContents >>= run 2 pP
    "-s":fs    -> mapM_ (runFile 0 pP) fs
    fs         -> mapM_ (runFile 2 pP) fs

