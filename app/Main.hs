module Main where

import Control.Monad (forM, forM_)
import qualified Data.Text.IO (putStrLn)
import Data.Text.IO.Utf8 (readFile)
import Parser (parseASTWithSpans)
import System.Environment (getArgs, getProgName)
import Text.Megaparsec
import Prelude hiding (init, last, readFile)

type SourceFilePath = FilePath

type CFilePath = FilePath

data DriverCommand
  = DumpSource SourceFilePath
  | DumpAST SourceFilePath
  | DumpPoly SourceFilePath
  | DumpMono SourceFilePath
  | EmitC SourceFilePath CFilePath
  | UnrecognizedCommand String Int
  | Help

parseCmd :: [String] -> DriverCommand
parseCmd ["dump-source", path] = DumpSource path
parseCmd ["dump-ast", path] = DumpAST path
parseCmd ["dump-poly", path] = DumpPoly path
parseCmd ["dump-mono", path] = DumpMono path
parseCmd ["emit-c", input, output] = EmitC input output
parseCmd [] = Help
parseCmd ("help" : _) = Help
--
parseCmd (name : args) = UnrecognizedCommand name (length args)

execute :: DriverCommand -> IO ()
execute (DumpSource path) = do
  file <- readFile path
  putStrLn "=================================================================="
  putStrLn $ "Dumping contents of the file `" ++ path ++ "`"
  putStrLn "=================================================================="
  putStrLn ""
  Data.Text.IO.putStrLn file
  putStrLn ""
execute (DumpAST path) = do
  file <- readFile path
  putStrLn "=================================================================="
  putStrLn $ "AST for the file `" ++ path ++ "`"
  putStrLn "=================================================================="
  putStrLn ""
  case parseASTWithSpans path file of
    Left errs -> putStr $ errorBundlePretty errs
    Right ast -> print ast
execute (UnrecognizedCommand cmd arity) = do
  progName <- getProgName
  putStrLn $ progName ++ ": Unrecognized command " ++ cmd ++ "/" ++ show arity
execute Help = do
  progName <- getProgName
  putStrLn $ progName ++ " - reference compiler for Lang2 programs. List of available commands:"
  putStrLn ""
  putStrLn $ "$ " ++ progName ++ " dump-source SOURCE # Print contents of SOURCE file verbatim."
  putStrLn $ "$ " ++ progName ++ " dump-ast SOURCE # Print abstract syntax tree (ASTs) for SOURCE."
  putStrLn $ "$ " ++ progName ++ " dump-poly SOURCE # Print polymorphic IR for SOURCE"
  putStrLn $ "$ " ++ progName ++ " dump-mono SOURCE # Print monomorphic IR for SOURCE"
  putStrLn $ "$ " ++ progName ++ " emit-c SOURCE OUT.c # Emit C code for the program"
  putStrLn ""
  putStrLn "To compile a simple program (test.lang2): "
  putStrLn ""
  putStrLn $ "$ " ++ progName ++ " emit-c test.lang2 test.c # Compile test.lang2 to test.c"
  putStrLn "$ cc -o test test.c # Compile the C file with your favourite C compiler"

main' :: [String] -> IO ()
main' = execute . parseCmd

main :: IO ()
main = getArgs >>= main'
