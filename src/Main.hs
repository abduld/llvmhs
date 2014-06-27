module Main where

import Text.Show.Pretty (ppShow)

import Control.Monad.Trans

import System.IO
import System.Environment
import System.Console.Haskeline

import qualified LLVM.General.AST as AST

initModule :: AST.Module
initModule = emptyModule "t jit"

process :: AST.Module -> String -> IO (Maybe AST.Module)
process line = do
  let res = parseToplevel line
  case res of
    Left err -> print err >> return Nothing
    Right ex -> mapM_ print ex

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "ready> "
    case minput of
      Nothing -> outputStrLn "Bye."
      Just input -> (liftIIO $ process input) >> loop
      
