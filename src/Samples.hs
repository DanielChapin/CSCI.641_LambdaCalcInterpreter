module Samples where

import Human
import Macros (namedMacros)

importPaths :: [FilePath]
importPaths = ["../human/stl"]

parsingSample :: IO ()
parsingSample = do
  prettyParseFile "../human/class/ex1.human"

transpileSample :: IO ()
transpileSample = do
  prettyTranspileFile "../human/class/ex1.human" importPaths namedMacros

executeSample :: IO ()
executeSample = do
  prettyExecuteFile "../human/class/ex1.human" importPaths namedMacros
