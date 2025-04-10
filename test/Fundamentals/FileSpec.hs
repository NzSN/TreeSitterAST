{-# LANGUAGE OverloadedStrings #-}
module Fundamentals.FileSpec (file_spec) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit

import Template.TypeScriptTemplate (import_statement)
import Fundamentals.File (File(..), (<<-), IO_Method(..))
import Template.Template (TArray(TArray), inst)


buffer_io_method :: [String] -> IO_Method [String]
buffer_io_method initial = IO_M
  (\_ -> return $ foldl (\x y -> x ++ y) "" initial)
  (\_ content -> return $ initial ++ [content])

file_spec :: TestTree
file_spec = testCase "File Write" $
  let f = File "adf" (buffer_io_method []) []
  in (return f)
        <<- (inst import_statement (TArray ["1", "2"]) "Path")
     >>= \f' -> (show $ buffer f') @?= "[\"import { 1,2 } from 'Path';\"]"
