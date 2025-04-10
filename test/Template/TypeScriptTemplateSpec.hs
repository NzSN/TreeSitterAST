{-# LANGUAGE OverloadedStrings #-}
module Template.TypeScriptTemplateSpec (template_testcase) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit

import Template.Template
import Template.TypeScriptTemplate

template_testcase :: TestTree
template_testcase = testCase "Introduction" $ do
  (inst import_statement $ TArray ["1","2"]) "Path" @?= "import { 1,2 } from 'Path';"
