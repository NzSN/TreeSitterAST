{-# LANGUAGE OverloadedStrings #-}
module Template.TypeScriptTemplateSpec (template_testcase) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import Template.Template
import Template.TypeScriptTemplate

template_testcase :: TestTree
template_testcase = testGroup "Template Tests"
  [testCase "Introduction" $ do
      (inst import_statement $ TArray ["1","2"]) "Path" @?= "import { 1,2 } from 'Path';",
   testCase "Introduction 2" $ do
      (inst class_declare "CC" (Just "Base")
        -- Properties
        (Just $ TArray ["1", "2"])
        -- Methods
        (Just $ TArray ["3", "4"])) @?=
        "class CC extends Base { 1 2 3 4 }"
  ]
