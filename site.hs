#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
{- hakyll script, builds website in ./_site/ -}

import Control.Arrow ((>>>))
import Hakyll

main:: IO ()
main = hakyll $ do
  match "*.hamlet" $ compile templateCompiler
  match "*.md" $ do
    route   $ setExtension "html"
    compile $ pageCompiler >>> applyTemplateCompiler "site.hamlet" >>> relativizeUrlsCompiler
  match "old-site/**" $ do
    route   idRoute
    compile copyFileCompiler
