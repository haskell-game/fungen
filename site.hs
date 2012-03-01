#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
{-
website build script
ghc --make site.hs && ./site build
-}

import Control.Arrow ((>>>)) --, (***), arr)
import Hakyll

main:: IO ()
main = hakyll $ do
    match "*.md" $ do
        route   $ setExtension "html"
        compile $ pageCompiler
            >>> applyTemplateCompiler "site.hamlet"
            >>> relativizeUrlsCompiler
    match "*.hamlet" $ compile templateCompiler
