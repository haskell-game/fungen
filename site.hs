#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
{- hakyll script, builds website in ./_site/ -}

import Control.Applicative
import Data.List
import Data.Monoid
import Debug.Trace
import Hakyll
import System.Process
import qualified Text.Blaze.Html5                 as H
import qualified Text.Blaze.Html5.Attributes      as A
import           Text.Blaze.Html                  (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String  (renderHtml)

main:: IO ()
main = do
  hakyll $ do
    match "site-templates/*" $ compile templateCompiler
    match "*.md" $ do
      route   $ setExtension "html"
      compile $ pandocCompiler >>= loadAndApplyTemplate "site-templates/site.html" defaultContext >>= relativizeUrls
    match ("site-files/**") $ do
      route   idRoute
      compile copyFileCompiler
  symlinkIndexHtml >> return ()

ensureSiteDir = system "mkdir -p _site"

symlinkIndexHtml = ensureSiteDir >> system "ln -sf README.html _site/index.html"

