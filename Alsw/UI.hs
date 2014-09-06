{-# LANGUAGE OverloadedStrings #-}
module Alsw.UI
  ( Config
  , defaultConfig
  , constructUI
  )
where

import Graphics.Vty.Widgets.All
import Graphics.Vty.LLInput
import Graphics.Vty.Attributes

import Control.Monad (forM_)
import qualified Data.Text as T
import Text.Regex.Posix
import System.Exit

import Alsw.Source

type ListWidget = Widget (List T.Text FormattedText)

callAction :: ListWidget -> (T.Text -> IO ()) -> IO ()
callAction lst act = do
  m <- getSelected lst
  case m of
    Just (_, (s, _)) -> act s
    Nothing          -> exitFailure -- TODO error handling

renderList :: ListWidget -> [T.Text] -> IO ()
renderList lst src = do
  clearList lst
  forM_ src $ \ l ->
    (addToList lst l =<< plainText l)

renderFilteredList :: (Source a) => ListWidget -> Regex -> a -> IO ()
renderFilteredList lst reg srcs = do
  let fsrc = filter (\ src -> match reg (T.unpack src) :: Bool) $ getSource srcs
  renderList lst fsrc

data Config = Config { infoAttr :: Attr
                     , errAttr :: Attr
                     , listAttr :: Attr
                     , titleString :: T.Text
                     , promptString :: T.Text
                     , footerString :: T.Text
                     }

defaultConfig :: Config
defaultConfig = Config { infoAttr = (black `on` white)
                       , errAttr = (white `on` red)
                       , listAttr = (fgColor white)
                       , titleString = "alsw: anything.el-like selector widget"
                       , promptString = "> "
                       , footerString = "source: "
                       }

constructUI :: (Source a) => a -> Config -> IO ()
constructUI srcs cfg = do
  let mainAction = head $ getActions srcs

  -- Header
  header <- (plainText $ titleString cfg) <++> (hFill ' ' 1)
  setNormalAttribute header (infoAttr cfg)

  -- Selector
  lst <- newTextList (listAttr cfg) (getSource srcs) 1
  prompt <- plainText $ promptString cfg
  e <- editWidget
  selector <- (return prompt) <++> (return e) <--> (return lst)
  selector `relayKeyEvents` e

  -- Footer
  errmsg <- plainText ""
  fw <- plainText $ T.append (footerString cfg) (getSourceName srcs)
  footer <- (return errmsg) <++> (return fw) <++> (hFill ' ' 1)
  setNormalAttribute footer (infoAttr cfg)

  -- Focus Group
  ui <- (return header) <--> (return selector) <--> (return footer)
  fg <- newFocusGroup
  _ <- addToFocusGroup fg selector

  -- Keymap
  e `onChange` \ s -> do
    let mreg = makeRegexM (T.unpack s) :: Maybe Regex
    case mreg of
      Nothing -> do
        setNormalAttribute footer (errAttr cfg)
        setText errmsg "**Invalid regular expression** "
      Just reg -> do
        setNormalAttribute footer (infoAttr cfg)
        setText errmsg ""
        renderFilteredList lst reg srcs

  selector `onKeyPressed` \ _ key mods ->
    case (key, mods) of
      (KASCII 'p', [MCtrl]) -> scrollUp lst >> return True
      (KASCII 'n', [MCtrl]) -> scrollDown lst >> return True
      (KEnter, []) -> callAction lst mainAction >> return True
      _ -> return False

  fg `onKeyPressed` \ _ key mods ->
    case (key, mods) of
      (KASCII 'g', [MCtrl]) -> exitSuccess
      (KEsc, [])            -> exitSuccess
      _                     -> return False

  -- Collection
  c <- newCollection
  _ <- addToCollection c ui fg

  -- Run
  runUi c defaultContext
