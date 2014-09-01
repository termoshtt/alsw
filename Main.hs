{-# LANGUAGE OverloadedStrings #-}

import System.Exit
import System.IO

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Graphics.Vty.Widgets.All
import Graphics.Vty.LLInput
import Graphics.Vty.Attributes


stdoutAction :: T.Text -> IO ()
stdoutAction s = TIO.hPutStr stdout s

main :: IO()
main = do
  -- configures
  let infoAttr = (black `on` white) -- attribute for header and footer
  let listAttr = (fgColor white)
  let titleString = "alsw: anything.el-like selector widget"
  let promptString = "> "
  let footerString = "source: "
  -- let sources = take 1000 $ repeat $ T.pack "homhom"
  let sources = ["homhom", "madoka", "saki"]
  let sourceName = "sample"

  -- Header
  header <- (plainText titleString) <++> (hFill ' ' 1)
  setNormalAttribute header infoAttr

  -- Selector
  lst <- newTextList listAttr sources 1
  prompt <- plainText promptString
  e <- editWidget
  selector <- (return prompt) <++> (return e) <--> (return lst)
  selector `relayKeyEvents` e

  -- Footer
  fw <- plainText (footerString `T.append` sourceName)
  footer <- (return fw) <++> (hFill ' ' 1)
  setNormalAttribute footer infoAttr

  -- Focus Group
  ui <- (return header) <--> (return selector) <--> (return footer)
  fg <- newFocusGroup
  _ <- addToFocusGroup fg selector

  -- Keymap
  selector `onKeyPressed` \ _ key mods ->
    case (key, mods) of
      (KASCII 'p', [MCtrl]) -> scrollUp lst >> return True
      (KASCII 'n', [MCtrl]) -> scrollDown lst >> return True
      (KEnter, []) -> do
        m <- getSelected lst
        case m of Just (_, (s, _)) -> stdoutAction s >> exitSuccess
                  Nothing          -> exitFailure
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
