{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import System.Exit

import Graphics.Vty.Widgets.All
import Graphics.Vty.LLInput
import Graphics.Vty.Attributes

newListView :: [T.Text] -> IO (Widget (List T.Text FormattedText))
newListView sources = case sources of
  (x:xs) -> do
    lst <- newListView xs
    addToList lst x =<< plainText x
    return lst
  [] -> newList (fgColor white) 1

main :: IO()
main = do
  -- configures
  let infoAttr = (black `on` white) -- attribute for header and footer
  let titleString = "alsw: anything.el-like selector widget"
  let promptString = "> "

  -- Header
  title <- (plainText titleString) <++> (hFill ' ' 1)
  setNormalAttribute title infoAttr
  prompt <- plainText promptString
  e <- editWidget
  header <- (return title) <--> (hBox prompt e)

  -- List
  let sources = ["homhom", "madoka", "saki"]
  -- let sources = take 1000 $ repeat "homhom"
  lst <- newListView sources

  -- Footer
  fw <- plainText ("source: " `T.append` "sample")
  footer <- (return fw) <++> (hFill ' ' 1)
  setNormalAttribute footer infoAttr

  ui <- (return header)
        <--> (return lst)
        <--> (return footer)

  -- Focus Group
  fg <- newFocusGroup
  _ <- addToFocusGroup fg e
  _ <- addToFocusGroup fg lst

  fg `onKeyPressed` \ _ key mods ->
    case (key, mods) of
      (KASCII 'q', [MCtrl]) -> exitSuccess
      _ -> return False

  -- Collection
  c <- newCollection
  _ <- addToCollection c ui fg

  -- Run
  runUi c defaultContext
