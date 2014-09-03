{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Graphics.Vty.Widgets.All
import Graphics.Vty.LLInput
import Graphics.Vty.Attributes

import Text.Regex.Posix
import Control.Monad (forM_)
import System.IO (stdout)
import System.Exit (exitFailure, exitSuccess)


stdoutAction :: T.Text -> IO ()
stdoutAction s = T.hPutStr stdout s

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

class Source a where
  getSource :: a -> [T.Text]
  getSourceName :: a -> T.Text


data StaticSource = StaticSource { source :: [T.Text]
                                 , name :: T.Text
                                 }

instance Source StaticSource where
  getSource ss = source ss
  getSourceName ss = name ss

holyquintet :: StaticSource
holyquintet = StaticSource { source = ["mami", "madoka", "homhom", "sayaka", "kyoko"]
                           , name = "holyquintet"
                           }

main :: IO()
main = do
  let srcs = holyquintet
  let cfg = defaultConfig

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
        clearList lst
        let fsrc = filter (\ src -> match reg (T.unpack src) :: Bool) $ getSource srcs
        forM_ fsrc $ \ l ->
          (addToList lst l =<< plainText l)

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
