{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

import Frontend
import Graphics.UI.Threepenny.Core as C hiding (text)
import Sound.Tidal.Context as T hiding (s, (#))
import System.Environment (getExecutablePath)
import System.FilePath (dropFileName)
import System.IO.Silently

main :: IO ()
main = do
  execPath <- dropFileName <$> getExecutablePath
  (outTidal, str) <- capture $ T.startTidal (T.superdirtTarget {oLatency = 0.1, oAddress = "127.0.0.1", oPort = 57120}) (T.defaultConfig {cVerbose = True})

  startGUI
    C.defaultConfig
      { jsStatic = Just $ execPath ++ "static",
        jsCustomHTML = Just "tidal.html"
      }
    $ setup str outTidal
