module Frontend where

import System.FilePath  (dropFileName)
import System.Environment (getExecutablePath)

import Control.Monad  (void)

import Sound.Tidal.Context as T hiding (mute,solo,(#),s)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C hiding (text)


import Backend


setup :: Stream -> String -> Window -> UI ()
setup str stdout win = void $ do
     --setup GUI
     void $ return win # set title "TidalCycles"

     UI.addStyleSheet win "tidal.css"
     UI.addStyleSheet win "theme.css"

     setCallBufferMode NoBuffering -- important for highlighting

     editor <- UI.textarea # set (attr "id") "editor0"

     output <- UI.pre # set UI.id_ "output"
                      #. "outputBox"
                      #+ [ string "output goes here" ]
                      # set style [("font-size","3vh")]
     displayP <- UI.div # set UI.id_ "displayP"
                       #. "displayBox"
                       # set style [("font-size","3vh"),("display","flex"),("flex-wrap","wrap")]
     -- continous display needs some more thought ...
     -- displayV <- UI.div # set UI.id_ "displayV"
     --                    #. "displayBox"
     fileInput <- UI.input # set UI.id_ "fileInput"
                           # set UI.type_ "file"
                           # set style [("display","none")]

     mainEditor <- UI.div #. "main" #+ [element editor] # set UI.style [("flex-grow","8")]
     container <- UI.div # set UI.id_ "editors" #. "flex-container" #+ [element mainEditor] # set UI.style [("display","flex"),("flex-wrap","wrap")]

     body <- UI.getBody win #. "CodeMirror cm-s-theme" # set UI.style [("background-color","black")]

     createShortcutFunctions str mainEditor

     _ <- (element body) #+
                       [--element displayV
                       element displayP
                       ,element container
                       ,element output
                       ,config
                       ]

     setupBackend str stdout

     _ <- (element body) #+
                        [element fileInput
                        ,tidalSettings
                        ]
     makeEditor "editor0"

tidalSettings :: UI Element
tidalSettings = do
          execPath <- liftIO $ dropFileName <$> getExecutablePath
          tidalKeys <- liftIO $ readFile $ execPath ++ "static/tidalConfig.js"
          settings <- mkElement "script" # set UI.text tidalKeys
          return settings

config :: UI Element
config = do
        execPath <- liftIO $ dropFileName <$> getExecutablePath
        conf <- liftIO $ readFile $ execPath ++ "static/config.js"
        mkElement "script" # set UI.text conf
