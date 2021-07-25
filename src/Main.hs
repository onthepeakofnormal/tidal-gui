{-# LANGUAGE FlexibleInstances #-}

import System.FilePath  (dropFileName)
import System.Environment (getExecutablePath)

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar  (newEmptyMVar, readMVar, tryTakeMVar, takeMVar, MVar, putMVar)
import Control.Monad  (void)
import Control.Monad.Reader (ReaderT, runReaderT, ask)

import Data.Map as Map (insert, empty)

import Sound.Tidal.Context as T hiding (mute,solo,(#))

import Text.Parsec  (parse)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C hiding (text)

import Foreign.JavaScript (JSObject)

import Parse
import Highlight
import Ui
import Configure
import Hint

main :: IO ()
main = do
    execPath <- dropFileName <$> getExecutablePath
    stream <- T.startStream T.defaultConfig [(T.superdirtTarget {T.oLatency = 0.1},
                                              [T.superdirtShape]
                                             ),
                                             (remoteTarget,
                                              [T.OSCContext "/code/highlight"]
                                             )
                                            ]
    startGUI C.defaultConfig {
          jsStatic = Just $ execPath ++ "static",
          jsCustomHTML     = Just "tidal.html"
        } $ setup stream


setup :: Stream -> Window -> UI ()
setup stream win = void $ do
     --setup GUI
     return win # set title "Tidal"
     definitions <- UI.textarea
                 # set (attr "id") "definitions-editor"
     control <- UI.textarea
                 # set (attr "id") "control-editor"

     output <- UI.pre #+ [ string "output goes here" ]
     errors <- UI.pre #+ [ string "errors go here" ]
     load <- UI.input
                  # set (attr "type") "file"
                  # set (attr "id") "fileInput"
                  # set (attr "onchange") "controlLoadFile()"
     save <- UI.button
                  # set UI.text "Save file"
                  # set (attr "onclick") "controlSaveFile()"
     body <- UI.getBody win
     execPath <- liftIO $ dropFileName <$> getExecutablePath
     tidalKeys <- liftIO $ readFile $ execPath ++ "static/tidalConfig.js"
     settings <- mkElement "script" # set UI.text tidalKeys
     makeCtrlEditor <- mkElement "script"
                       # set UI.text "const controlEditor = CodeMirror.fromTextArea(document.getElementById('control-editor'), controlEditorSettings);"
     makeDefsEditor <- mkElement "script"
                       # set UI.text "const definitionsEditor = CodeMirror.fromTextArea(document.getElementById('definitions-editor'), definitionsEditorSettings);"

     --highlight (experimental)
     pats <- liftIO $ newEmptyMVar
     liftIO $ forkIO $ highlightLoop [] stream win pats

     let env = Env win stream output errors pats
         runI = runReaderT interpretCommands env

     createHaskellFunction "evaluate" runI
     createHaskellFunction "hush" (bigHush stream pats)
     createHaskellFunction "mute1" (mute stream pats 1)
     createHaskellFunction "mute2" (mute stream pats 2)
     createHaskellFunction "mute3" (mute stream pats 3)
     createHaskellFunction "mute4" (mute stream pats 4)
     createHaskellFunction "mute5" (mute stream pats 5)
     createHaskellFunction "mute6" (mute stream pats 6)
     createHaskellFunction "mute7" (mute stream pats 7)
     createHaskellFunction "mute8" (mute stream pats 8)
     createHaskellFunction "mute9" (mute stream pats 9)

     -- put elements on body
     UI.getBody win #+ [element definitions, element control, element load, element save, element settings, element makeCtrlEditor, element makeDefsEditor, element errors, element output]

data Env = Env {window :: Window
                ,stream :: Stream
                ,output :: Element
                ,errors :: Element
                ,patS :: MVar PatternStates
                }

-- to combine UI and IO actions with an environment
instance MonadUI (ReaderT Env IO) where
 liftUI m = do
           env <- ask
           let win = window env
           liftIO $ runUI win m

interpretCommands :: ReaderT Env IO ()
interpretCommands  = do
       env <- ask
       let out = output env
           err = errors env
           str = stream env
       contentsControl <- liftUI editorValueControl
       contentsDef <- liftUI editorValueDefinitions
       line <- liftUI getCursorLine
       let blocks = getBlocks contentsControl
           blockMaybe = getBlock line blocks
       case blockMaybe of
           Nothing -> void $ liftUI $ element err # set UI.text "Failed to get Block"
           Just (Block blockLineStart blockLineEnd block) -> do
                   let parsed = parse parseCommand "" block
                       p = streamReplace str
                   case parsed of
                         Left e -> do
                           liftUI $ flashError blockLineStart blockLineEnd
                           void $ liftUI $ element err # set UI.text ( "Parse error in " ++ show e )
                         Right command -> case command of
                                         (D num string) -> do
                                                 res <- liftIO $ runHintSafe string contentsDef
                                                 case res of
                                                     Right (Right pat) -> do
                                                                       liftUI $ flashSuccess blockLineStart blockLineEnd
                                                                       let patStatesMVar = patS env
                                                                           win = window env
                                                                       liftUI $ element out # set UI.text (show pat )
                                                                       liftUI $ element err # set UI.text ""
                                                                       liftIO $ p num $ pat |< orbit (pure $ num-1)
                                                                       patStates <- liftIO $ tryTakeMVar patStatesMVar
                                                                       case patStates of
                                                                             Just pats -> do
                                                                                 let newPatS = Map.insert num (PS pat blockLineStart False False) pats
                                                                                 liftIO $ putMVar patStatesMVar $ newPatS
                                                                             Nothing -> do
                                                                                 let newPatS = Map.insert num (PS pat blockLineStart False False) Map.empty
                                                                                 liftIO $ putMVar patStatesMVar $ newPatS
                                                     Right (Left e) -> do
                                                                     liftUI $ flashError blockLineStart blockLineEnd
                                                                     void $ liftUI $ element err # set UI.text (parseError e)
                                                     Left e -> do
                                                                     liftUI $ flashError blockLineStart blockLineEnd
                                                                     void $ liftUI $ element err # set UI.text (show e)
                                         (Hush)      -> do
                                                 liftUI $ flashSuccess blockLineStart blockLineEnd
                                                 liftIO $ bigHush str (patS env)
                                         (Cps x)     -> do
                                                 liftUI $ flashSuccess blockLineStart blockLineEnd
                                                 liftIO $ streamOnce str $ cps (pure x)
                                         (Other s)   -> do
                                                 res <- liftIO $ runHintSafeOther s contentsDef str
                                                 case res of
                                                   Right (Right action) -> do
                                                                   liftUI $ flashSuccess blockLineStart blockLineEnd
                                                                   liftIO $ action
                                                   Right (Left e) -> do
                                                                   liftUI $ flashError blockLineStart blockLineEnd
                                                                   void $ liftUI $ element err # C.set UI.text (parseError e)
                                                   Left e -> do
                                                                   liftUI $ flashError blockLineStart blockLineEnd
                                                                   void $ liftUI $ element err # C.set UI.text (show e)
                                         (T s)        -> do
                                                  res <- liftIO $ getType s contentsDef
                                                  case res of
                                                    (Right t) -> do
                                                                  liftUI $ flashSuccess blockLineStart blockLineEnd
                                                                  void $ liftUI $ element out # set UI.text t
                                                    (Left e) -> do
                                                                  liftUI $ flashError blockLineStart blockLineEnd
                                                                  void $ liftUI $ element err # C.set UI.text (parseError e)
