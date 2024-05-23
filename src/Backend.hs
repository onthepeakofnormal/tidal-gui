{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module Backend where

import System.FilePath  (dropFileName)
import System.Environment (getExecutablePath)
import System.Directory (listDirectory, doesDirectoryExist)

import System.Clock

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar  (newEmptyMVar, MVar, putMVar, takeMVar)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Exception  (SomeException)
import Control.Monad.Catch (try)
import Control.Monad.Except


import Sound.Tidal.Context as T hiding (mute,solo,(#),s)
import Sound.Tidal.ID

import Sound.Osc.Fd as O

import Text.Parsec  (parse)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C hiding (text)

import Data.IORef (IORef, readIORef, modifyIORef, newIORef)

import Foreign.JavaScript (JSObject)


import Parse
import Ui
import Hint

type CurrentLine
  = Int

data EvalMode
  = L | B | A
  deriving (Eq, Show)

data Env
  = Env {windowE :: Window
        ,streamE :: Stream
        ,hintM :: MVar InterpreterMessage
        ,hintR :: MVar InterpreterResponse
        ,lineE :: Maybe CurrentLine
        ,evalModeE :: EvalMode
        ,editorE :: JSObject
        }

type I = ReaderT Env (ExceptT String IO)

-- to combine UI and IO actions with an environment
instance MonadUI I where
  liftUI m = do
            env <- ask
            let win = windowE env
            liftIO $ runUI win m


runI :: I () -> Env -> IO ()
runI i env = resolveError env (runExceptT $ runReaderT i env)

resolveError :: Env -> IO (Either String ()) -> IO ()
resolveError env xA = do
                    x <- xA
                    case x of
                      Left err -> do
                        out <- runUI (windowE env) getOutputEl
                        void $ runUI (windowE env) $ element out # set UI.text err
                      Right _ -> return ()

flashErrorI :: Int -> Int -> I ()
flashErrorI st en = do
            Env {editorE = cm} <- ask
            liftUI $ flashError cm st en

flashSuccessI :: Int -> Int -> I ()
flashSuccessI st en = do
            Env {editorE = cm} <- ask
            liftUI $ flashSuccess cm st en

outputI :: String -> I ()
outputI st = do
            out <- liftUI getOutputEl
            void $ liftUI $ element out # set UI.text st

clearOut :: Int -> Int -> I ()
clearOut strt en = succI strt en ""

throwE :: Int -> Int -> String -> I a
throwE i k err = flashErrorI i k >> throwError err

succI :: Int -> Int -> String -> I ()
succI strt en o = flashSuccessI strt en >> outputI o

checkEditor :: I ()
checkEditor = do
            Env {editorE = cm} <- ask
            x <- liftUI (checkUndefined cm)
            case x of
              "yes" -> flashErrorI 0 1 >> throwError "Oops, try evaluating again!"
              _ -> return ()

getBlockContent :: I [Block]
getBlockContent = do
     Env { editorE = cm, lineE = mayl, evalModeE = m} <- ask
     contents <- liftUI $ getValue cm
     l <- case mayl of
            Just x -> return x
            Nothing -> liftUI $ getCursorLine cm
     let blockMaybe = case m of
                        L -> [getLineContent l (linesNum contents)]
                        B -> [getBlock l $ getBlocks contents]
                        A -> fmap Just $ getBlocks contents
         bs = concatMap (\x -> case x of Nothing -> []; Just y -> [y]) blockMaybe
     case bs of
       [] -> throwE 0 1 "Failed to get block!"
       xs -> return xs


parseBlock :: Block -> I Command
parseBlock (Block lst len cont) = do
  case parse parseCommand "" cont of
        Left e -> throwE lst len (show e)
        Right c -> return c

-- evaluate the given expression, if a string is returned, print it to the console
statI :: Int -> Int -> String -> I ()
statI strt en s = do
        Env {hintM = mMV, hintR = rMV} <- ask
        liftIO $ putMVar mMV $ MStat s
        res <- liftIO $ takeMVar rMV
        case res of
          RStat (Just "()") -> clearOut strt en
          RStat (Just outputString) -> succI strt en outputString
          RStat Nothing -> clearOut strt en
          RError e -> throwE strt en e
          _ -> clearOut strt en

-- ask the interpreter for the type of the given expression
typeI :: Int -> Int -> String -> I ()
typeI strt en s = do
           Env {hintM = mMV, hintR = rMV} <- ask
           liftIO $ putMVar mMV $ MType s
           res <- liftIO $ takeMVar rMV
           case res of
             (RType t) -> succI strt en t
             (RError e) -> throwE strt en e
             _ -> clearOut strt en

-- evaluate the given expression expecting a string, which will replace the line
makroI :: Int -> Int -> String -> I ()
makroI strt en s = do
  Env {editorE = cm, lineE = mayl, hintM = mMV, hintR = rMV} <- ask
  liftIO $ putMVar mMV $ MStat s
  res <- liftIO $ takeMVar rMV
  line <- case mayl of
         Just x -> return x
         Nothing -> liftUI $ getCursorLine cm
  case res of
    RStat (Just "()") -> throwE strt en "A makro has to return a string"
    RStat (Just outputString) -> (liftUI $ runFunction $ ffi ("(%1).replaceRange(" ++ outputString ++ ", {line: (%2), ch: 0}, {line: (%2), ch: 50})") cm line)
    RStat Nothing -> throwE strt en "A makro has to return a string"
    RError e -> throwE strt en e
    _ -> clearOut strt en

hushI :: I ()
hushI = do
     Env {streamE = str} <- ask
     liftIO $ hush str

confI :: Int -> Int -> String -> I ()
confI strt en p = do
          x <- liftUI $ setDefPath p
          case x of
              Left _ -> throwE strt en "Perhaps you don't have authority to write to the config file?\nTry running with sudo or admin privileges"
              Right True -> succI strt en ("Successfully set path to: " ++ p)
              Right False -> throwE strt en "This path doesn't seem to exist!"

listenI :: Int -> Int -> String -> Int -> I ()
listenI strt en s i = do
     env <- ask
     liftUI $ void $ liftIO $ forkIO $ listen s i env
     succI strt en ("Listening on port " ++ s ++ ":" ++ show i)

hydraI :: Int -> Int -> String -> I ()
hydraI strt en s = do
        x <- liftUI $ hydraJob s
        case x == "" of
          True -> clearOut strt en
          False -> throwE strt en (show x)

interpretCommandsManyI :: I ()
interpretCommandsManyI = do
                  checkEditor
                  bs <- getBlockContent
                  commands <- sequence $ map parseBlock bs
                  _ <- sequence $ map (\(c,Block str en _) -> interpretCommandsI str en c) (zip commands bs)
                  return ()

interpretCommandsI :: Int -> Int -> Command -> I ()
interpretCommandsI strt en c = case c of
                  Statement s -> statI strt en s
                  T t -> typeI strt en t
                  M x -> makroI strt en x
                  Hush -> hushI
                  Conf DefPath p -> confI strt en p
                  Listen s i -> listenI strt en s i
                  Hydra s -> hydraI strt en s

setupBackend :: Stream -> String -> UI ()
setupBackend str stdout = do

       win <- askWindow
       env <- startInterpreter str stdout

       setupBPMTap str

       createHaskellFunction "evaluateBlock" (\cm -> runI interpretCommandsManyI (env Nothing B cm))
       createHaskellFunction "evaluateLine" (\cm -> runI interpretCommandsManyI (env Nothing L cm))
       createHaskellFunction "evaluateAll" (\cm -> runI interpretCommandsManyI (env Nothing A cm))

       createHaskellFunction "evaluateBlockLine" (\cm l -> runI interpretCommandsManyI (env (Just l) B cm))
       createHaskellFunction "evaluateLineLine" (\cm l -> runI interpretCommandsManyI (env (Just l) L cm))

       createHaskellFunction "displayLoop" (runUI win $ displayLoop str)
       void $ liftIO $ forkIO $ runUI win $ runFunction $ ffi "requestAnimationFrame(displayLoop)"

       on disconnect win $ \_ -> (runFunction $ ffi "saveFile()")

lookupDefPath :: UI String
lookupDefPath = callFunction $ ffi "config.defPath"

lookupTemplatePath :: UI String
lookupTemplatePath = callFunction $ ffi "config.tempPath"

setDefPath :: String -> UI (Either SomeException Bool)
setDefPath s = do
          execPath <- liftIO $ dropFileName <$> getExecutablePath
          b <- liftIO $ doesDirectoryExist s
          case b of
            True -> try $ liftIO $ writeFile (execPath ++ "static/config.js") ("const config = {defPath:" ++ s ++ " }") >> return True
            False -> return $ Right False

getBootDefs :: UI [String]
getBootDefs = do
       bootPath <- lookupDefPath
       b <- liftIO $ doesDirectoryExist bootPath
       case b of
         False -> do
             execPath <- liftIO $ dropFileName <$> getExecutablePath
             userDefsPaths <- liftIO $ listDirectory $ execPath ++ "static/definitions/"
             bootDefs <- liftIO $ sequence $ map (\x -> readFile $ execPath ++ "static/definitions/" ++ x) userDefsPaths
             return bootDefs
         True -> do
             userDefsPaths <- liftIO $ listDirectory $ bootPath
             bootDefs <- liftIO $ sequence $ map (\x -> readFile $ bootPath ++"/"++ x) userDefsPaths
             return bootDefs


startInterpreter :: Stream -> String -> UI (Maybe CurrentLine -> EvalMode -> JSObject -> Env)
startInterpreter str stdout = do

           win <- askWindow
           mMV <- liftIO newEmptyMVar
           rMV <- liftIO newEmptyMVar
           bootDefs <- getBootDefs
           execPath <- liftIO $ dropFileName <$> getExecutablePath
           ghcMode <- liftIO $ readFile $ execPath ++ "static/ghc_mode.txt"


           if ghcMode == "WITH_GHC=TRUE\n"
              then void $ liftIO $ forkIO $ startHintJob True str bootDefs mMV rMV -- True = safe
              else void $ liftIO $ forkIO $ startHintJob False str bootDefs mMV rMV

           out <- getOutputEl
           _ <- if ghcMode == "WITH_GHC=TRUE\n"
                    then element out # set UI.text ("Started interpreter using local GHC installation \n" ++ stdout)
                    else element out # set UI.text ("Started interpreter with packaged GHC \n" ++ stdout)

           createHaskellFunction "replaceWordByDef" (\cm -> runUI win $ replaceWordByDef mMV rMV cm)
           return $ Env win str mMV rMV

createShortcutFunctions :: Stream -> Element -> UI ()
createShortcutFunctions str mainEditor = do
                       editorsRef <- liftIO $ newIORef [mainEditor]
                       win <- askWindow
                       createHaskellFunction "addEditor" (runUI win $ addEditor editorsRef)
                       createHaskellFunction "removeEditor" (runUI win $ removeEditor editorsRef)

                       createHaskellFunction "hush" (hush str >> (runUI win $ updateDisplay str))

                       createHaskellFunction "muteP1" (muteP str 1 >> (runUI win $ updateDisplay str))
                       createHaskellFunction "muteP2" (muteP str 2 >> (runUI win $ updateDisplay str))
                       createHaskellFunction "muteP3" (muteP str 3 >> (runUI win $ updateDisplay str))
                       createHaskellFunction "muteP4" (muteP str 4 >> (runUI win $ updateDisplay str))
                       createHaskellFunction "muteP5" (muteP str 5 >> (runUI win $ updateDisplay str))
                       createHaskellFunction "muteP6" (muteP str 6 >> (runUI win $ updateDisplay str))
                       createHaskellFunction "muteP7" (muteP str 7 >> (runUI win $ updateDisplay str))
                       createHaskellFunction "muteP8" (muteP str 8 >> (runUI win $ updateDisplay str))
                       createHaskellFunction "muteP9" (muteP str 9 >> (runUI win $ updateDisplay str))

getOutputEl :: UI Element
getOutputEl = do
         win <- askWindow
         elMay <- getElementById win "output"
         case elMay of
           Nothing -> error "can't happen"
           Just el -> return el

getDisplayElV :: UI Element
getDisplayElV = do
        win <- askWindow
        elMay <- getElementById win "displayV"
        case elMay of
          Nothing -> error "can't happen"
          Just el -> return el


getValue :: ToJS a => a -> UI String
getValue cm = callFunction $ ffi "getV(%1)" cm

createHaskellFunction name fn = do
  handler <- ffiExport fn
  runFunction $ ffi ("window." ++ name ++ " = %1") handler

getCursorLine :: ToJS a => a -> UI Int
getCursorLine cm = callFunction $ (ffi "getCursorLine(%1)") cm

makeEditor :: String -> UI ()
makeEditor i = runFunction $ ffi $ "CodeMirror.fromTextArea(document.getElementById('" ++ i ++ "'), editorSettings);"

addEditor :: IORef [Element]  -> UI ()
addEditor ref = do
        old <- liftIO $ readIORef ref
        let x = show $ length old
        editor <- UI.textarea # set (attr "id") ("editor" ++ x)
        d <- UI.div #. "main" #+ [element editor] # set UI.style [("flex-grow","8")]
        liftIO $ modifyIORef ref (\xs -> xs ++ [d])
        redoEditorLayout ref
        makeEditor ("editor" ++ x)

removeEditor :: IORef [Element] -> UI ()
removeEditor ref = do
          xs <- liftIO $ readIORef ref
          case length xs == 1 of
            True -> return ()
            False -> do
                liftIO $ modifyIORef ref (\ys -> take (length xs - 1) ys)
                redoEditorLayout ref

redoEditorLayout :: IORef [Element] -> UI ()
redoEditorLayout ref = do
            win <- askWindow
            eds <- liftIO $ readIORef ref
            editorsMay <- getElementById win "editors"
            case editorsMay of
              Nothing -> error "cant happen"
              Just editors -> void $ element editors # set UI.children eds


replaceWordByDef :: MVar InterpreterMessage -> MVar InterpreterResponse -> JSObject -> UI ()
replaceWordByDef mMV rMV cm = do
      loc <- (callFunction $ ffi "(%1).findWordAt((%1).getCursor())" cm) :: UI JSObject
      word <- (callFunction $ (ffi "(%1).getRange((%2).anchor, (%2).head)" cm loc)) :: UI String
      liftIO $ putMVar mMV $ MStat ("return $ " ++ word)
      res <- liftIO $ takeMVar rMV
      case res of
        RStat (Just outputString) -> runFunction $ ffi ("(%1).replaceRange(" ++ outputString ++ ", (%2).anchor, (%2).head)") cm loc
        _ -> return ()

noDoubleQuotes :: String -> String
noDoubleQuotes = init . tail



--OSC

loadTemplate :: I ()
loadTemplate = do
          env <- ask
          tempPath <- liftUI $ lookupTemplatePath
          templates <- liftIO $ listDirectory $ tempPath
          template <- liftIO $ readFile $ tempPath ++ "/" ++ templates!!0
          liftUI $ runFunction $ ffi "(%1).getDoc().setValue(%2);" (editorE env) template
          liftIO $ runI interpretCommandsManyI (env {evalModeE = A})

listen :: String -> Int -> Env -> IO ()
listen s listenPort env = do
            local <- udpServer s listenPort
            loopOSC env local
            where loopOSC e l = do -- wait for, read and act on OSC message
                         m <- recvMessage l
                         e' <- actOSC e m
                         loopOSC e' l

actOSC :: Env -> Maybe O.Message -> IO Env
actOSC env (Just (Message "/eval" [])) = (runUI (windowE env) $ runFunction $ ffi "evaluateBlock(document.querySelector(\"#editor0 + .CodeMirror\").CodeMirror)") >> return env
actOSC env (Just (Message "/hush" [])) = (runUI (windowE env) $ (liftIO $ hush $ streamE env) >> updateDisplay (streamE env)) >> return env
actOSC env (Just (Message "/eval/all" [])) = runI interpretCommandsManyI (env {evalModeE = A}) >> return env
actOSC env (Just (Message "/eval/block" [Int32 line])) = runI interpretCommandsManyI (env {evalModeE = B, lineE = Just $ (fromIntegral line) - 1 }) >> return env
actOSC env (Just (Message "/eval/line" [Int32 line])) = runI interpretCommandsManyI (env {evalModeE = L, lineE = Just $ (fromIntegral line) - 1 }) >> return env
actOSC env (Just (Message "/go/line" [Int32 line])) = (runUI (windowE env) $ runFunction $ ffi "(document.querySelector(\"#editor0 + .CodeMirror\").CodeMirror).setCursor(%1)" (fromIntegral line :: Int)) >> return env
actOSC env (Just (Message "/mute" [AsciiString s])) = (runUI (windowE env) $ (liftIO $ muteP (streamE env) (ID $ ascii_to_string s)) >> updateDisplay (streamE env)) >> return env
actOSC env (Just (Message "/print" [AsciiString s])) = (runUI (windowE env) $ getOutputEl # (set UI.text $ "Recieved message: " ++ ascii_to_string s)) >> return env
actOSC env (Just (Message "/hydra/set" [AsciiString s1, AsciiString s2])) = (runUI (windowE env) $ hydraJob (ascii_to_string s1 ++ "=" ++ ascii_to_string s2)) >> return env
actOSC env (Just (Message "/tidal/incBy" [AsciiString s1, Double d])) = (runUI (windowE env) $ liftIO $ increaseBy (streamE env) (ascii_to_string s1) d) >> return env
actOSC env (Just (Message "/action" [AsciiString s])) = (runI (statI (-1) (-1) (ascii_to_string s)) env) >> return env
actOSC env (Just (Message "/loadTemplate" [])) = runI loadTemplate env >> return env
actOSC env (Just m) = (runUI (windowE env) $ getOutputEl # (set UI.text $ "Unhandeled OSC message: " ++ show m)) >> return env
actOSC env _ = return env

--tidal values

increaseBy :: Stream -> String -> Double -> IO ()
increaseBy str s d = do
                va <- streamGet str s
                case va of
                  Just (VPattern p) -> streamSet str s (fmap (\x -> case x of; VF f -> f + d; _ -> 0) p)
                  Nothing -> streamSet str s (pure $ (0 :: Double))
                  _ -> return ()


--hydra

startHydra :: UI ()
startHydra = do
  runFunction $ ffi "hydra = new Hydra({canvas: document.getElementById(\"hydraCanvas\"),detectAudio: false})"
  hijackScreen

hijackScreen :: UI ()
hijackScreen = do
        execPath <- liftIO $ dropFileName <$> getExecutablePath
        h <- liftIO $ readFile $ execPath ++ "static/hijackScreen.js"
        runFunction $ ffi h

hydraJob :: String -> UI String
hydraJob s = callFunction $ ffi "function f(x) {try{eval(x); return \"\";} catch (error) { return error.message; }}; f(%1)" s

getWindowWidth :: UI Double
getWindowWidth = callFunction $ ffi "window.innerWidth"

getWindowHeight :: UI Double
getWindowHeight = callFunction $ ffi "window.innerHeight"

-- bpm tap

setupBPMTap :: Stream -> UI ()
setupBPMTap str = do
  win <- askWindow
  bpmEl <- getbpmEl
  ref <- liftIO $ newIORef []
  on UI.click bpmEl $ const $ tapBPM str ref
  createHaskellFunction "tapBPM" (runUI win $ tapBPM str ref)

tapBPM :: Stream -> IORef ([Double]) -> UI ()
tapBPM s ref = do
  nowS <- liftIO $ getTime Monotonic
  let now = (fromIntegral $ toNanoSecs nowS) / 1000000000
  times <- liftIO $ readIORef ref
  case length times > 0 of
    False -> liftIO $ modifyIORef ref (const [now])
    True -> if now - times!!0 > 5 then liftIO $ modifyIORef ref (const [now]) else
              case times of
                (x:[]) -> do
                    let avg = now-x
                    liftIO $ streamOnce s $ cps $ pure $ 1/(avg*4)
                    liftIO $ modifyIORef ref (const $ now:times)
                (x:(y:[])) -> do
                    let n1 = x-y
                        n2 = now-x
                        avg = (n1 + n2)/2
                    liftIO $ streamOnce s $ cps $ pure $ 1/(avg*4)
                    liftIO $ modifyIORef ref (const $ now:times)
                (x:(y:(z:_))) -> do
                    let n1 = y-z
                        n2 = x-y
                        n3 = now-x
                        avg = (n1 + n2 + n3)/3
                    liftIO $ streamOnce s $ cps $ pure $ 1/(avg*4)
                    liftIO $ modifyIORef ref (const $ now:times)
                _ -> return ()
