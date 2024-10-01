module Ui where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (modifyMVar_, readMVar, takeMVar, tryPutMVar)
import Control.Monad (void)
import Data.Map as Map (delete, empty, insert, lookup, toList)
import Foreign.JavaScript (JSObject)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C hiding (text)
import Sound.Tidal.Context hiding ((#))
import Sound.Tidal.ID

-- this displays values that are possibly continously changing without any interaction of the user (currently only cps, but could also display values in busses etc.)
displayLoop :: Stream -> UI ()
displayLoop stream = do
  cpsx <- fmap fromRational $ liftIO $ streamGetCPS stream
  now <- liftIO $ streamGetNow stream
  cpsEl <- getcpsEl
  cycEl <- getcycEl
  bpmEl <- getbpmEl
  void $ element cpsEl # set UI.text (show $ truncNum cpsx 4)
  void $ element bpmEl # set UI.text (show $ truncNum (cpsx * 60 * 4) 1)
  cycmod <- getcycMod
  case cycmod of
    Just x -> void $ element cycEl # set UI.text (show $ (mod (round now :: Int) x) + 1)
    Nothing -> void $ element cycEl # set UI.text (show $ (round now :: Int))
  runFunction $ ffi "requestAnimationFrame(displayLoop)"

truncNum :: Double -> Int -> Double
truncNum x m = (fromIntegral (floor (x * t))) / t
  where
    t = 10 ^ m

getcpsEl :: UI Element
getcpsEl = do
  win <- askWindow
  elMay <- getElementById win "cps"
  case elMay of
    Nothing -> error "can't happen"
    Just el -> return el

getbpmEl :: UI Element
getbpmEl = do
  win <- askWindow
  elMay <- getElementById win "bpm"
  case elMay of
    Nothing -> error "can't happen"
    Just el -> return el

getcycEl :: UI Element
getcycEl = do
  win <- askWindow
  elMay <- getElementById win "cyc"
  case elMay of
    Nothing -> error "can't happen"
    Just el -> return el

getcycMod :: UI (Maybe Int)
getcycMod = do
  x <- callFunction $ ffi "document.getElementById(\"cycmod\").textContent"
  case reads x of
    [(m, "")] -> return $ Just m
    _ -> return Nothing

-- this function should be called after any event that might change the playstate of the stream
updateDisplay :: Stream -> UI ()
updateDisplay stream = do
  display <- getDisplayElP
  playMap <- liftIO $ readMVar (sPMapMV stream)
  els <- playMapEl stream playMap
  void $ element display # set UI.children els

getDisplayElP :: UI Element
getDisplayElP = do
  win <- askWindow
  elMay <- getElementById win "displayP"
  case elMay of
    Nothing -> error "can't happen"
    Just el -> return el

showPlayState :: PlayState -> String
showPlayState (PlayState _ mt solo _)
  | mt = "m "
  | solo = "s "
  | otherwise = "p "

showPlayMap :: PlayMap -> String
showPlayMap pMap = concat [i ++ ": " ++ showPlayState ps ++ " " | (i, ps) <- pList]
  where
    pList = toList pMap

playStateEl :: Stream -> (PatId, PlayState) -> UI Element
playStateEl str (idd, ps) = do
  el <- UI.span # set UI.text (idd ++ ":" ++ showPlayState ps) # set UI.style [("background-color", "rgba(0,0,0,0.5)"), ("padding-right", "1vw"), ("padding-left", "1vw")]
  on UI.click el $ \_ -> (liftIO $ removeP str (ID idd)) >> updateDisplay str
  return el

playMapEl :: Stream -> PlayMap -> UI [Element]
playMapEl str pm = sequence $ fmap (playStateEl str) (toList pm)

removeP :: Stream -> ID -> IO ()
removeP str i = do
  pState <- takeMVar $ sPMapMV str
  let newPState = Map.delete (fromID i) pState
  void $ tryPutMVar (sPMapMV str) newPState

muteP :: Stream -> ID -> IO ()
muteP str i = do
  pState <- takeMVar $ sPMapMV str
  case Map.lookup (fromID i) pState of
    Just (p@(PlayState _ mt _ _)) -> do
      let newPState = Map.insert (fromID i) (p {psMute = not mt}) pState
      void $ tryPutMVar (sPMapMV str) newPState
    Nothing -> void $ tryPutMVar (sPMapMV str) pState

hush :: Stream -> IO ()
hush str = modifyMVar_ (sPMapMV str) (\_ -> return Map.empty)

-- flash on evaluation

checkUndefined :: (ToJS a) => a -> UI String
checkUndefined cm = callFunction $ ffi "(function (a) { if (typeof a === 'undefined' || a === null) {return \"yes\";} else { return \"no\"; } })(%1)" cm

highlightBlock :: JSObject -> Int -> Int -> String -> UI JSObject
highlightBlock cm lineStart lineEnd color = do
  undef <- checkUndefined cm
  case undef of
    "no" -> callFunction $ ffi "((%1).markText({line: %2, ch: 0}, {line: %3, ch: 0}, {css: %4}))" cm lineStart lineEnd color
    _ -> callFunction $ ffi "return {}"

unHighlight :: JSObject -> UI ()
unHighlight mark = runFunction $ ffi "if (typeof %1 !== 'undefined'){%1.clear()};" mark

flashSuccess :: JSObject -> Int -> Int -> UI ()
flashSuccess cm lineStart lineEnd = do
  mark <- highlightBlock cm lineStart (lineEnd + 1) "background-color: green"
  liftIO $ threadDelay 100000
  unHighlight mark
  flushCallBuffer

flashError :: JSObject -> Int -> Int -> UI ()
flashError cm lineStart lineEnd = do
  mark <- highlightBlock cm lineStart (lineEnd + 1) "background-color: red"
  liftIO $ threadDelay 100000
  unHighlight mark
  flushCallBuffer

-- hydra try

hydraOut :: JSObject -> UI ()
hydraOut o = runFunction $ ffi "%1.out()" o

hydraColor :: Int -> Int -> Int -> JSObject -> UI JSObject
hydraColor i j k o = callFunction $ ffi "%4.color(%1,%2,%3)" i j k o

hydraNoise :: Int -> UI JSObject
hydraNoise i = callFunction $ ffi "noise(%1)" i

hydraModulate :: JSObject -> JSObject -> UI JSObject
hydraModulate o1 o2 = callFunction $ ffi "%1.modulate(%2)" o1 o2

(~|~) :: UI JSObject -> UI JSObject -> UI JSObject
(~|~) o1 o2 = do
  ob1 <- o1
  ob2 <- o2
  hydraModulate ob1 ob2

-- works
hydraTry :: UI ()
hydraTry = hydraNoise 1 ~|~ hydraNoise 10 >>= hydraColor 1 0 1 >>= hydraOut
