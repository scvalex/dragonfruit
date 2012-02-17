module Main where

import Common
import Control.Concurrent.MVar
import Control.Monad
import Graphics.Rendering.Cairo
import qualified Graphics.Rendering.Cairo.Matrix as M
import Graphics.UI.Gtk

import Instances ()
import Types
import Skyline

data State = State { getCurrent :: VBox
                   , getParams  :: SkylineParameters
                   , getCanvases :: [(DrawingArea, Frame, MVar Skyline)]
                   , getHistoryContainer :: VBox
                   , getNextHandlers :: MVar [(ConnectId DrawingArea)] }

main :: IO ()
main = do
  initGUI

  window <- windowNew
  window `onDestroy` mainQuit

  set window [containerBorderWidth := 10]

  topBox <- vBoxNew False 3
  set window [containerChild := topBox]

  titleLabel <- labelNew (Just "Dragonfruit")
  set topBox [containerChild := titleLabel]
  set topBox [boxChildPacking titleLabel := PackNatural]

  mainScroller <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy mainScroller PolicyAutomatic PolicyAutomatic
  set topBox [containerChild := mainScroller]

  evoBox <- vBoxNew False 1
  scrolledWindowAddWithViewport mainScroller evoBox
  set evoBox [containerBorderWidth := 5]

  let params = SkylineParameters { getMutationRate = 0.05 }
  skies <- forM [1..3] $ \_ -> spawn params
  (choiceBox, canvases) <- newChoiceBox params skies
  containerAdd evoBox choiceBox
  set evoBox [boxChildPacking choiceBox := PackNatural]

  nextHandlerMV <- newEmptyMVar
  stateMV <- newMVar $ State { getCurrent = choiceBox
                             , getParams  = params
                             , getCanvases = canvases
                             , getHistoryContainer = evoBox
                             , getNextHandlers = nextHandlerMV }

  nextHdls <- forM canvases $ \(canvas, _, _) -> do
    canvas `on` buttonPressEvent $ tryEvent $ do
      LeftButton <- eventButton
      liftIO $ newSaveChoice stateMV canvas
  putMVar nextHandlerMV nextHdls

  widgetShowAll window

  mainGUI

newChoiceBox :: SkylineParameters
             -> [Skyline]
             -> IO (VBox, [(DrawingArea, Frame, MVar Skyline)])
newChoiceBox params skies = do
  choiceBox <- vBoxNew True 1

  canvases <- forM skies $ \sky -> do
    (canvas, canvasF, skyMV) <- newFramedCanvas params sky
    containerAdd choiceBox canvasF
    return (canvas, canvasF, skyMV)

  return (choiceBox, canvases)

newFramedCanvas :: SkylineParameters
                -> Skyline
                -> IO (DrawingArea, Frame, MVar Skyline)
newFramedCanvas params sky = do
  canvas <- drawingAreaNew
  canvasF <- frameNew
  containerAdd canvasF canvas
  widgetSetSizeRequest canvas 200 100

  skyMV <- newMVar sky
  onExpose canvas $ const (updateCanvas canvas skyMV params)

  return (canvas, canvasF, skyMV)

updateCanvas :: DrawingArea -> MVar Skyline -> SkylineParameters -> IO Bool
updateCanvas canvas skyMV params = do
  win <- widgetGetDrawWindow canvas
  (width, height) <- widgetGetSize canvas
  sky <- readMVar skyMV
  renderWithDrawable win (drawSkyline width height sky params)
  return True

mutateOnce :: MVar Skyline -> SkylineParameters -> IO ()
mutateOnce skyMV params = modifyMVar_ skyMV $ mutate params

newSaveChoice :: MVar State -> DrawingArea -> IO ()
newSaveChoice stateMV selectedCanvas = do
  s@State { getCurrent = choiceBox
          , getParams  = params
          , getCanvases = canvases
          , getHistoryContainer = evoBox
          , getNextHandlers = nextHandlersMV } <- takeMVar stateMV
  nextHandlers <- takeMVar nextHandlersMV
  mapM signalDisconnect nextHandlers

  containerRemove evoBox choiceBox

  selectedSkyMV <- newEmptyMVar
  forM canvases $ \(canvas, canvasF, skyMV) -> do
    if canvas == selectedCanvas
      then do
        containerRemove choiceBox canvasF
        containerAdd evoBox canvasF
        set evoBox [boxChildPacking canvasF := PackNatural]
        putMVar selectedSkyMV =<< readMVar skyMV
      else do
        widgetDestroy canvasF
        widgetDestroy canvas

  selectedSky <- takeMVar selectedSkyMV
  skies <- forM [1..3 :: Int] $ \_ -> mutate params selectedSky

  (newChoice, newCanvases) <- newChoiceBox params skies
  containerAdd evoBox newChoice
  set evoBox [boxChildPacking newChoice := PackNatural]

  nextHdls <- forM newCanvases $ \(canvas, _, skyMV) -> do
    mutateOnce skyMV params
    canvas `on` buttonPressEvent $ tryEvent $ do
      LeftButton <- eventButton
      liftIO $ newSaveChoice stateMV canvas
  putMVar nextHandlersMV nextHdls

  widgetShowAll evoBox

  putMVar stateMV s{getCurrent = newChoice, getCanvases = newCanvases}

drawSkyline :: Int -> Int -> Skyline -> SkylineParameters -> Render ()
drawSkyline width height sky params = do
  setupCanvas width height
  inUserCoordinates width height $ visualize params sky

setupCanvas :: Int -> Int -> Render ()
setupCanvas devWidth devHeight = do
  -- White-out canvas
  setSourceRGBA 1.0 1.0 1.0 1.0
  paint

  when debug $ keepState (boundingBox devWidth devHeight)

boundingBox :: Int -> Int -> Render ()
boundingBox devWidth devHeight = do
  inUserCoordinates devWidth devHeight $ do
    setSourceRGBA 0.0 0.0 0.0 1.0
    rectangle 0.1 0.1 7.8 0.8
    stroke

inUserCoordinates :: Int -> Int -> Render a -> Render a
inUserCoordinates devWidth devHeight render =
    keepState $ do
      -- Switch to user coordinates
      let scaleX  = realToFrac devWidth / 8.0
          scaleY  = realToFrac devHeight

      setLineCap LineCapRound
      setLineJoin LineJoinRound
      setLineWidth $ 4 / max scaleX scaleY

      -- Set up user coordinates
      scale scaleX scaleY
      -- Positive y-axis upwards
      translate 0.0 1.0
      let flipY = M.Matrix 1 0 0 (-1) 0 0
      transform flipY

      render

keepState :: Render t -> Render t
keepState render = save >> render >>= \x -> restore >> return x

redrawCanvas :: DrawingArea -> IO ()
redrawCanvas canvas = do
  (w, h) <- widgetGetSize canvas
  widgetQueueDrawArea canvas 0 0 w h
