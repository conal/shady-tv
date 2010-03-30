-- {-# LANGUAGE #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Shady.MechanicsGL
-- Copyright   :  (c) Conal Elliott
-- License     :  AGPLv3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Some utilities for shader generation
----------------------------------------------------------------------

module Shady.MechanicsGLGtk (shadyInit) where

-- import Data.IORef

import Graphics.Rendering.OpenGL as GL hiding (Sink)

import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk (AttrOp((:=)))
import qualified Graphics.UI.Gtk.OpenGL as GtkGL

import qualified Graphics.Glew as Glew

import Shady.Misc (Action,Sink)

-- temp.

-- | Initialize Shady and return a wrapper for a drawing command.
-- Supplying that drawing command will cause it to be invoked regularly.
shadyInit :: String -> IO (Sink Action)
shadyInit title =
  do -- putStrLn "shadyInit"
     Gtk.initGUI
     -- putStrLn "past initGUI"
     GtkGL.initGL
     -- putStrLn "past initGL"
     glconfig <- GtkGL.glConfigNew [GtkGL.GLModeRGBA,
                                    GtkGL.GLModeDepth,
                                    GtkGL.GLModeDouble,
                                    GtkGL.GLModeAlpha ]
     canvas <- GtkGL.glDrawingAreaNew glconfig
     -- putStrLn "made canvas"
     -- Gtk.widgetSetSizeRequest canvas 600 600
     -- Gtk.widgetSetSizeRequest canvas 320 480   -- like iphone
     Gtk.widgetSetSizeRequest canvas 400 400
     -- Initialise some GL setting just before the canvas first gets shown
     -- (We can't initialise these things earlier since the GL resources that
     -- we are using wouldn't heve been setup yet)
     -- TODO experiment with moving some of these steps.
     Gtk.onRealize canvas $ GtkGL.withGLDrawingArea canvas $ \_ ->
       do -- setupMatrices  -- do elsewhere, e.g., runSurface
          depthFunc  $= Just Less
          drawBuffer $= BackBuffers
          clearColor $= Color4 0 0 0.2 1
          -- experimental {
          -- blendFunc $= (SrcAlpha, One)
          -- blend     $= Enabled
          -- GL.cullFace $= Just Back
          -- } experimental
          -- putStrLn "glEnableVSync"
          -- glEnableVSync True
     -- Sync canvas size with GL viewport
     Gtk.onExpose canvas $ \_ -> 
       do (w',h') <- Gtk.widgetGetSize canvas
          let w = fromIntegral w' ; h = fromIntegral h'
          -- viewport $= ((Position 0 0), (Size (fromIntegral w) (fromIntegral h)))
          let dim :: GLsizei; start :: GLsizei -> GLint
              dim = w `min` h ; start s = fromIntegral ((s - dim) `div` 2)
          viewport $= ((Position (start w) (start h)), (Size dim dim))
          return True
     window <- Gtk.windowNew
     -- putStrLn "made window"
     Gtk.set window [ Gtk.containerBorderWidth := 8
                    , Gtk.windowTitle          := title
                    , Gtk.containerChild       := canvas
                    ]
     -- putStrLn "showing window"
     Gtk.widgetShowAll window
     -- putStrLn "calling glewInit"
     Glew.glewInit
     -- putStrLn "past glewInit"
     -- Gtk.mainGUI
     -- putStrLn "returning from shady Init"
     return $ timedDisplay window canvas

timedDisplay :: Gtk.Window -> GtkGL.GLDrawingArea -> Action -> IO ()
timedDisplay window canvas render = 
  do timeout <- Gtk.timeoutAddFull (display canvas render >> return True)
                                   Gtk.priorityDefaultIdle period
     Gtk.onDestroy window (Gtk.timeoutRemove timeout >> Gtk.mainQuit)
     Gtk.mainGUI
 where
   period = 1000 `div` 60   -- milliseconds


-- -- | Initialize Shady and return a wrapper for a drawing command.
-- -- Supplying that drawing command will cause it to be invoked regularly.
-- shadyInit' :: String -> IO (Sink Action)
-- shadyInit' title =
--   do initGTV
--      glconfig <- GtkGL.glConfigNew [GtkGL.GLModeRGBA,
--                                     GtkGL.GLModeDepth,
--                                     GtkGL.GLModeDouble,
--                                     GtkGL.GLModeAlpha ]
--      canvas <- GtkGL.glDrawingAreaNew glconfig
--      Gtk.widgetSetSizeRequest canvas 400 400
--      Gtk.onRealize canvas $ GtkGL.withGLDrawingArea canvas $ \_ ->
--        do -- setupMatrices  -- do elsewhere, e.g., runSurface
--           depthFunc  $= Just Less
--           drawBuffer $= BackBuffers
--           clearColor $= Color4 0 0 0.2 1
--      -- Sync canvas size with GL viewport
--      Gtk.onExpose canvas $ \_ -> 
--        do (w',h') <- Gtk.widgetGetSize canvas
--           let w = fromIntegral w' ; h = fromIntegral h'
--           let dim :: GLsizei; start :: GLsizei -> GLint
--               dim = w `min` h ; start s = fromIntegral ((s - dim) `div` 2)
--           viewport $= ((Position (start w) (start h)), (Size dim dim))
--           return True
--      -- Holds redraw action
--      drawRef <- newIORef (return () :: Action)
--      slider  <- timeSlider (\ t -> writeIORef drawRef .........)
--      window <- Gtk.windowNew
--      -- putStrLn "made window"
--      Gtk.set window [ Gtk.containerBorderWidth := 8
--                     , Gtk.windowTitle          := title
--                     , Gtk.containerChild       := canvas
--                     ]
--      -- putStrLn "showing window"
--      Gtk.widgetShowAll window
--      -- putStrLn "calling glewInit"
--      Glew.glewInit
--      -- putStrLn "returning from shadyInit'"
--      return $ timedDisplay window canvas

-- timeSlider :: Sink Double -> IO Gtk.Widget
-- timeSlider refresh =
--   do w <- Gtk.hScaleNewWithRange 0 10 0.01
--      Gtk.set w [ Gtk.rangeValue := 0, Gtk.scaleDigits := 5 ]
--      let getter = Gtk.get w Gtk.rangeValue
--      Gtk.onRangeChangeValue w (\ _ _ -> refresh >> return False)
--      -- TODO: experiment with return False vs True
--      return (Gtk.toWidget w, getter)



display :: GtkGL.GLDrawingArea -> Sink Action
display canvas render =
  do GtkGL.withGLDrawingArea canvas $ \glwindow ->
       do GL.clear [GL.DepthBuffer, GL.ColorBuffer]
          render
          -- glWaitVSync
          finish
          GtkGL.glDrawableSwapBuffers glwindow
     return ()


-- Hm.  Where do I use
--    Gtk.widgetQueueDraw canvas


