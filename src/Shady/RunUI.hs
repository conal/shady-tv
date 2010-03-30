{-# LANGUAGE TypeFamilies, TypeSynonymInstances, MultiParamTypeClasses
  , FlexibleInstances, FlexibleContexts, UndecidableInstances
  #-}
{-# OPTIONS_GHC -Wall #-}

----------------------------------------------------------------------
-- |
-- Module      :  Shady.RunUI
-- Copyright   :  (c) Conal Elliott 2010
-- License     :  AGPLv3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Geometry, containing surfaces.  Start simple.  Later, add
-- transformation & lights.
----------------------------------------------------------------------

module Shady.RunUI (Render,Compile(..), samplerIn, runUI) where

import Control.Applicative ((<$>))

import Graphics.Glew (glewInit)

import Control.Compose (result)

import qualified Shady.Vec as V
import Shady.Language.Exp hiding (indices)
import Shady.Misc (Sink,Action)
import Shady.Run (MeshSize,grid)
import Shady.MechanicsGL (EyePos,mkIndices,setupMatrices)
import Shady.RunSurface (FullSurf,compileSurf,EyePosE)
import Text.PrettyPrint.Leijen.DocExpr (HasExpr)

-- For samplerIn
import Graphics.Rendering.OpenGL (TextureObject(..))

-- For UI examples
import Interface.TV.Gtk


-- Geometry.  For now just a surface-with-image.
type GeometryE = FullSurf

-- | Renderer
type Render a = MeshSize -> EyePos -> IO a

mapR :: (a -> b) -> (Render a -> Render b)
mapR = result.result.fmap

-- initG :: Action
-- initG = do putStrLn "initG"
--            initGUI
--            initGL
--            return ()

-- Compile with exactly one argument.  Used after progressive uncurrying
compile1 :: (FromE u', u ~ ExpT u', HasType u, HasExpr u) =>
            (u' -> GeometryE) -> Render (Sink u)
compile1 f size eyePos@(ex,ey,ez) =
  do glewInit
     -- putStrLn "setupMatrices"
     -- setupMatrices eyePos
     -- putStrLn "mkIndices"
     setI <- mkIndices indices
     -- putStrLn "compileSurf"
     -- Set up matrices before rendering surface
     compileSurf size vertices eyePosE (setupMatrices eyePos >> setI) f
 where
   eyePosE :: EyePosE
   eyePosE = pureE (V.vec3 ex ey ez)
   (indices,vertices) = grid size

-- | Compile
class Compile src obj where
  compile :: src -> Render obj

-- Single argument.  This case does the work.
instance (FromE u', u ~ ExpT u', HasType u, HasExpr u) =>
         Compile (u' -> GeometryE) (Sink u) where
  compile = compile1

-- Just geometry.  Delegate to function from unit
instance Compile GeometryE Action where
  compile g = mapR ($ ()) (compile (\ () -> g))

-- Two or more arguments.  Delegate to uncurried version.
instance Compile ((a',b') -> c') ((a,b) -> c) => Compile (a' -> b' -> c') (a -> b -> c) where
  compile = mapR curry . compile . uncurry

-- I don't know how to handle this last case with a type family, so I'm
-- using MPTC + fundep instead.  Maybe an Uncurry type family.


{--------------------------------------------------------------------
    UIs
--------------------------------------------------------------------}

-- | Input an image from a file.
samplerIn :: In Sampler2
samplerIn = (sampler2 . textureID) <$> textureIn
 where
   textureID (TextureObject i) = i     -- not exported from OpenGL

-- | Compile and run.  See also 'compileUI'
runUI :: Compile src obj => MeshSize -> EyePos -> Out obj -> src -> Action
runUI size eye out src =
  runOutIO "Shady" out (compile src size eye)
