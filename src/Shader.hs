{-# LANGUAGE PackageImports, FlexibleInstances, FlexibleContexts, TypeFamilies #-}
module Shader (mandelbrotShader) where

import Prelude hiding ((<*))

import Graphics.GPipe
import Graphics.GPipe.Context.GLFW (newContext', GLFWWindow, WindowConf)
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW

import Data.Monoid

import Linear
import Constants
import Complex
import Lists

mandelbrotShader :: Shader os (ContextFormat RGBFloat ds) (PrimitiveArray p ((B2 Float, B3 Float), B2 Float)) ()
mandelbrotShader = do
    ps <- toPrimitiveStream id
    let ps' = fmap (mandelbrotColor . transformStream) ps
    fragmentStream <- rasterize (const (FrontAndBack, ViewPort (V2 0 0) (V2 gWidth gHeight), DepthRange 0 1)) ps'
    drawContextColor (const (ContextColorOption NoBlending (V3 True True True))) fragmentStream

mandelbrotColor :: (a ~ S V Float) => (V4 a, V3 a) -> (V4 a, V3 a)
mandelbrotColor (V4 x y j k, _) = (V4 x y j k, V3 r g b)
    where mandelIterations :: S V Float
          mandelIterations = snd $ while (\(z, n) -> magnitude (mandel (x:+y) (fromPair z)) <* 2
                                                     &&* n <* maxIterations) 
                                         (\(z, n) -> (toPair (mandel (x:+y) (fromPair z)), n+1)) 
                                         ((0, 0), 0)
          maxIterations = 256
          g = ifB (mandelIterations ==* maxIterations) 0 (mandelIterations / maxIterations)
          b = 0
          r = 0

transformStream :: (Floating a) 
                => ((V2 a, V3 a), V2 a)
                -> (V4 a, V3 a)
transformStream ((pos, col), V2 xPos yPos) = (V4 xFinal yFinal 0 1, col)
    where V2 xFinal yFinal = toGLPos $ pos + V2 xPos yPos

toGLPos :: (Floating a) => V2 a -> V2 a
toGLPos (V2 x y) = V2 ((x/fWidth - 0.5)*2) ((0.5 - y/fHeight)*2)

----------------------------------------------------------------

mandel :: (Floating a) => Complex a -> Complex a -> Complex a
mandel c z = square z + c