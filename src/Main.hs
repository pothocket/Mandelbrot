{-# LANGUAGE RankNTypes, ScopedTypeVariables, PackageImports, TypeFamilies, TupleSections #-}

module Main where

import Graphics.GPipe
import Graphics.GPipe.Context.GLFW (newContext', GLFWWindow, WindowConf)
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW
import Graphics.GPipe.Context.GLFW.Input

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)

import System.IO (hSetBuffering, BufferMode(..), stdout)

import Constants
import Shader
import Util

theWindow :: ContextFactory c ds GLFWWindow
theWindow = newContext' [] (GLFW.WindowConf gWidth gHeight gTitle)

main :: IO ()
main = runContextT theWindow (ContextFormatColor RGB8) $ do
    vertexBuffer :: Buffer os (B2 Float, B3 Float) <- newBuffer 4
    posBuffer :: Buffer os (B2 Float) <- newBuffer $ gWidth * gHeight

    let positions = do
            x <- [1..gWidth]
            y <- [1..gHeight]
            return . fmap fromIntegral $ V2 x y
        asdf = [V2 0 0, V2 100 100, V2 200 200]

    writeBuffer vertexBuffer 0 $ map (, V3 1 1 0) quad
    writeBuffer posBuffer 0 positions

    lift $ hSetBuffering stdout NoBuffering
    
    shader <- compileShader mandelbrotShader
    loop vertexBuffer posBuffer shader

loop :: (Num a, Color c Float ~ V3 a, ContextColorFormat c, b2 ~ (b,b1),
         b ~ (B2 Float, B3 Float), b1 ~ B2 Float, f ~ ContextFormat c ds)
     => Buffer os b
     -> Buffer os b1
     -> CompiledShader os f (PrimitiveArray Triangles b2)
     -> ContextT GLFWWindow os (ContextFormat c ds) IO ()
loop vb pb shader = do
    render $ do
        clearContextColor (V3 0 0 0)
        posArray <- newVertexArray pb
        vertexArray <- newVertexArray vb
        let primitiveArray = toPrimitiveArrayInstanced TriangleStrip (,) vertexArray posArray
        shader primitiveArray
    swapContextBuffers

    --writeBuffer vb 0 $ map (,V3 1 1 0) myTriangle

    closeRequested <- GLFW.windowShouldClose
    unless closeRequested $
        loop vb pb shader

{-
myShader :: Shader os (ContextFormat RGBFloat ds) (PrimitiveArray p ((B2 Float, B3 Float), B2 Float)) ()
myShader = do
    ps <- toPrimitiveStream id
    let ps' = fmap transformStream ps
    let ps'' = fmap (\(V4 x y j k, V3 r g b) -> (V4 x y j k, V3 x y (0.5 * (y + x)))) ps'
    fragmentStream <- rasterize (const (FrontAndBack, ViewPort (V2 0 0) (V2 gWidth gHeight), DepthRange 0 1)) ps''
    drawContextColor (const (ContextColorOption NoBlending (V3 True True True))) fragmentStream
-}


myTriangle :: [V2 Float]
myTriangle = [ V2 0 0
             , V2 64 0
             , V2 0 64
             ]

quad :: [V2 Float]
quad = [ V2 0 0
       , V2 0 1
       , V2 1 0 
       , V2 1 1
       ]

toV4Pos :: (Num a) => V2 a -> V4 a
toV4Pos (V2 x y) = V4 x y 0 1
