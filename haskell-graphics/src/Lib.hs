module Lib
    ( someFunc
    ) where

import Control.Monad (when)
import Data.Typeable

import qualified Graphics.UI.GLFW as GLFW

import Graphics.GL.Core33
import Graphics.GL.Types
import Foreign -- includes many sub-modules
import Foreign.C.String (newCAStringLen, newCString)
import Interactivity (moveUp, moveLeft, moveDown, moveRight)
import Triangles (getVertices, drawTriangleHoles, batchDrawTriangles)
import ShaderUtils
import ShaderSources
import PointerUtils
import Linear
import Control.Lens

winWidth = 1080 
winHeight = 1080 
winTitle = "Serpenski Triangles"
step = 0.01 -- The step with which to apply
iterations = 2



baseTriangleVerticies = [
        0.0, (sqrt 3)/4, 0.0,
        0.5, - (sqrt 3)/4, 0.0,
        -0.5, - (sqrt 3)/4, 0.0
        ] :: [GLfloat]


startingSubTriangle = [
        0.25, 0.00, 0.0,
        0, -0.43301270189 , 0.0,
        -0.25, 0.00, 0.0
        ] :: [GLfloat]

attachShaders vertexShaderSource fragmentShaderSource transP = do
    vertexShader <-  ShaderUtils.compileShader GL_VERTEX_SHADER vertexShaderSource 
    fragmentShader <- ShaderUtils.compileShader GL_FRAGMENT_SHADER fragmentShaderSource

    case ( vertexShader, fragmentShader) of 
        (Just vertexShader, Just fragmentShader) -> do
            -- Link Arrays
            shaderProgram <- glCreateProgram
            glAttachShader shaderProgram vertexShader
            glAttachShader shaderProgram fragmentShader
            glLinkProgram shaderProgram
            PointerUtils.checkError shaderProgram GL_LINK_STATUS glGetProgramiv glGetProgramInfoLog  "GL Link error"
            glDeleteShader vertexShader
            glDeleteShader fragmentShader
            glUseProgram shaderProgram  
            doUpdate transP shaderProgram

        _ -> do 
            putStrLn "One of the shaders is nothing"
    return ()

doUpdate transP shaderProgram = do
    -- Attach Green shader
    timeValue <- maybe 0 realToFrac <$> GLFW.getTime
    let greenValue = sin timeValue / 2 + 0.5
    ourColor <- newCString "ourColor"

    -- Attach 
    vertexColorLocation <- glGetUniformLocation shaderProgram ourColor
    glUniform4f vertexColorLocation 0.0 greenValue 0.0 1.0
    transform <- newCString "transform1"
    transformLoc <- glGetUniformLocation shaderProgram transform
    glUniformMatrix4fv transformLoc 1 GL_FALSE (castPtr transP)


callback pointer window key scanCode keyState modKeys = do
    when (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed) 
        (GLFW.setWindowShouldClose window True)
    when (key == GLFW.Key'Up && (keyState == GLFW.KeyState'Pressed || keyState == GLFW.KeyState'Repeating)) 
        (moveUp 0.01 pointer )
    when (key == GLFW.Key'Down && (keyState == GLFW.KeyState'Pressed || keyState == GLFW.KeyState'Repeating)) 
        (moveDown 0.01 pointer )
    when (key == GLFW.Key'Right && (keyState == GLFW.KeyState'Pressed || keyState == GLFW.KeyState'Repeating)) 
        (moveLeft 0.01 pointer )
    when (key == GLFW.Key'Left && (keyState == GLFW.KeyState'Pressed || keyState == GLFW.KeyState'Repeating)) 
        (moveRight 0.01 pointer )



draw :: IO ()
draw = do
    succeeded  <- GLFW.init
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 3)
    GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
    GLFW.windowHint (GLFW.WindowHint'Resizable False)
    GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True);
    maybeWindow <- GLFW.createWindow winWidth winHeight winTitle Nothing Nothing
    let triangleVerticies = baseTriangleVerticies

    case maybeWindow of
        Nothing -> putStrLn "Failed to create a GLFW window!"
        Just window -> do
            transP <- malloc
            let transformMatrix = mkTransformationMat identity (V3 (0.0::GLfloat) 0.0 0.0 ) 
            poke transP (transpose transformMatrix)
            GLFW.setKeyCallback window (Just (callback transP))

            -- calibrate the viewport
            GLFW.makeContextCurrent (Just window)
            (x,y) <- GLFW.getFramebufferSize window
            glViewport 0 0 (fromIntegral x) (fromIntegral y)

            -- enter our main loop
            let loop = do
                    shouldContinue <- not <$> GLFW.windowShouldClose window
                    when shouldContinue $ do
                        GLFW.pollEvents
                        glClearColor 0.0 0.0 0.0 1.0
                        glClear GL_COLOR_BUFFER_BIT
                        attachShaders ShaderSources.vertexShaderSourceTransform ShaderSources.fragmentShaderSourceBlue transP
                        batchDrawTriangles triangleVerticies 

                        attachShaders ShaderSources.vertexShaderSourceTransform ShaderSources.fragmentShaderSourcePulse transP
                        drawTriangleHoles iterations startingSubTriangle 
                        GLFW.swapBuffers window
                    loop
            loop
    GLFW.terminate

someFunc :: IO ()
someFunc = draw 
