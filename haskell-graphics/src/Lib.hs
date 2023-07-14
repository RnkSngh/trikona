module Lib
    ( someFunc
    ) where

import Control.Monad (when)

import qualified Graphics.UI.GLFW as GLFW

import Graphics.GL.Core33
import Graphics.GL.Types
import Foreign -- includes many sub-modules
import Foreign.C.String (newCAStringLen)
import ShaderUtils
import ShaderSources
import PointerUtils

winWidth = 1620
winHeight = 1080 
winTitle = "Serpenski Triangles"
vertices = [
        -0.5, -0.5, 0.0, -- first vertex
        0.5, -0.5, 0.0, -- second vertex
        0.0,  0.5, 0.0 -- third vertex
        ] :: [GLfloat]

verticesSize = fromIntegral $ sizeOf (0.0 :: GLfloat) * (length vertices)



drawTriangle = do
    verticesP <- newArray vertices
    vertexShader <-  ShaderUtils.compileShader GL_VERTEX_SHADER ShaderSources.vertexShaderSource 
    fragmentShader <- ShaderUtils.compileShader GL_FRAGMENT_SHADER ShaderSources.fragmentShaderSource 

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


            vboP <- malloc
            glGenBuffers 1 vboP
            vbo <- peek vboP
            vaoP <- malloc
            glGenVertexArrays 1 vaoP
            vao <- peek vaoP
            glBindVertexArray vao
            glBindBuffer GL_ARRAY_BUFFER vbo

            glBufferData GL_ARRAY_BUFFER verticesSize (castPtr verticesP) GL_STATIC_DRAW
            let threeFloats = fromIntegral $ sizeOf (0.0::GLfloat) * 3
            glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE threeFloats nullPtr
            glEnableVertexAttribArray 0
            -- glBindVertexArray 0

            glBindVertexArray vao
            glDrawArrays GL_TRIANGLES 0 3
            -- glBindVertexArray 0

        _ -> do 
            putStrLn "One of the shaders is nothing"

    return () 


draw :: IO ()
draw = do
    succeeded  <- GLFW.init
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 3)
    GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
    GLFW.windowHint (GLFW.WindowHint'Resizable False)
    GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True);
    -- GLFW.defaultWindowHints
    maybeWindow <- GLFW.createWindow winWidth winHeight winTitle Nothing Nothing
    case maybeWindow of
        Nothing -> putStrLn "Failed to create a GLFW window!"
        Just window -> do
            -- calibrate the viewport
            GLFW.makeContextCurrent (Just window)
            (x,y) <- GLFW.getFramebufferSize window
            glViewport 0 0 (fromIntegral x) (fromIntegral y)

            -- enter our main loop
            let loop = do
                    shouldContinue <- not <$> GLFW.windowShouldClose window
                    when shouldContinue $ do
                        GLFW.pollEvents
                        -- glClearColor 0 1.0 0.0 1.0
                        -- glClear GL_COLOR_BUFFER_BIT
                        drawTriangle 
                        GLFW.swapBuffers window
                        loop
            loop
    GLFW.terminate

someFunc :: IO ()
someFunc = draw 
