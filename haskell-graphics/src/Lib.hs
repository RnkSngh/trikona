module Lib
    ( someFunc
    ) where


import Control.Monad (when)
-- GLFW-b, qualified for clarity
import qualified Graphics.UI.GLFW as GLFW
-- gl, all types and funcs here will already start with "gl"
import Graphics.GL.Core33
import Graphics.GL.Types

winWidth = 1620
winHeight = 1080 
winTitle = "Serpenski Triangles"


callback :: GLFW.KeyCallback
callback window key scanCode keyState modKeys = do
    print key
    when (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed)
        (GLFW.setWindowShouldClose window True)


draw :: IO ()
draw = do
    GLFW.init
    GLFW.defaultWindowHints
    maybeWindow <- GLFW.createWindow winWidth winHeight winTitle Nothing Nothing
    case maybeWindow of
        Nothing -> putStrLn "Failed to create a GLFW window!"
        Just window -> do
            -- enable keys
            GLFW.setKeyCallback window (Just callback)
            -- calibrate the viewport
            GLFW.makeContextCurrent (Just window)
            (x,y) <- GLFW.getFramebufferSize window
            glViewport 0 0 (fromIntegral x) (fromIntegral y)
            -- enter our main loop
            let loop = do
                    shouldContinue <- not <$> GLFW.windowShouldClose window
                    when shouldContinue $ do
                        -- event poll
                        GLFW.pollEvents
                        -- drawing
                        glClearColor 0 1.0 0.0 1.0
                        glClear GL_COLOR_BUFFER_BIT
                        -- swap buffers and go again
                        GLFW.swapBuffers window
                        loop
            loop
    GLFW.terminate

someFunc :: IO ()
someFunc = draw 

