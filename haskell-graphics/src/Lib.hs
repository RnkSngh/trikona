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


draw :: IO ()
draw = do
    succeeded  <- GLFW.init
    GLFW.defaultWindowHints
    maybeWindow <- GLFW.createWindow winWidth winHeight winTitle Nothing Nothing
    case maybeWindow of
        Nothing -> putStrLn "Failed to create a GLFW window!"
        Just window -> do
            -- calibrate the viewport
            GLFW.makeContextCurrent (Just window)
            (x,y) <- GLFW.getFramebufferSize window
            glViewport 0 0 (fromIntegral x) (fromIntegral y)
            glClearColor 0 1.0 0.0 1.0
            glClear GL_COLOR_BUFFER_BIT

            GLFW.swapBuffers window
            -- enter our main loop
            let loop = do
                    shouldContinue <- not <$> GLFW.windowShouldClose window
                    when shouldContinue $ do
                        GLFW.pollEvents
                        -- event poll
                        -- drawing
                        -- swap buffers and go again
                        loop
            loop
    GLFW.terminate

someFunc :: IO ()
someFunc = draw 
