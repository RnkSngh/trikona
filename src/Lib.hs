module Lib
    ( draw  
    ) where

import Control.Monad (when)
import Data.Typeable

import qualified Graphics.UI.GLFW as GLFW

import Graphics.GL.Core33
import Graphics.GL.Types
import Foreign -- includes many sub-modules
import Foreign.C.String (newCAStringLen, newCString)
import Interactivity (move, rotateY, rotateX)
import Triangles (getVertices, drawTriangleHoles, batchDrawTriangles, drawTriangle, getTriangleHoleCoordinates)
import ShaderUtils
import ShaderSources
import PointerUtils
import Linear
import Control.Lens
import Pyramids (batchDrawPyramids)

winWidth = 1080 
winHeight = 1080 
winTitle = "Serpenski Triangles"
step = 0.01 -- The step with which to apply
iterations = 10 



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



pyramidVerticies = [
        (sqrt (8/9)), 0, -(1/3),
        -(sqrt (2/9)), sqrt (2/3), -(1/3),
        -(sqrt (2/9)), -sqrt (2/3), -(1/3),
        0, 0, 1
        ] :: [GLfloat]

pyramidVerticies2 = map (\x -> x/2) pyramidVerticies  



attachShaders vertexShader fragmentShader transP = do
    -- Link Arrays
    shaderProgram <- glCreateProgram
    glAttachShader shaderProgram vertexShader
    glAttachShader shaderProgram fragmentShader
    glLinkProgram shaderProgram
    PointerUtils.checkError shaderProgram GL_LINK_STATUS glGetProgramiv glGetProgramInfoLog  "GL Link error"
    glDeleteShader vertexShader
    glDeleteShader fragmentShader
    return shaderProgram  

-- Update time and ourColor uniforms
updateUniforms transP projectionP shaderProgram ourColorCString transformCString projectionCString = do
    -- Calculate green shader
    timeValue <- maybe 0 realToFrac <$> GLFW.getTime
    let greenValue = sin timeValue / 2 + 0.5

    -- Attach time uniform 
    vertexColorLocation <- glGetUniformLocation shaderProgram ourColorCString 
    glUniform4f vertexColorLocation (1-greenValue) (greenValue/2) greenValue 1.0

    -- Attach transform uniform
    transformLoc <- glGetUniformLocation shaderProgram transformCString 
    glUniformMatrix4fv transformLoc 1 GL_FALSE (castPtr transP)

    projectionLoc <- glGetUniformLocation shaderProgram projectionCString 
    glUniformMatrix4fv projectionLoc 1 GL_FALSE (castPtr projectionP)


    return ()


handleKeyClick pointer window key scanCode keyState modKeys = do
    when (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed) 
        (GLFW.setWindowShouldClose window True)
    when (key == GLFW.Key'Up && (keyState == GLFW.KeyState'Pressed || keyState == GLFW.KeyState'Repeating)) 
        (move _y 0.01 pointer )
    when (key == GLFW.Key'Down && (keyState == GLFW.KeyState'Pressed || keyState == GLFW.KeyState'Repeating)) 
        (move _y (-0.01) pointer )
    when (key == GLFW.Key'Right && (keyState == GLFW.KeyState'Pressed || keyState == GLFW.KeyState'Repeating)) 
        (move _x 0.01 pointer )
    when (key == GLFW.Key'Left && (keyState == GLFW.KeyState'Pressed || keyState == GLFW.KeyState'Repeating)) 
        (move _x (-0.01) pointer )
    when (key == GLFW.Key'J && (keyState == GLFW.KeyState'Pressed || keyState == GLFW.KeyState'Repeating)) 
        (move _z (0.01) pointer )
    when (key == GLFW.Key'K && (keyState == GLFW.KeyState'Pressed || keyState == GLFW.KeyState'Repeating)) 
        (move _z (-0.01) pointer )
    putStrLn (show key)
    when (key == GLFW.Key'D && (keyState == GLFW.KeyState'Pressed || keyState == GLFW.KeyState'Repeating)) 
        (rotateY (pi/50) pointer)
    when (key == GLFW.Key'A && (keyState == GLFW.KeyState'Pressed || keyState == GLFW.KeyState'Repeating)) 
        (rotateY (-pi/50) pointer)
    when (key == GLFW.Key'W && (keyState == GLFW.KeyState'Pressed || keyState == GLFW.KeyState'Repeating)) 
        (rotateX (pi/50) pointer)
    when (key == GLFW.Key'S && (keyState == GLFW.KeyState'Pressed || keyState == GLFW.KeyState'Repeating)) 
        (rotateX (-pi/50) pointer)






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

    ourColorCString <- newCString "ourColor"
    transformCString <- newCString "transform1"
    projectionCString <- newCString "projection"

    case maybeWindow of
        Nothing -> putStrLn "Failed to create a GLFW window!"
        Just window -> do
            transP <- malloc
            let transformMatrix = mkTransformationMat identity (V3 (0.0::GLfloat) 0.0 0.0 ) 
            poke transP (transpose transformMatrix)
            move _z (2.0)  transP

            projectionP <- malloc
            let projection  = perspective ((pi /4) ::Float) 1.0 1.0 100.0
            putStrLn $ show projection
            poke projectionP projection 
            
            GLFW.setKeyCallback window (Just (handleKeyClick transP))

            -- calibrate the viewport
            GLFW.makeContextCurrent (Just window)
            (x,y) <- GLFW.getFramebufferSize window
            glViewport 0 0 (fromIntegral x) (fromIntegral y)

            fragmentShaderBlue <- ShaderUtils.compileShader GL_FRAGMENT_SHADER ShaderSources.fragmentShaderSourceBlue
            fragmentShaderPulse <- ShaderUtils.compileShader GL_FRAGMENT_SHADER ShaderSources.fragmentShaderSourcePulse
            vertexShader <- ShaderUtils.compileShader GL_VERTEX_SHADER ShaderSources.vertexShaderSourceTransform 

            case (fragmentShaderBlue, vertexShader, fragmentShaderPulse) of 
                (Just fragmentShaderBlue, Just vertexShader, Just fragmentShaderPulse)-> do

                    blueShaderProgram <- attachShaders vertexShader fragmentShaderBlue transP
                    pulseShaderProgram <- attachShaders vertexShader fragmentShaderPulse transP

                    let triangleHoleVertices = getTriangleHoleCoordinates iterations startingSubTriangle

                    -- Render loop
                    let loop = do
                            shouldContinue <- not <$> GLFW.windowShouldClose window
                            when shouldContinue $ do
                                GLFW.pollEvents
                                glClearColor 0.0 0.0 0.0 1.0
                                glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT
          
                                glUseProgram blueShaderProgram 
                                updateUniforms transP projectionP blueShaderProgram ourColorCString transformCString projectionCString
                                drawTriangle triangleVerticies 

                                glUseProgram pulseShaderProgram 
                                updateUniforms transP projectionP pulseShaderProgram ourColorCString transformCString projectionCString
                                
                                batchDrawTriangles triangleHoleVertices

                                GLFW.swapBuffers window
                                loop
                    loop
                _ -> do
                    putStrLn "Shaders Didn't Compile"
    GLFW.terminate
