module Lib
    ( someFunc
    ) where

import Control.Monad (when)
import Data.Typeable

import qualified Graphics.UI.GLFW as GLFW

import Graphics.GL.Core33
import Graphics.GL.Types
import Foreign -- includes many sub-modules
import Foreign.C.String (newCAStringLen)
import ShaderUtils
import ShaderSources
import PointerUtils

winWidth = 2080 
winHeight = 2080 
winTitle = "Serpenski Triangles"
baseTriangleVerticies = [
        0.0, (sqrt 3)/4 + 0.5 , 0.0,
        0.5, - (sqrt 3)/4+0.5, 0.0,
        -0.5, - (sqrt 3)/4 + 0.5, 0.0
        ] :: [GLfloat]


triangle2 = [
        0.25, 0.50, 0.0,
        0, -0.43301270189 + 0.5 , 0.0,
        -0.25, 0.50, 0.0
        ] :: [GLfloat]

-- subTriangles :: IO [[GLfloat]]
-- subTriangles = [[
--         0.25, 0.0, 0.0, -- 0-2
--         0, -0.43301270189, 0.0, -- 3-5
--         -0.25, 0.0, 0.0 -- 6-8 
--         ]] :: [[GLfloat]]




indices = [
    0, 1, 2
    ] :: [GLuint]

indicesSize = fromIntegral $ sizeOf (0 :: GLuint) * (length indices) 

-- Returns a 9 coordinate array of an up-side down equilateral triangle at the given anchor point  
getVertices :: (GLfloat, GLfloat) -> GLfloat -> [GLfloat]
getVertices (x_1, y_1) l = [ x_1 + b, y_1, 0.0, x_1 + (b/2), y_1 - h, 0.0, x_1, y_1, 0.0 ]
    where
        b = l
        h = l * (sqrt 3) / 2 
    



attachShaders vertexShaderSource fragmentShaderSource = do

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

        _ -> do 
            putStrLn "One of the shaders is nothing"
    return ()






drawTriangle :: [GLfloat]  -> String -> String -> IO() 
drawTriangle vertices vertexShaderSource fragmentShaderSource = do
    let verticesSize = fromIntegral $ sizeOf (0.0 :: GLfloat) * (length vertices)
    vertices1P <- newArray vertices 
    indicesP <- newArray indices
    vboP <- malloc
    vaoP <- malloc
    eboP <- malloc

    glGenBuffers 1 vboP
    vbo <- peek vboP
    glGenVertexArrays 1 vaoP
    vao <- peek vaoP

    glGenBuffers 1 eboP
    ebo <- peek eboP


    glBindVertexArray vao
    glBindBuffer GL_ARRAY_BUFFER vbo
    glBufferData GL_ARRAY_BUFFER verticesSize (castPtr vertices1P) GL_STATIC_DRAW

    glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo 
    glBufferData GL_ELEMENT_ARRAY_BUFFER indicesSize (castPtr indicesP) GL_STATIC_DRAW

    let threeFloats = fromIntegral $ sizeOf (0.0::GLfloat) * 3
    glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE threeFloats nullPtr
    glEnableVertexAttribArray 0

    glDrawElements GL_TRIANGLES 3 GL_UNSIGNED_INT nullPtr
    glBindVertexArray 0

    return ()


-- drawTriangleHoles :: [[GLfloat]] -> String -> String -> IO() 
drawTriangleHoles level anchorTriangle vertexShaderSource fragmentShaderSource =  do
    -- let a = \x drawTriangle
     

    -- let a = sequence $ 
    -- verticies <- subTriangles  
    -- let a = \x -> [drawTriangle x vertexShaderSource fragmentShaderSource ]
    -- let b = \x -> drawTriangle x vertexShaderSource fragmentShaderSource 


    
    -- putStrLn ( "typeOf typeOf argument " ++ (show $ typeOf $ sequence $ IO (a triangle2)))
    -- putStrLn ( "typeOf typeOf argument " ++ (show $ typeOf a))
    -- let y = \x -> a vertexShaderSource fragmentShaderSource
    -- x = fmap a verticies 

    -- putStrLn "working"
    -- x <- a <$> subTriangles
    -- fmap a subTriangles
    -- x <- b <$> subTriangles
    
    -- let x = [ putStrLn "PRINTING" | i <- triangle2] 

    
    -- returns going from [float] ->  IO() ; we want [float] -> [IO()]
    -- x = 
    -- putStrLn ( "typeOf typeOf argument" ++ (show $ typeOf subTri1))
    -- putStrLn ( "typeOf Subtriangles" ++ (show $ typeOf subTriangles))
    -- a $ subTriangles !! 0
    -- drawTriangle triangle2 vertexShaderSource fragmentShaderSource 

    
    
    -- putStrLn ("Drawing Triangle Level: " ++ show level ++ " " ++ (concat $ map (\x -> show x ++ ", ") anchorTriangle ))
    drawTriangle  anchorTriangle vertexShaderSource fragmentShaderSource

    if level == 0 
        then return()
        else do
            let w =  (anchorTriangle !! 0) - (anchorTriangle !! 6) -- Width of current triangleHole 
            let h = (anchorTriangle !! 1) - (anchorTriangle !! 4)
            let p_x = anchorTriangle !! 6 
            let p_y =  anchorTriangle !! 7 

            let leftBottomTriangle = getVertices ( p_x - w/4, p_y - h/2 ) (w/2)
            let rightBottomTriangle = getVertices (p_x + 3*w/4, p_y - h/2 ) (w/2) 
            let topTriangle = getVertices (p_x + w/4 , p_y + h/2 ) (w/2) 

            drawTriangleHoles (level-1) leftBottomTriangle vertexShaderSource fragmentShaderSource
            drawTriangleHoles (level-1) (rightBottomTriangle) vertexShaderSource fragmentShaderSource
            drawTriangleHoles (level-1) (topTriangle) vertexShaderSource fragmentShaderSource
        

    

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
                        attachShaders ShaderSources.vertexShaderSource ShaderSources.fragmentShaderSourceBlue
                        drawTriangle baseTriangleVerticies ShaderSources.vertexShaderSource ShaderSources.fragmentShaderSourceBlue

                        attachShaders ShaderSources.vertexShaderSource ShaderSources.fragmentShaderSourceBlack
                        drawTriangleHoles 7 triangle2 ShaderSources.vertexShaderSource ShaderSources.fragmentShaderSourceBlack 
                        GLFW.swapBuffers window
                        loop
            loop
    GLFW.terminate

someFunc :: IO ()
someFunc = draw 
