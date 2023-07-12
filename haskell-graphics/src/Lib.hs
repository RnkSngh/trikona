module Lib
    ( someFunc
    ) where


import Control.Monad (when)
-- GLFW-b, qualified for clarity
import qualified Graphics.UI.GLFW as GLFW
-- gl, all types and funcs here will already start with "gl"
import Graphics.GL.Core33
import Graphics.GL.Types
import Foreign -- includes many sub-modules
import Foreign.C.String (newCAStringLen)

winWidth = 1620
winHeight = 1080 
winTitle = "Serpenski Triangles"

vertices = [
        -0.5, -0.5, 0.0, -- first vertex
        0.5, -0.5, 0.0, -- second vertex
        0.0,  0.5, 0.0, -- third vertex
        -0.72,  -0.75, 0.20, 
        0.20,  -0.25, 0.20, 
        0.0,  0.5, 0.0 
        ] :: [GLfloat]
verticesSize = fromIntegral $ sizeOf (0.0 :: GLfloat) * (length vertices)

vertexShaderSource version = "#version " ++ version ++ " core\n\
    \layout (location = 0) in vec3 position;\
    \void main()\
    \{\
    \    gl_Position = vec4(position.x, position.y, position.z, 1.0);\
    \}" 

fragmentShaderSource = unlines ["#version 330 core",
    "out vec4 FragColor;",
    "",
    "void main()",
    "{",
    "    FragColor = vec4(1.0f, 0.5f, 0.2f, 1.0f);",
    "}"]

compileVertexShader version =  do
    putStrLn $ "Trying Version: " ++ version
    vertexShader <- glCreateShader GL_VERTEX_SHADER
    (sourceP,len) <- newCAStringLen (vertexShaderSource version)
    linesPtrsPtr <- newArray [sourceP]
    lengthsPtr <- newArray [fromIntegral len]
    glShaderSource vertexShader 1 linesPtrsPtr lengthsPtr
    glCompileShader vertexShader
    vertexSuccessP <- malloc
    glGetShaderiv vertexShader GL_COMPILE_STATUS vertexSuccessP
    vertexSuccess <- peek vertexSuccessP
    case vertexSuccess of
        GL_FALSE ->  do
            putStrLn "Vertex Shader Compile Error:"
            let infoLength = 512
            resultP <- malloc
            infoLog <- mallocArray (fromIntegral infoLength)
            glGetShaderInfoLog vertexShader (fromIntegral infoLength) resultP infoLog
            result <- fromIntegral <$> peek resultP
            logBytes <- peekArray result infoLog
            putStrLn (map (toEnum.fromEnum) logBytes)
            return Nothing
        GL_TRUE -> do
            return (Just vertexShader)

compileFragmentShader = do
    fragmentShader <- glCreateShader GL_FRAGMENT_SHADER
    (sourceP,len) <- newCAStringLen fragmentShaderSource 
    linesPtrsPtr <- newArray [sourceP]
    lengthsPtr <- newArray [fromIntegral len]
    glShaderSource fragmentShader 1 linesPtrsPtr lengthsPtr
    glCompileShader fragmentShader
    fragmentSuccessP <- malloc
    glGetShaderiv fragmentShader GL_COMPILE_STATUS fragmentSuccessP
    fragmentSuccess <- peek fragmentSuccessP
    case fragmentSuccess  of 
        GL_FALSE -> do
            putStrLn "Fragment Shader Compile Error:"
            let infoLength = 512
            resultP <- malloc
            infoLog <- mallocArray (fromIntegral infoLength)
            glGetShaderInfoLog fragmentShader (fromIntegral infoLength) resultP infoLog
            result <- fromIntegral <$> peek resultP
            logBytes <- peekArray result infoLog
            putStrLn (map (toEnum.fromEnum) logBytes)
            return Nothing
        GL_TRUE -> do
            return (Just fragmentShader)

        



versionsArray =  [ "330"] 
drawTriangle = do
    verticesP <- newArray vertices

    (vertexShader:_) <- sequence (map compileVertexShader versionsArray)
    fragmentShader <- compileFragmentShader

    case ( vertexShader, fragmentShader) of 
        (Just vertexShader, Just fragmentShader) -> do
            -- Link Arrays
            shaderProgram <- glCreateProgram
            glAttachShader shaderProgram vertexShader
            glAttachShader shaderProgram fragmentShader
            glLinkProgram shaderProgram
            linkingSuccessP <- malloc
            glGetProgramiv shaderProgram GL_LINK_STATUS linkingSuccessP
            linkingSuccess <- peek linkingSuccessP
            when (linkingSuccess == GL_FALSE) $ do
                putStrLn "Program Linking Error:"
                let infoLength = 512
                resultP <- malloc
                infoLog <- mallocArray (fromIntegral infoLength)
                glGetProgramInfoLog shaderProgram (fromIntegral infoLength) resultP infoLog
                result <- fromIntegral <$> peek resultP
                logBytes <- peekArray result infoLog
                putStrLn (map (toEnum.fromEnum) logBytes)

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
            glBindVertexArray 0

            glBindVertexArray vao
            glDrawArrays GL_TRIANGLES 0 3
            glBindVertexArray 0

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
            glClearColor 0 1.0 0.0 1.0
            glClear GL_COLOR_BUFFER_BIT

            drawTriangle 
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
