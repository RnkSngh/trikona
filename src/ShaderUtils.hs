
module ShaderUtils 
(compileShader) where

import Graphics.GL.Core33
import Foreign -- includes many sub-modules
import Foreign.C.String (newCAStringLen)
import PointerUtils

compileShader shaderType shaderSource = do
    shader <- glCreateShader shaderType 
    (sourceP,len) <- newCAStringLen shaderSource 
    linesPtrsPtr <- newArray [sourceP]
    lengthsPtr <- newArray [fromIntegral len]
    glShaderSource shader 1 linesPtrsPtr lengthsPtr
    glCompileShader shader

    vertexSuccess <- PointerUtils.checkError shader GL_COMPILE_STATUS glGetShaderiv glGetShaderInfoLog "GL Compile Error"
    case vertexSuccess of
        GL_FALSE ->  do
            return Nothing
        GL_TRUE -> do
            return (Just shader)

