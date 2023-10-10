module Pyramids ( batchDrawPyramids) where
import Graphics.GL.Core33
import Graphics.GL.Types
import Foreign -- includes many sub-modules

pyramidConnectivity = [
    0, 1, 2,
    0, 1, 3, 
    0, 2, 3,
    1, 2, 3
    ] :: [GLuint]


batchDrawPyramids vertices = do 
    let verticesSize = fromIntegral $ sizeOf (0.0 :: GLfloat) * (length vertices)
    let numTriangles = fromIntegral (length vertices)
    let indicesSize = fromIntegral $ sizeOf (0 :: GLuint) * (length pyramidConnectivity) 
    vertices1P <- newArray vertices 
    indicesP <- newArray pyramidConnectivity 
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

    glDrawElements GL_TRIANGLES numTriangles GL_UNSIGNED_INT nullPtr
    glBindVertexArray 0

    free vboP
    free vaoP
    free eboP
    free indicesP 
    free vertices1P 
    return () 



