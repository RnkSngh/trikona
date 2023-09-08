module Triangles (drawTriangle, batchDrawTriangles , drawTriangleHoles, getVertices) where
import Graphics.GL.Core33
import Graphics.GL.Types
import Foreign -- includes many sub-modules
import Foreign.C.String (newCAStringLen, newCString)
import Interactivity (moveUp, moveLeft, moveDown, moveRight)
import ShaderUtils
import ShaderSources
import PointerUtils
import Linear
import Control.Lens

connectivity = [
    0, 1, 2
    ] :: [GLuint]

drawTriangle :: [GLfloat]  -> IO() 
drawTriangle vertices = do
    let verticesSize = fromIntegral $ sizeOf (0.0 :: GLfloat) * (length vertices)
    vertices1P <- newArray vertices 
    let indicesSize = fromIntegral $ sizeOf (0 :: GLuint) * (length connectivity) 

    indicesP <- newArray connectivity 
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

    free vboP
    free vaoP
    free eboP
    free indicesP 
    free vertices1P 

    return ()

batchDrawTriangles vertices = do 
    let verticesSize = fromIntegral $ sizeOf (0.0 :: GLfloat) * (length vertices)
    let numTriangles = round ( (fromIntegral (length vertices)) / 3)
    let indices = [0.. numTriangles-1]
    let indicesSize = fromIntegral $ sizeOf (0 :: GLuint) * (length indices) 
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

    glDrawElements GL_TRIANGLES numTriangles GL_UNSIGNED_INT nullPtr
    glBindVertexArray 0

    free vboP
    free vaoP
    free eboP
    free indicesP 
    free vertices1P 
    return () 



drawTriangleHoles :: Int -> [GLfloat] -> IO()
drawTriangleHoles level anchorTriangle =  do
    drawTriangle  anchorTriangle 
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
            drawTriangleHoles (level-1) leftBottomTriangle 
            drawTriangleHoles (level-1) (rightBottomTriangle)
            drawTriangleHoles (level-1) (topTriangle)

-- Returns a 9 coordinate array of an up-side down equilateral triangle at the given anchor point  
getVertices :: (GLfloat, GLfloat) -> GLfloat -> [GLfloat]
getVertices (x_1, y_1) l = [ x_1 + b, y_1, 0.0, x_1 + (b/2), y_1 - h, 0.0, x_1, y_1, 0.0 ]
    where
        b = l
        h = l * (sqrt 3) / 2 

getTriangleHoleCoordinates :: Int -> [GLfloat] -> [GLfloat]
getTriangleHoleCoordinates level anchorTriangle = 
    if level == 0 
    then [ ]
    else 
        let w =  (anchorTriangle !! 0) - (anchorTriangle !! 6) -- Width of current triangleHole 
        let h = (anchorTriangle !! 1) - (anchorTriangle !! 4)
        let p_x = anchorTriangle !! 6 
        let p_y =  anchorTriangle !! 7 
        let leftBottomTriangle = getVertices ( p_x - w/4, p_y - h/2 ) (w/2)
        let rightBottomTriangle = getVertices (p_x + 3*w/4, p_y - h/2 ) (w/2) 
        let topTriangle = getVertices (p_x + w/4 , p_y + h/2 ) (w/2) 
        fold (++) [anchorTriangle,  getTriangleHoleCoordinates (level-1) leftBottomTriangle , 
        getTriangleHoleCoordinates (level-1) (rightBottomTriangle),
        getTriangleHoleCoordinates (level-1) (topTriangle)] 

