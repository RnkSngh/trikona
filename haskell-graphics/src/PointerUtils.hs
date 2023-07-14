module PointerUtils (checkError) where


import Control.Monad (when)
import Graphics.GL.Core33
import Foreign -- includes many sub-modules
import Foreign.C.String (newCAStringLen)

checkError program code glGetiv getInfoLog errorString= do
    successP <- malloc
    glGetiv program code successP
    success <- peek successP
    when (success == GL_FALSE) $ do
        putStrLn errorString
        let infoLength = 512
        resultP <- malloc
        infoLog <- mallocArray (fromIntegral infoLength)
        getInfoLog program (fromIntegral infoLength) resultP infoLog
        result <- fromIntegral <$> peek resultP
        logBytes <- peekArray result infoLog
        putStrLn (map (toEnum.fromEnum) logBytes)
    return success 

