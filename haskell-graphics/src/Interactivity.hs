module Interactivity (move, rotateY, rotateX) where

import Foreign -- includes many sub-modules
import Linear
import Control.Lens

move row step pointer = do
    a <- peek pointer 
    poke pointer (over (_w.row) (\x -> x - step) a)
    
rotateY step pointer = do
    let rotQ = axisAngle (V3 0 0 1) (step)
    let rotMat = fromQuaternion rotQ
    let transformationMat = transpose (mkTransformationMat rotMat (V3 0 0 0)  )
    a <- peek pointer
    let newA = transformationMat !*! a 
    poke pointer newA

rotateX step pointer = do
   let rotQ = axisAngle (V3 1 1 0) (step)
   let rotMat = fromQuaternion rotQ
   let transformationMat = transpose (mkTransformationMat rotMat (V3 0 0 0)  )
   a <- peek pointer
   let newA = transformationMat !*! a 
   poke pointer newA

 


    

