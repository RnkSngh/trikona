module Interactivity (move, rotateY, rotateX) where

import Foreign -- includes many sub-modules
import Linear
import Control.Lens

move address step pointer = do
    a <- peek pointer 
    poke pointer (over (_w.address) (\x -> x - step) a)
    
rotateY step pointer = do
    let rotQ = axisAngle (V3 0 1 0) (step)
    let rotMat = fromQuaternion rotQ
    let transformationMat = transpose (mkTransformationMat rotMat (V3 0 0 0)  )
    a <- peek pointer
    let newA = a !*! transformationMat
    poke pointer newA

rotateX step pointer = do
   let rotQ = axisAngle (V3 1 0 0) (step)
   let rotMat = fromQuaternion rotQ
   let transformationMat = transpose (mkTransformationMat rotMat (V3 0 0 0)  )
   a <- peek pointer
   let newA = a !*! transformationMat
   poke pointer newA

 


    

