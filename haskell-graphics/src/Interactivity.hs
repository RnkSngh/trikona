module Interactivity (moveUp, moveDown, moveRight, moveLeft) where

import Foreign -- includes many sub-modules
import Linear
import Control.Lens
import Foreign.C.String (newCAStringLen, newCString)

moveUp step pointer = do
    a<- peek pointer 
    poke pointer (over (_w._y) (\y -> y + step) a)

moveDown step pointer = do
    a <- peek pointer 
    poke pointer (over  (_w._y) (\y -> y - step) a)


moveLeft step pointer = do
    a <- peek pointer 
    poke pointer (over (_w._x) (\x -> x + step) a)

moveRight step pointer = do
    a <- peek pointer 
    poke pointer (over (_w._x) (\x -> x - step) a)
    



    

