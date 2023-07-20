module Interactivity (moveUp, moveDown, moveRight, moveLeft) where

import Foreign -- includes many sub-modules
import Linear
import Control.Lens
import Foreign.C.String (newCAStringLen, newCString)

moveUp step pointer = do
    a <- peek pointer 
    let x = a ^._x
    let y = a ^._y
    let z = a ^._z
    let w = a ^._w
    let newX = V4 (x ^._x) (x ^._y) (x ^._z)  (x ^._w + step)
    let newY = V4 (y ^._x) (y ^._y) (y ^._z)  (y ^._w )
    let newZ = V4 (z ^._x) (z ^._y) (z ^._z)  (z ^._w )
    let newA = V4 newX newY newZ w
    poke pointer (newA)
    return ()

moveDown step pointer = do
    a <- peek pointer 
    let x = a ^._x
    let y = a ^._y
    let z = a ^._z
    let w = a ^._w
    let newX = V4 (x ^._x) (x ^._y) (x ^._z)  (x ^._w)
    let newY = V4 (y ^._x) (y ^._y) (y ^._z)  (y ^._w - step)
    let newZ = V4 (z ^._x) (z ^._y) (z ^._z)  (z ^._w )
    let newA = V4 newX newY newZ w
    poke pointer (newA)
    return ()

moveLeft step pointer = do
    a <- peek pointer 
    let x = a ^._x
    let y = a ^._y
    let z = a ^._z
    let w = a ^._w
    let newX = V4 (x ^._x) (x ^._y) (x ^._z)  (x ^._w - step)
    let newY = V4 (y ^._x) (y ^._y) (y ^._z)  (y ^._w )
    let newZ = V4 (z ^._x) (z ^._y) (z ^._z)  (z ^._w )
    let newA = V4 newX newY newZ w
    poke pointer (newA)
    return ()

moveRight step pointer = do
    a <- peek pointer 
    let x = a ^._x
    let y = a ^._y
    let z = a ^._z
    let w = a ^._w
    let newX = V4 (x ^._x) (x ^._y) (x ^._z)  (x ^._w )
    let newY = V4 (y ^._x) (y ^._y) (y ^._z)  (y ^._w  + step )
    let newZ = V4 (z ^._x) (z ^._y) (z ^._z)  (z ^._w )
    let newA = V4 newX newY newZ w
    poke pointer (newA)
    return ()
    

