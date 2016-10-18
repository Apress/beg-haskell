module Simple where

import Prelude
import FFI

main :: Fay ()
main = alert "Hello from Fay!"

alert :: String -> Fay ()
alert = ffi "alert(%1)"
