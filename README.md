needle
===

ASCII-fied arrow notation.

Don't use this yet! It is very early days.

## Example

    doNumerousThings :: (Int,Int,Int) -> (Int, Int)
    doNumerousThings = [needle|
                        a :==={negate}==\
        Int }==\                         \
               {(+)}==\======{flip div}==)\(==>
        Int }==/      \      /             \
                      \      /              \==>
               Int }=={(-)}==/===: a
      |]

    doNumerousThings (3,2,1) = (0,-4)
