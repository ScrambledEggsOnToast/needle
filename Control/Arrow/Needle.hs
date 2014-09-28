{-|
Module      : Control.Arrow.Needle
Description : ASCII-fied arrow notation.
Copyright   : (c) 2014 Josh Kirklin
License     : MIT
Maintainer  : jjvk2@cam.ac.uk

Needle is a domain specific language for ASCII-fied arrow notation. This module enables the use of needle within Haskell by making use of Template Haskell.

In needle, data travels along rails. There are three types of rail, and data travels in different directions on each:

    [@=@] left to right
    [@\\@] down
    [@/@] up

Data enters a rail with @}@, and exits with @>@.

When rails are joined, their contents are concatenated. When rails are split, their contents are duplicated.

An external arrow can be embedded in a rail by enclosing it between a @{@ and a @}@.

Inputs and outputs of rails can be asigned labels with a @:@.

Rails can cross one another, if one of the rails has gone \'underground\' by entering a \'tunnel\'. A tunnel entrance is specified by a @)@, and a tunnel exit is specified by a @(@.

Most questions should be answered by a short example:

> import Control.Arrow.Needle
>
> nTest :: (Int, Int, Int) -> (Int, (Int, Int, Int))
> nTest = [nd|
>                         aLabel:==={div 2}===\
>     }====\                                  \
>          {uncurry (+)}==\=================) \ (==>
>     }====/              \                   \
>                         \                   \=={nTriple}=>
>                         \               
>                      }=={uncurry (-)}====:aLabel
> |]
> 
> nTriple = [nd|
>     }==\==>
>        \==>
>        \==>
> |]

>>> nTest (3,2,1)
(5,(-1,-1,-1))

-}

module Control.Arrow.Needle (
    nd
  , ndFile
    -- * Reexported for your convenience
  , module Control.Arrow
) where


import Control.Arrow

import Control.Arrow.Needle.TH (nd, ndFile)

