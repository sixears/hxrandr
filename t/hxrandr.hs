import Prelude ()

-- base --------------------------------

import System.IO  ( IO )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, defaultMain, testGroup )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import XRandR  ( tests )

main :: IO ()
main = defaultMain tests
