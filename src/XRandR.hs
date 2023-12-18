{-# LANGUAGE UnicodeSyntax #-}

module XRandR
  ( tests
  ) where

import Base1T

import Prelude ( undefined )

-- fpath -------------------------------

import FPath.Error.FPathError ( AsFPathError )

-- log-plus ----------------------------

import Log ( Log )

-- logging-effect ----------------------

import Control.Monad.Log ( LoggingT, MonadLog, Severity(Notice) )

-- mockio ------------------------------

import MockIO        ( noMock )
import MockIO.DoMock ( DoMock, HasDoMock )

-- mockio-log --------------------------

import MockIO.Log             ( MockIOClass )
import MockIO.Log.MonadReader ( debug, info )

-- monadio-plus ------------------------

import MonadIO                       ( say )
import MonadIO.Base                  ( getArgs )
import MonadIO.Error.CreateProcError ( AsCreateProcError )
import MonadIO.Error.ProcExitError   ( AsProcExitError )

-- optparse-applicative ----------------

import Options.Applicative             ( Parser )
import Options.Applicative.Help.Pretty ( empty, vcat )

-- stdmain -----------------------------

import StdMain            ( stdMain )
import StdMain.UsageError ( AsUsageError, UsageFPProcIOTPError )

-- textual-plus ------------------------

import TextualPlus                         ( parseText, tparse )
import TextualPlus.Error.TextualParseError ( AsTextualParseError )

-- trifecta ----------------------------

import Text.Trifecta.Result ( Result(Failure, Success) )

-- trifecta-plus -----------------------

import TrifectaPlus ( eiText, tParse', testParse )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import XRandR.Paths      qualified as Paths
import XRandR.T.TestData qualified as TestData

import XRandR.Types ( XRandR(XRandR) )

--------------------------------------------------------------------------------

newtype Options = Options ()

parseOptions ∷ Parser Options
parseOptions = pure (Options ())

----------------------------------------

myMain ∷ (HasCallStack,
          AsIOError ε, AsFPathError ε, AsUsageError ε, AsCreateProcError ε,
          AsProcExitError ε, AsTextualParseError ε, Printable ε) ⇒
         DoMock → Options → LoggingT (Log MockIOClass) (ExceptT ε IO) Word8
myMain do_mock opts =
  case tParse' @XRandR TestData.red0T of
    Failure e → say (eiText e) ⪼ return 10
    Success x → say x ⪼ return 0

----------------------------------------

main ∷ MonadIO μ ⇒ μ ()
main = do
  let desc = vcat $ [ "manage X screens", empty ]
  getArgs ≫ stdMain desc parseOptions (myMain @UsageFPProcIOTPError)

--------------------

tests ∷ TestTree
tests = testGroup "XRandR" [ testParse TestData.red0T TestData.red0
                           , testParse TestData.dog0T TestData.dog0
                           ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
