{-| all unit tests for pcre module -}
module PCRE.T.Tests
  ( tests )
where

import Base1T

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified PCRE
import qualified PCRE.GroupID
import qualified PCRE.REMatch
import qualified PCRE.ReplExpr
import qualified PCRE.ReplText

------------------------------------ tests -------------------------------------

{-| unit tests -}
tests ∷ TestTree
tests = testGroup "PCRE" [ PCRE.tests
                         , PCRE.GroupID.tests
                         , PCRE.REMatch.tests
                         , PCRE.ReplExpr.tests
                         , PCRE.ReplText.tests
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
