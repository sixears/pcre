{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE RankNTypes    #-}

{- | match against a PCRE; show the results, incl. captures -}

import Base1

-- logging-effect ----------------------

import Control.Monad.Log  ( LoggingT )

-- log-plus ----------------------------

import Log  ( Log )

-- mockio-log --------------------------

import MockIO.MockIOClass  ( MockIOClass )

-- monadio-plus ------------------------

import MonadIO       ( say )
import MonadIO.Base  ( getArgs )

-- optparse-applicative ----------------

import Options.Applicative.Builder   ( flag, help, argument, long, metavar
                                     , option, short, strArgument, value )
import Options.Applicative.NonEmpty  ( some1 )
import Options.Applicative.Types     ( Parser )

-- optparse-plus -----------------------

import OptParsePlus  ( parsecReader, readM )

-- pcre --------------------------------

import PCRE        ( (≃), (?=~), replace, PCRE )
import PCRE.Error  ( AsREFnError, AsREGroupError )

-- stdmain -----------------------------

import StdMain  ( stdMainNoDR )

-- text --------------------------------

import Data.Text qualified as T

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import PCRE.Error        ( PCREError )
import PCRE.Match        ( fmtMatch )
import PCRE.REPlacement  ( REPlacement( REPlacement ) )
import PCRE.ReplText     ( ReplText )

--------------------------------------------------------------------------------

data ShowREMatch = NoShowREMatch | ShowREMatch
  deriving (Eq,Show)

------------------------------------------------------------

data Options = Options { _pcre        ∷ PCRE
                       , _args        ∷ NonEmpty 𝕋
                       , _showREMatch ∷ ShowREMatch
                       , _replacement ∷ 𝕄 ReplText
                       }

--------------------

pcre ∷ Lens' Options PCRE
pcre = lens _pcre (\ o p → o { _pcre = p })

--------------------

args ∷ Lens' Options (NonEmpty 𝕋)
args = lens _args (\ o as → o { _args = as })

--------------------

showREMatch ∷ Lens' Options ShowREMatch
showREMatch = lens _showREMatch (\ o s → o { _showREMatch = s })

--------------------

replacement ∷ Lens' Options (𝕄 ReplText)
replacement = lens _replacement (\ o r → o { _replacement = r })

------------------------------------------------------------

parseOptions ∷ Parser Options
parseOptions =
  Options ⊳ argument readM (metavar "PCRE")
          ⊵ some1 (strArgument (metavar "MATCH-TARGET"))
          ⊵ flag NoShowREMatch ShowREMatch (ю [short 'r', long "show-re-match"
                                              , help "show REMatch datum"])
          ⊵ option (𝕵 ⊳ parsecReader) (ю [ short   'p'
                                         , long    "replace"
                                         , long    "replacement"
                                         , metavar "REPLACEMENT"
                                         , help    "show effected replacement"
                                         , value   𝕹
                                         ])

----------------------------------------

indent ∷ ℕ → 𝕋 → 𝕋
indent n t = T.unlines $ (T.replicate (fromIntegral n) " " ⊕) ⊳ T.lines t

----------------------------------------

sayT ∷ MonadIO μ ⇒ 𝕋 → μ ()
sayT = say

----------------------------------------

chomp ∷ 𝕋 → 𝕋
chomp = T.dropWhileEnd (≡ '\n')

----------------------------------------

printMatch ∷ (MonadIO μ, AsREFnError ε, AsREGroupError ε, MonadError ε μ) ⇒
             Options → 𝕋 → μ ()
printMatch opts t = do
  sayT "Match"
  say (chomp ∘ indent 2 ∘ fmtMatch $ t ?=~ (opts ⊣ pcre))
  when (opts ⊣ showREMatch ≡ ShowREMatch) $ do
    sayT "--------"
    sayT "REMatch"
    case (opts ⊣ pcre) ≃ t of
      𝕹   → sayT "no match"
      𝕵 m → say ∘ chomp ∘ indent 2 $ toText m
  case opts ⊣ replacement of
    𝕹   → return ()
    𝕵 r → do sayT "--------"
             replace (REPlacement (opts ⊣ pcre) r) t ≫ \ case
               𝕹 → sayT "no replace"
               𝕵 p → sayT $ [fmt|replacement »%T«: %w|] r p
  sayT "================\n"

----------------------------------------

myMain ∷ ∀ ε .
         (HasCallStack, Printable ε, AsREFnError ε, AsREGroupError ε) ⇒
         Options → LoggingT (Log MockIOClass) (ExceptT ε IO) Word8
myMain opts = forM_ (opts ⊣ args) (printMatch opts) ⪼ return 0

------------------------------------------------------------

{- | Run the program (using `getArgs` to provide arguments) -}
main ∷ IO ()
main = do
  let progDesc ∷ 𝕋 = "match arguments against a PCRE"
  getArgs ≫ (\ as → stdMainNoDR progDesc parseOptions (myMain @PCREError) as)

--------------------------------------------------------------------------------

{-
{- | unit tests -}
tests ∷ TestTree
tests = testGroup "Rename" [ Rename.RegexFile.tests ]


--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests
-}

-- that's all, folks! ----------------------------------------------------------
