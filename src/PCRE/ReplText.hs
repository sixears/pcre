{- | PCRE Replacement Text; akin to the RHS of a perl s/// expression.
     This represents some text, with embedded replacement expressions
     (see `PCRE.ReplExpr`) that may be applied to an RE match to generate some
     new text. -}

module PCRE.ReplText
  ( ReplText(..), ReplTextFrag(..), repltext, tests )
where

-- base --------------------------------

import Control.Applicative  ( many, some )
import Control.Monad        ( return )
import Data.Eq              ( Eq( (==) ) )
import Data.Function        ( ($) )
import Data.List.NonEmpty   ( NonEmpty( (:|) ) )
import System.Exit          ( ExitCode )
import System.IO            ( IO )
import Text.Show            ( Show )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode          ( (≡) )
import Data.Function.Unicode    ( (∘) )
import Numeric.Natural.Unicode  ( ℕ )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toText )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⋪) )
import Data.MoreUnicode.Either       ( 𝔼 )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Monad        ( (≫) )
import Data.MoreUnicode.Monoid       ( ю )
import Data.MoreUnicode.String       ( 𝕊 )
import Data.MoreUnicode.Text         ( 𝕋 )

-- parsec-plus -------------------------

import Parsec.Error  ( ParseError )
import ParsecPlus    ( Parsecable( parser, parsec ), parse )

-- parsers -----------------------------

import Text.Parser.Char         ( char, noneOf )
import Text.Parser.Combinators  ( eof )

-- parser-plus -------------------------

import ParserPlus  ( parseBackslashedChar, tries )

-- quasiquoting ------------------------

import QuasiQuoting  ( QuasiQuoter, liftParsec, mkQQExp )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@=?), testCase )

-- tasty-plus --------------------------

import TastyPlus  ( assertIsLeft, assertRight
                  , runTestsP, runTestsReplay, runTestTree )

-- template-haskell --------------------

import Language.Haskell.TH         ( Name, Q )
import Language.Haskell.TH.Syntax  ( Exp( AppE, ConE ), Lift( liftTyped )
                                   , TExp( TExp ) )

-- text --------------------------------

import Data.Text  ( append, pack, singleton )

-- text-printer ------------------------

import qualified Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import PCRE.GroupID   ( GroupID( GIDName, GIDNum ) )
import PCRE.ReplExpr  ( ReplExpr( ReplExpr ) )
import PCRE.ReplFn    ( ReplArg( ReplArgF, ReplArgN, ReplArgT, ReplArgZ )
                      , ReplFn( ReplFn ) )

--------------------------------------------------------------------------------

{- | Simple lifter that applies a constructor to a value. -}
liftTExp ∷ ∀ α τ . Lift τ ⇒ Name → τ → Q (TExp α)
liftTExp y x = ⟦ x ⟧ ≫ return ∘ TExp ∘ AppE (ConE y)

------------------------------------------------------------

data ReplTextFrag = RTFText 𝕋 | RTFExpr ReplExpr
  deriving (Eq,Show)

instance Printable ReplTextFrag where
  print (RTFText t) = P.text t
  print (RTFExpr e) = P.text $ toText e

instance Lift ReplTextFrag where
  liftTyped (RTFText t) =
    liftTExp 'RTFText t
  liftTyped (RTFExpr e) = liftTExp 'RTFExpr e

instance Parsecable ReplTextFrag where
  parser =
    tries ((RTFExpr ⊳ parser) :| [ RTFText ∘ pack ⊳ some (noneOf "$\\")
                                 , RTFText ∘ singleton ⊳ parseBackslashedChar
                                 , RTFText ∘ singleton ⊳ (char '$' ⋪ char '$')
                                 ])

parseReplTextFragTests ∷ TestTree
parseReplTextFragTests =
  let prse           ∷ 𝕊 → 𝔼 Parsec.Error.ParseError ReplTextFrag
      prse s         = parsec s s
      check          ∷ 𝕊 → ReplTextFrag → TestTree
      check s c      = testCase s $ assertRight (c @=?) (prse s)
      checkFail      ∷ 𝕊 → TestTree
      checkFail s    = testCase s $ assertIsLeft (prse s)
   in testGroup "parseReplTextFrag"
    [ check "${.foo bar}"
            (RTFExpr $ ReplExpr [ReplFn "foo" []] (GIDName "bar"))
    , check "$$"         (RTFText "$")
    , check "\\n"        (RTFText "\n")
    , check "\\t"        (RTFText "\t")
    , check "\\\\"       (RTFText "\\")
    , check "foo bar "   (RTFText "foo bar ")
    , check "!@#&*(%^x"  (RTFText "!@#&*(%^x")
    , checkFail "\\a"
    , checkFail "$"
    , checkFail "${}"
    , checkFail "${"
    , checkFail "${foo bar}"
    , checkFail "${ .x y .z}"
    , checkFail "${1foo}"
    ]

----------------------------------------

newtype ReplText = ReplText [ReplTextFrag]
  deriving Show

instance Eq ReplText where
  (ReplText rtfs) == (ReplText rtfs') = mergeReplText rtfs ≡ mergeReplText rtfs'

instance Printable ReplText where
  print (ReplText frags) = P.text $ ю (toText ⊳ frags)

instance Lift ReplText where
  liftTyped (ReplText frags) = do
    fs ← ⟦ frags ⟧
    return ∘ TExp $ AppE (ConE 'ReplText) fs

repltext ∷ QuasiQuoter
repltext = mkQQExp "ReplText" (liftParsec (\ s → parsec @ReplText s s))

mergeReplText ∷ [ReplTextFrag] → [ReplTextFrag]
mergeReplText []  = []
mergeReplText [x] = [x]
-- mergeReplText xs@(_ :| []) = xs
mergeReplText (RTFExpr e : (x:xs)) =
  RTFExpr e : mergeReplText (x : xs)
mergeReplText (RTFText t : (RTFText t' : xs)) =
  mergeReplText (RTFText (t `append` t') : xs)
mergeReplText xs@(RTFText _ : (RTFExpr _ : [])) = xs
mergeReplText (RTFText t : (RTFExpr e : (x:xs))) =
  RTFText t : (RTFExpr e : mergeReplText (x : xs))

instance Parsecable ReplText where
  parser = ReplText ∘ mergeReplText ⊳ many parser

parseReplTextTests ∷ TestTree
parseReplTextTests =
  let prse           ∷ 𝕊 → 𝔼 Parsec.Error.ParseError ReplText
      prse s         = parse (parser ⋪ eof) s s
      check          ∷ 𝕊 → ReplText → TestTree
      check s c      = testCase s $ assertRight (c @=?) (prse s)
      checkFail    ∷ 𝕊 → TestTree
      checkFail s = testCase s $ assertIsLeft (prse s)
   in testGroup "parseReplText"
    [ check "${.foo bar}$$\\nfoo bar\\t!@#%^&*() xy\\\\${ .x .y 17 }"
            (ReplText $ (RTFExpr $ ReplExpr ([ReplFn "foo" []]) (GIDName "bar"))
                     : [ RTFText "$\nfoo bar\t!@#%^&*() xy\\"
                       , RTFExpr $
                           ReplExpr [ReplFn x [] | x ← [ "x", "y" ]] (GIDNum 17)
                       ])
    , check "${.foo(\"aa\",2,-3,+4,5.0) bar}"
            (ReplText $ (RTFExpr $ ReplExpr ([ReplFn "foo"
                                                     [ ReplArgT "aa"
                                                     , ReplArgN 2
                                                     , ReplArgZ (-3)
                                                     , ReplArgZ 4
                                                     , ReplArgF 5.0
                                                     ]])
                                   (GIDName "bar"))
                      : [])
    , checkFail "\\a"
    , checkFail "$"
    , checkFail "${}"
    , checkFail "${"
    , checkFail "${foo bar}"
    , checkFail "${ .x y .z}"
    , checkFail "${1foo}"
    ]

------------------------------------ tests -------------------------------------

tests ∷ TestTree
tests = testGroup "ReplText" [ parseReplTextFragTests, parseReplTextTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
