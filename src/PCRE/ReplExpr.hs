{- | A group (backreference) identifier, along with a (possibly empty) list of
     replacement functions, which may be applied to an RE match to produce some
     `Text` (typically to insert at the location where the backreference was
     made).
 -}

module PCRE.ReplExpr
  ( ReplExpr(..), tests )
where

-- base --------------------------------

import Control.Applicative  ( many )
import Control.Monad        ( return )
import Data.Eq              ( Eq )
import Data.Function        ( ($) )
import System.Exit          ( ExitCode )
import System.IO            ( IO )
import Text.Show            ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode    ( (∘) )
import Numeric.Natural.Unicode  ( ℕ )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵), (⋫), (⋪) )
import Data.MoreUnicode.Either       ( 𝔼 )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Monoid       ( ю )
import Data.MoreUnicode.String       ( 𝕊 )

-- parsec-plus -------------------------

import Parsec.Error  ( ParseError )
import ParsecPlus    ( Parsecable( parser, parsec ) )

-- parsers -----------------------------

import Text.Parser.Char  ( char, spaces, string )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@=?), testCase )

-- tasty-plus --------------------------

import TastyPlus  ( assertIsLeft, assertRight
                  , runTestsP, runTestsReplay, runTestTree )

-- template-haskell --------------------

import Language.Haskell.TH.Syntax  ( Exp( AppE, ConE ), Lift( liftTyped )
                                   , TExp( TExp ) )

-- text-printer ------------------------

import qualified Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import PCRE.GroupID  ( GroupID( GIDName, GIDNum ) )
import PCRE.ReplFn   ( ReplArg( ReplArgF, ReplArgN, ReplArgT, ReplArgZ )
                     , ReplFn( ReplFn ) )

--------------------------------------------------------------------------------

{- | A sequence of replacement functions, along with a group (backreference)
     identifier, which may be applied to an RE match to produce some `Text`
     (typically to insert at the location where the backreference was made).
 -}

data ReplExpr = ReplExpr [ReplFn] GroupID
  deriving (Eq,Show)

--------------------

instance Printable ReplExpr where
  print (ReplExpr fns gid) = P.string $ show (fns,gid)

--------------------

instance Lift ReplExpr where
  liftTyped (ReplExpr fns gid) = do
    fns' ← ⟦ fns ⟧
    gid' ← ⟦ gid ⟧
    return ∘ TExp $ AppE (AppE (ConE 'ReplExpr) fns') gid'

--------------------

instance Parsecable ReplExpr where
  parser = ReplExpr ⊳ (string "${" ⋫ spaces ⋫ (many $ parser ⋪ spaces))
                    ⊵ (parser ⋪ spaces ⋪ char '}')

----------

parseReplExprTests ∷ TestTree
parseReplExprTests =
  let prse           ∷ 𝕊 → 𝔼 Parsec.Error.ParseError ReplExpr
      prse s         = parsec s s
      check          ∷ 𝕊 → ReplExpr → TestTree
      check s c      = testCase s $ assertRight (c @=?) (prse s)
      checkFail    ∷ 𝕊 → TestTree
      checkFail s = testCase s $ assertIsLeft (prse s)
   in testGroup "parseReplExpr"
    [ check "${.foo bar}"
            (ReplExpr ([ReplFn "foo" []]) (GIDName "bar"))
    , check "${ .foo  bar  }"
            (ReplExpr ([ReplFn "foo" []]) (GIDName "bar"))
    , check "${ .foo 17}"
            (ReplExpr ([ReplFn "foo" []]) (GIDNum 17))
    , check "${ .foo(13) 17}"
            (ReplExpr ([ReplFn "foo" [ ReplArgN 13 ]])
                      (GIDNum 17))
    , check "${ .foo(12,3.45,-6.7,8.0) 17}"
            (ReplExpr ([ReplFn "foo" [ ReplArgN 12, ReplArgF 3.45
                                     , ReplArgF (-6.7), ReplArgF 8 ]])
                      (GIDNum 17))
    , check "${ .foo(\"foo\",\"bar\\n\\\\baz\") 17}"
            (ReplExpr ([ReplFn "foo" [ ReplArgT "foo", ReplArgT "bar\n\\baz" ]])
                      (GIDNum 17))
    , check "${ .foo(\"\") 17}"
            (ReplExpr ([ReplFn "foo" [ ReplArgT "" ]])
                      (GIDNum 17))
    , check "${ .foo(+2) 17}"
            (ReplExpr ([ReplFn "foo" [ ReplArgZ 2 ]])
                      (GIDNum 17))
    , check "${ .foo(-3) 17}"
            (ReplExpr ([ReplFn "foo" [ ReplArgZ (-3) ]])
                      (GIDNum 17))
    , check "${ .foo(+2,-3) 17}"
            (ReplExpr ([ReplFn "foo" [ ReplArgZ 2, ReplArgZ (-3) ]])
                      (GIDNum 17))
    , check (ю [ "${ .foo(\"", ['\\', '\"'], "\") 17}" ])
            (ReplExpr ([ReplFn "foo" [ ReplArgT "\"" ]])
                      (GIDNum 17))
    , check "${ .x .y z}"
            (ReplExpr ([ReplFn x [] | x ← ["x","y"]])
                      (GIDName "z"))
    , checkFail "${}"
    , checkFail "${"
    , checkFail "${foo bar}"
    , checkFail "${ .x y .z}"
    , checkFail "${1foo}"
    ]

------------------------------------ tests -------------------------------------

tests ∷ TestTree
tests = testGroup "ReplExpr" [ parseReplExprTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
