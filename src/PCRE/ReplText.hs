{- | PCRE Replacement Text; akin to the RHS of a perl s/// expression.
     This represents some text, with embedded replacement expressions
     (see `PCRE.ReplExpr`) that may be applied to an RE match to generate some
     new text. -}

module PCRE.ReplText
  ( ReplText(..), ReplTextFrag(..), repltext, tests )
where

import Base1T

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

-- template-haskell --------------------

import Language.Haskell.TH         ( Name )
import Language.Haskell.TH.Syntax  ( Code, Exp( AppE, ConE ), Lift( liftTyped )
                                   , Quote, TExp( TExp ), liftCode )

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
-- liftTExp ∷ ∀ α τ . Lift τ ⇒ Name → τ → Q (TExp α)
-- liftTExp y x = ⟦ x ⟧ ≫ return ∘ TExp ∘ AppE (ConE y)
liftCExp ∷ ∀ α τ χ . (Lift τ, Quote χ) ⇒ Name → τ → Code χ α
liftCExp y x = liftCode $ ⟦ x ⟧ ≫ return ∘ TExp ∘ AppE (ConE y)

------------------------------------------------------------

{-| a single piece of a regular expression replacement template; either some
    static text, or a group-based expression -}
data ReplTextFrag = RTFText 𝕋 | RTFExpr ReplExpr
  deriving (Eq,Show)

instance Printable ReplTextFrag where
  print (RTFText t) = P.text t
  print (RTFExpr e) = P.text $ toText e

instance Lift ReplTextFrag where
  liftTyped (RTFText t) = liftCExp 'RTFText t
  liftTyped (RTFExpr e) = liftCExp 'RTFExpr e

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

{- | regular expression replacement text template -}
newtype ReplText = ReplText [ReplTextFrag]
  deriving Show

instance Eq ReplText where
  (ReplText rtfs) == (ReplText rtfs') = mergeReplText rtfs ≡ mergeReplText rtfs'

instance Printable ReplText where
  print (ReplText frags) = P.text $ ю (toText ⊳ frags)

instance Lift ReplText where
  liftTyped (ReplText frags) = liftCode $ do
    fs ← ⟦ frags ⟧
    return ∘ TExp $ AppE (ConE 'ReplText) fs

{- | quasi-quoting constructor for `ReplText` -}
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

{-| unit tests -}
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
