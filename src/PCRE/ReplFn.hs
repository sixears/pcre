{- | A function name & arguments, together defining a transformation that may be
     applied to some text (typically a backreference, possibly having already
     been transformed by other replacement functions).
 -}

module PCRE.ReplFn
  ( ReplArg(..), ReplFn(..), applyFn, replFns )
where

import Prelude  ( Float, (-) )

-- base --------------------------------

import Control.Applicative  ( pure, many )
import Control.Monad        ( return )
import Data.Eq              ( Eq )
import Data.Function        ( ($) )
import Data.List            ( dropWhile )
import Data.List.NonEmpty   ( NonEmpty( (:|) ) )
import Data.Maybe           ( fromMaybe )
import Data.String          ( IsString )
import Text.Read            ( read )
import Text.Show            ( Show )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode          ( (≡) )
import Data.Function.Unicode    ( (∘) )
import Data.Monoid.Unicode      ( (⊕) )
import Prelude.Unicode          ( ℤ )
import Numeric.Natural.Unicode  ( ℕ )

-- hashable ----------------------------

import Data.Hashable  ( Hashable )

-- lens --------------------------------

import Control.Lens.Review  ( (#) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (∤), (⊵), (⋫), (⋪) )
import Data.MoreUnicode.Char         ( ℂ )
import Data.MoreUnicode.Either       ( pattern 𝕷, pattern 𝕽 )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Maybe        ( pattern 𝕵, pattern 𝕹 )
import Data.MoreUnicode.Monoid       ( ю )
import Data.MoreUnicode.String       ( 𝕊 )
import Data.MoreUnicode.Text         ( 𝕋 )

-- parsec ------------------------------

import Text.Parsec.Prim   ( ParsecT, Stream )

-- parsec-plus -------------------------

import ParsecPlus  ( Parsecable( parser ) )

-- parsers -----------------------------

import Text.Parser.Char         ( alphaNum, char, letter, noneOf, oneOf,spaces )
import Text.Parser.Combinators  ( option, sepBy )

-- parser-plus -------------------------

import ParserPlus  ( digits, parseBackslashedChar, tries )

-- template-haskell --------------------

import Language.Haskell.TH.Syntax  ( Lift )

-- text --------------------------------

import Data.Text  ( last, length, map, pack, replicate, singleton, toTitle,zip )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

-- unordered-containers ----------------

import qualified Data.HashMap.Lazy  as  HashMap
import Data.HashMap.Lazy  ( HashMap, (!?) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import PCRE.Error  ( AsREFnError( _REFnError ), REFnError, throwAsREFnError )

--------------------------------------------------------------------------------

{- | An argument to an RE replacement function -}
data ReplArg = ReplArgT 𝕋 | ReplArgN ℕ | ReplArgZ ℤ | ReplArgF Float
  deriving (Eq,Lift,Show)

----------

instance Parsecable ReplArg where
  parser =
    let c3 ∷ 𝕊 → 𝕊 → 𝕊 → 𝕊
        c3 xs ys zs = ю [ xs, ys, ".", zs ]
        parseT ∷ Stream s m ℂ ⇒ ParsecT s u m 𝕋
        parseT = pack ⊳
          (char '"' ⋫ many (parseBackslashedChar ∤ noneOf "\\\"") ⋪ char '"')
        parseF ∷ Stream s m ℂ ⇒ ParsecT s u m Float
        parseF = read ⊳
          (c3 ⊳ option "" (pure ⊳ oneOf "+-") ⊵ digits ⊵ char '.' ⋫ digits)
        parseZ ∷ Stream s m ℂ ⇒ ParsecT s u m ℤ
        parseZ = (read ∘ dropWhile (≡ '+')) ⊳ ((:) ⊳ oneOf "+-" ⊵ digits)
     in tries ( ((ReplArgF ⊳ parseF)
             :| [ ReplArgZ ⊳ parseZ
                , ReplArgN ∘ read ⊳ digits
                , ReplArgT ⊳ parseT
                ]) )

------------------------------------------------------------

{- | A function name & arguments, together defining a transformation that may be
     applied to some text (typically a backreference, possibly having already
     been transformed by other replacement functions).
 -}
data ReplFn = ReplFn 𝕋 [ReplArg]
  deriving (Eq,Lift,Show)

----------

instance Parsecable ReplFn where
  parser =
    ReplFn ⊳ (pack ⊳ ((:) ⊳ (char '.' ⋫ letter)
                          ⊵ many (alphaNum ∤ char '-')))
           ⊵ option [] (char '(' ⋫ spaces ⋫ sepBy parser (char ',')
                                 ⋪ spaces ⋪ char ')')

------------------------------------------------------------

{-| Replacement function name -}

newtype ReplFnName = ReplFnName 𝕋
  deriving (Eq, Hashable, IsString)

------------------------------------------------------------

{- | Transliterate a `Text`.  Replace some set of characters with another
     set of characters in a `Text`.  Takes precisely two `Text` args, being the
     "from" list and the "to" list.  Each character in the "from" list, when
     seen in the input string, is replaced with the corresponding character in
     the "to" list (by position).  If the "from" list is longer than the "to"
     list, then the last character of the "to" is used.

     If characters in the "from" list are repeated; then which one is 'chosen'
     is undefined.

     It is an error to pass an empty "to" list.
 -}

tr ∷ (AsREFnError ε, MonadError ε η) ⇒ 𝕋 → [ReplArg] → η 𝕋
tr _ [ReplArgT _, ReplArgT ""] =
  throwAsREFnError $ [fmt|when not truncating, tr to string must be non-empty|]
tr x [ReplArgT from, ReplArgT to] =
  let ttable ∷ HashMap ℂ ℂ
      ttable =
        let len = length from - length to
            lst = singleton $ last to
         in HashMap.fromList $ zip from (to ⊕ replicate len lst)
   in return $ map (\ c → fromMaybe c (ttable !? c)) x
tr _ args = throwAsREFnError $ [fmt|Bad args to tr: '%w'|] args

----------------------------------------

{- | Title-case a `Text`; that is, ensure that every word begins with an
     upper-case letter.  Takes no args.  -}

title ∷ (AsREFnError ε, MonadError ε η) ⇒ 𝕋 → [ReplArg] → η 𝕋
title t []   = return $ toTitle t
title _ args = throwAsREFnError $ [fmt|'title' takes no args (got '%w')|] args

------------------------------------------------------------

{-| A standard set of replacement functions -}
replFns ∷ MonadError REFnError η ⇒ HashMap ReplFnName (𝕋 → [ReplArg] → η 𝕋)
replFns = HashMap.fromList [ ("title", title), ("tr", tr)  ]

----------------------------------------

{-| Apply a replacement function to some `Text`. -}
applyFn ∷ ∀ ε η . (AsREFnError ε, MonadError ε η) ⇒ 𝕋 → ReplFn → η 𝕋
applyFn t (ReplFn fnname fnargs) = do
  case replFns !? ReplFnName fnname of
    𝕹   → throwAsREFnError $ [fmt|no such function: '%t'|] fnname
    𝕵 f → case f t fnargs of
            𝕷 e → throwError $ _REFnError # e
            𝕽 r → return r

-- that's all, folks! ----------------------------------------------------------
