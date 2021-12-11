{- | Base types & functions for PCRE -}

module PCRE.Base
  ( REParsecable(..), compRE )
where

-- base --------------------------------

import Data.Bifunctor  ( first )

-- base-unicode-symbols ----------------

import Data.Function.Unicode    ( (∘) )

-- lens --------------------------------

import Control.Lens.Review  ( (#) )

-- monaderror-io -----------------------

import MonadError  ( fromRight )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- more-unicode ------------------------

import Data.MoreUnicode.Either   ( 𝔼 )
import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Text     ( 𝕋 )

-- parsec-plus -------------------------

import ParsecPlus  ( Parsecable( parser ), eitherParsec )

-- parser-plus -------------------------

import ParserPlus  ( stringMaybeDQuoted )

-- regex-with-pcre ---------------------

import Text.RE.PCRE.Text  ( RE, compileRegex )

-- text --------------------------------

import Data.Text  ( pack, unpack )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import PCRE.Error  ( AsREParseError( _REParseError ), REParseError )

--------------------------------------------------------------------------------

{- | A newtype wrapper for RE to make it `Parsecable`. -}
newtype REParsecable = REParsecable { unREParsecable ∷ RE }

instance Parsecable REParsecable where
  parser =
    REParsecable ⊳ eitherParsec stringMaybeDQuoted (compRE @REParseError ∘ pack)

------------------------------------------------------------

compRE ∷ ∀ ε η . (AsREParseError ε, MonadError ε η) ⇒ 𝕋 → η RE
compRE =
  fromRight ∘ first (_REParseError #) ∘ compileRegex @(𝔼 REParseError) ∘ unpack

-- that's all, folks! ----------------------------------------------------------
