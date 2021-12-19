{- | Base types & functions for PCRE -}

module PCRE.Base
  ( REParsecable(..), compRE )
where

import Base1

-- monaderror-io -----------------------

import MonadError  ( fromRight )

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

{- | compile a regular expression, in a MonadError context -}
compRE ∷ ∀ ε η . (AsREParseError ε, MonadError ε η) ⇒ 𝕋 → η RE
compRE =
  fromRight ∘ first (_REParseError #) ∘ compileRegex @(𝔼 REParseError) ∘ unpack

-- that's all, folks! ----------------------------------------------------------
