{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax         #-}

{- | Base types & functions for PCRE -}

module PCRE.Base
  ( PCRE(..)
  , compRE
  , pcre
  , reSource
  , (?=~)
  ) where

import Base1T
import Prelude ( error )

-- monaderror-io -----------------------

import MonadError ( fromRight )

-- optparse-applicative ----------------

import Options.Applicative.Builder ( eitherReader )

-- optparse-plus -----------------------

import OptParsePlus ( OptReader(readM) )

-- quasiquoting ------------------------

import QuasiQuoting ( QuasiQuoter, mkQQExp )

-- parsec-plus -------------------------

import ParsecPlus ( Parsecable(parser), eitherParsec )

-- parser-plus -------------------------

import ParserPlus ( stringMaybeDQuoted )

-- regex -------------------------------

import Text.RE.Tools ( IsRegex(makeRegex, makeRegexWith, makeSearchReplaceWith, matchMany, matchOnce, regexSource) )

-- regex-with-pcre ---------------------

import Text.RE.PCRE.Text ( Match, RE,
                           SearchReplace(SearchReplace, getSearch, getTemplate) )
import Text.RE.PCRE.Text qualified

-- template-haskell --------------------

import Language.Haskell.TH.Syntax ( Exp(AppE, LitE, VarE), Lit(StringL) )

-- text --------------------------------

import Data.Text qualified as T

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import PCRE.Error ( AsREParseError(_REParseError), REParseError )

--------------------------------------------------------------------------------

{- | A newtype wrapper for RE to make it `Parsecable`, `OptReader`. -}
newtype PCRE = PCRE { unPCRE :: RE }

instance Show PCRE where
  show = reSource

instance Parsecable PCRE where
  parser =
    eitherParsec stringMaybeDQuoted (compRE @REParseError ∘ T.pack)

instance IsRegex PCRE 𝕋 where
  matchOnce (PCRE re) = matchOnce re
  matchMany (PCRE re) = matchMany re
  makeRegexWith opts t = PCRE ⊳ makeRegexWith opts t
  makeSearchReplaceWith opts t t' = do
    search_replace ← makeSearchReplaceWith opts t t'
    let search   = getSearch   search_replace
        template = getTemplate search_replace
    return $ SearchReplace (PCRE search) template

  regexSource (PCRE re) = regexSource re

------------------------------------------------------------

{- | compile a regular expression, in a MonadError context -}
compRE ∷ ∀ ε η . (AsREParseError ε, MonadError ε η) ⇒ 𝕋 → η PCRE
compRE = fromRight ∘ first (_REParseError #) ∘ makeRegex

__compRE__ ∷ 𝕊 → PCRE
__compRE__ s = let __fromRight__ (𝕽 r) = r
                   __fromRight__ (𝕷 e) = error$ [fmt|error parsing '%s': %T|]s e
               in  __fromRight__ ∘ compRE @REParseError $ T.pack s

------------------------------------------------------------

(?=~) ∷ 𝕋 → PCRE → Match 𝕋
t ?=~ re = (Text.RE.PCRE.Text.?=~) t (unPCRE re)

instance OptReader PCRE where
  readM = let eParse ∷ 𝕊 → 𝔼 𝕊 PCRE
              eParse = first toString ∘ compRE @REParseError ∘ T.pack
          in  eitherReader eParse

reSource ∷ PCRE → 𝕊
reSource = Text.RE.PCRE.Text.reSource ∘ unPCRE

pcre ∷ QuasiQuoter
pcre =
  mkQQExp "PCRE" (\ s → 𝕵 (return (AppE (VarE '__compRE__) (LitE (StringL s)))))

-- that's all, folks! ----------------------------------------------------------
