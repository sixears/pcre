{-# LANGUAGE UnicodeSyntax #-}

{- | OptParse utilities for PCRE -}

module PCRE.OptParse
  ( parseRE
  ) where

import Base1T

-- optparse-applicative ----------------

import Options.Applicative.Builder ( argument, eitherReader, metavar )
import Options.Applicative.Types   ( Parser )

-- regex-with-pcre ---------------------

import Text.RE.PCRE.Text ( RE )

-- text --------------------------------

import Data.Text qualified as T

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import PCRE.Base  ( compRE )
import PCRE.Error ( REParseError )

--------------------------------------------------------------------------------

{- | parse a PCRE at the cmdline -}
parseRE ∷ Parser RE
parseRE = let eParse ∷ 𝕊 → 𝔼 𝕊 RE
              eParse = first toString ∘ compRE @REParseError ∘ T.pack
          in  argument (eitherReader eParse) (metavar "PCRE")

-- that's all, folks! ----------------------------------------------------------
