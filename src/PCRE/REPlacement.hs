{-# LANGUAGE UnicodeSyntax #-}
{-| combined RE and replacement text -}

module PCRE.REPlacement
  ( REPlacement(..)
  ) where

import Base1T

-- hashable ----------------------------

import Data.Hashable ( Hashable(hashWithSalt) )

-- parsec-plus -------------------------

import Parsec.Error ( ParseError )
import ParsecPlus   ( Parsecable(parsec, parser), eitherParsec )

-- parser-plus -------------------------

import ParserPlus ( stringMaybeDQuoted )

-- parsers -----------------------------

import Text.Parser.Char ( char )

-- text-printer ------------------------

import Text.Printer qualified as P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import PCRE.Base     ( PCRE, reSource )
import PCRE.ReplText ( ReplText )

--------------------------------------------------------------------------------

{- | An RE with its replacement pattern. -}
data REPlacement = REPlacement PCRE ReplText

--------------------

instance Eq REPlacement where
  (REPlacement r rtext) == (REPlacement r' rtext') =
    (reSource r ≡ reSource r') ∧ (rtext ≡ rtext')

--------------------

instance Show REPlacement where
  show (REPlacement r rtext) =
    [fmt|REPlacement: »%s« → »%t«|] (reSource r) (toText rtext)

--------------------

instance Hashable REPlacement where
  hashWithSalt i r = hashWithSalt i (show r)

--------------------

instance Parsecable REPlacement where
  parser = REPlacement ⊳ (parser) ⋪ many (char '\t')
                       ⊵ (eitherParsec stringMaybeDQuoted
                                       (\ s → parsec @_ @ParseError s s))

--------------------

instance Printable REPlacement where
  print (REPlacement r rtext) = P.text $ [fmt|%s → %T|] (reSource r) rtext

-- that's all, folks! ----------------------------------------------------------
