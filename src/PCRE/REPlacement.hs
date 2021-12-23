{-| combined RE and replacement text -}

module PCRE.REPlacement
  ( REPlacement(..) )
where

import Base1

-- parsec-plus -------------------------

import Parsec.Error  ( ParseError )
import ParsecPlus    ( Parsecable( parser, parsec ), eitherParsec )

-- parser-plus -------------------------

import ParserPlus  ( stringMaybeDQuoted )

-- parsers -----------------------------

import Text.Parser.Char  ( char )

-- regex-with-pcre ---------------------

import Text.RE.PCRE.Text  ( RE, reSource )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import PCRE.Base      ( unREParsecable )
import PCRE.ReplText  ( ReplText )

--------------------------------------------------------------------------------

{- | An RE with its replacement pattern. -}
data REPlacement = REPlacement RE ReplText

instance Eq REPlacement where
  (REPlacement r rtext) == (REPlacement r' rtext') =
    (reSource r ≡ reSource r') ∧ (rtext ≡ rtext')

instance Show REPlacement where
  show (REPlacement r rtext) =
    [fmt|REPlacement: »%s« → »%t«|] (reSource r) (toText rtext)

instance Parsecable REPlacement where
  parser = REPlacement ⊳ (unREParsecable ⊳ parser) ⋪ many (char '\t')
                       ⊵ (eitherParsec stringMaybeDQuoted
                                       (\ s → parsec @_ @ParseError s s))

-- that's all, folks! ----------------------------------------------------------
