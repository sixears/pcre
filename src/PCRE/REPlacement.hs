{-| combined RE and replacement text -}

module PCRE.REPlacement
  ( REPlacement(..) )
where

import Base1

-- hashable ----------------------------

import Data.Hashable  ( Hashable( hashWithSalt ) )

-- parsec-plus -------------------------

import Parsec.Error  ( ParseError )
import ParsecPlus    ( Parsecable( parser, parsec ), eitherParsec )

-- parser-plus -------------------------

import ParserPlus  ( stringMaybeDQuoted )

-- parsers -----------------------------

import Text.Parser.Char  ( char )

-- regex-with-pcre ---------------------

import Text.RE.PCRE.Text  ( RE, reSource )

-- text-printer ------------------------

import qualified Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import PCRE.Base      ( unREParsecable )
import PCRE.ReplText  ( ReplText )

--------------------------------------------------------------------------------

{- | An RE with its replacement pattern. -}
data REPlacement = REPlacement RE ReplText

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
  parser = REPlacement ⊳ (unREParsecable ⊳ parser) ⋪ many (char '\t')
                       ⊵ (eitherParsec stringMaybeDQuoted
                                       (\ s → parsec @_ @ParseError s s))

--------------------

instance Printable REPlacement where
  print (REPlacement r rtext) = P.text $ [fmt|%s → %T|] (reSource r) rtext

-- that's all, folks! ----------------------------------------------------------
