{-| Utilities for working with `RE.Match` -}

module PCRE.Match
  ( fmtMatch )
where

import Base1

-- array -------------------------------

import Data.Array  ( assocs )

-- base --------------------------------

import Data.List  ( sortOn )

-- regex -------------------------------

import Text.RE.Replace  ( capturedText, captureLength, captureNames
                        , captureOffset, getCaptureName, getCaptureOrdinal
                        , matchArray
                        )

-- regex-with-pcre ---------------------

import Text.RE.PCRE.Text  ( Match, matchSource )

-- text --------------------------------

import Data.Text  ( unlines )

-- unordered-containers ----------------

import qualified  Data.HashMap.Strict  as  HashMap

--------------------------------------------------------------------------------

{-| A nice-looking multiline `𝕋` representing a `Match` datum. -}
fmtMatch ∷ (Show α, Printable α) ⇒ Match α → 𝕋
fmtMatch m =
  let ord = getCaptureOrdinal
      fmtCapI i c = let offs = captureOffset c
                        len  = captureLength c
                        txt  = capturedText c
                     in [fmt|%02d (%03d-%03d): %w|] (ord i) offs (offs+len) txt
      fmtCapN n i = [fmt|%-12t: %02d|] (getCaptureName n) (ord i)
      sortedCaptures =  sortOn fst ∘ HashMap.toList ∘ captureNames
   in unlines $ ю [ [ [fmt|%-12t: %w|] "source" (matchSource m) ]
                  , [ fmtCapI i c | (i,c) ← assocs (matchArray m) ]
                  , [ fmtCapN n i | (n,i) ← sortedCaptures m ]
              ]
-- that's all, folks! ----------------------------------------------------------
