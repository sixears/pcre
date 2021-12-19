{- | Encapsulation of groups (numbered), captures (by name), source, pre- and
     post-sequences for a PCRE match. -}

module PCRE.REMatch
  ( REMatch
  , (≃), (=~), reMatch
  , sourceCaptures, sourceGroups, sourcePre, sourcePost, sourceText

  , tests
  )
where

import Base1T

-- regex -------------------------------

import Text.RE.Replace  ( Capture( captureLength, captureOffset
                                 , captureSource, capturedText )
                        , CaptureID( IsCaptureOrdinal )
                        , CaptureName( getCaptureName )
                        , Match
                        , (!$$)
                        , captureNames, matchCaptures
                        )

-- regex-with-pcre ---------------------

import Text.RE.PCRE.Text  ( RE, (?=~), reSource )

-- text --------------------------------

import Data.Text  ( drop, take )

-- unordered-containers ----------------

import qualified Data.HashMap.Lazy  as  HashMap
import Data.HashMap.Lazy  ( HashMap, (!?), foldMapWithKey )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import PCRE.Base      ( compRE )
import PCRE.Error     ( REParseError, throwAsREGroupError )
import PCRE.GroupID   ( Groupable( group ), GroupID( GIDName, GIDNum )
                      , ToGroupID( toGroupID ), groupNm )

--------------------------------------------------------------------------------

{-| details of a regular expression match, including pre- & post-, and capture
    groups by name & position -}
data REMatch α = REMatch { _sourceText     ∷ α
                         , _sourceGroups   ∷ [α]
                         , _sourceCaptures ∷ HashMap 𝕋 α
                         , _sourcePre      ∷ α
                         , _sourcePost     ∷ α
                         }
  deriving (Eq,Show)

--------------------

instance Groupable (REMatch 𝕋) where
  group r (toGroupID → gid) match =
    case match !! gid of
      𝕵 t → return t
      𝕹   → throwAsREGroupError $
              [fmt|group not found: %t in match of '%t' against re '%s'|]
                (groupNm gid) (match ⊣ sourceText) (reSource r)

----------

groupTests ∷ TestTree
groupTests =
  let r1 = compRE @REParseError @(𝔼 _) "${iggy}(fo+)${pop}(.ar)"
      reM1 = REMatch "foobar" ["foo","bar"]
                     (HashMap.fromList [("iggy","foo"),("pop","bar")]) "" ""
   in testGroup "group"
    [ testCase "=~" $ assertRight (@=? 𝕵 reM1) $ (=~ "foobar") ⊳ r1
    ]

----------------------------------------

{-| matched stuff (typically 𝕋, but doesn't have to be) -}
sourceText ∷ Lens' (REMatch α) α
sourceText = lens _sourceText (\ rem st → rem { _sourceText = st })

{-| match captures, by position -}
sourceGroups ∷ Lens' (REMatch α) [α]
sourceGroups = lens _sourceGroups (\ rem sg → rem { _sourceGroups = sg })

{-| match captures, by name -}
sourceCaptures ∷ Lens' (REMatch α) (HashMap 𝕋 α)
sourceCaptures = lens _sourceCaptures (\ rem sc → rem { _sourceCaptures = sc })

{-| the stuff prior any match -}
sourcePre ∷ Lens' (REMatch α) α
sourcePre = lens _sourcePre (\ rem st → rem { _sourcePre = st })

{-| the stuff after any match -}
sourcePost ∷ Lens' (REMatch α) α
sourcePost = lens _sourcePost (\ rem st → rem { _sourcePost = st })

instance HasIndex (REMatch α) where
  type Indexer (REMatch α) = GroupID
  type Elem    (REMatch α) = α
--  index (GIDName t) m = _sourceCaptures m !? t
  index (GIDName t) m = (m ⊣ sourceCaptures) !? t
  index (GIDNum  0) m = 𝕵 $ m ⊣ sourceText
  index (GIDNum  i) m = (m ⊣ sourceGroups) !! (i -1)

{-| convert a `Match 𝕋` to a `𝕄 (REMatch 𝕋)` (in particular, creating a
    `HashMap` of named groups) -}
reMatch ∷ Match 𝕋 → 𝕄 (REMatch 𝕋)
reMatch m = matchCaptures m ⊲
  \ (cap,caps) →
     let n !$$! i = n !$$ IsCaptureOrdinal i
         cs       = foldMapWithKey
                      (\ nm i → HashMap.singleton (getCaptureName nm)
                                                  (m !$$! i))
                      (captureNames m)
         pre = take (captureOffset cap) (captureSource cap)
         post = drop (captureLength cap + captureOffset cap) (captureSource cap)
      in REMatch (capturedText cap) (capturedText ⊳ caps) cs pre post


{-| match some `𝕋` against an `RE`; returning an `REMatch` -}
(=~) ∷ RE → 𝕋 → 𝕄 (REMatch 𝕋)
r =~ t = reMatch (t ?=~ r)

{- | alias for `=~` -}
(≃) ∷ RE → 𝕋 → 𝕄 (REMatch 𝕋)
r ≃ t = reMatch (t ?=~ r)

--------------------------------------------------------------------------------

{-| unit tests -}
tests ∷ TestTree
tests = testGroup "REMatch" [ groupTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
