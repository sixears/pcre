{-# LANGUAGE UnicodeSyntax #-}
{- | Encapsulation of groups (numbered), captures (by name), source, pre- and
     post-sequences for a PCRE match.

     BEWARE: When using zero-width assertions, capture names are borked by
     the Match type, meaning we'll get Exceptions.  I've stared at it, but cannot
     see a workaround :-(
-}

module PCRE.REMatch
  ( REMatch
  , reMatch
  , sourceCaptures
  , sourceGroups
  , sourcePost
  , sourcePre
  , sourceText
  , tests
  , (=~)
  , (~~)
  , (≃)
  , (≈)
  ) where

import Base1T
import Prelude  ( error )

-- base --------------------------------

import Data.Foldable  ( maximum )
import Data.List      ( sortOn, zip )
import Data.Maybe     ( isJust )

-- lens --------------------------------

import Control.Lens.Getter ( view )

-- natural -----------------------------

import Natural.Length  ( length )

-- regex -------------------------------

import Text.RE.Replace ( Capture(captureLength, captureOffset, captureSource,
                                 capturedText),
                         CaptureID(IsCaptureOrdinal),CaptureName(getCaptureName),
                         Match, captureNames, getCaptureOrdinal, matchArray,
                         matchCaptures, (!$$) )

-- regex-with-pcre ---------------------

import Text.RE.PCRE.Text ( reSource )

-- text --------------------------------

import Data.Text ( drop, take, unlines )

-- text-printer ------------------------

import Text.Printer qualified as P

-- unordered-containers ----------------

import Data.HashMap.Lazy ( HashMap, foldMapWithKey, (!?) )
import Data.HashMap.Lazy qualified as HashMap

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import PCRE.Base    ( PCRE(unPCRE), compRE, (?=~) )
import PCRE.Error   ( REParseError, throwAsREGroupError )
import PCRE.GroupID ( GroupID(GIDName, GIDNum), Groupable(group),
                      ToGroupID(toGroupID), groupNm )

--------------------------------------------------------------------------------

{-| details of a regular expression match, including pre- & post-, and capture
    groups by name & position -}
data REMatch α = REMatch { _sourceText     :: α
                         , _sourceGroups   :: [α]
                         , _sourceCaptures :: HashMap 𝕋 α
                         , _sourcePre      :: α
                         , _sourcePost     :: α
                         }
  deriving (Eq, Show)

--------------------

instance Groupable (REMatch 𝕋) where
  group r (toGroupID → gid) match =
    case match !! gid of
      𝓙 t → return t
      𝓝   → throwAsREGroupError $
              [fmt|group not found: %t in match of '%t' against re '%s'|]
                (groupNm gid) (match ⊣ sourceText) (reSource $ unPCRE r)

--------------------

instance Show α ⇒ Printable (REMatch α) where
  print r =
    let sortedCaptures =  sortOn fst ∘ HashMap.toList ∘ view sourceCaptures
     in P.text ∘ unlines $ ю
      [ [ [fmt|%-12t: %w|] "pre"   (r ⊣ sourcePre)  ]
      , [ [fmt|%-12t: %w|] "match" (r ⊣ sourceText) ]
      , [ [fmt|%-12t: %w|] "post"  (r ⊣ sourcePost) ]
      , [ [fmt|%02d%10s: %w|] i " " a | (i,a) ← zip [(0∷ℕ)..] (r⊣sourceGroups) ]
      , [ [fmt|%-12t: %w|] n t | (n,t) ← sortedCaptures r ]
      ]

--------------------

groupTests ∷ TestTree
groupTests =
  let compre = compRE @REParseError @(𝔼 _)
      r1 = compre "${iggy}(fo+)${pop}(.ar)"
      reM1 = REMatch "foobar" ["foo","bar"]
                     (HashMap.fromList [("iggy","foo"),("pop","bar")]) "" ""
{- BEWARE!  CAPTURE NAMES USED AFTER ZERO-WIDTH ASSERTIONS ARE BORKED

      r2 = compre "^${a}(.*/)?(?=[^/]+$)${b}(.*)(?:-)${c}(.*)$"
      reM2 = REMatch "/foo/bar/xx-yy" ["/foo/bar/","xx","yy"]
                     (HashMap.fromList [("a","/foo/bar/"),("b","xx"),("c","yy")])
                     "" ""
      r3 = compre "^${a}(.*/)?(?=[^/]+$)(.*)(?:-)${c}(.*)$"
      reM3 = REMatch "/foo/bar/xx-yy" ["/foo/bar/","yy"]
                     (HashMap.fromList [("a","/foo/bar/"),("c","yy")])
                     "" ""
-}

      r2 = compre "^${a}(.*/)?${b}(.*)(?:-)${c}(.*)$"
      reM2 = REMatch "/foo/bar/xx-yy" ["/foo/bar/","xx","yy"]
                     (HashMap.fromList [("a","/foo/bar/"),("b","xx"),("c","yy")])
                     "" ""
      r3 = compre "^${a}(.*/)?.*(?:-)${c}(.*)$"
      reM3 = REMatch "/foo/bar/xx-yy" ["/foo/bar/","yy"]
                     (HashMap.fromList [("a","/foo/bar/"),("c","yy")])
                     "" ""

   in testGroup "group"
    [ testCase "=~ r1" $ assertRight (𝓙 reM1 @=?) $ (=~ "foobar") ⊳ r1
    , testCase "=~ r2" $ assertRight (𝓙 reM2 @=?) $ (=~ "/foo/bar/xx-yy") ⊳ r2
    , testCase "=~ r3" $ assertRight (𝓙 reM3 @=?) $ (=~ "/foo/bar/xx-yy") ⊳ r3
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
  index (GIDName t) m = (m ⊣ sourceCaptures) !? t
  index (GIDNum  0) m = 𝓙 $ m ⊣ sourceText
  index (GIDNum  i) m = (m ⊣ sourceGroups) !! (i -1)

{-| convert a `Match 𝕋` to a `𝕄 (REMatch 𝕋)` (in particular, creating a
    `HashMap` of named groups) -}
reMatch ∷ Match 𝕋 → 𝕄 (REMatch 𝕋)
reMatch m = matchCaptures m ⊲
  \ (cap,caps) →
     let n !$$! i = n !$$ IsCaptureOrdinal i
         cs       =
           -- detect problems with capture lookup; specifically, if the regex
           -- includes zero-width assertions, the by-name capture borks, I think
           -- because the zero-width group gets assigned an ordinal but the
           -- capture never appears in the matchArray.  While it's not obvious
           -- why that should affect below, a consequence is that !$$ gets
           -- skewed.

           -- we try to detect this by looking for when CaptureOrdinals refer to
           -- positions not present in the matchArray

           let max_ordinal ∷ ℕ = getCaptureOrdinal ⊳ toList (captureNames m)
                                                   & maximum & fromIntegral
               match_count ∷ ℕ = length $ matchArray m
               msg = ю [ "match captures refer to ordinals out-of-range; "
                       , "maybe there are zero-width assertions in play?" ]
           in  if max_ordinal ≥ match_count
               then error $ [fmt|%s %w (%d ≥ %d)|] msg m max_ordinal match_count
               else foldMapWithKey (\ nm i → HashMap.singleton(getCaptureName nm)
                                                              (m !$$! i))
                                   (captureNames m)
         pre      = take (captureOffset cap) (captureSource cap)
         post     = drop (captureLength cap + captureOffset cap)
                         (captureSource cap)
     in  REMatch { _sourceText     = capturedText cap
                 , _sourceGroups   = capturedText ⊳ caps
                 , _sourceCaptures = cs
                 , _sourcePre      = pre
                 , _sourcePost     = post }


{-| match some `𝕋` against an `RE`; returning an `REMatch` -}
(=~) ∷ PCRE → 𝕋 → 𝕄 (REMatch 𝕋)
r =~ t = reMatch (t ?=~ r)

{- | alias for `=~` -}
(≃) ∷ PCRE → 𝕋 → 𝕄 (REMatch 𝕋)
r ≃ t = reMatch (t ?=~ r)

{- | does match (at all) -}
(~~) ∷ 𝕋 → PCRE → 𝔹
r ~~ t = isJust $ t =~ r

{- | alias for `~~` -}
(≈) ∷ 𝕋 → PCRE → 𝔹
(≈) = (~~)


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
-- PSA        ^${path}(.*/)?(?=[^/]+$)${series}(.*?)\.(?:\.202\d\.)?S${s}(\d{2})E${e}(\d{2})(?:\.${name}(.*?))?(?:\.(?:216|108)0p\..*)\.${ext}(mp4|mkv)        "${path}${series} - ${s}x${e} - ${.tr(\".\",\" \") name}.${ext}"
-- rename -f /home/martyn/.rename/tv --dry-run The.Morning.Show.S02E01.1080p.WEBRip.x265-RARBG.mp4 The.Morning.Show.S02E02.1080p.WEBRip.x265-RARBG.mp4 The.Morning.Show.S02E03.1080p.WEBRip.x265-RARBG.mp4 The.Morning.Show.S02E04.1080p.WEBRip.x265-RARBG.mp4 The.Morning.Show.S02E05.1080p.WEBRip.x265-RARBG.mp4 The.Morning.Show.S02E06.1080p.WEBRip.x265-RARBG.mp4 The.Morning.Show.S02E07.1080p.WEBRip.x265-RARBG.mp4 The.Morning.Show.S02E08.1080p.WEBRip.x265-RARBG.mp4 The.Morning.Show.S02E09.1080p.WEBRip.x265-RARBG.mp4 The.Morning.Show.S02E10.1080p.WEBRip.x265-RARBG.mp4 --debug
