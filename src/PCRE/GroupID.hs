{- | PCRE Wrapper & replacement utilities. -}

module PCRE.GroupID
  ( Groupable(..), GroupID(..), ToGroupID(..), groupNm

  , tests
  )
where

import Base1T

-- array -------------------------------

import Data.Array  ( assocs )

-- base --------------------------------

import Data.List  ( lookup )
import Text.Read  ( read )

-- parsec-plus -------------------------

import ParsecPlus  ( Parsecable( parser ) )

-- parsers -----------------------------

import Text.Parser.Char  ( CharParsing, alphaNum, letter )

-- parser-plus -------------------------

import ParserPlus  ( digits )

-- regex -------------------------------

import Text.RE.Replace  ( Capture( Capture, capturedText )
                        , CaptureID( IsCaptureName, IsCaptureOrdinal )
                        , CaptureName( CaptureName )
                        , CaptureOrdinal( CaptureOrdinal )
                        , Match( captureNames, matchArray, matchSource )
                        , findCaptureID, matchCapture, matchedText
                        )

-- regex-with-pcre ---------------------

import Text.RE.PCRE.Text  ( RE, (?=~), reSource )

-- template-haskell --------------------

import Language.Haskell.TH         ( Name )
import Language.Haskell.TH.Syntax  ( Code, Exp( AppE, ConE ), Lift( liftTyped )
                                   , Quote, TExp( TExp ), liftCode )

-- text --------------------------------

import Data.Text  ( pack, unpack )

-- text-printer ------------------------

import qualified Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import PCRE.Base     ( compRE )
import PCRE.Error    ( AsREGroupError, REParseGroupError, throwAsREGroupError )

--------------------------------------------------------------------------------

data GroupID = GIDName 𝕋 | GIDNum ℕ
  deriving (Eq,Show)

--------------------

instance Printable GroupID where
  print (GIDNum  n) = P.text $ [fmt|%d|] n
  print (GIDName n) = P.text $ [fmt|%t|] n

--------------------

instance Lift GroupID where
 liftTyped (GIDName t) = liftCExp 'GIDName t
 liftTyped (GIDNum  n) = liftCExp 'GIDNum  n

--------------------

instance Parsecable GroupID where
  parser =
    let natural    ∷ CharParsing φ ⇒ φ ℕ
        natural    = read ⊳ digits
        identifier ∷ CharParsing φ ⇒ φ 𝕋
        identifier = pack ⊳ ((:) ⊳ letter ⊵ many alphaNum)
      in (GIDName ⊳ identifier) ∤ (GIDNum ⊳ natural)

--------------------

capID ∷ GroupID → CaptureID
capID (GIDNum  i) = IsCaptureOrdinal ∘ CaptureOrdinal $ fromIntegral i
capID (GIDName t) = IsCaptureName $ CaptureName t

groupNm ∷ GroupID → 𝕋
groupNm (GIDName t) = [fmt|'%t'|] t
groupNm (GIDNum  i) = [fmt|%d|]   i

----------------------------------------

class ToGroupID α where
  toGroupID ∷ α → GroupID

--------------------

instance ToGroupID GroupID where
  toGroupID = id

--------------------

instance ToGroupID ℕ where
  toGroupID = GIDNum

--------------------

instance ToGroupID 𝕋 where
  toGroupID = GIDName

--------------------

instance ToGroupID CaptureID where
  toGroupID (IsCaptureName    (CaptureName    t)) = GIDName t
  toGroupID (IsCaptureOrdinal (CaptureOrdinal i)) = GIDNum $ fromIntegral i

------------------------------------------------------------

class Groupable γ where
  {- RE is provided purely for error messages -}
  group ∷ ∀ ε α η .
           (ToGroupID α, AsREGroupError ε, MonadError ε η, HasCallStack) ⇒
           RE → α → γ → η 𝕋

--------------------


{- | Simple lifter that applies a constructor to a value. -}
liftCExp ∷ ∀ α τ χ . (Lift τ, Quote χ) ⇒ Name → τ → Code χ α
liftCExp y x = liftCode $ ⟦ x ⟧ ≫ return ∘ TExp ∘ AppE (ConE y)

{- | Simple lifter that applies a constructor to a value. -}
-- liftTExp ∷ ∀ α τ . Lift τ ⇒ Name → τ → Q (TExp α)
-- liftTExp y x = ⟦ x ⟧ ≫ return ∘ TExp ∘ AppE (ConE y)

instance Groupable (Match 𝕋) where
  group r (toGroupID → gid) match = do
      n ← case findCaptureID (capID gid) (captureNames match) of
        𝕷 e → throwAsREGroupError (pack e)
        𝕽 n → return n
      case lookup n (assocs $ matchArray match) of
        𝕹 → throwAsREGroupError $
              [fmt|group not found: %t in match of '%t' against re '%s'|]
                (groupNm gid) (matchSource match) (reSource r)
        𝕵 g → return $ capturedText g

--------------------

groupTests ∷ TestTree
groupTests =
  let r1 = compRE @_ @(𝔼 _) "${iggy}(fo+)${pop}(.ar)"
      gpge ∷ ToGroupID α ⇒ RE → α → Match 𝕋 → 𝔼 REParseGroupError 𝕋
      gpge = group
      gpgeT ∷ RE → 𝕋 → Match 𝕋 → 𝔼 REParseGroupError 𝕋
      gpgeT = gpge
      gpgeN ∷ RE → ℕ → Match 𝕋 → 𝔼 REParseGroupError 𝕋
      gpgeN = gpge
      testCapT t ex =
        testCase ("capture " ⊕ unpack t) $
          assertRight (ex @=?) $
            ("foobar" ?=~) ⊳ r1 ≫ \ m → join $ (\ r → gpgeT r t m) ⊳ r1
      testCapTFail t =
        testCase ("capture " ⊕ unpack t) $
          assertIsLeft $
            ("foobar" ?=~) ⊳ r1 ≫ \ m → join $ (\ r → gpgeT r t m) ⊳ r1
      testCapN i ex =
        testCase ("capture " ⊕ show i) $
          assertRight (ex @=?) $
            ("foobar" ?=~) ⊳ r1 ≫ \ m → join $ (\ r → gpgeN r i m) ⊳ r1
      testCapNFail i =
        testCase ("capture " ⊕ show i) $
          assertIsLeft $
            ("foobar" ?=~) ⊳ r1 ≫ \ m → join $ (\ r → gpgeN r i m) ⊳ r1
   in testGroup "group"
    [ testCase "parse r1" $ assertRight (const $ assertSuccess "parse r1") r1
    , testCase "match foobar" $
        assertRight ((@=? 𝕵 "foobar") ∘ matchedText) $ ("foobar" ?=~) ⊳ r1
    , testCase "capture foobar" $
        assertRight ((@=? 𝕵 (Capture "foobar" "foobar" 0 6)) ∘ matchCapture) $
          ("foobar" ?=~) ⊳ r1
    , testCapT "iggy" "foo"
    , testCapT "pop"  "bar"
    , testCapTFail "xxx"
    , testCapN 0 "foobar"
    , testCapN 2 "bar"
    , testCapNFail 3
    ]

------------------------------------------------------------

--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "GroupID" [ groupTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
