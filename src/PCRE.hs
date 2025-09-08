{-# LANGUAGE UnicodeSyntax #-}
{- | PCRE Wrapper & replacement utilities. -}

module PCRE
  ( PCRE
  , REPlacement
  , compRE
  , reMatch
  , replace
  , replace1
  , replaceMany
  , replaceSome
  , tests
  , (=~)
  , (?=~)
  , (≃)
  , (~~)
  , (≈)
  ) where

import Base1T
import Prelude ( error )

-- base --------------------------------

import Data.List          ( reverse )
import Data.List.NonEmpty ( nonEmpty )
import Data.Maybe         ( fromMaybe )

-- parsec-plus -------------------------

import Parsec.Error ( ParseError )
import ParsecPlus   ( Parsecable(parsec) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import PCRE.Base        ( PCRE, compRE, pcre, (?=~) )
import PCRE.Error       ( AsREFnError, AsREGroupError, REFnGroupError )
import PCRE.GroupID     ( GroupID(GIDName, GIDNum) )
import PCRE.REMatch     ( reMatch, sourcePost, sourcePre, (=~), (~~), (≃), (≈) )
import PCRE.REPlacement ( REPlacement(REPlacement) )
import PCRE.ReplExpr    ( ReplExpr(ReplExpr), applyExpr )
import PCRE.ReplText    ( ReplText(ReplText), ReplTextFrag(RTFExpr, RTFText) )

--------------------------------------------------------------------------------

{- | Make a regex replacement; if the regex does not match, return Nothing. -}
replace ∷ ∀ ε η . (AsREFnError ε, AsREGroupError ε, MonadError ε η) ⇒
          REPlacement → 𝕋 → η (𝕄 𝕋)
replace (REPlacement r (ReplText repl)) t = sequence $
  go ⊳ r ≃ t
  where
        -- go' ∷ (AsREGroupError ε, MonadError ε η) ⇒ ReplTextFrag → η 𝕋
        go' _ (RTFText x) = return x
        go' n (RTFExpr x) = applyExpr x r n
        -- go ∷ (AsREGroupError ε, MonadError ε η) ⇒ REMatch 𝕋 → η 𝕋
        go m = (\ xs → m ⊣ sourcePre ⊕ ю xs ⊕ m ⊣ sourcePost)
             ⊳ (sequence $ go' m ⊳ repl)

replaceTests ∷ TestTree
replaceTests = testGroup "replace"
  [ testCase "repl0 re1 nonsense" $
      𝕽 𝕹 @=? replace @REFnGroupError @(𝔼 _) rep1_0 "nonsense"
  , testCase "repl0 re1 foobar" $
      𝕽 (𝕵 ">>bar<< (foo) [foobar]") @=? replace @REFnGroupError rep1_0 "foobar"
  , testCase "repl1 re1 foobar" $
      𝕽 (𝕵 ">>bar<< (foo) [foobar] $ \n") @=?
        replace @REFnGroupError rep1_1 "foobar"
  , testCase "repl2 re2 Hello.mum!" $
      𝕽 (𝕵 "Hello Mum!") @=?
        replace @REFnGroupError rep2_2 "Hello.mum!"
  , testCase "repl3 re3 /tmp/foo.barend" $
      𝕽 (𝕵 "/tmp/quux.Bar/end") @=?
        replace @REFnGroupError rep3_3 "/tmp/foo.barend"
  ]

----------------------------------------

{- | Make at most one regex replacement.

     The input list is a list of tagged replacements; the first match found is
     effected, and the replacement returned along with the tag prefix; if no
     regex matches, return Nothing.
 -}
replace1 ∷ ∀ ε α η . (AsREFnError ε, AsREGroupError ε, MonadError ε η) ⇒
           [(α,REPlacement)] → 𝕋 → η (𝕄 (α,𝕋))
replace1 []         _ = return 𝕹
replace1 ((a,r):rs) t =
  replace r t ≫ maybe (replace1 rs t) (return ∘ 𝕵 ∘ (a,))

replace1Tests ∷ TestTree
replace1Tests = testGroup "replace1" $
  let check ∷ TestName → [(ℤ,REPlacement)] → 𝕋 → (ℤ,𝕋) → TestTree
      check nm reps txt ex =
        testCase nm $ 𝕽 (𝕵 ex) @=? replace1 @REFnGroupError reps txt
   in [ testCase "rep1_0 rep1_1 nonsense" $
          𝕽 𝕹 @=? replace1 @REFnGroupError @_ @(𝔼 _)
                           [(0∷ℤ,rep1_0),(1,rep1_1)] "nonsense"
      , check "rep1_0,rep1_1"
              [(0,rep1_0),(1,rep1_1)] "foobar" (0,">>bar<< (foo) [foobar]")
      , check "rep1_0,rep3_3"
              [(0,rep1_0),(3,rep3_3)] "foobar" (0,">>bar<< (foo) [foobar]")
      , check "rep1_1,rep1_0"
              [(1,rep1_1),(0,rep1_0)] "foobar" (1,">>bar<< (foo) [foobar] $ \n")
      , check "rep3_3,rep1_0"
              [(3,rep3_3),(0,rep1_0)] "foobar" (0,">>bar<< (foo) [foobar]")
      ]

----------------------------------------

{- | Make none or more regex replacements; if no regex matches, return the
     incoming Text value.
 -}
replaceMany ∷ ∀ ε α η . (AsREFnError ε, AsREGroupError ε, MonadError ε η) ⇒
              [(α,REPlacement)] → 𝕋 → η ([α],𝕋)
replaceMany rs t = first reverse ⊳
  foldM (\ (as,s) (a,r) → fromMaybe (as,s) ⊳ ((a:as,) ⊳⊳ replace r s)) ([],t) rs

replaceManyTests ∷ TestTree
replaceManyTests = testGroup "replaceMany" $
  let check ∷ TestName → [(ℤ,REPlacement)] → 𝕋 → ([ℤ],𝕋) → TestTree
      check nm reps txt ex =
        testCase nm $ (𝕽 ex) @=? replaceMany @REFnGroupError reps txt
   in [ check "rep1_0,rep1_1 nonsense" [(0,rep1_0),(1,rep1_1)]
              "nonsense" ([],"nonsense")
      , check "rep1_0,rep1_1" [(0,rep1_0),(1,rep1_1)]
              "foobar" ([0,1],">>bar<< (foo) [>>bar<< (foo) [foobar] $ \n]")
      , check "rep1_1,rep1_0" [(1,rep1_1),(0,rep1_0)]
              "foobar" ([1,0],">>bar<< (foo) [>>bar<< (foo) [foobar]] $ \n")
      , check "rep1_1,rep3_3,rep1_0" [(1,rep1_1),(3,rep3_3),(0,rep1_0)]
              "foobar" ([1,0],">>bar<< (foo) [>>bar<< (foo) [foobar]] $ \n")
      ]

----------------------------------------

{- | Make one or more regex replacements; if no regex matches, return Nothing.
 -}
replaceSome ∷ ∀ ε α η . (AsREFnError ε, AsREGroupError ε, MonadError ε η) ⇒
              [(α,REPlacement)] → 𝕋 → η (𝕄 (NonEmpty α,𝕋))

replaceSome rs t = (\ (xs,u) → (,u) ⊳ nonEmpty xs) ⊳ replaceMany rs t

replaceSomeTests ∷ TestTree
replaceSomeTests = testGroup "replaceSome" $
  let check ∷ TestName → [(ℤ,REPlacement)] → 𝕋 → 𝕄 (NonEmpty ℤ, 𝕋) → TestTree
      check nm reps txt ex =
        testCase nm $ (𝕽 $ ex) @=? replaceSome @REFnGroupError reps txt
   in [ check "rep1_0,rep1_1 nonsense" [(0,rep1_0),(1,rep1_1)] "nonsense" 𝕹
      , check "rep1_0,rep1_1" [(0,rep1_0),(1,rep1_1)]
              "foobar" (𝕵(0:|[1],">>bar<< (foo) [>>bar<< (foo) [foobar] $ \n]"))
      , check "rep1_1,rep1_0" [(1,rep1_1),(0,rep1_0)]
              "foobar" (𝕵(1:|[0],">>bar<< (foo) [>>bar<< (foo) [foobar]] $ \n"))
      , check "rep1_1,rep3_3,rep1_0" [(1,rep1_1),(3,rep3_3),(0,rep1_0)]
              "foobar" (𝕵(1:|[0],">>bar<< (foo) [>>bar<< (foo) [foobar]] $ \n"))
      ]

--------------------------------------------------------------------------------

------------------------------------------------------------
--                       test data                        --
------------------------------------------------------------

re1 ∷ PCRE
re1 = [pcre|${iggy}(fo+)${pop}(.ar)|]
re2 ∷ PCRE
re2 = [pcre|^${one}(\w+)${two}(\..*)$|]
re3 ∷ PCRE
re3 = [pcre|(?<=/)foo\.*(.{3})|]

repl0 ∷ ReplText -- ">>${pop}<< (${1}) [${0}]"
repl0 = let repnam t = RTFExpr $ ReplExpr [] (GIDName t)
            repnum i = RTFExpr $ ReplExpr [] (GIDNum i)
         in ReplText $ [ RTFText ">>"
                       , repnam "pop"
                       , RTFText "<<"
                       , RTFText " ("
                       , repnum 1
                       , RTFText ") ["
                       , repnum 0
                       , RTFText "]"
                       ]

fromRight ∷ Show α ⇒ 𝔼 α β → β
fromRight (𝕷 a) = error $ [fmt|fromRight: %w|] a
fromRight (𝕽 b) = b

parsec' ∷ 𝕋 → 𝕋 → ReplText
parsec' name val = fromRight $ parsec @_ @ParseError name val

repl1 ∷ ReplText
repl1 = parsec' "repl1" ">>${pop}<< (${1}) [${0}] $$ \n"

repl2 ∷ ReplText
repl2 = parsec' "repl2" "${1}${.title.tr(\".\",\" \") 2}"

repl3 ∷ ReplText
repl3 = parsec' "repl3" "quux.${.title 1}/"

rep1_0 ∷ REPlacement -- s/${iggy}(fo+)${pop}(.ar)/>>${pop}<< (${1}) [${0}]/
rep1_0 = REPlacement re1 repl0

rep1_1 ∷ REPlacement --s/${iggy}(fo+)${pop}(.ar)/>>${pop}<< (${1}) [${0}] $$ \n/
rep1_1 = REPlacement re1 repl1

rep2_2 ∷ REPlacement -- s/^${one}(\w+)${two}(\..*)$/${1}${.title.tr("."," ") 2}/
rep2_2 = REPlacement re2 repl2

rep3_3 ∷ REPlacement -- s/(?<=/)foo\.*(.{3})$/quux.${.title 1}\//
rep3_3 = REPlacement re3 repl3

----------------------------------------

{-| unit tests -}
tests ∷ TestTree
tests = testGroup "parseReplacementText" [ replaceTests
                                         , replace1Tests
                                         , replaceManyTests
                                         , replaceSomeTests
                                         ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
