{- | PCRE Error Types -}

module PCRE.Error
  ( AsREFnError(..), AsREGroupError(..), AsREParseError(..)
  , REFnError, REFnGroupError, REGroupError, REParseError, REParseGroupError
  , throwAsREFnError, throwAsREGroupError, throwREFnError
  )
where

import Base1

-- base --------------------------------

import Control.Monad.Fail  ( MonadFail( fail ) )

-- text --------------------------------

import Data.Text  ( pack )

-- text-printer ------------------------

import qualified Text.Printer  as  P

--------------------------------------------------------------------------------

{- | Failed to parse an RE -}

data REParseError = REParseError 𝕋 CallStack
  deriving Show

--------------------

instance Eq REParseError where
  (REParseError t _) == (REParseError t' _) = t == t'

--------------------

instance HasCallstack REParseError where
  callstack = lens (\ (REParseError _ cs) → cs)
                   (\ (REParseError t _) cs → REParseError t cs)

--------------------

instance Printable REParseError where
  print (REParseError t _) = P.text t

--------------------

instance MonadFail (𝔼 REParseError) where
  fail = 𝕷 ∘ (\ t → REParseError t callStack) ∘ pack

----------------------------------------

{- | Typeclass for errors that may be an `REParseError` -}

class AsREParseError ε where
  _REParseError ∷ Prism' ε REParseError

--------------------

instance AsREParseError REParseError where
  _REParseError = id

------------------------------------------------------------

{- | requested group not found in RE -}
data REGroupError = REGroupError 𝕋 CallStack
  deriving Show

--------------------

instance Eq REGroupError where
  (REGroupError t _) == (REGroupError t' _) = t == t'

--------------------

instance HasCallstack REGroupError where
  callstack = lens (\ (REGroupError _ cs) → cs)
                   (\ (REGroupError t _) cs → REGroupError t cs)


--------------------

instance Printable REGroupError where
  print (REGroupError t _) = P.text t

--------------------

instance MonadFail (𝔼 REGroupError) where
  fail = 𝕷 ∘ (\ t → REGroupError t callStack) ∘ pack

----------------------------------------

{- | Typeclass for errors that may be an `REGroupError`. -}

class AsREGroupError ε where
  _REGroupError ∷ Prism' ε REGroupError

--------------------

instance AsREGroupError REGroupError where
  _REGroupError = id

----------------------------------------

{- | throw an error that may be an `REGroupError` -}
throwAsREGroupError ∷ ∀ ε α η .
                      (AsREGroupError ε, MonadError ε η, HasCallStack) ⇒ 𝕋 → η α
throwAsREGroupError = throwError ∘ (_REGroupError #)  ∘ reGroupError

------------------------------------------------------------

{- | either an `REGroupError` or an `REParseError` -}
data REParseGroupError = REPGE_GroupE REGroupError
                       | REPGE_ParseE REParseError
  deriving (Eq,Show)

----------------------------------------

_REPGE_GroupE ∷ Prism' REParseGroupError REGroupError
_REPGE_GroupE = prism' REPGE_GroupE (\ case REPGE_GroupE e → 𝕵 e; _ → 𝕹)

----------

_REPGE_ParseE ∷ Prism' REParseGroupError REParseError
_REPGE_ParseE = prism' REPGE_ParseE (\ case REPGE_ParseE e → 𝕵 e; _ → 𝕹)

----------------------------------------

instance AsREGroupError REParseGroupError where
  _REGroupError = _REPGE_GroupE ∘ _REGroupError

--------------------

instance AsREParseError REParseGroupError where
  _REParseError = _REPGE_ParseE ∘ _REParseError

----------------------------------------

reGroupError ∷ HasCallStack ⇒ 𝕋 → REGroupError
reGroupError t = REGroupError t callStack

------------------------------------------------------------

{- | either a `REGroupError` or a `REFnError` -}
data REFnGroupError = REFGE_GroupE REGroupError
                    | REFGE_FnE    REFnError
  deriving (Eq,Show)

----------------------------------------

_REFGE_GroupE ∷ Prism' REFnGroupError REGroupError
_REFGE_GroupE = prism' REFGE_GroupE (\ case REFGE_GroupE e → 𝕵 e; _ → 𝕹)

----------

_REFGE_FnE ∷ Prism' REFnGroupError REFnError
_REFGE_FnE = prism' REFGE_FnE (\ case REFGE_FnE e → 𝕵 e; _ → 𝕹)

----------------------------------------

instance Exception REFnGroupError

--------------------

instance Printable REFnGroupError where
  print (REFGE_GroupE e) = print e
  print (REFGE_FnE    e) = print e

--------------------

instance HasCallstack REFnGroupError where
  callstack =
    let
      getter (REFGE_GroupE e)    = e ⊣ callstack
      getter (REFGE_FnE    e)    = e ⊣ callstack
      setter (REFGE_GroupE e) cs = REFGE_GroupE (e & callstack ⊢ cs)
      setter (REFGE_FnE    e) cs = REFGE_FnE    (e & callstack ⊢ cs)
    in
      lens getter setter

----------------------------------------

instance AsREGroupError REFnGroupError where
  _REGroupError = _REFGE_GroupE ∘ _REGroupError

--------------------

instance AsREFnError REFnGroupError where
  _REFnError = _REFGE_FnE ∘ _REFnError

------------------------------------------------------------

{- | error when calling an RE function; typically no such function (by name),
     else wrong argument count or type -}
-- really should divvy up into
--   - no such function
--   - wrong arg count
--   - wrong arg types
--   - right arg types, but illegal values
data REFnError = REFnError 𝕋 CallStack
  deriving Show

--------------------

instance Eq REFnError where
  (REFnError t _) == (REFnError t' _) = t == t'

--------------------

instance HasCallstack REFnError where
  callstack = lens (\ (REFnError _ cs) → cs)
                   (\ (REFnError t _) cs → REFnError t cs)

--------------------

instance Printable REFnError where
  print (REFnError t _) = P.text t

----------------------------------------

{- | make a `REFnError` -}
reFnError ∷ HasCallStack ⇒ 𝕋 → REFnError
reFnError t = REFnError t callStack

----------------------------------------

{- | throw a `REFnError` -}
throwREFnError ∷ ∀ α η . (MonadError REFnError η, HasCallStack) ⇒
                 𝕋 → η α
throwREFnError = throwError ∘ reFnError

----------------------------------------

{- | Typeclass for errors that may be an `REFnError`. -}

class AsREFnError ε where
  _REFnError ∷ Prism' ε REFnError

instance AsREFnError REFnError where
  _REFnError = id

----------------------------------------

{- | throw an error that may be an `REFnError` -}
throwAsREFnError ∷ ∀ ε α η . (AsREFnError ε, MonadError ε η, HasCallStack) ⇒
                   𝕋 → η α
throwAsREFnError = throwError ∘ (_REFnError #)  ∘ reFnError

-- that's all, folks! ----------------------------------------------------------
