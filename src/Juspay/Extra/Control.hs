{-
 Copyright 2023-24, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under
 the terms of the GNU Affero General Public License as published by the
 Free Software Foundation, either version 3 of the License, or (at your option)
 any later version. This program is distributed in the hope that it will be
 useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 See the GNU Affero General Public License for more details. You should have
 received a copy of the GNU Affero General Public License along with this
 program. If not, see <https://www.gnu.org/licenses/>.
-}

{-#LANGUAGE RankNTypes #-}

-- | Some useful extra functions and combinators  for writing
-- business logic code and some notes on the topic.
module Juspay.Extra.Control
 ( except
 , exceptMap
 , exceptMaybe
 , exceptMaybeM
 , when'
 , unless'
 -- * Runner
 , runExceptT
 -- * Additional re-exports
 , whenLeft
 -- $notes
 ) where

import Prelude
import Control.Monad.Trans.Except
    ( except, runExceptT, withExceptT, ExceptT(..) )
import Data.Either.Extra ( maybeToEither )
import Data.Either.Combinators (whenLeft)

-- | Like except but maps an error.

-- | Like except but maps an error.
-- Map a function over the error type of an 'Either' within the context of 'ExceptT'.
--
-- Given a function f :: e -> e' and an 'Either' value, it lifts the function to
-- operate on the error type within the 'ExceptT' monad transformer.
--
-- The resulting 'ExceptT' computation has the same behavior as the original
-- 'ExceptT', but with the error type transformed using the provided function.
--
-- Example:
-- @
--   let result = exceptMap (\err -> show err) (Left "Error")
--   -- result :: ExceptT String m a
-- @
exceptMap :: forall a e e' m . Monad m => (e -> e') -> Either e a -> ExceptT e' m a
exceptMap f = withExceptT f . except

-- | Lifts a 'Maybe' value into an 'ExceptT' computation with a specified error value.
--
-- Given an error value 'e' and a 'Maybe' value, it converts the 'Maybe' into
-- an 'Either' with the provided error value. Then, it wraps the 'Either' in the
-- 'ExceptT' monad transformer.
--
-- The resulting 'ExceptT' computation behaves similarly to the original 'Maybe',
-- but with the added capability of handling a specific error in the 'ExceptT'.
--
-- Example:
-- >>> let result = exceptMaybe "Error" (Just 42)
-- >>> -- result :: ExceptT String m Int
exceptMaybe :: forall a e m . Monad m => e -> Maybe a -> ExceptT e m a
exceptMaybe e = except . maybeToEither e

-- | Constructs a computation from @Maybe a@ inside a monad @m@.
-- Lifts a monadic 'Maybe' value into an 'ExceptT' computation with a specified error value.
--
-- Given an error value 'e' and a monadic 'Maybe' value, it converts the 'Maybe'
-- into an 'Either' with the provided error value within the underlying monad.
-- Then, it wraps the 'Either' in the 'ExceptT' monad transformer.
--
-- The resulting 'ExceptT' computation behaves similarly to the original monadic 'Maybe',
-- but with the added capability of handling a specific error in the 'ExceptT'.
--
-- Example:
-- >>> let result = exceptMaybeM "Error" (return (Just 42))
-- >>> -- result :: ExceptT String m Int
exceptMaybeM :: forall a e m . Monad m => e -> m (Maybe a) -> ExceptT e m a
exceptMaybeM e = ExceptT . fmap (maybeToEither e)

-- | Runs an action when condition is met; otherwise just pass an input
-- to the next action.
-- Conditionally applies a function to a value based on a Boolean condition.
--
-- Given a Boolean condition, a function 'f' of type (a -> m a) is applied to
-- a value of type 'a' if the condition is 'True'. If the condition is 'False',
-- the original value is lifted into the applicative context without modification.
--
-- The resulting computation is of type (a -> m a), where 'm' is an applicative functor.
--
-- Example:
-- >>> let result = when' True (\x -> Just (x + 1)) 3
-- >>> -- result :: Maybe Int
when' :: forall a m . Applicative m => Bool -> (a -> m a) -> (a -> m a)
when' True f = f
when' False _ = pure

-- TODO can we implement it using ifM?
-- when' cond trueAct = ifM (pure cond) trueAct pure

-- | The opposite of 'when''
-- Conditionally applies a function to a value based on the negation of a Boolean condition.
--
-- This function is a composition of 'when'', where the condition is the negation of
-- the provided Boolean argument. It applies a function 'f' of type (a -> m a) to
-- a value of type 'a' if the condition is 'False'. If the condition is 'True',
-- the original value is lifted into the applicative context without modification.
--
-- The resulting computation is of type (a -> m a), where 'm' is an applicative functor.
--
-- Example:
-- >>> let result = unless' True (\x -> Just (x + 1)) 3
-- >>> -- result :: Maybe Int
unless' :: forall a m . Applicative m => Bool -> (a -> m a) -> (a -> m a)
unless' = when' . not

-- $notes
--
-- = Streamlining BL using Either monad
--
-- Quite typical business logic we can constantly observe in Euler's codebase looks like this:
--
-- > foo :: Flow SomeType
-- > doSth = do
-- >   maybeBar <- loadBar
-- >   case maybeBar of
-- >    Nothing -> do
-- >      logErrorT "doSth" "no bar"
-- >      throwException ex
-- >    Just bar -> do
-- >      case (validateBar) of
-- >        Failed e -> do
-- >          logErrorT "doSth" "malformed bar"
-- >          throwException ex
-- >        Parsed goofBar -> do
-- >          case (findBazByBar goodBar) of
-- >           Left err -> ...
-- >           Right baz -> ...
-- >
-- >           ... and so on ...
--
-- Such code suffers from several flaws:
--
-- * it's hard to read due to its verbosity and tendency to shift to the right
--
-- * it's highly imperative -- we do some calculations and perform some effects at the same time
--
-- * we can't see wood for the trees: this is known as
-- <https://existentialtype.wordpress.com/2011/03/15/boolean-blindness/ boolean blindness>
-- which takes a form of `Either`/`Maybe` blindness in this case
--
-- * flow control is very repetitive: in fact it boils down to the functionality of possibly
-- short-circuit failing computation provided by `Either` monad; all @case@s are just repeating
-- `>>=` every time on end
--
-- So let's try to refactor a sample of such code. We have the @authenticate@ function from
-- @euler-webservice@ project:
--
-- > authenticate
-- >   :: RouteParameters
-- >   -> Maybe Signature
-- >   -> Flow (Either Text ETM.MerchantAccount)
-- > authenticate routeParams _ = case lookupRP @Authorization routeParams of
-- >   Nothing -> do
-- >     logErrorT "authenticateRequestWithouthErr" "No authorization found in header"
-- >     throwException $ Errs.eulerAccessDenied "API key not present in Authorization header"
-- >   Just apiKeyStr -> do
-- >     flip catch handleErrorResponse . flip catch handleECErrorResponse $
-- >       case extractApiKey apiKeyStr of
-- >         Left err -> do
-- >           logErrorT "API key extracting" $ "Can't extract API key" <> " error: " <> err
-- >           throwException $ Errs.eulerAccessDenied "Invalid API key."
-- >         Right apiKey -> do
-- >           mk <- whenNothingM (ETM.getActiveByApiKey apiKey)
-- >             (throwException Errs.ecAccessDenied)
-- >           merchantAccount' <- whenNothingM (ETM.loadMerchantDBById mk.merchantAccountId)
-- >             (throwException Errs.ecAccessDenied)
-- >           case ETM.parseMerchantAccount merchantAccount' of
-- >             P.Failed e -> do
-- >               logErrorT "DB MerchantAccount Validation" $ show e
-- >               throwException Errs.internalError
-- >             P.Result validMAcc -> do
-- >               let authScope = getAuthScope routeParams
-- >               isScope <- if authScope == MERCHANT then ipAddressFilters validMAcc (lookupRP @XForwardedFor routeParams) else pure True
-- >               if isScope then pure $ Right validMAcc else (pure . Left . show) Errs.ecBadOrigin
-- >   where
-- >     handleErrorResponse :: ETypes.ErrorResponse -> Flow (Either Text ETM.MerchantAccount)
-- >     handleErrorResponse err = pure $ Left (show err)
-- >
-- >     handleECErrorResponse :: ETypes.ECErrorResponse -> Flow (Either Text ETM.MerchantAccount)
-- >     handleECErrorResponse err = pure $ Left (show err)
--
-- Let's start with defining an ad-hoc data-type to describe potential outcomes from @authorize@.
-- We are interested only in failure which might occur since its resulting type
-- @Flow (Either Text ETM.MerchantAccount)@ already encodes the successful result
-- in its @Right@ part. So we just have to define:
--
-- > data AuthError =
-- >  ...
--
-- and skim through the function's body looking for all flow interruptions, giving them
-- a meaningful names and collecting all actions we'd like to perform in comments for
-- later use. We are free to store any additional information in data constructors if
-- needed for required actions:
--
-- > data AuthError
-- >   = AuthorizationHeaderIsEmpty
-- >       --   logErrorT "authenticateRequestWithouthErr" "No authorization found in header"
-- >       --   throwException $ Errs.eulerAccessDenied "API key not present in Authorization header"
-- >   | InvalidApiKey Text
-- >       -- logErrorT "API key extracting" $ "Can't extract API key" <> " error: " <> err
-- >       -- throwException $ Errs.eulerAccessDenied "Invalid API key."
-- >   | KeyNotActive
-- >       -- (throwException Errs.ecAccessDenied)
-- >   | MerchantNotFound
-- >       -- (throwException Errs.ecAccessDenied)
-- >   | MerchantAccountValidationFailed Text
-- >         -- logErrorT "DB MerchantAccount Validation" $ show e
-- >         -- throwException Errs.internalError
-- >   | BadOrigin
-- >         -- throwException Errs.ecBadOrigin
--
-- Haskell allows us to define such <https://jaspervdj.be/posts/2016-05-11-ad-hoc-datatypes.html ad-hoc-datatypes>
-- without hassle. They aim three goals:
--
-- * making possible outcomes explicitly defined
--
-- * tearing apart two phases of execution: figuring out what should be done and performing those actions
--
-- * separating sets of data which are needed for every particular case (the linked article gives better example of this)
--
-- Now lets remove all "unhappy" paths from the function being refactored. What's left is essentially
-- the sequence of the happy path actions. So we have:
--
-- > authenticate
-- >   :: RouteParameters
-- >   -> Maybe Signature
-- >   -> Flow (Either Text ETM.MerchantAccount)
-- > authenticate routeParams _ =
-- >   case lookupRP @Authorization routeParams of...
-- >   case extractApiKey apiKeyStr of...
-- >   mk <- ETM.getActiveByApiKey apiKey...
-- >   merchantAccount' <- ETM.loadMerchantDBById mk.merchantAccountId...
-- >   case ETM.parseMerchantAccount merchantAccount' of
-- >   ...
-- >   let authScope = getAuthScope routeParams
-- >   isScope <- if authScope == MERCHANT then ipAddressFilters validMAcc (lookupRP @XForwardedFor routeParams) else pure True
-- >   if isScope then pure $ Right validMAcc else (pure . Left . show) Errs.ecBadOrigin
-- >   ...
--
-- Now we can go ahead boldly and rewrite it in the exception monad while extracting stubs of separate actions:
--
-- > authenticate
-- >   :: RouteParameters
-- >   -> Maybe Signature
-- >   -> Flow (Either Text ETM.MerchantAccount)
-- > authenticate routeParams _ = do
-- >     runExceptT $
-- >           getAuthHeader
-- >       >>= extractKey
-- >       >>= findActiveKey
-- >       >>= loadMerchantAccount
-- >       >>= validateMerchantAccount
-- >       >>= runIpAddressFilters
-- >     >>= handleAuthResult
-- >
-- >     where
-- >       getAuthHeader = undefined -- lookupRP @Authorization routeParams
-- >       ...
-- >       and so on
--
-- Well, being written like this the whole flow can be read literally in no more than ten seconds.
-- It's declarative, linear and leverages the implementation of '>>=' for 'Except' monad.
-- But we have to make all actions compatible with the @Either AuthError ETM.MerchantAccount@.
--
-- In order to do this we can start off with some functions from "Control.Monad.Trans.Except" module.
--
-- > except :: Monad m => Either e a -> ExceptT e m a
-- > withExceptT :: Functor m => (e -> e') -> ExceptT e m a -> ExceptT e' m a
--
-- Let's build up several convenient functions and combinators to wire up our actions smoothly.
-- Those ones are exactly what this module consists of, we will use all of them while refactoring
-- @authorize@ function. But let's revise their types and designation:
--
-- > -- | Like except but maps an error
-- > exceptMap :: forall a e e' m . Monad m => (e -> e') -> Either e a -> ExceptT e' m a
-- >
-- > -- | Like except, but for 'Maybe a', constructs a computation in the exception monad
-- > -- taking a value for a left value
-- > exceptMaybe :: forall a e m . Monad m => e -> Maybe a -> (ExceptT e m a)
-- >
-- > -- | Constructs a computation from 'Maybe a' inside a monad m
-- > exceptMaybeM :: forall a e m . Monad m => e -> m (Maybe a) -> (ExceptT e m a)
-- >
-- > -- | Runs an action when condition is met; otherwise just pass an input
-- > --  to the next action
-- > when' :: forall a m . Applicative m => Bool -> (a -> m a) -> (a -> m a)
-- >
-- > -- | The opposite of 'when''
-- > unless' :: forall a m . Applicative m => Bool -> (a -> m a) -> (a -> m a)
--
-- So let's  make all original actions compatible, taking on the first one which is is following:
--
-- > case lookupRP @Authorization routeParams of...
--
-- Here we have to lift @Maybe Text@ into @ExceptT AuthError Flow Text@.
-- This is exactly what 'exceptMaybe' does taking a value to use for @Left@
-- in the case of missing values in the underlying 'Maybe'. So we basically say
-- here: lookup @Authorization@ header and use @AuthorizationHeaderIsEmpty@ when
-- there no such header exists:
--
-- > getAuthHeader = exceptMaybe AuthorizationHeaderIsEmpty $ lookupRP @Authorization routeParams
--
-- The next action is an 'Either' already, so we only have to map the @Left@ to @AuthError@.
-- This is what `exceptMap` does:
--
-- > extractKey apiKeyStr = exceptMap InvalidApiKey $ extractApiKey apiKeyStr
--
-- Next two are identical - they are just @Flow Maybe a@ so we just have to attach
-- corresponding errors using `exceptMaybeM`:
--
-- > findActiveKey apiKey = exceptMaybeM KeyNotActive $ ETM.getActiveByApiKey apiKey
-- > loadMerchantAccount mk = exceptMaybeM MerchantNotFound $ ETM.loadMerchantDBById mk.merchantAccountId
--
-- Next action is parsing done by a custom parser in "Juspay.Extra.Parsing" module.
-- At the time of writing it doesn't provide a mapping to 'Either' but we easily can
-- write it (later we will definitely put it into that package itself):
--
-- > -- | Maps a parsing result to Either
-- > parsedToExcept :: P.Parsed b -> Either (NonEmptyVector P.ParsingError) b
-- > parsedToExcept = \case
-- >   P.Failed errs -> Left errs
-- >   P.Result x    -> Right x
--
-- The last action is tricky, so let's break it down:
--
-- > -- get the scope
-- > let authScope = getAuthScope routeParams
-- > isScope <-
-- >   -- check IP-address for MERCHANT scope only
-- >   if authScope == MERCHANT then
-- >     ipAddressFilters validMAcc (lookupRP @XForwardedFor routeParams)
-- >   -- otherwise just return True
-- >   else pure True
-- > if isScope
-- >   -- just return the result
-- >   then pure $ Right validMAcc
-- >   -- return bad origin error
-- >   else (pure . Left . show) Errs.ecBadOrigin
--
-- Again, when we see "just return True" or "just return the result" we should start
-- thinking about whether there exists an abstraction which can do the job for us.
--
-- First obvious observation here is that no action is required when @authScope /= MERCHANT@.
-- So we can use `when'` combinator to skip the check, so now we have:
--
-- > authenticate routeParams _ = do
-- >     runExceptT $
-- >           getAuthHeader
-- >       ...
-- >       >>= when' (getAuthScope routeParams == MERCHANT) runIpAddressFilters
-- >     >>= handleAuthResult
--
-- We can just 'lift' the original action and use monadic 'ifM':
--
-- > runIpAddressFilters macc =
-- >   ifM (lift $ ipAddressFilters macc (lookupRP @XForwardedFor routeParams))
-- >     (pure macc)
-- >     (throwE BadOrigin)
--
-- And now the final part - let's handle the result of our well-written computation.
-- It's straightforward:
--
-- > handleAuthResult :: Either AuthError ETM.MerchantAccount -> Flow (Either Text ETM.MerchantAccount)
-- > handleAuthResult res = do
-- >   -- perform effects on errors
-- >   whenLeft res handleError
-- >   pure $ first show res
--
-- In @handleErrors@ we can pattern match over @AuthError@ constructors and perform
-- the needed actions pulling them out of the comments in @AuthError@ where we put
-- them in the beginning:
--
-- > handleError :: AuthError -> Flow ()
-- > handleError err = do
-- >   case err of
-- >     AuthorizationHeaderIsEmpty -> logErrorT "authenticateRequestWithouthErr" "No authorization found in header"
-- >     InvalidApiKey msg -> logErrorT "API key extracting" $ "Can't extract API key" <> " error: " <> msg
-- >     MerchantAccountValidationFailed msg -> logErrorT "DB MerchantAccount Validation" $ msg
-- >     _ -> pure ()
-- >   pure ()
--
-- To preserve back-compatibility we have to map @AuthError@ back to @Text@. So finally
-- we have a little bit longer but far more well-structured code (I removed all parts
-- that are to go to __euler-hs__):
--
-- > data AuthError
-- >   = AuthorizationHeaderIsEmpty
-- >   | InvalidApiKey Text
-- >   | KeyNotActive
-- >   | MerchantNotFound
-- >   | MerchantAccountValidationFailed Text
-- >   | BadOrigin
-- >
-- > authenticate
-- >   :: RouteParameters
-- >   -> Maybe Signature
-- >   -> Flow (Either Text ETM.MerchantAccount)
-- > authenticate routeParams _ = do
-- >     runExceptT $
-- >           getAuthHeader
-- >       >>= extractKey
-- >       >>= findActiveKey
-- >       >>= loadMerchantAccount
-- >       >>= validateMerchantAccount
-- >       >>= when' (getAuthScope routeParams == MERCHANT) runIpAddressFilters
-- >     >>= handleAuthResult
-- >
-- >     where
-- >       getAuthHeader = _ $ lookupRP @Authorization routeParams
-- >
-- >       extractKey apiKeyStr = exceptMap InvalidApiKey $
-- >       extractApiKey apiKeyStr
-- >
-- >       findActiveKey apiKey = exceptMaybeM KeyNotActive $ ETM.getActiveByApiKey apiKey
-- >
-- >       loadMerchantAccount mk = exceptMaybeM MerchantNotFound $ ETM.loadMerchantDBById mk.merchantAccountId
-- >
-- >       validateMerchantAccount macc = exceptMap (MerchantAccountValidationFailed . show)
-- >         $ parsedToExcept $ ETM.parseMerchantAccount macc
-- >
-- >       runIpAddressFilters macc =
-- >         ifM (lift $ ipAddressFilters macc (lookupRP @XForwardedFor routeParams))
-- >           (pure macc)
-- >           (throwE BadOrigin)
-- >
-- >       handleAuthResult :: Either AuthError ETM.MerchantAccount -> Flow (Either Text ETM.MerchantAccount)
-- >       handleAuthResult res = do
-- >         whenLeft res handleError
-- >         pure $ first report res
-- >
-- >         report AuthorizationHeaderIsEmpty = show $ Errs.eulerAccessDenied "API key not present in Authorization header"
-- >         report (InvalidApiKey _) = show $ Errs.eulerAccessDenied "Invalid API key."
-- >         report (MerchantAccountValidationFailed _ ) = show Errs.internalError
-- >         report (BadOrigin) = show Errs.ecBadOrigin
-- >         report _ = show Errs.ecAccessDenied
-- >
-- >       handleError :: AuthError -> Flow ()
-- >       handleError err = do
-- >         case err of
-- >           AuthorizationHeaderIsEmpty -> logErrorT "authenticateRequestWithouthErr" "No authorization found in header"
-- >           InvalidApiKey msg -> logErrorT "API key extracting" $ "Can't extract API key" <> " error: " <> msg
-- >           MerchantAccountValidationFailed msg -> logErrorT "DB MerchantAccount Validation" $ msg
-- >           _ -> pure ()
-- >         pure ()
