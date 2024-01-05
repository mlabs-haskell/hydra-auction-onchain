{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module HydraAuctionOnchain.Types.Error
  ( ErrorCode (fromErrorCode, toErrorCode)
  , ErrorCodePrefix (errorCodePrefix)
  , errCode
  , passert
  , passertMaybe
  , passertMaybeData
  ) where

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as Text (pack, stripPrefix, unpack)
import Data.Universe (Universe (universe))
import Language.Haskell.TH (Exp (LitE), Lit (StringL), Q)
import Plutarch.Api.V2 (PMaybeData)
import Plutarch.Extra.Bool (passert)
import Plutarch.Extra.Maybe (passertPDJust, passertPJust)
import Safe (atMay)

-- | Prefix an error code with a short text tag.
-- Typically, this should be defined like this:
--   errorCodePrefix = const "ABCD"
--
-- Make sure that error code prefixes are unique per error type.
class ErrorCodePrefix a where
  errorCodePrefix :: Text

-- | Types which are used to describe errors as short error codes in scripts.
-- Laws:
--   1. fromErrorCode . toErrorCode = Just
--   2. (toErrorCode <$> fromErrorCode x) == (const x <$> fromErrorCode x)
class (ErrorCodePrefix a, Eq a, Universe a) => ErrorCode a where
  -- | Get the short error code used in a script for given error type.
  toErrorCode :: a -> Text

  -- | Get the error type from an error code,
  -- assuming that the error code produced from that error type.
  fromErrorCode :: Text -> Maybe a

-- | Sequentially ordered types have sequentially ordered error codes.
-- Assuming that Universe implementation is correct, this instance should
-- satisfy the ErrorCode laws.
instance (Universe a, Eq a, ErrorCodePrefix a) => ErrorCode a where
  toErrorCode x = prefix <> numericCode
    where
      -- fromJust should not result in an error here if Universe is correct.
      numericCode = Text.pack $ show $ fromJust $ elemIndex x universe
      prefix = errorCodePrefix @a

  fromErrorCode x = atMay universe =<< numericCode
    where
      numericCode = read . Text.unpack <$> Text.stripPrefix prefix x
      prefix = errorCodePrefix @a

-- | Get the string literal from given error 'e'.
-- Use this with template haskell splices, e.g. $(errCode MyError)
errCode :: ErrorCode e => e -> Q Exp
errCode e = pure (LitE (StringL (Text.unpack (toErrorCode e))))

passertMaybe :: Text -> Term s (PMaybe a) -> Term s a
passertMaybe err mval = passertPJust # pconstant err # mval

passertMaybeData :: PIsData a => Text -> Term s (PMaybeData a) -> Term s a
passertMaybeData err mval = passertPDJust # pconstant err # mval
