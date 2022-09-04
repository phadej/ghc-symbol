{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE Trustworthy           #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | A (slightly unsafe) term level implementation for 'Symbol'.
--
-- As 'Symbol' in @base@ doesn't have any term-level operations,
-- it's sometimes necessary to have two copies of the same data,
-- the first one using 'String' on term level and
-- second one using 'Symbol' to be promtoed to type level.
--
-- In GHC-9.2 the similar problem was fixed for @Nat@ and @Natural@,
-- which were distinct types (kinds) before that.
--
-- As 'String' is a list of 'Char's, we cannot make @type 'Symbol' = 'String'@,
-- but we could do @newtype 'Symbol' = MkSymbol 'String'@.
-- This module /fakes/ that by using 'unsafeCoerce' under the hood.
--
-- Fleshing out 'Symbol' on term level is suggested in
-- https://gitlab.haskell.org/ghc/ghc/-/issues/10776#note_109601
-- in 2015.
--
-- This implementation is slightly unsafe, as currently 'Symbol' is defined
-- as empty data type:
--
-- @
-- data 'Symbol'
-- @
--
-- This means that you can write
--
-- @
-- dangerous :: Symbol -> Int
-- dangerous x = case x of
-- @
--
-- and because GHC sees through everything, and knows that 'Symbol' is empty,
-- the above compiles without a warning.
--
-- If 'Symbol' was defined as @newtype Symbol = Symbol Any@, the
-- above problem would go away, and also implementation of this
-- module would be safer, as 'unsafeCoerce'ing from lifted type to 'Any'
-- and back is guaranteed to work.
--
-- Of course life would be easier if we just had
--
-- @
-- newtype 'Symbol' = MkSymbol 'String'
-- @
--
-- but until that is done, you may find this module useful.
--
-- /Note:/ 'Symbol' is not @Text@. @Text@ has an invariant: it represents /valid/ Unicode text.
-- 'Symbol' is just a list of characters (= Unicode codepoints), like 'String'.
-- E.g.
--
-- >>> "\55555" :: String
-- "\55555"
--
-- >>> "\55555" :: Symbol
-- "\55555"
--
-- but @text@ replaces surrogate codepoints:
--
-- >>> "\55555" :: Text
-- "\65533"
--
-- 'Symbol' could use some packed representation of list of characters,
-- if also 'KnownSymbol' would use it as well. Currently
-- 'KnownSymbol' dictionary carries a 'String', so having
-- 'Symbol' be a 'String' is justified'.
--
module GHC.Symbol (
    -- * Symbol type
    Symbol,
    symbolToString,
    consSymbol,
    unconsSymbol,
    -- * Type level
    KnownSymbol,
    symbolVal,
    symbolVal',
    AppendSymbol,
    CmpSymbol,
    someSymbolVal,
    SomeSymbol (..),
    sameSymbol,
) where

import Control.DeepSeq            (NFData (..))
import Data.Binary                (Binary (..))
import Data.String                (IsString (..))
import GHC.Exts                   (IsList (..), Proxy#)
import GHC.TypeLits
       (AppendSymbol, CmpSymbol, KnownSymbol, SomeSymbol, Symbol, sameSymbol)
import Language.Haskell.TH.Syntax (Lift (..))
import Text.Printf                (PrintfArg (..))
import Unsafe.Coerce              (unsafeCoerce)

import qualified GHC.TypeLits as GHC

-- $setup
-- >>> :set -XOverloadedStrings -XTypeApplications -XDataKinds
-- >>> import Data.Text (Text)
-- >>> import Data.Proxy (Proxy (..))

-------------------------------------------------------------------------------
-- Conversions
-------------------------------------------------------------------------------

symbolToString :: Symbol -> String
symbolToString = unsafeCoerce @Symbol @String

symbolFromString :: String -> Symbol
symbolFromString = unsafeCoerce @String @Symbol

-------------------------------------------------------------------------------
-- Public interface
-------------------------------------------------------------------------------

-- this is not exported though, use mempty or "".
emptySymbol :: Symbol
emptySymbol = symbolFromString ""

-- | Prepend a character to a 'Symbol'
--
-- >>> consSymbol 'a' "cute"
-- "acute"
--
consSymbol :: Char -> Symbol -> Symbol
consSymbol c s = symbolFromString (c : symbolToString s)

-- | Inverse of 'consSymbol'
--
-- >>> unconsSymbol ""
-- Nothing
--
-- >>> unconsSymbol "mother"
-- Just ('m',"other")
--
unconsSymbol :: Symbol -> Maybe (Char, Symbol)
unconsSymbol s = case symbolToString s of
    []   -> Nothing
    c:s' -> Just (c, symbolFromString s')

-- instances

-- |
--
-- >>> "foo" :: Symbol
-- "foo"
--
instance Show Symbol where
    showsPrec d s = showsPrec d (symbolToString s)

instance Read Symbol where
    readsPrec d s = [ (symbolFromString x, s') | ~(x, s') <- readsPrec d s ]

instance Eq Symbol where
    x == y = symbolToString x == symbolToString y

instance Ord Symbol where
    compare x y = compare (symbolToString x) (symbolToString y)

-- |
--
-- >>> "foo" :: Symbol
-- "foo"
--
instance IsString Symbol where
    fromString = symbolFromString

instance Semigroup Symbol where
     x <> y = symbolFromString (symbolToString x ++ symbolToString y)

instance Monoid Symbol where
    mempty = emptySymbol
    mappend = (<>)

instance NFData Symbol where
    rnf s = rnf (symbolToString s)

instance Binary Symbol where
    get = fmap symbolFromString get
    put s = put (symbolToString s)

instance Lift Symbol where
    liftTyped s = [|| symbolFromString s' ||] where s' = symbolToString s

instance PrintfArg Symbol where
    formatArg s = formatArg (symbolToString s)

instance IsList Symbol where
    type Item Symbol = Char
    fromList = symbolFromString
    toList   = symbolToString

-------------------------------------------------------------------------------
-- TypeLits
-------------------------------------------------------------------------------

-- |
--
-- >>> symbolVal (Proxy @"foobar")
-- "foobar"
--
symbolVal :: forall n proxy. KnownSymbol n => proxy n -> Symbol
symbolVal p = symbolFromString (GHC.symbolVal p)

symbolVal' :: forall n. KnownSymbol n => Proxy# n -> Symbol
symbolVal' p = symbolFromString (GHC.symbolVal' p)

-- |
--
-- >>> someSymbolVal "foobar"
-- "foobar"
--
someSymbolVal :: Symbol -> SomeSymbol
someSymbolVal s = GHC.someSymbolVal (symbolToString s)
