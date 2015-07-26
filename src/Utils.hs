module Utils
    ( fromBool
    ) where

fromBool :: Bool -> a -> Maybe a
fromBool True = Just
fromBool _ = const Nothing
