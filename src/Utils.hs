module Utils
    ( fromBool
    , third
    ) where

fromBool :: Bool -> a -> Maybe a
fromBool True = Just
fromBool _ = const Nothing

third :: (a, b, c) -> c
third (_, _, x) = x
