{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}

module Data.Pair where

pattern (:-) :: a -> b -> (a, b)
pattern a :- b = (a, b)

type a :- b = (a, b)
