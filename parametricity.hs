module Parametricity where

triple :: a -> a -> a
triple x _  = x

triple' :: a -> a -> a
triple' _ x = x

tripdiff :: a -> b -> b
tripdiff _ y = y
