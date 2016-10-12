module Weekday where

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday

f :: Weekday -> String
f Friday = "Miller Time"
