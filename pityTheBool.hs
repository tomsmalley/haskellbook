import Data.Int

data BigSmall = Big Bool
              | Small Bool
              deriving (Eq, Show)

-- Big Bool + Small Bool
--  1 * 2   +   1  * 2
--  2 + 2
--  4

data NumberOrBool = Numba Int8
                  | BoolyBool Bool
                  deriving (Eq, Show)

myNumba = Numba (-128)

-- Numba Int8 + BoolyBool Bool
--   1  * 256 +     1    * 2
--   256 + 2
--   258
