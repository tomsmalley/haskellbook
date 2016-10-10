import Control.Applicative
import Text.Trifecta
import Data.List (intercalate, sort)

data NumberOrString = NOSS String
                    | NOSI Integer
                    deriving Eq
instance Show NumberOrString where
    show (NOSS s) = s
    show (NOSI i) = show i
instance Ord NumberOrString where
    NOSS s `compare` NOSS s' = compare s s'
    NOSI i `compare` NOSI i' = compare i i'
    NOSI _ `compare` NOSS _ = LT
    NOSS _ `compare` NOSI _ = GT

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata deriving Eq
instance Show SemVer where
    show (SemVer a b c p m) = (intercalate "." $ map show [a,b,c]) ++ build ++ metadata
      where build = if null p then "" else "-" ++ (intercalate "." $ map show p)
            metadata = if null m then "" else "+" ++ (intercalate "." $ map show m)
instance Ord SemVer where
    SemVer a b c p _ `compare` SemVer a' b' c' p' _ =
        case compare (a, b, c) (a', b', c') of
            EQ -> preCompare
            _  -> compare (a, b, c) (a', b', c')
        where preCompare | null p && null p' = EQ
                         | null p = GT
                         | null p' = LT
                         | otherwise = compare p p'

parseSemVer :: Parser SemVer
parseSemVer = undefined

parseNos :: Parser NumberOrString
parseNos = do
    --skipMany (oneOf "\n")
    v <- (NOSI <$> integer) <|> (NOSS <$> some letter)
    --skipMany (oneOf "\n")
    return v

main = do
    print "Show testing"
    let testmeta = [ (SemVer 1 0 0 [NOSS "alpha"] [NOSS "001"])
                   , (SemVer 1 0 0 [] [NOSI 20130313144700])
                   , (SemVer 1 0 0 [NOSS "beta"] [NOSS "exp", NOSS "sha", NOSS "5114f85"])
                   ]
    print testmeta

    print "Ord testing - book example"
    print $ SemVer 2 1 1 [] [] > SemVer 2 1 0 [] []
    print "Ord testing - semver.org example"
    let testorder = [ (SemVer 1 0 0 [NOSS "alpha"] [])
                    , (SemVer 1 0 0 [NOSS "alpha", NOSI 1] [])
                    , (SemVer 1 0 0 [NOSS "alpha", NOSS "beta"] [])
                    , (SemVer 1 0 0 [NOSS "beta"] [])
                    , (SemVer 1 0 0 [NOSS "beta", NOSI 2] [])
                    , (SemVer 1 0 0 [NOSS "beta", NOSI 11] [])
                    , (SemVer 1 0 0 [NOSS "rc", NOSI 1] [])
                    , (SemVer 1 0 0 [] [])
                    , (SemVer 1 0 1 [] [])
                    , (SemVer 1 1 1 [] [])
                    , (SemVer 2 0 0 [] [])
                    ]
    print testorder
    print . sort $ reverse testorder
    print $ testorder == sort testorder

    print "Parse testing"
    print $ parseString parseSemVer mempty "2.1.1"
    print $ parseString parseSemVer mempty "1.0.0-x.7.z.92"
