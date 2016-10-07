data Product a b = Product a b deriving (Eq, Show)

data Sum a b = First a
             | Second b
             deriving (Eq, Show)

data RecordProduct a b = RecordProduct { pFirst :: a
                                       , pSecond :: b
                                       } deriving (Eq, Show)

type Awesome = Bool
type Name = String

person :: Product Name Awesome
person = Product "Simon" True

{-
data Twitter = Twitter deriving (Eq, Show)
data AskFm = AskFm deriving (Eq, Show)

socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter
-}

data SocialNetwork = Twitter
                   | AskFm
                   deriving (Eq, Show)

myRecord :: RecordProduct Integer Float
myRecord = RecordProduct { pFirst = 42
                         , pSecond = 0.00001
                         }

data OperatingSystem = GnuPlusLinux
                     | OpenBsdPlusNevermindJustBsdStill
                     | Mac
                     | Windows
                     deriving (Eq, Show)

data ProgrammingLanguage = Haskell
                         | Agda
                         | Idris
                         | PureScript
                         deriving (Eq, Show)

data Programmer = Programmer { os :: OperatingSystem
                             , lang :: ProgrammingLanguage
                             } deriving (Eq, Show)

nineToFive :: Programmer
nineToFive = Programmer { os = Mac
                        , lang = Haskell
                        }

feelingWizardly :: Programmer
feelingWizardly = Programmer { lang = Agda
                             , os = GnuPlusLinux
                             }

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [ GnuPlusLinux
                      , OpenBsdPlusNevermindJustBsdStill
                      , Mac
                      , Windows
                      ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer {os = x, lang = y} | x <- allOperatingSystems, y <- allLanguages]
