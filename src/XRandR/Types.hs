{-# LANGUAGE UnicodeSyntax #-}
module XRandR.Types
  ( Connected(Connected, Disconnected)
  , Device(Device)
  , Dimensions(Dimensions)
  , Frequency(Frequency)
  , IsCurrent(IsCurrent)
  , IsPreferred(IsPreferred)
  , ModeLine(ModeLine)
  , RelativePosition(Primary)
  , Resolution(Resolution)
  , Screen(Screen)
  , ScreenSize(ScreenSize)
  , XPosition(XPosition)
  , XRandR(XRandR)
--  , tests
  ) where

import Base1T

import Prelude ( (*) )

-- base --------------------------------

import Control.Applicative ( optional )
import Control.Monad.Fail  ( fail )
import Data.Char           ( isAlphaNum )
import Data.Ratio          ( (%) )
import Text.Read           ( read )

-- base-unicode-symbols ----------------

import Prelude.Unicode ( ℚ )

-- charset -----------------------------

import Data.CharSet ( build, singleton, union )

-- parsers -----------------------------

import Text.Parser.Char        ( anyChar, char, digit, newline, oneOfSet, space,
                                 string )
import Text.Parser.Combinators ( Parsing, try, (<?>) )

-- text --------------------------------

import Data.Text ( intercalate )

-- text-printer ------------------------

import Text.Printer qualified as P

-- textual-plus ------------------------

import TextualPlus ( TextualPlus(textual') )

--------------------------------------------------------------------------------

(⩻) ∷ Parsing η ⇒ η α → 𝕊 → η α
(⩻) = (<?>)

data Connected = Connected | Disconnected deriving (Eq, Show)

instance TextualPlus Connected where
  textual' = string "connected" ⋫ pure Connected
           ∤ string "disconnected" ⋫ pure Disconnected
          ⩻ "Connected"

------------------------------------------------------------

data IsCurrent = IsCurrent | IsNotCurrent deriving (Eq, Show)

instance Printable IsCurrent where
  print IsCurrent = P.char '*'

instance TextualPlus IsCurrent where
  textual' = char '*' ⋫ pure IsCurrent
           ∤ char ' ' ⋫ pure IsNotCurrent
           ⩻ "IsCurrent"

------------------------------------------------------------

data IsPreferred = IsPreferred | IsNotPreferred deriving (Eq, Show)

instance TextualPlus IsPreferred where
  textual' = char '+' ⋫ pure IsPreferred
           ∤ char ' ' ⋫ pure IsNotPreferred
           ⩻ "IsPreferred"

instance Printable IsPreferred where
  print IsPreferred = P.char '+'

------------------------------------------------------------

data RelativePosition = Primary deriving (Eq, Show)

instance TextualPlus RelativePosition where
  textual' = string "primary" ⋫ pure Primary ⩻ "RelativePosition"

------------------------------------------------------------

data XPosition = XPosition { _size :: ScreenSize
                           , _x    :: ℕ
                           , _y    :: ℕ
                           }
  deriving (Eq, Show)

instance TextualPlus XPosition where
  textual' = let natural = read ⊳ many digit
             in  XPosition ⊳ (ScreenSize ⊳ (natural ⋪ char 'x') ⊵ natural)
                           ⊵ (char '+' ⋫ natural) ⊵ (char '+' ⋫ natural)
                          ⩻ "XPosition"

------------------------------------------------------------

data Dimensions = Dimensions { w :: ℕ
                               -- ^ in mm
                             , h :: ℕ
                               -- ^ in mm
                             }
  deriving (Eq, Show)

instance TextualPlus Dimensions where
  textual' = let natural = read ⊳ many digit
             in  Dimensions ⊳ (natural ⋪ string "mm x ")
                            ⊵ (natural ⋪ string "mm")
                           ⩻ "Dimensions"

------------------------------------------------------------

data ScreenSize = ScreenSize { _width  :: ℕ
                             , _height :: ℕ
                             }
  deriving (Eq, Show)

instance Printable ScreenSize where
  print (ScreenSize w h) = P.text $ [fmt|%dx%d|] w h

instance TextualPlus ScreenSize where
  textual' = let natural = read ⊳ many digit
             in  ScreenSize ⊳ (natural ⋪ string " x ") ⊵ natural
                           ⩻ "ScreenSize"

------------------------------------------------------------

data Resolution = Resolution { _resolution_x :: ℕ
                             , _resolution_y :: ℕ
                             }
  deriving (Eq, Show)

instance TextualPlus Resolution where
  textual' = let natural = read ⊳ many digit
             in  Resolution ⊳ (natural ⋪ string "x") ⊵ natural
                           ⩻ "Resolution"

------------------------------------------------------------

data Frequency = Frequency { _rate        :: ℚ
                           , _isCurrent   :: IsCurrent
                           , _isPreferred :: IsPreferred
                           }
  deriving (Eq, Show)

instance Printable Frequency where
  print (Frequency q c p) = P.text $ [fmt|%5.2f%T%T|] q c p

mkFreq int dec = Frequency $ ((read int*100) + read dec) % 100

instance TextualPlus Frequency where
  textual' = mkFreq ⊳ some digit ⊵ (char '.' ⋫ some digit)
                    ⊵ textual'
                    ⊵ textual'
                   ⩻ "Frequency"

------------------------------------------------------------

data ModeLine = ModeLine { _resolution  :: Resolution
                         , _frequencies :: NonEmpty Frequency
                         }
  deriving (Eq, Show)

instance TextualPlus ModeLine where
  textual' = ModeLine ⊳ (some space ⋫ textual')
                      ⊵ fromList ⊳ (some (some (char ' ') ⋫ textual'))
                      ⋪ newline
                     ⩻ "ModeLine"

------------------------------------------------------------

data Device = Device { -- _connected :: Connected
                       _device_name :: 𝕊
                     , _connected   :: Connected
                     , _position    :: 𝕄 RelativePosition
                     , _xposition   :: 𝕄 XPosition
                     , _dimensions  :: 𝕄 Dimensions
                     , _modelines   :: [ModeLine]
                     }
  deriving (Eq, Show)

mkDevice ∷ 𝕊 → Connected
         → (𝔼 (𝕄 RelativePosition, 𝕄 XPosition)
              (𝕄 XPosition, 𝕄 RelativePosition))
         → 𝕄 Dimensions → [ModeLine] → Device
mkDevice dn c (𝕷 (rp,xp)) d m = Device dn c rp xp d m
mkDevice dn c (𝕽 (xp,rp)) d m = Device dn c rp xp d m

instance Printable Device where
  print d = P.string $ show d

instance TextualPlus Device where
  textual' =
    let (∪) = union
        alphaNumHyphen = oneOfSet $ build isAlphaNum ∪ singleton '-'
    in mkDevice ⊳ (some alphaNumHyphen ⋪ char ' ') -- device_name
                ⊵ (textual' ⋪ char ' ')            -- connected
                ⊵ (𝕷 ⊳ optional (textual' ⋪ char ' ')   -- relative position
                      ⊵ optional (textual' ⋪ char ' '))   -- xposition
                ⋪ string "(normal left inverted right x axis y axis)"
                ⊵ optional (char ' ' ⋫ textual') ⋪ newline
                ⊵ many textual'
                ⩻ "Device"

------------------------------------------------------------

data Screen = Screen { _screenid :: ℕ
                     , _minimum  :: ScreenSize
                     , _current  :: ScreenSize
                     , _maximum  :: ScreenSize
                     , _devices  :: [Device]
                     }
  deriving (Eq, Show)

instance Printable Screen where
  print s = P.text $ [fmt|Screen %d (%T < %T < %T)\n%t|]
                     (_screenid s) (_minimum s) (_current s) (_maximum s)
                     (intercalate "\n" $ ("  " ◇) ⊳ toText ⊳ _devices s)

instance TextualPlus Screen where
  textual' =
    let natural = read ⊳ many digit
    in  Screen ⊳ (string "Screen " ⋫ natural)
               ⊵ (string ": minimum " ⋫ textual')
               ⊵ (string ", current " ⋫ textual')
               ⊵ (string ", maximum " ⋫ textual')
               ⋪ newline
               ⊵ some textual'
              ⩻ "Screen"

------------------------------------------------------------

newtype XRandR = XRandR [Screen]
  deriving (Eq, Show)

instance Printable XRandR where
  print (XRandR ss) = P.text $ intercalate "\n" (toText ⊳ ss)

instance TextualPlus XRandR where
  textual' = XRandR ⊳ some textual'

-- testing ---------------------------------------------------------------------

{-
tests ∷ TestTree
tests = testGroup "XRandR" [ testParse TestData.red0T TestData.red0
                           ]

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests
-}

-- that's all, folks! ----------------------------------------------------------
