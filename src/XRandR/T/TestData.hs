{-# LANGUAGE UnicodeSyntax #-}
module XRandR.T.TestData
  ( dog0T
  , dog1T
  , red0
  , red0T
  ) where

import Base1T

-- base --------------------------------

import Data.Ratio ( (%) )

-- text --------------------------------

import Data.Text ( unlines )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import XRandR.Types ( Connected(Connected, Disconnected), Device(Device),
                      Dimensions(Dimensions), Frequency(Frequency),
                      IsCurrent(IsCurrent), IsPreferred(IsPreferred),
                      ModeLine(ModeLine), RelativePosition(Primary),
                      Resolution(Resolution), Screen(Screen),
                      ScreenSize(ScreenSize), XPosition(XPosition),
                      XRandR(XRandR) )

--------------------------------------------------------------------------------

{-| sample xrandr output -}
red0T ∷ 𝕋
red0T = unlines
  [ "Screen 0: minimum 8 x 8, current 1920 x 1200, maximum 32767 x 32767"
  , "eDP1 connected primary 1920x1200+0+0 (normal left inverted right x axis y"
    ◇ " axis) 290mm x 180mm"
  , "   1920x1200     59.95*+"
  , "DP1 disconnected (normal left inverted right x axis y axis)"
  , "DP2 disconnected (normal left inverted right x axis y axis)"
  , "DP3 disconnected (normal left inverted right x axis y axis)"
  , "DP4 disconnected (normal left inverted right x axis y axis)"
  , "DP5 disconnected (normal left inverted right x axis y axis)"
  , "HDMI1 disconnected (normal left inverted right x axis y axis)"
  , "VIRTUAL1 disconnected (normal left inverted right x axis y axis)"
  ]

--------------------

red0 ∷ XRandR
red0 =
  let
      ss_1920        = ScreenSize 1920 1200
      res_1920       = Resolution 1920 1200
      xpos_1920_0_0  = XPosition ss_1920 0 0
      freq_59_95_cp  = Frequency (1199%20) IsCurrent IsPreferred
      modeline_1920  = ModeLine res_1920 (pure freq_59_95_cp)
      dim_290_180    = Dimensions 290 180
      eDP1           = Device "eDP1" Connected (𝕵 Primary) (𝕵 xpos_1920_0_0)
                              (𝕵 dim_290_180) [modeline_1920]
      empty_dev name = Device name Disconnected 𝕹 𝕹 𝕹 []
  in  XRandR [ Screen 0 (ScreenSize 8 8) ss_1920 (ScreenSize 32767 32767)
               [ eDP1
               , empty_dev "DP1"
               , empty_dev "DP2"
               , empty_dev "DP3"
               , empty_dev "DP4"
               , empty_dev "DP5"
               , empty_dev "HDMI1"
               , empty_dev "VIRTUAL1"
               ] ]

----------------------------------------

dog0T ∷ 𝕋
dog0T = unlines
  [ "Screen 0: minimum 8 x 8, current 2520 x 2560, maximum 16384 x 16384"
  , "HDMI-0 connected 1440x2560+1080+0 right (normal left inverted right x axis"
    ◇ "y axis) 553mm x 311mm"
  , "   2560x1440     59.95*+"
  , "   1920x1080     60.00    59.94    50.00    60.00    50.04"
  , "   1680x1050     59.95"
  , "   1440x900      59.89"
  , "   1280x1440     59.91"
  , "   1280x1024     75.02    60.02"
  , "   1280x960      60.00"
  , "   1280x720      60.00    59.94    50.00"
  , "   1024x768      75.03    70.07    60.00"
  , "   800x600       75.00    72.19    60.32    56.25"
  , "   720x576       50.00"
  , "   720x480       59.94"
  , "   640x480       75.00    72.81    59.94    59.93"
  , "HDMI-1 connected (normal left inverted right x axis y axis)"
  , "   3840x2160     60.00 +  59.94    50.00    30.00    29.97    25.00    "
    ◇ "23.98"
  , "   2560x1440     59.95"
  , "   2048x1280     59.92"
  , "   2048x1152     60.00"
  , "   1920x1200     59.88"
  , "   1920x1080     60.00    59.94    50.00    60.00    50.04"
  , "   1680x1050     59.95"
  , "   1600x1200     60.00"
  , "   1280x1024     75.02    60.02"
  , "   1280x800      59.81"
  , "   1280x720      59.94    50.00"
  , "   1152x864      75.00"
  , "   1024x768      75.03    60.00"
  , "   800x600       75.00    60.32"
  , "   720x576       50.00"
  , "   720x480       59.94"
  , "   640x480       75.00    59.94    59.93"
  , "HDMI-2 connected primary 1080x1920+0+0 left (normal left inverted right x "
    ◇ "axis y axis) 509mm x 286mm"
  , "   1920x1080     60.00*+  59.94    50.00    60.00    50.04"
  , "   1680x1050     59.95"
  , "   1440x900      59.89"
  , "   1440x576      50.00"
  , "   1440x480      59.94"
  , "   1280x1024     75.02    60.02"
  , "   1280x960      60.00"
  , "   1280x720      60.00    59.94    50.00"
  , "   1152x864      75.00"
  , "   1024x768      75.03    70.07    60.00"
  , "   800x600       75.00    72.19    60.32    56.25"
  , "   720x576       50.00"
  , "   720x480       59.94"
  , "   640x480       75.00    72.81    59.94    59.93"
  , "HDMI-3 disconnected (normal left inverted right x axis y axis)"
  ]

dog1T ∷ 𝕋
dog1T = unlines
  [ "Screen 0: minimum 8 x 8, current 5080 x 2880, maximum 16384 x 16384"
  , "HDMI-0 connected 1440x2560+3640+0 right (normal left inverted right x "
    ◇ "axis y axis) 553mm x 311mm"
  , "   2560x1440     59.95*+"
  , "   1920x1080     60.00    59.94    50.00    60.00    50.04"
  , "   1680x1050     59.95"
  , "   1440x900      59.89"
  , "   1280x1440     59.91"
  , "   1280x1024     75.02    60.02"
  , "   1280x960      60.00"
  , "   1280x720      60.00    59.94    50.00"
  , "   1024x768      75.03    70.07    60.00"
  , "   800x600       75.00    72.19    60.32    56.25"
  , "   720x576       50.00"
  , "   720x480       59.94"
  , "   640x480       75.00    72.81    59.94    59.93"
  , "HDMI-1 connected 2560x1440+1080+0 inverted (normal left inverted right x "
    ◇ "axis y axis) 597mm x 336mm"
  , "   3840x2160     60.00 +  59.94    50.00    30.00    29.97    25.00    "
    ◇ "23.98"
  , "   2560x1440     59.95*"
  , "   2048x1280     59.92"
  , "   2048x1152     60.00"
  , "   1920x1200     59.88"
  , "   1920x1080     60.00    59.94    50.00    60.00    50.04"
  , "   1680x1050     59.95"
  , "   1600x1200     60.00"
  , "   1280x1024     75.02    60.02"
  , "   1280x800      59.81"
  , "   1280x720      59.94    50.00"
  , "   1152x864      75.00"
  , "   1024x768      75.03    60.00"
  , "   800x600       75.00    60.32"
  , "   720x576       50.00"
  , "   720x480       59.94"
  , "   640x480       75.00    59.94    59.93"
  , "HDMI-2 connected 1080x1920+0+0 left (normal left inverted right x axis y "
    ◇ "axis) 509mm x 286mm"
  , "   1920x1080     60.00*+  59.94    50.00    60.00    50.04"
  , "   1680x1050     59.95"
  , "   1440x900      59.89"
  , "   1440x576      50.00"
  , "   1440x480      59.94"
  , "   1280x1024     75.02    60.02"
  , "   1280x960      60.00"
  , "   1280x720      60.00    59.94    50.00"
  , "   1152x864      75.00"
  , "   1024x768      75.03    70.07    60.00"
  , "   800x600       75.00    72.19    60.32    56.25"
  , "   720x576       50.00"
  , "   720x480       59.94"
  , "   640x480       75.00    72.81    59.94    59.93"
  , "HDMI-3 connected primary 2560x1440+1080+1440 (normal left inverted right "
    ◇ "x axis y axis) 597mm x 336mm"
  , "   3840x2160     60.00 +  59.94    50.00    29.97    25.00    23.98"
  , "   2560x1440     59.95*"
  , "   2048x1280     60.20"
  , "   2048x1152     60.00"
  , "   2048x1080     24.00"
  , "   1920x1200     59.88"
  , "   1920x1080     60.00    59.94    50.00    23.98    60.00    50.04"
  , "   1680x1050     59.95"
  , "   1600x1200     60.00"
  , "   1280x1024     75.02    60.02"
  , "   1280x800      59.81"
  , "   1280x720      59.94    50.00"
  , "   1152x864      75.00"
  , "   1024x768      75.03    60.00"
  , "   800x600       75.00    60.32"
  , "   720x576       50.00"
  , "   720x480       59.94"
  , "   640x480       75.00    59.94    59.93"
  ]


-- that's all, folks! ----------------------------------------------------------
