module XRandR.Paths where

import FPath.AbsFile  ( AbsFile, absfile )

xrandr :: AbsFile
xrandr = [absfile|__xrandr__/bin/xrandr|]
