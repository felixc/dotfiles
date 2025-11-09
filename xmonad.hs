{-# OPTIONS_GHC -Wno-deprecations #-}

import System.Exit

import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.Replace
import XMonad.Util.Run

import Data.Monoid(All(..))

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

myModMask       = mod4Mask
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- run program launcher
    [ ((modMask,               xK_o     ), shellPrompt myLauncherConfig)
    -- close focused window
    , ((modMask .|. shiftMask, xK_c     ), kill1)
     -- Rotate through the available layout algorithms
    , ((modMask,               xK_space ), sendMessage NextLayout)
    -- Move focus to the next window
    , ((modMask,               xK_Tab   ), windows W.focusDown)
    -- Move focus to the previous window
    , ((modMask .|. shiftMask, xK_Tab   ), windows W.focusUp)
    -- Move focus to the next window
    , ((modMask,               xK_j     ), windows W.focusDown)
    -- Move focus to the previous window
    , ((modMask,               xK_k     ), windows W.focusUp  )
    -- Move focus to the master window
    , ((modMask,               xK_m     ), windows W.focusMaster  )
    -- Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
    -- Swap the focused window with the previous window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )
    -- Shrink the master area
    , ((modMask,               xK_h     ), sendMessage Shrink)
    -- Expand the master area
    , ((modMask,               xK_l     ), sendMessage Expand)
    -- Shrink a window vertically
    , ((modMask,               xK_z     ), sendMessage MirrorShrink)
    -- Expand a window vertically
    , ((modMask,               xK_a     ), sendMessage MirrorExpand)
    -- Push window back into tiling
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)
    -- Increment the number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
    -- Deincrement the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))
    -- Quit xmonad
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    -- Restart xmonad
    , ((modMask              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ] ++
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    -- mod-control-shift-[1..9], Copy client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask), (copy, shiftMask .|. controlMask)]]

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
    ]

myLayout = smartSpacing 4 (ResizableTall 1 (3/100) (3/5) []) ||| smartSpacing 4 (Mirror (ResizableTall 1 (3/100) (1/2) [])) ||| simpleTabbed

myManageHook = composeAll
    [ className =? "firefox-beta"   --> doShift "2"
    , className =? "discord"        --> doShift "1"
    , className =? "steam"          --> doShift "9"
    , className =? "fontforge"      --> doFloat
    , appName   =? "desktop_window" --> doIgnore
    , isFullscreen                  --> doFullFloat
    ]

myLauncherConfig = def
  { font = "xft:Inconsolata:size=12"
  , height = 38
  }

main = do
  replace
  xmonad $ docks $ withUrgencyHook NoUrgencyHook $ ewmh def {
    focusFollowsMouse  = True,
    borderWidth        = 0,
    modMask            = myModMask,
    workspaces         = myWorkspaces,
    keys               = myKeys,
    mouseBindings      = myMouseBindings,
    layoutHook         = avoidStruts $ myLayout,
    handleEventHook    = handleEventHook def <+> fullscreenEventHook,
    manageHook         = myManageHook
  }
