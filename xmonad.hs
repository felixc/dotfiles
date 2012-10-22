import System.Exit

import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.Replace
import XMonad.Util.Run

import Data.Monoid(All(..))

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

myTerminal      = "x-terminal-emulator"
myBorderWidth   = 1
myModMask       = mod4Mask
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]
myNormalBorderColor  = "#606060"
myFocusedBorderColor = "#337711"

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launch a terminal
    [ ((modMask,               xK_Return), spawn $ XMonad.terminal conf)
    -- run program launcher
    , ((modMask,               xK_p     ), shellPrompt defaultXPConfig)
    -- close focused window
    , ((modMask .|. shiftMask, xK_c     ), kill1)
     -- Rotate through the available layout algorithms
    , ((modMask,               xK_space ), sendMessage NextLayout)
    -- Move focus to the next window
    , ((modMask,               xK_Tab   ), windows W.focusDown)
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
    , ((modMask              , xK_q     ),
       broadcastMessage ReleaseResources >> restart "$HOME/.cabal/bin/xmonad" True)
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

myLayout = smartBorders (ResizableTall 1 (3/100) (1/2) [] ||| Full ||| Mirror (ResizableTall 1 (3/100) (1/2) []))

myManageHook = composeAll
    [ title =? "Contact List"       --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , className =? "Iceweasel"      --> doShift "2"
    , className =? "Emacs"          --> doShift "3"
    , isFullscreen                  --> doFullFloat
    , manageDocks
    ]

myDzenPP = defaultPP
  { ppCurrent = wrap "^fg(#aecf96)[^fg(white)" "^fg(#aecf96)]^fg()" . pad
  , ppHidden = pad
  , ppVisible = pad
  , ppUrgent = wrap "^fg(#ff0000)<" ">^fg()"
  , ppSep = " ^r(4x4) "
  , ppWsSep = " "
  , ppTitle = wrap "^fg(#aecf96)[^fg(white)" "^fg(#aecf96)]^fg()" . pad
  , ppLayout = (\x -> "")
  }

main = do
  replace
  h <- spawnPipe "processWindowTitle.sh | dzen2 -ta l -p -e 'onstart=lower'"
  xmonad $ withUrgencyHook NoUrgencyHook $ ewmh defaultConfig {
    terminal           = myTerminal,
    focusFollowsMouse  = True,
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,
    keys               = myKeys,
    mouseBindings      = myMouseBindings,
    layoutHook         = avoidStruts $ myLayout,
    logHook            = dynamicLogWithPP myDzenPP { ppOutput = hPutStrLn h },
    manageHook         = myManageHook
  }
