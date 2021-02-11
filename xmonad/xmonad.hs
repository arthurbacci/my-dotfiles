----------------------------------
-- Arthur Bacci's XMonad Config --
----------------------------------

--------
-- Imports
----

-- Default
import XMonad hiding ((|||))
import Data.Monoid
import System.Exit
import XMonad.Util.Run
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
-- Layouts
import XMonad.Layout.ResizableTile        -- A alternative to Tall Layout
import XMonad.Layout.BinarySpacePartition -- Each window divides in two
import XMonad.Layout.Tabbed               -- Tabs on top
-- Layout modifiers
import XMonad.Layout.Spacing           -- Adds some space between the windows
import XMonad.Layout.NoBorders         -- Removes the borders
import XMonad.Layout.LayoutCombinators -- To combine the layouts and use JumpToLayout
import XMonad.Layout.Renamed           -- Rename the layouts
-- XMobar
import XMonad.Hooks.ManageDocks -- To the windows don't hide the bar
import XMonad.Hooks.DynamicLog  -- To transfer data to XMobar
-- Run prompt
import XMonad.Prompt
import XMonad.Prompt.Shell
-- Change volume
import XMonad.Actions.Volume -- To change the volume
import XMonad.Util.Dzen      -- To show the volume
-- More
-- import XMonad.Hooks.InsertPosition -- New windows will appear bellow and not above
                                   -- https://stackoverflow.com/questions/50666868/how-to-modify-order-of-tabbed-windows-in-xmonad?rq=1

import XMonad.Actions.Minimize
import XMonad.Layout.Minimize

import XMonad.Layout.TwoPane

import XMonad.Layout.Accordion

import XMonad.Actions.GridSelect

alert = dzenConfig centered . show . round
centered = 
        onCurr (center 200 100)
    >=> addArgs ["-fg", "#eceff4"]
    >=> addArgs ["-bg", "#282a36"]

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--

spawnSelectedName :: GSConfig String -> [(String, String)] -> X ()
spawnSelectedName conf lst = gridselect conf lst >>= flip whenJust spawn

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask  , xK_Return ), spawn "alacritty")

    -- launch emacs client
    , ((modm .|. controlMask, xK_Return ), spawn "emacsclient -c")

    {-
    , ((modm .|. shiftMask  , xK_a      ), runSelectedAction defaultGSConfig
        [ ("pcmanfm", spawn "pcmanfm")
        , ("brave",   spawn "brave"  )
        , ("ncmpcpp", spawn "alacritty -e fish -C /usr/bin/ncmpcpp")
        ]) -}

    , ((modm .|. shiftMask  , xK_a      ), spawnSelectedName defaultGSConfig
      [ ( "pcmanfm"  , "pcmanfm"                                          )
      , ( "brave"    , "brave"                                            )
      , ( "ncmpcpp"  , "alacritty -e fish -C /usr/bin/ncmpcpp"            )
      , ( "ted"      , "alacritty -e fish -C /home/arthur/.local/bin/ted" )
      , ( "nitrogen" , "nitrogen"                                         )
      ])

    -- https://superuser.com/questions/389737/how-do-you-make-volume-keys-and-mute-key-work-in-xmonad
    -- volume
    , ((modm                , xK_F6     ), lowerVolume 5 >> getVolume >>= alert)
    , ((modm                , xK_F8     ), raiseVolume 5 >> getVolume >>= alert)
    , ((modm .|. shiftMask  , xK_F6     ), lowerVolume 1 >> getVolume >>= alert)
    , ((modm .|. shiftMask  , xK_F8     ), raiseVolume 1 >> getVolume >>= alert)
    , ((0                   , 0x1008FF11), lowerVolume 5 >> getVolume >>= alert)
    , ((0                   , 0x1008FF13), raiseVolume 5 >> getVolume >>= alert)
    , ((shiftMask           , 0x1008FF11), lowerVolume 1 >> getVolume >>= alert)
    , ((shiftMask           , 0x1008FF13), raiseVolume 1 >> getVolume >>= alert)
 -- , ((0                   , 0x1008FF12), toggleMute >> return ())
    , ((modm                , xK_F7     ), toggleMute >> return ())
    
    
    -- launch dmenu
    , ((modm                , xK_p      ), spawn "dmenu_run")

    -- Take a screenshot
    , ((0                   , xK_Print  ), spawn "xfce4-screenshooter")

    -- close focused window
    , ((modm .|. shiftMask  , xK_c      ), kill)

     -- Rotate through the available layout algorithms
    , ((modm                , xK_space  ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask  , xK_space  ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    --, ((modm                , xK_n      ), refresh)

    , ((modm                , xK_n      ), withFocused minimizeWindow)
    , ((modm .|. shiftMask  , xK_n      ), withLastMinimized maximizeWindowAndFocus)

    , ((modm                , xK_g      ), goToSelected defaultGSConfig)
    
    -- Move focus to the next window
    , ((modm                , xK_j      ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm                , xK_k      ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm                , xK_m      ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm                , xK_Return ), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask  , xK_j      ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask  , xK_k      ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm                , xK_h      ), sendMessage Shrink)

    -- Expand the master area
    , ((modm                , xK_l      ), sendMessage Expand)

    -- Shink the non master area
    , ((modm .|. shiftMask  , xK_h      ), sendMessage MirrorShrink)

    -- Expand the non master area
    , ((modm .|. shiftMask  , xK_l      ), sendMessage MirrorExpand)

    -- Push window back into tiling
    , ((modm                , xK_t      ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm                , xK_comma  ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm                , xK_period ), sendMessage (IncMasterN (-1)))

    , ((modm                , xK_f      ), runSelectedAction defaultGSConfig $
        map (\x -> (x, sendMessage $ JumpToLayout x))
        ["Tiled", "MTiled", "BSP", "MBSP", "TwoPane", "MTwoPane", "Accordion", "MAccordion", "Mono", "Full"]
      )

    , ((modm .|. shiftMask  , xK_l      ), runSelectedAction defaultGSConfig $
        map (\x -> (show x, windows $ W.greedyView x)) (XMonad.workspaces conf))
    
    -- Quit xmonad
    , ((modm .|. shiftMask
             .|. controlMask, xK_q      ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm                , xK_q      ), spawn "killall xmobar; xmonad --restart")

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    , ((modm .|. shiftMask  , xK_slash  ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]
    ++

    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (drop 9 $ XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, controlMask), (W.shift, controlMask .|. shiftMask)]]
    ++

    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- [(xK_e, 0), (xK_w, 1)]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                        
                                       >> windows W.shiftMaster))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    , ((modm, button4), (\w -> focus w >> sendMessage Shrink))
    , ((modm, button5), (\w -> focus w >> sendMessage Expand))

    , ((modm .|. shiftMask, button4), (\w -> focus w >> sendMessage MirrorShrink))
    , ((modm .|. shiftMask, button5), (\w -> focus w >> sendMessage MirrorExpand)) ]



myLayout = tiled ||| mirrortiled ||| bsp ||| mirrorbsp ||| two ||| twoMirror ||| acc ||| accMirror ||| mono ||| fullscreen
  where
    general = minimize

    -- default tiling algorithm partitions the screen into two panes with more adjustment
    tiled_template = general $ ResizableTall nmaster delta ratio []
     
    tiled          = renamed [Replace "Tiled" ] $ avoidStruts $        tiled_template
    mirrortiled    = renamed [Replace "MTiled"] $ avoidStruts $ Mirror tiled_template

    -- Real fullscreen
    fullscreen     = renamed [Replace "Full"  ] $ general $ noBorders $ Full

    -- Selected window divides into two
    bsp_template   = general $ emptyBSP
     
    bsp            = renamed [Replace "BSP"   ] $ avoidStruts $        bsp_template
    mirrorbsp      = renamed [Replace "MBSP"  ] $ avoidStruts $ Mirror bsp_template

    twoTemplate    = general $ TwoPane delta ratio
    two            = renamed [Replace "TwoPane" ] $ avoidStruts $        twoTemplate
    twoMirror      = renamed [Replace "MTwoPane"] $ avoidStruts $ Mirror twoTemplate

    accTemplate    = general $ Accordion
    acc            = renamed [Replace "Accordion" ] $ avoidStruts $        accTemplate
    accMirror      = renamed [Replace "MAccordion"] $ avoidStruts $ Mirror accTemplate

     -- One window
    mono           = renamed [Replace "Mono"  ] $ avoidStruts $ general $ noBorders $ Full

     -- The default number of windows in the master pane
    nmaster        = 1

     -- Default proportion of screen occupied by master pane
    ratio          = 1/2

    delta          = 3/100


myManageHook = (composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "Gpick"          --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ])

myEventHook = mempty

myStartupHook = do
  spawn "xrandr --output HDMI1 --primary --mode 1280x1024 --rate 60 --output VGA1 --mode 1280x1024 --rate 60 --left-of HDMI1 && nitrogen --restore"
  spawn "setxkbmap br &"
  spawn "xsetroot -cursor_name left_ptr &"
  spawn "xset s off &"
  spawn "xset s 0 0 &"
  spawn "xset -dpms &"
  spawn "emacs --daemon &"


myPP = def { ppCurrent         = xmobarColor "#8fbcbb" "" . wrap "<" ">"
           , ppTitle           = xmobarColor "#8fbcbb" "" . shorten 32
           , ppHiddenNoWindows = xmobarColor "#4c566a" ""
	   , ppHidden          = id
           , ppUrgent          = xmobarColor "red" "yellow"
	   , ppWsSep           = ""
	   , ppSep             = "  " }

main = do
  xmproc0 <- spawnPipe "xmobar -x 0 /home/arthur/.config/xmobar/xmobarrc"
  xmproc1 <- spawnPipe "xmobar -x 1 /home/arthur/.config/xmobar/xmobarrc"
        
  xmonad $ docks def {
        -- Basic configurations
    terminal           = "alacritty",
    focusFollowsMouse  = True,
    clickJustFocuses   = False,
    borderWidth        = 1,
    modMask            = mod4Mask,
    workspaces         = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9"],
    normalBorderColor  = "#282a36",
    focusedBorderColor = "#ff5555",
      
    -- Key bindings
    keys               = myKeys,
    mouseBindings      = myMouseBindings,
      
    -- Hooks
    layoutHook         = myLayout,
    manageHook         = myManageHook,
    handleEventHook    = myEventHook,
    logHook            = dynamicLogWithPP $ myPP { ppOutput = \x -> hPutStrLn xmproc0 x >> hPutStrLn xmproc1 x },
    startupHook        = myStartupHook }
    

help :: String
help = "You does not need help"
