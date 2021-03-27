import XMonad hiding ((|||))
import Data.Monoid
import Data.Tuple
import System.Exit
import XMonad.Util.Run
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import XMonad.Layout.ResizableTile
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import XMonad.Layout.Accordion
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Renamed
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Actions.Volume
import XMonad.Util.Dzen
import XMonad.Actions.Minimize
import XMonad.Layout.Minimize
import XMonad.Actions.GridSelect
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Column
import XMonad.Layout.Grid
import XMonad.Actions.NoBorders
import XMonad.Util.SpawnOnce

alert = dzenConfig cfg1 . show . round
cfg1 = addArgs ["-xs", "1"] >=> addArgs ["-fg", "#eceff4"]
   >=> addArgs ["-bg", "#282a36"] >=> addArgs ["-geometry", "200x100+540+462"]

spawnSelectedName :: GSConfig String -> [(String, String)] -> X ()
spawnSelectedName conf lst = gridselect conf lst >>= flip whenJust spawn

textEditor :: String
textEditor = "emacsclient -c"

terminalEm :: String
terminalEm = "alacritty"

sencondTerminalEm :: String
sencondTerminalEm = "st"

terminalMultiplexer :: String
terminalMultiplexer = terminalEm ++ " -e fish -c 'tmux attach || tmux'"

programsMenu :: String
programsMenu = "dmenu_run"

screenshooter :: (String, String)
screenshooter = ("xfce4-screenshooter", "flameshot gui")

gridPrograms :: [(String, String)]
gridPrograms =
  [ ( "pcmanfm"      , "pcmanfm"                                   )
  , ( "brave"        , "brave"                                     )
  , ( "bitwarden"    , "bitwarden"                                 )
  , ( "ncmpcpp"      , terminalEm ++ " -e ncmpcpp"                 )
  , ( "emacs"        , terminalEm ++ " -e emacsclient -c -nw"      )
  , ( "emacs-gui"    , "emacsclient -c"                            )
  , ( "teditor"      , terminalEm ++ " -e ted"                     )
  , ( "nitrogen"     , "nitrogen"                                  )
  , ( "transmission" , "transmission-gtk"                          )
  , ( "ripcord"      , "ripcord"                                   )
  , ( "palemoon"     , "palemoon"                                  )
  , ( "luakit"       , "luakit"                                    )
  , ( "discord"      , "brave --incognito https://discord.com/app" )
  , ( "tmux"         , terminalMultiplexer                         )
  ]

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    [ ((modm .|. shiftMask  , xK_Return ), spawn terminalEm)
    , ((modm .|. shiftMask  , xK_t      ), spawn sencondTerminalEm)
    , ((modm .|. controlMask, xK_Return ), spawn textEditor)

    , ((modm .|. controlMask
             .|. shiftMask  , xK_Return ), spawn terminalMultiplexer)

    , ((modm .|. shiftMask
             .|. controlMask, xK_a      ), spawnSelectedName defaultGSConfig gridPrograms)

    , ((0                   , xK_Print  ), spawn $ fst screenshooter)
    , ((modm                , xK_Print  ), spawn $ snd screenshooter)

    , ((modm                , xK_p      ), spawn programsMenu)

    , ((modm .|. shiftMask  , xK_p      ), spawn "bash /home/arthur/scripts/search.sh")
    , ((modm .|. shiftMask  , xK_b      ), spawn
        "BRIGHTNESS=$(echo -ne '' | dmenu -p 'brightness:') ; xrandr --output HDMI1 --brightness $BRIGHTNESS --output VGA1 --brightness $BRIGHTNESS")
    , ((modm                , xK_s      ), spawn "echo -ne '' | dmenu | xargs xinput --set-prop 8 169 1, 0, 0, 0, 1, 0, 0, 0, ")

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

    , ((modm .|. shiftMask  , xK_c      ), kill)

    , ((modm                , xK_space  ), sendMessage NextLayout)
    , ((modm .|. shiftMask  , xK_space  ), setLayout $ XMonad.layoutHook conf)
    , ((modm                , xK_f      ), runSelectedAction defaultGSConfig $
        map (\x -> (x, sendMessage $ JumpToLayout x)) layoutNames)

    , ((modm                , xK_n      ), withFocused minimizeWindow)
    , ((modm .|. shiftMask  , xK_n      ), withLastMinimized maximizeWindowAndFocus)

    , ((modm                , xK_g      ), goToSelected defaultGSConfig)

    , ((modm                , xK_j      ), windows W.focusDown  )
    , ((modm                , xK_k      ), windows W.focusUp    )
    , ((modm                , xK_m      ), windows W.focusMaster)

    , ((modm                , xK_Return ), windows W.swapMaster)
    , ((modm .|. shiftMask  , xK_j      ), windows W.swapDown  )
    , ((modm .|. shiftMask  , xK_k      ), windows W.swapUp    )

    , ((modm                , xK_h      ), sendMessage Shrink)
    , ((modm                , xK_l      ), sendMessage Expand)
    , ((modm .|. shiftMask  , xK_h      ), sendMessage MirrorShrink)
    , ((modm .|. shiftMask  , xK_l      ), sendMessage MirrorExpand)

    , ((modm                , xK_t      ), withFocused $ windows . W.sink)

    , ((modm                , xK_comma  ), sendMessage (IncMasterN 1))
    , ((modm                , xK_period ), sendMessage (IncMasterN (-1)))

    , ((modm .|. shiftMask
             .|. controlMask, xK_q      ), io (exitWith ExitSuccess))

    , ((modm                , xK_q      ), spawn
        "xmonad --recompile; killall xmobar; killall dunst; killall stalonetray; xmonad --restart")

    , ((modm                , xK_u      ), incScreenSpacing 5)
    , ((modm .|. shiftMask  , xK_u      ), incWindowSpacing 5)
    , ((modm                , xK_i      ), decScreenSpacing 5)
    , ((modm .|. shiftMask  , xK_i      ), decWindowSpacing 5)
    , ((modm                , xK_o      ), setScreenSpacing (Border  0  0  0  0))
    , ((modm .|. shiftMask  , xK_o      ), setWindowSpacing (Border 10 10 10 10))

    , ((modm                , xK_b      ), withFocused toggleBorder)

    , ((modm .|. shiftMask  , xK_slash  ),
        spawn ("echo \"" ++ help ++ "\" > ~/.xmonad/help.txt && " ++ textEditor ++ " ~/.xmonad/help.txt"))
    ]

    ++

    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    [((modm .|. shiftMask  , xK_l      ), runSelectedAction defaultGSConfig $
        map (\x -> (show x, windows $ W.greedyView x)) (XMonad.workspaces conf))]

    ++

    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- [(xK_e, 0), (xK_w, 1)]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

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

layoutNames :: [String]
layoutNames =
    [ "Tiled"  , "MTiled"
    , "Three"  , "MThree"
    , "TwoPane", "MTwoPane"
    , "Grid"   , "MGrid"
    , "Column" , "MColumn"
    , "Mono"   , "Full"
    ]
myLayout = tiled ||| mirrortiled ||| three  ||| threeMirror  ||| two  ||| twoMirror
       ||| grid  ||| gridMirror  ||| column ||| columnMirror ||| mono ||| fullscreen
  where
    general2 = spacingRaw False (Border 0 0 0 0) True (Border 10 10 10 10) True

    gridTemplate   = minimize $ noBorders $ Grid
    grid           = renamed [Replace  "Grid"] $ general2 $ avoidStruts $        gridTemplate
    gridMirror     = renamed [Replace "MGrid"] $ general2 $ avoidStruts $ Mirror gridTemplate

    tiled_template = minimize $ noBorders $ ResizableTall nmaster delta ratio []
    tiled          = renamed [Replace "Tiled" ] $ general2 $ avoidStruts $        tiled_template
    mirrortiled    = renamed [Replace "MTiled"] $ general2 $ avoidStruts $ Mirror tiled_template

    fullscreen     = renamed [Replace "Full"  ] $ minimize $ noBorders $ Full

    twoTemplate    = minimize $ noBorders $ TwoPane delta ratio
    two            = renamed [Replace "TwoPane" ] $ general2 $ avoidStruts $        twoTemplate
    twoMirror      = renamed [Replace "MTwoPane"] $ general2 $ avoidStruts $ Mirror twoTemplate

    mono           = renamed [Replace "Mono"  ] $ general2 $ avoidStruts $ minimize $ noBorders $ Full

    threeTemplate  = minimize $ noBorders $ ThreeCol nmaster (delta) (ratio)
    three          = renamed [Replace "Three" ] $ general2 $ avoidStruts $        threeTemplate
    threeMirror    = renamed [Replace "MThree"] $ general2 $ avoidStruts $ Mirror threeTemplate

    columnTemplate = minimize $ noBorders $ Column 1
    column         = renamed [Replace  "Column"] $ general2 $ avoidStruts $        columnTemplate
    columnMirror   = renamed [Replace "MColumn"] $ general2 $ avoidStruts $ Mirror columnTemplate

    nmaster        = 1
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
  spawnOnce "xrandr --output HDMI1 --primary --mode 1280x1024 --rate 60 --pos 1400x0 --output VGA1 --mode 1280x1024 --rate 60 && nitrogen --restore"
  spawnOnce "setxkbmap br"
  spawnOnce "xsetroot -cursor_name left_ptr"
  spawnOnce "xset s off"
  spawnOnce "xset s 0 0"
  spawnOnce "xset -dpms"
  spawnOnce "dunst"
  spawnOnce "stalonetray"

myPP = def { ppCurrent         = xmobarColor "#8fbcbb" "" . wrap "<" ">"
           , ppTitle           = xmobarColor "#8fbcbb" "" . shorten 32
           , ppHiddenNoWindows = xmobarColor "#4c566a" ""
           , ppHidden          = id
           , ppUrgent          = xmobarColor "red" "yellow"
           , ppWsSep           = ""
           , ppSep             = " <icon=separator.xpm/> " }

main = do
  xmproc0 <- spawnPipe "xmobar -x 0 /home/arthur/.config/xmobar/xmobarrc --dock"
  xmproc1 <- spawnPipe "xmobar -x 1 /home/arthur/.config/xmobar/xmobarrc --dock"

  xmonad $ docks def
    { terminal           = terminalEm
    , focusFollowsMouse  = False
    , clickJustFocuses   = False
    , borderWidth        = 1
    , modMask            = mod4Mask
    , workspaces         = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
    , normalBorderColor  = "#555555"
    , focusedBorderColor = "#AAAAAA"

    , keys               = myKeys
    , mouseBindings      = myMouseBindings

    , layoutHook         = myLayout
    , manageHook         = myManageHook
    , handleEventHook    = myEventHook
    , logHook            = dynamicLogWithPP $ myPP { ppOutput = \x -> hPutStrLn xmproc0 x >> hPutStrLn xmproc1 x }
    , startupHook        = myStartupHook
    }

help :: String
help = unlines
    [ "Arthur Bacci's XMonad Config"
    , ""
    , "This configuration was made using alacritty as the terminal emulator"
    , ""
    , "    Mod-S-Escape      Opens the terminal emulator"
    , "    Mod-C-S-Escape    Opens the text editor"
    , "    Mod-C-S-A         Opens the grid launcher"
    ]
