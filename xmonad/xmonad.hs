-- ~/.xmonad/xmonad.hs

----------------------------------
-- Arthur Bacci's XMonad Config --
----------------------------------

--------
-- Imports
----

-- Default
import XMonad hiding ((|||))
import Data.Monoid
import Data.Tuple
import System.Exit
import XMonad.Util.Run
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
-- Layouts
import XMonad.Layout.ResizableTile
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import XMonad.Layout.Accordion
-- Layout modifiers
import XMonad.Layout.Spacing           -- Adds some space between the windows
import XMonad.Layout.NoBorders         -- Removes the borders
import XMonad.Layout.LayoutCombinators -- To use JumpToLayout |
import XMonad.Layout.Renamed -- Rename the layouts |
import XMonad.Hooks.ManageDocks -- Related to the bar |
import XMonad.Hooks.DynamicLog  ----------------------'
import XMonad.Actions.Volume -- Change Volume |
import XMonad.Util.Dzen      -----------------'
import XMonad.Actions.Minimize -- Minimize with Mod-N (Mod-Shift-N for undo) |
import XMonad.Layout.Minimize  ----------------------------------------------'
import XMonad.Actions.GridSelect -- Grid menu |
import XMonad.Layout.ThreeColumns -- Three Colums Layout |
import XMonad.Layout.Column
import XMonad.Layout.Grid
import XMonad.Layout.Spacing
import XMonad.Actions.NoBorders

alert = dzenConfig cfg1 . show . round
cfg1 = addArgs ["-xs", "1"] >=> addArgs ["-fg", "#eceff4"] >=> addArgs ["-bg", "#282a36"]

spawnSelectedName :: GSConfig String -> [(String, String)] -> X ()
spawnSelectedName conf lst = gridselect conf lst >>= flip whenJust spawn


--  --  --  --  --  --
  -- Definitions  --
--  --  --  --  --  --

textEditor :: String
textEditor = "emacsclient -c"

terminalEm :: String
terminalEm = "alacritty"

terminalMultiplexer :: String
terminalMultiplexer = "alacritty -e fish -c 'tmux attach || tmux'" -- If could not connect to the session, create one

programsMenu :: String
programsMenu = "dmenu_run"

screenshooter :: (String, String)
screenshooter = ("xfce4-screenshooter", "flameshot gui")

gridPrograms :: [(String, String)]
gridPrograms =
  [ ( "pcmanfm"      , "pcmanfm"                                   )
  , ( "brave"        , "brave"                                     )
  , ( "bitwarden"    , "bitwarden"                                 )
  , ( "ncmpcpp"      , "alacritty -e ncmpcpp"                      )
  , ( "emacs"        , "alacritty -e emacsclient -c -nw"           )
  , ( "emacs-gui"    , "emacsclient -c"                            )
  , ( "teditor"      , "alacritty -e ted"                          )
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

    , ((modm .|. controlMask, xK_Return ), spawn textEditor)

    , ((modm .|. controlMask
             .|. shiftMask  , xK_Return ), spawn terminalMultiplexer)

    , ((modm .|. shiftMask
             .|. controlMask, xK_a      ), spawnSelectedName defaultGSConfig gridPrograms)

    -- https://superuser.com/questions/389737/how-do-you-make-volume-keys-and-mute-key-work-in-xmonad
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
    
    , ((modm                , xK_p      ), spawn programsMenu)

    , ((modm .|. shiftMask  , xK_p      ), spawn "bash /home/arthur/scripts/search.sh")

    , ((0                   , xK_Print  ), spawn $ fst screenshooter)

    , ((modm                , xK_Print  ), spawn $ snd screenshooter)

    , ((modm .|. shiftMask  , xK_c      ), kill)

    , ((modm                , xK_space  ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask  , xK_space  ), setLayout $ XMonad.layoutHook conf)

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
        map (\x -> (x, sendMessage $ JumpToLayout x)) layoutNames
      )

    , ((modm .|. shiftMask  , xK_l      ), runSelectedAction defaultGSConfig $
        map (\x -> (show x, windows $ W.greedyView x)) (XMonad.workspaces conf))
    
    -- Quit xmonad
    , ((modm .|. shiftMask
             .|. controlMask, xK_q      ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm                , xK_q      ), spawn
        "xmonad --recompile; killall xmobar; killall emacs; killall dunst; killall stalonetray; xmonad --restart")
    
    , ((modm                , xK_u      ), incScreenSpacing 5)
    , ((modm .|. shiftMask  , xK_u      ), incWindowSpacing 5)
    
    , ((modm                , xK_i      ), decScreenSpacing 5)
    , ((modm .|. shiftMask  , xK_i      ), decWindowSpacing 5)
    
    , ((modm                , xK_o      ), setScreenSpacing (Border  0  0  0  0))
    , ((modm .|. shiftMask  , xK_o      ), setWindowSpacing (Border 10 10 10 10))
    
    , ((modm                , xK_b      ), withFocused toggleBorder)

    , ((modm                , xK_s      ), spawn "echo -ne '' | dmenu | xargs xinput --set-prop 8 169 1, 0, 0, 0, 1, 0, 0, 0, ") -- Change sensitivity

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    , ((modm .|. shiftMask  , xK_slash  ),
        spawn ("echo \"" ++ help ++ "\" > ~/.xmonad/help.txt && " ++ textEditor ++ " ~/.xmonad/help.txt"))
    ]
    ++

    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    {-
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (drop 9 $ XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, controlMask), (W.shift, controlMask .|. shiftMask)]]
    ++
    -}

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


layoutNames :: [String]
layoutNames =
    [ "Tiled"  , "MTiled"
    , "Three"  , "MThree"
    , "TwoPane", "MTwoPane"
    , "Grid"   , "MGrid"
    , "Column" , "MColumn"
    , "Mono"   , "Full"
    ]
myLayout = tiled  ||| mirrortiled  ||| three ||| threeMirror ||| two ||| twoMirror |||
           grid ||| gridMirror ||| column ||| columnMirror ||| mono  ||| fullscreen
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

    nmaster        = 1 -- The default number of windows in the master pane
    ratio          = 1/2 -- Default proportion of screen occupied by master pane
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
  spawn "setxkbmap br"
  spawn "xsetroot -cursor_name left_ptr"
  spawn "xset s off"
  spawn "xset s 0 0"
  spawn "xset -dpms"
  spawn "emacs --daemon"
  spawn "dunst"
  spawn "stalonetray"


myPP = def { ppCurrent         = xmobarColor "#8fbcbb" "" . wrap "<" ">"
           , ppTitle           = xmobarColor "#8fbcbb" "" . shorten 32
           , ppHiddenNoWindows = xmobarColor "#4c566a" ""
           , ppHidden          = id
           , ppUrgent          = xmobarColor "red" "yellow"
           , ppWsSep           = ""
           , ppSep             = " - " }

main = do
  xmproc0 <- spawnPipe "xmobar -x 0 /home/arthur/.config/xmobar/xmobarrc --dock"
  xmproc1 <- spawnPipe "xmobar -x 1 /home/arthur/.config/xmobar/xmobarrc --dock"
        
  xmonad $ docks def {
    terminal           = "alacritty",
    focusFollowsMouse  = False,
    clickJustFocuses   = False,
    borderWidth        = 1,
    modMask            = mod4Mask,
    workspaces         = ["1", "2", "3", "4", "5", "6", "7", "8", "9"], --, "A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9"],
    normalBorderColor  = "#555555",
    focusedBorderColor = "#AAAAAA",
      
    keys               = myKeys,
    mouseBindings      = myMouseBindings,
      
    layoutHook         = myLayout,
    manageHook         = myManageHook,
    handleEventHook    = myEventHook,
    logHook            = dynamicLogWithPP $ myPP { ppOutput = \x -> hPutStrLn xmproc0 x >> hPutStrLn xmproc1 x },
    startupHook        = myStartupHook }
    
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


