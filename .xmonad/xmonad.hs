import XMonad
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat
import XMonad.Layout.PerWorkspace
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import System.Exit
import System.IO
main = do
    h <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
    xmonad $ defaultConfig
        { workspaces = ["irc", "www", "float", "gfx", "ink"]
        , layoutHook = smartBorders $ onWorkspace "float" simplestFloat $ tile ||| Full
        , logHook =  dynamicLogWithPP (bar h)
        , manageHook = myManageHook <+> manageHook defaultConfig
        , modMask = mod1Mask
        , keys = myKeys
        , borderWidth = 1
        , focusedBorderColor = "#888888"
        , normalBorderColor = "#181818"
        }
  where
    tile = gaps [(U,17), (D,3), (L,3), (R,3)] $ spacing 3 $ ResizableTall 1 (3/100) (5/8) []

myManageHook = composeAll . concat $
    [ [ className =? c               --> doFloat | c <- cFloats ]
    , [ title     =? t               --> doFloat | t <- tFloats ]]
  where cFloats = ["Gimp", "Pidgin", "ROX-Filer"]
        tFloats = ["Firefox Preferences", "Downloads", "Add-ons", "Rename", "Create" ]

bar h = defaultPP {
        ppOutput = hPutStrLn h
        , ppHiddenNoWindows = id
        , ppTitle = xmobarColor "#888888" "" . shorten 50
        , ppCurrent = xmobarColor "#b0393f" "" . wrap "<fc=#9c8e2d>[</fc>" "<fc=#9c8e2d>]</fc>"
        , ppLayout   = xmobarColor "#9c8e2d" "" . 
        \sy -> case sy of
                "Spacing 3 ResizableTall" -> " []= "
                "SimplestFloat"           -> " ><> "
                "Full"                    -> " [M] "  
                _  -> sy
        , ppSep = " "
        }

term  = "urxvtc"
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask .|. shiftMask, xK_Return), spawn term)
    , ((modMask .|. shiftMask, xK_w), spawn "firefox")
    , ((modMask .|. shiftMask, xK_u), spawn "~/bin/uzblt")
    , ((modMask .|. shiftMask, xK_r), spawn "gmrun")
    , ((modMask .|. shiftMask, xK_f), spawn "rox")
    , ((0,               0x1008ff11), spawn "ossv.sh pcm -2")
    , ((0,               0x1008ff12), spawn "ossmix pcm 0:0")
    , ((0,               0x1008ff13), spawn "ossv.sh pcm +2")
    , ((0,               0x1008ff14), spawn "mpc --no-status toggle")
    , ((0,               0x1008ff16), spawn "mpc --no-status prev")
    , ((0,               0x1008ff17), spawn "mpc --no-status next")
    , ((modMask .|. shiftMask, xK_c), kill)
    , ((modMask,               xK_space ), sendMessage NextLayout)
    , ((modMask,               xK_j     ), windows W.focusDown)
    , ((modMask,               xK_k     ), windows W.focusUp  )
    , ((modMask,               xK_Return), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )
    , ((modMask,               xK_h     ), sendMessage Shrink)
    , ((modMask,               xK_l     ), sendMessage Expand)
    , ((modMask .|. controlMask,    xK_j), sendMessage MirrorShrink)
    , ((modMask .|. controlMask,    xK_k), sendMessage MirrorExpand)
    , ((modMask .|. shiftMask, xK_t     ), gets (M.keys . W.floating . windowset)
                                            >>= mapM_ (windows . W.sink))
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))
    , ((modMask .|. shiftMask, xK_q     ), restart "xmonad" True)
    ]
    ++
    [((m .|. modMask, k), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
