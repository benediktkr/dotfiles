-- 
-- XMonad config file
-- Benedikt Kristinsson
-- Welcome down the rabbit hole
--
-- Requires (ubuntu packages):
--   dmenu
--   libghc6-xmonad-contrib-dev
--   libghc6-xmonad-dev
--   xmobar
--   
-- xmonad(additionalKeys(defaultConfig, {my keys}))
--
-- Key codes:
--   XF86AudioLowerVolume  0x1008ff11
--   XF86AudioMute         0x1008ff12
--   XF86AudioRaiseVolume  0x1008ff13
--
-- Volume controls are also implemented in XMonad.Actions.Volume, found in the cabal package xmonad-extras
-- but it conflicts with depencies in Debian. 
--
-- TODO:
--   Touchpad tapping, see #xmonad log


import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Actions.NoBorders
import XMonad.Layout.NoBorders
import XMonad.Layout.Accordion
import XMonad.Layout.Named
import System.IO

modm = mod4Mask

main = do
  -- xmobar accepts input on its stdin. Rarther then
  --   running `monad | xmobar`, we do this.
  xmproc <- spawnPipe "xmobar ~/.xmobarrc"
  xmonad $ defaultConfig {
    modMask = modm,
    workspaces = "term" : "editor" : "web" : "ent" : "work" : map show [6..9],
    terminal = "xterm  -bg '#000000' -fg '#8bb381'",
    focusFollowsMouse = False,
    manageHook = manageDocks <+> manageHook defaultConfig,
    layoutHook =  avoidStruts (layoutHook defaultConfig ||| Accordion) ||| named "Fullscreen" (noBorders  Full),
    logHook = dynamicLogWithPP xmobarPP {
      -- We get output to xmobar with hPutStrLn xmproc (a pipe to xmobar)
      ppOutput = hPutStrLn xmproc,
      ppTitle = xmobarColor "green" "" . shorten 50
      }
    } `additionalKeys`
    [ ((0, xK_Print), spawn "scrot ~/scrot.png"),
      ((modm, xK_z), spawn "xscreensaver-command -lock"),
      ((modm, xK_c), kill),
      ((modm .|. shiftMask, xK_BackSpace), spawn "emacsclient -n -c -a \"\""),
      ((modm, xK_g), withFocused toggleBorder),
      ((0, 0x1008ff11), spawn "amixer -q set Master 2%-"),
      ((0, 0x1008ff13), spawn "amixer -q set Master 2%+"),
      ((0, 0x1008ff12), spawn "amixer -q set Master toggle")
    ]



