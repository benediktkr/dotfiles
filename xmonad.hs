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
-- TODO:
--   xterm colors like emacs theme
--   Add network statistics on xmobar
--   Why isn't ~/.Xdefaults loaded? (xrdb -merge)
--   Touchpad tapping, see #xmonad log
--   Fullscreen Totem

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

main = do
  -- xmobar accepts input on its stdin. Rarther then
  --   running `monad | xmobar`, we do this.
  xmproc <- spawnPipe "xmobar ~/.xmobarrc"
  xmonad $ defaultConfig {
    modMask = mod4Mask,
    terminal = "xterm  -bg '#000000' -fg '#8bb381'",
    focusFollowsMouse = False,
    manageHook = manageDocks <+> manageHook defaultConfig,
    layoutHook = avoidStruts  $  layoutHook defaultConfig,
    logHook = dynamicLogWithPP xmobarPP {
      -- We get output to xmobar with hPutStrLn xmproc (a pipe to xmobar)
      ppOutput = hPutStrLn xmproc,
      ppTitle = xmobarColor "green" "" . shorten 50
      }
    } `additionalKeys`
    [ ((0, xK_Print), spawn "scrot ~/scrot.png"),
      ((mod4Mask, xK_z), spawn "xscreensaver-command -lock"),
      ((mod4Mask .|. shiftMask, xK_BackSpace), spawn "emacsclient -n -c -a \"\"")
    ]

