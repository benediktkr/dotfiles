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
-- TODO:
--   Touchpad tapping, see #xmonad log
--   Fullscreen Totem (hide xmobar, set layout=full, remove border)

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Actions.NoBorders
import XMonad.Layout.NoBorders
import XMonad.Layout.Accordion
import XMonad.Layout.Named
import qualified Data.Map as M
import System.IO

modm = mod4Mask

main = do
  -- xmobar accepts input on its stdin. Rarther then
  --   running `monad | xmobar`, we do this.
  xmproc <- spawnPipe "xmobar ~/.xmobarrc"
  xmonad $ defaultConfig {
    modMask = modm,
    workspaces = "term" : "editor" : "web" : "ent" : "school" : map show[6..9],
    terminal = "xterm  -bg '#000000' -fg '#8bb381'",
    focusFollowsMouse = False,
    manageHook = manageDocks <+> manageHook defaultConfig,
    --layoutHook = avoidStruts  $  layoutHook defaultConfig,
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
      ((modm, xK_g), withFocused toggleBorder)
      
    ]



