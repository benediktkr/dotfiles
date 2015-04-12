{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, PatternSynonyms, RecordWildCards, DeriveDataTypeable, FlexibleContexts #-}

import XMonad 
import XMonad.Operations
import XMonad.Core
import XMonad.Layout.LayoutModifier
import Codec.Binary.UTF8.String
import qualified Data.Foldable as F
import Control.Exception.Extensible
import Control.Concurrent
import Data.List
import Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NE
import Control.Monad
import Data.Monoid

import XMonad.Util.WorkspaceCompare
import XMonad.Util.NamedWindows
import XMonad.Util.Run

import XMonad.Layout.LayoutModifier
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageDocks
import System.Process hiding (runCommand)
import System.Posix.Process
import System.Exit
import XMonad.Util.Dmenu
import XMonad.Hooks.DynamicLog
import XMonad.Actions.Commands
import XMonad.Actions.ShowText  -- flashText defaultSTConfig 1 "Shell"
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import System.IO
import System.Posix.IO
import XMonad.Layout.Mosaic
import XMonad.Layout.Tabbed
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Actions.WindowBringer
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Layout.Circle
import XMonad.Util.NamedScratchpad
import Control.Applicative
import Text.Printf
import Data.Map (Map, fromList)
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import XMonad.Util.Themes

main = xmonad =<< xmobar'' defaults where

xmobar'' ∷ LayoutClass l Window ⇒ XConfig l → IO (XConfig (ModifiedLayout AvoidStruts l))
xmobar'' = statusBar "xmobar" myPP toggleStrutsKey 

myPP :: PP
myPP = defaultPP { ppCurrent = xmobarColor "white" "black" . wrap "[" "]"
                 , ppTitle   = xmobarColor "white" "" . shorten 40
                               
                 }
  -- xmobarConf = defaults {
  --   normalBorderColor = "#007FFF"
  -- }

(∪) = M.union
infixl 0 ∶
(∶) = (,)

myKeys ∷ XConfig Layout → Map (KeyMask, KeySym) (X ())
myKeys = (∪) <$> mykeys <*> keys defaultConfig where
  mykeys XConfig {modMask = modm} = let

    shif     = modm .|. shiftMask
    modKey   = (modm,)
    shiftKey = (shif,)
  
    nextWindow     = spawn "tmux next-window     -t xmonad"
    previousWindow = spawn "tmux previous-window -t xmonad"

    in fromList [
      modKey   xK_x∶      spawn "xterm -e 'emacsclient.emacs-snapshot -c -t -a=\"\"'",
      shiftKey xK_g∶      spawn "xterm -e '/usr/bin/ghci'",
      modKey   xK_f∶      spawn "aur",
      shiftKey xK_f∶      spawn "pkill aur",
      shiftKey xK_x∶      spawn "xkill",
      modKey   xK_y∶      defaultCommands >>= runCommand,
      shiftKey xK_a∶      sendMessage ToggleGaps,

      -- | CycleWS
      modKey   xK_Down∶   nextWS,
      modKey   xK_Up∶     prevWS,
      shiftKey xK_Down∶   shiftToNext,
      shiftKey xK_Up∶     shiftToPrev,
      modKey   xK_Right∶  nextScreen,
      modKey   xK_Left∶   prevScreen,
      shiftKey xK_Right∶  shiftNextScreen,
      shiftKey xK_Left∶   shiftPrevScreen,
      modKey   xK_z∶      toggleWS,
      shiftKey xK_z∶      foo,
      modKey   xK_o∶      clipboardFirefox,

      -- | Tmux
      shiftKey xK_p∶  previousWindow,
      shiftKey xK_n∶  nextWindow,
      
      modKey   xK_s∶ lastTmuxWindow,
      shiftKey xK_s∶ newLastShell,
      shiftKey xK_k∶ killCurrentTmuxWindow,
      shiftKey xK_o∶ nextPane, -- newWindow' MUSIC, -- nextPane,

      -- | Named scratchpad
      -- shiftKey xK_Return∶ openLastShell,
      modKey   xK_a∶ openProg Alsamixer [newWindow, renameWindow, selectWindow],
      modKey   xK_g∶ openProg GHCi      [newWindow, renameWindow, selectWindow],

      -- | IRC
      modKey   xK_i∶ floatScratchpad "irc" ircFloat,
      modKey   xK_i∶ return (),
      
      modKey   xK_u∶ floatScratchpad "emacs"  centerFloat,
      shiftKey xK_u∶ spawn "xterm -e 'emacsclient -nw'",
      shiftKey xK_r∶ namedScratchpadAction scratch "root",
      shiftKey xK_t∶ withFocused (flt centerFloat),
      shiftKey xK_i∶ withFocused (flt ircFloatBig)
      ]

data Pos = AnyPos | Pos Int deriving Show

data Prog' = Prog {
  name     ∷ String,
  commands ∷ NonEmpty String,
  pos      ∷ Pos
} deriving Show

pattern ROOT  = Prog "ROOT"  ("sudo su -" :| [])      (Pos 0)
pattern MUSIC = Prog "Music" ("zsh" :| ["alsamixer"]) (Pos 1)
pattern GHCI  = Prog "GHCi"  ("ghci" :| [])           (Pos 2)
pattern SHELL = Prog "SHELL" ("zsh" :| [])            AnyPos

data Prog = Root | Alsamixer | GHCi
  deriving (Bounded, Enum)

instance Show Prog where
  show = \case
    Root      → "sudo su -"
    Alsamixer → "alsamixer"
    GHCi      → "ghci"

lastTmuxWindow ∷ X ()
lastTmuxWindow = tmux

nextPane ∷ X ()
nextPane = undefined

killCurrentTmuxWindow ∷ X ()
killCurrentTmuxWindow = spawn "tmux kill-window -t xmonad"

newWindow ∷ Prog → X ()
newWindow prog = spawn $ printf "tmux new-window -t xmonad:%d \"%s\"" (fromEnum prog) (show prog)

renameWindow ∷ Prog → X ()
renameWindow prog = spawn $ printf "tmux rename-window -t xmonad:%d \"%s\"" (fromEnum prog) name
  where
    name = takeWhile (/= ' ') (show prog)

selectWindow ∷ Prog → X ()
selectWindow prog = spawn $ printf "tmux select-window -t xmonad:%d" (fromEnum prog)

data FocusedWindow
  = NoWindow
  | FocusedWindow Window
  | TmuxWindow Window
  | SameTmux Window

windowStatus ∷ Pos → X FocusedWindow
windowStatus pos = do
  let getFocused ∷ X (Maybe Window)
      getFocused = withWindowSet (return . W.peek)

  focused ← getFocused
  case focused of
    Nothing     → return NoWindow
    Just window → do
      tmuxWindow ← isTmuxWindow window
      if tmuxWindow
        then do
          num ← activeWindowNumber
          return $ case (num, pos) of
            (Just activeNum, Pos req)
              | activeNum == req → SameTmux  window
            _                    → TmuxWindow window
        else return (FocusedWindow window)

getFocused ∷ X (Maybe Window)
getFocused = withWindowSet (return . W.peek)  

newLastShell ∷ X ()
newLastShell = do
  num           ← nextWindow
  currentWindow ← getFocused
  case currentWindow of
    Just win → do
      weAreXmonadTmux  ← isTmuxWindow win
      unless weAreXmonadTmux
        tmux
    Nothing → tmux
  void (readProcess' "tmux" ["new-window", "-t", "xmonad:" ++ show num, "zsh"] "")

nextWindow ∷ X Int
nextWindow = do
  output ← readProcess' "tmux" ["list-windows", "-t", "xmonad"] ""

  case output of
    (ExitFailure _, _, _)    → return 0
    (ExitSuccess, windows, _) → do
      let number = takeWhile (/= ':') (last (lines windows))
      return (read number + 1)

newWindow' ∷ Prog' → X ()
newWindow' Prog{commands=firstCommand :| restCommands, ..} = do
  case pos of
    Pos position → do
      spawn (printf "tmux new-window -t xmonad:%d \"%s\"" position firstCommand)
      spawn (printf "tmux rename-window -t xmonad:%d %s"  position name)
      io (threadDelay 100000)
      F.for_ restCommands $ \cmd → do
        spawn (printf "tmux split-window -h -t xmonad:%d \"%s\"" position cmd)
        spawn (printf "tmux select-layout -t xmonad:%d tiled"    position)
    AnyPos → do
      F.for_ (firstCommand :| restCommands) $ \cmd → do
        position ← nextWindow
        spawn (printf "tmux new-window -t xmonad:%d \"%s\"" position cmd)
        spawn (printf "tmux rename-window -t xmonad:%d %s"  position name)

-- selectWindow ∷ Prog → X ()
-- selectWindow prog = spawn $ printf "tmux select-window -t xmonad:%d" (fromEnum prog)

-- [newWindow, renameWindow, selectWindow],
openProg' ∷ Prog' → X ()
openProg' Prog{..} = do
  status ← windowStatus pos
  case status of
    NoWindow        → tmux
    FocusedWindow{} → return ()
    TmuxWindow{}    → return ()
    SameTmux win    → tmux

  newWindow' Prog{..}

openProg ∷ Prog → [Prog → X ()] → X ()
openProg prog actions = do
  currentWindow ← getFocused

  case currentWindow of
    -- No focused window
    Nothing → tmux
    Just window → do
      weAreXmonadTmux  ← isTmuxWindow window
      activeTmuxWindow ← activeWindowNumber
      unless (weAreXmonadTmux && activeTmuxWindow /= Just (fromEnum prog)) $
        tmux

  sequence_ [ action prog | action ← actions ]

tmux ∷ X ()
tmux = floatScratchpad "tmux" centerFloat

isTmuxWindow ∷ Window → X Bool
isTmuxWindow = runQuery (title =? "xmonad-tmux") 

readProcess' ∷ FilePath → [String] → String → X (ExitCode, String, String)
readProcess' a b c = io $ do
  uninstallSignalHandlers
  read ← readProcessWithExitCode a b c
  installSignalHandlers
  return read
  
activeWindowNumber ∷ X (Maybe Int)
activeWindowNumber = do
  output ← readProcess' "tmux" ["list-windows", "-t", "xmonad"] ""

  case output of
    (ExitSuccess, stdout, stderr) → do
      let activeOne = head $ filter (" (active)" `isSuffixOf`) (lines stdout)
      return $ Just $ read $ takeWhile (/= ':') $ head $ words activeOne
    (ExitFailure _, _, _) → return Nothing

toggleFocused ∷ X ()
toggleFocused = withFocused $ \w ->
  ifX isFloating
    (windows . W.sink $ w)
    (float w)
  
getTitle ∷ Window → X String
getTitle = runQuery title

infixr 1 `hasTitle`
hasTitle ∷ Window → String → X Bool
w `hasTitle` title = (== title) <$> getTitle w

floatScratchpad ∷ String → W.RationalRect → X ()
floatScratchpad name pos = do
  withFocused $ \window → do
    whenX (window `hasTitle` "xmonad-" ++ name) $ do
      flt pos window
  
  namedScratchpadAction scratch name
  
  withFocused $ \window → do
    whenX (window `hasTitle` "xmonad-" ++ name) $ do
      flt pos window

flt ∷ W.RationalRect → Window → X ()
flt pos w = windows (W.float w pos)

ifX ∷ X Bool → X a → X a → X a
ifX x t f = x >>= \b -> if b then t else f  

-- whenX ∷ X Bool → X () → X ()
-- whenX b t = ifX b t (return ())

unlessX ∷ X Bool → X () → X ()
unlessX b e = ifX b (return ()) e

isFloating ∷ X Bool
isFloating = withWindowSet $ \w -> return . maybe False id .
    (return . flip M.member (W.floating w) . W.focus
         <=< W.stack . W.workspace . W.current) $ w
  
floatFocused ∷ W.RationalRect → X ()
floatFocused pos = withFocused float

toggleFloat :: Ord a => a -> W.RationalRect -> W.StackSet i l a s sd -> W.StackSet i l a s sd
toggleFloat w r s@W.StackSet{W.floating = floating}
  | w `M.member` floating = s { W.floating = M.delete w floating }
  | otherwise             = s { W.floating = M.insert w r floating }

unfloatFocused ∷ X ()
unfloatFocused = withFocused unfloat
                           
unfloat ∷ Window → X ()
unfloat = windows . W.sink

sink ∷ X ()
sink = withFocused (windows . W.sink)
  
doRectFloat :: W.RationalRect  -- ^ The rectangle to float the window in. 0 to 1; x, y, w, h.
            -> ManageHook
doRectFloat r = ask >>= \w -> doF (W.float w r)
       
--   import XMonad.Actions.ShowText
--   handleEventHook    = handleEventHook defaultConfig <+> handleTimerEvent 
--   ((modm, xK_u), flashText defaultSTConfig 1 "→" >> nextWS)
-- Byrtir "→" á skjánum í eina sekúndu

defaults = defaultConfig {
  terminal           = "xterm",
  modMask            = mod4Mask,
  borderWidth        = 1,
  workspaces         = ["1:Firefox", "2:Emacs", "3:IRC", "4:SKÓLI", "5", "6"],
  normalBorderColor  = inactiveColor (theme donaldTheme), -- "#dddddd",
  focusedBorderColor = activeColor   (theme donaldTheme), -- "#456def",
  keys               = myKeys,
  -- mouseBindings      = myMouseBindings,
  layoutHook         = myLayout,
  manageHook         = myManageHook <> namedScratchpadManageHook scratch --  <> manageHook defaultConfig
  -- handleEventHook    = handleEventHook defaultConfig <+> handleTimerEvent                        
  }

myManageHook = composeAll [ title =? i --> doRectFloat ircFloat | i <- ["xmonad-irc"] ]

-- theGap (Mirror tiled)

  -- normalBorderColor  = "#dddddd",
  -- focusedBorderColor = "#456def",


myTabConfig = defaultTheme {
  activeColor         = "#1B467A",
  activeTextColor     = "#6C85A3",
  activeBorderColor   = "#456DEF",

  inactiveColor       = "#6C85A3",
  inactiveTextColor   = "#2B558A",
  inactiveBorderColor = "#546273"
}

 -- myLayout = tabbed shrinkText (theme kavonFireTheme)

 -- main = xmonad defaultConfig {layoutHook = myLayout}

myLayout = Mirror tiled
       -- ||| spacing 10 (Mirror (mosaic 4 []))
       ||| mind the gap (tabbed shrinkText (theme donaldTheme))
       -- ||| mind the gap Full
       -- ||| tabbed shrinkText myTabConfig
       -- ||| Circle
  where
    mind = id

    the = id
  
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio
    
    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio   = 1/2

    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

    gap = gaps [(U,20), (R,20), (D, 20), (L, 20)] 


-- Scratchpads
scratch ∷ NamedScratchpads
scratch = [
--  center "alsamixer" "alsamixer",
  NS "alsamixer" "xterm -T xmonad-tmux -e 'tmux attach-session -t xmonad || tmux new-session -s xmonad zsh'" (title =? "xmonad-tmux") (customFloating centerFloat),

  center "ghci" "ghci",
  
  -- center "tmux" "/bin/zsh",

  center "emacs" "emacsclient -nw -a=\"\"",

  makePad "irc" ircFloat "ssh nullspace.sudo.is -t -- tmux attach",

  NS "tmux" "xterm -T xmonad-tmux -e 'tmux attach-session -t xmonad || tmux new-session -s xmonad \"sudo su -\"'" (title =? "xmonad-tmux") (customFloating centerFloat)

--   NS "tmux" "xterm -T xmonad-tmux -e 'python'" (title =? "xmonad-tmux") (customFloating centerFloat)
  ]

  -- NS "root" cmd (title =? "xmonad-root") (customFloating centerFloat) where
  --   cmd = "xterm -T xmonad-root -e 'sudo su -'"

center ∷ String → String → NamedScratchpad
center = flip makePad centerFloat

makePad ∷ String → W.RationalRect → String → NamedScratchpad
makePad str pos cmd =
  NS str command (title =? ("xmonad-" ++ str)) (customFloating pos) where
    command = printf "xterm -T xmonad-%s -e '%s'" str cmd

centerFloat ∷ W.RationalRect
centerFloat = W.RationalRect 0.10 0.10 0.80 0.80

ircFloat ∷ W.RationalRect
ircFloat = W.RationalRect 0 0.78 1 0.24

ircFloatBig ∷ W.RationalRect
ircFloatBig = W.RationalRect 0 0.57 1 0.46

scratchpadDefaultRect :: W.RationalRect
scratchpadDefaultRect = W.RationalRect 0.25 0.375 0.5 0.25

notify ∷ String → X ()
notify str = spawn ("notify-send " ++ str)

clipboardFirefox ∷ X ()
clipboardFirefox = do
  spawn "aur -new-tab \"$(xclip -o)\""

foo ∷ X ()
foo = do
  poo ← dmenu []
  spawn $ "aur -new-tab '"
       ++ "https://www.google.se/search?client=ubuntu&channel=fs&q="
       ++ poo
       ++ "'"

xmobarPP' :: PP
xmobarPP' = defaultPP { ppCurrent = xmobarColor "yellow" "" . wrap "[" "]"
                      , ppTitle   = xmobarColor "green"  "" . shorten 40
                      , ppVisible = wrap "(" ")"
                      , ppUrgent  = xmobarColor "red" "yellow"
                      }

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )