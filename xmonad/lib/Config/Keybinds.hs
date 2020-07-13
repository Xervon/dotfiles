module Config.Keybinds
( keybinds
, mousebinds
, showKeybindings
) where

import qualified Config.Defaults    as C
import qualified Config.Prompts     as P
import qualified Config.ScratchPads as SP

import System.Exit
import System.IO
import qualified Data.Map as M

import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.FlexibleManipulate
import XMonad.Actions.MessageFeedback
import XMonad.Actions.Navigation2D
import XMonad.Actions.Promote
import XMonad.Actions.WithAll
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Hidden
import qualified XMonad.Layout.MultiToggle as MT
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.ResizableTile
import XMonad.Layout.SubLayouts
import XMonad.Util.EZConfig
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
import XMonad.Util.Paste as P
import XMonad.Util.Run
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Shell
import qualified XMonad.StackSet as W

keybinds :: XConfig Layout -> [((KeyMask, KeySym), NamedAction)]
keybinds conf = let
  subKeys str ks = subtitle str : mkNamedKeymap conf ks
  wsKeys  = map show $ ([1..9] ++ [0] :: [Int])
  dirKeys = ["j", "k", "h", "l"]
  dirs    = [ D ,  U ,  L ,  R ]

  zipM  m nm ks as f = zipWith (\k d -> (m ++ k, addName nm $ f d)) ks as
  zipM' m nm ks as f b = zipWith (\k d -> (m ++ k, addName nm $ f d b)) ks as

  tryMsgR x y = sequence_ [(tryMessageWithNoRefreshToCurrent x y), refresh]

  toggleCopyToAll = wsContainingCopies >>= \ws -> case ws of
                                                    [] -> windows copyToAll
                                                    _  -> killAllOtherCopies

  toggleFloat w   = windows (\s -> if M.member w (W.floating s)
                                   then W.sink w s
                                   else W.float w (W.RationalRect (1/3) (1/4) (1/2) (4/5)) s)
  in
    subKeys "System"
    [ (        "M-q"                  , addName "Restart XMonad"                            $ spawn "\"$HOME/.xmonad/rebuild.sh\" && xmonad --restart")
    , (        "M-S-q"                , addName "Quit XMonad"                               $ confirmPrompt P.dangerPrompt "Quit XMonad?" $ io (exitWith ExitSuccess))
    , (        "M-x"                  , addName "Lock screen"                               $ spawn C.lock)
    , (        "M-S-x"                , addName "Switch autolock"                           $ spawn C.autolockToggle)
    ] ^++^

    subKeys "Actions"
    [ (        "M-S-w"                , addName "Capture screen"                            $ spawn C.snapshot)
    ] ^++^

    subKeys "Launchers"
    [ (        "M-<Space>"            , addName "Launcher"                                  $ shellPrompt $ P.autocomplete $ P.fuzzy P.defaultPrompt)
    , (        "M-<Return>"           , addName "Terminal"                                  $ spawn C.terminal)
    , (        "M-S-<Return>"         , addName "Editor"                                    $ spawn C.editor)
    , (        "M-C-<Return>"         , addName "Web browser"                               $ spawn C.webBrowser)
    ] ^++^

    subKeys "Scratchpads"
    (map (\c -> (C.keybind c, addName (C.help c) $ namedScratchpadAction SP.scratchPads (C.name c))) C.namedScratchpads) ^++^

    subKeys "Windows"
    ( [ (      "M-<Backspace>"      , addName "Close window"                               $ kill1)
      , (      "M-S-<Backspace>"    , addName "Kill all windows on current workspace"      $ confirmPrompt P.dangerPrompt "Kill all?" $ killAll)

      , (      "M-S-d"                , addName "Toggle duplicated to all workspaces"        $ toggleCopyToAll)

      , (      "M-p"                , addName "Hide window to stack"                       $ withFocused hideWindow)
      , (      "M-S-p"              , addName "Restore window from stack"                  $ popNewestHiddenWindow)

      , (      "M-m"                , addName "Promote to master"                          $ promote)
      , (      "M1-m"               , addName "Promote to master"                          $ promote)

      , (      "M-z m"              , addName "Focus master"                               $ windows W.focusMaster)
      , (      "M-z u"              , addName "Focus urgent"                               $ focusUrgent)

      , (      "M1-j"               , addName "Focus next window"                          $ windows W.focusDown)
      , (      "M1-k"               , addName "Focus previous window"                      $ windows W.focusUp)
      , (      "M-M1-j"             , addName "Focus previous window"                      $ windows W.swapDown)
      , (      "M-M1-k"             , addName "Focus previous window"                      $ windows W.swapUp)
      , (      "M1-S-j"             , addName "Move previous window to current stack pos"  $ sequence_
                                                                                              [ windows W.swapUp
                                                                                              , windows W.focusDown
                                                                                              ])
      , (      "M1-S-k"             , addName "Move next window to current stack pos"      $ sequence_
                                                                                              [ windows W.focusUp
                                                                                              , windows W.swapDown
                                                                                              ])

      , (      "M-["                , addName "Expand (L on BSP)"                         $ tryMsgR (ExpandTowards L) (Shrink))
      , (      "M-]"                , addName "Expand (R on BSP)"                         $ tryMsgR (ExpandTowards R) (Expand))
      , (      "M-S-["              , addName "Expand (U on BSP)"                         $ tryMsgR (ExpandTowards U) (MirrorShrink))
      , (      "M-S-]"              , addName "Expand (D on BSP)"                         $ tryMsgR (ExpandTowards D) (MirrorExpand))

      , (      "M-M1-["             , addName "Shrink (L on BSP)"                         $ tryMsgR (ShrinkFrom L) (Shrink))
      , (      "M-M1-]"             , addName "Shrink (R on BSP)"                         $ tryMsgR (ShrinkFrom R) (Expand))
      , (      "M-M1-S-["           , addName "Shrink (U on BSP)"                         $ tryMsgR (ShrinkFrom U) (MirrorShrink))
      , (      "M-M1-S-]"           , addName "Shrink (D on BSP)"                         $ tryMsgR (ShrinkFrom D) (MirrorExpand))

      , (      "M-u"                , addName "Remove window from sublayout"              $ withFocused (sendMessage . UnMerge))
      ]

      ++ zipM' "M-"                           "Navigate window"                             dirKeys dirs windowGo   True
      ++ zipM' "M-S-"                         "Move window"                                 dirKeys dirs windowSwap True
      ++ zipM  "M-C-"                         "Merge w/ sublayout"                          dirKeys dirs (sendMessage . pullGroup)
    ) ^++^

    subKeys "Workspaces & Projects"
    ( [
      ]

      ++ zipM "M-"                            "View workspace"                              wsKeys [0..] (withNthWorkspace W.greedyView)
      ++ zipM "M-S-"                          "Move window to workspace"                    wsKeys [0..] (withNthWorkspace W.shift)
      ++ zipM "M-d "                          "Copy window to workspace"                    wsKeys [0..] (withNthWorkspace copy)
      ++ zipM "M-d M-"                        "Copy window to workspace"                    wsKeys [0..] (withNthWorkspace copy)
    ) ^++^

    subKeys "Layout Management"
    [ (       "M-<Tab>"             , addName "Cycle Layouts"                             $ sendMessage NextLayout)
    , (       "M-M1-<Tab>"          , addName "Cycle sublayouts"                          $ toSubl NextLayout)
    , (       "M-S-<Tab>"           , addName "Reset Layout"                              $ setLayout $ XMonad.layoutHook conf)

    , (       "M-t"                 , addName "Toggle float"                              $ withFocused toggleFloat)

    , (       "M-f"                 , addName "Toggle fullscreen"                         $ sequence_ [ (withFocused $ windows . W.sink)
                                                                                                      , (sendMessage $ MT.Toggle FULL)
                                                                                                      ])
    , (       "M-S-f"               , addName "Toggle fake fullscreen"                    $ sequence_ [ (P.sendKey P.noModMask xK_F11)
                                                                                                      , tryMsgR (ExpandTowards L) (Shrink)
                                                                                                      , tryMsgR (ExpandTowards R) (Expand)
                                                                                                      ])
    ]

mousebinds :: (XConfig Layout -> M.Map (ButtonMask, Button) (Window -> X ()))
mousebinds (XConfig {XMonad.modMask = myModMask}) = M.fromList $
  [ ((myModMask, button1), (\w -> mouseWindow position w))
  , ((myModMask, button3), (\w -> mouseWindow resize w))
  ]

-- Display keyboard mappings using zenity
-- from https://github.com/thomasf/dotfiles-thomasf-xmonad/
--              blob/master/.xmonad/lib/XMonad/Config/A00001.hs
showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
    h <- spawnPipe "zenity --text-info --font='SourceCode Pro'"
    hPutStr h (unlines $ showKm x)
    hClose h
    return ()
