{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleContexts #-}
module Config.Layouts
( layoutHook
) where

import qualified Config.Defaults as C
import qualified Config.Themes   as T

import XMonad ((|||), Typeable, Window)
import XMonad.Hooks.ManageDocks
import XMonad.Layout
import XMonad.Layout.Accordion
import XMonad.Layout.AutoMaster
import XMonad.Layout.BoringWindows
import XMonad.Layout.ComboP
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import XMonad.Layout.Hidden
import XMonad.Layout.LayoutBuilder
import XMonad.Layout.Master
import qualified XMonad.Layout.MultiToggle as MT
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.PerScreen
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation

barFull = avoidStruts $ Simplest

data FULLBAR = FULLBAR deriving (Read, Show, Eq, Typeable)
instance MT.Transformer FULLBAR Window where
    transform FULLBAR x k = k barFull (\_ -> x)

layoutHook = fullscreenFloat
           $ fullScreenToggle
           $ fullBarToggle
           $ mirrorToggle
           $ reflectToggle
           $ onWorkspaces (map show [C.WEB, C.QUTE]) tallAccordion
           $ (flex ||| tallAccordion ||| tabs)
  where
    reflectToggle     = MT.mkToggle (MT.single REFLECTX)
    mirrorToggle      = MT.mkToggle (MT.single MIRROR)
    fullBarToggle     = MT.mkToggle (MT.single FULLBAR)
    fullScreenToggle  = MT.mkToggle (MT.single FULL)
    defaultModifiers l = avoidStruts
                       $ addTopBar
                       $ hiddenWindows l

    named n           = renamed [(XMonad.Layout.Renamed.Replace       n)]
    trimNamed w n     = renamed [(XMonad.Layout.Renamed.CutWordsLeft  w),
                                 (XMonad.Layout.Renamed.PrependWords  n)]
    suffixed n        = renamed [(XMonad.Layout.Renamed.AppendWords   n)]
    trimSuffixed w n  = renamed [(XMonad.Layout.Renamed.CutWordsRight w),
                                 (XMonad.Layout.Renamed.AppendWords   n)]

    addTopBar         = noFrillsDeco shrinkText T.topBarTheme

    borderToIntList b = map fromInteger [top b, bottom b, left b, right b]

    mySpacing         = spacingRaw C.smartGaps C.outerGap C.isOuterGap C.innerGap C.isInnerGap
    sGap g            = quot g 2
    myGaps            = gaps $ zip [U, D, L, R] $ borderToIntList C.innerGap
    mySmallGaps       = gaps $ zip [U, D, L, R] $ map sGap $ borderToIntList C.innerGap
    myBigGaps         = gaps $ zip [U, D, L, R] $ map (*2) $ borderToIntList C.innerGap

    tabs =
        named "Tabs"
      $ defaultModifiers
      $ addTabs shrinkText T.tabTheme
      $ Simplest

    tallAccordion =
        named "Tall Accordion"
      $ defaultModifiers
      $ Mirror $ autoMaster 1 (2/100)
      $ Mirror Accordion
    -- tallAccordion' =
    --     named "Tall Accordion"
    --   $ defaultModifiers
    --   $ layoutN 1 (relBox (2/5) 0 1     1) (Just $ relBox 0 0 1 1) Full
    --   $ layoutAll (relBox 0     0 (2/5) 1)                         Accordion

    flex =
        named "Flex"
      -- don't forget: even though we are using X.A.Navigation2D
      -- we need windowNavigation for merging to sublayouts
      $ windowNavigation
      $ defaultModifiers
      $ addTabs shrinkText T.tabTheme
      -- $ subLayout [] (Simplest ||| (mySpacing $ Accordion))
      $ subLayout [] (Simplest ||| Accordion)
      $ ifWider C.smallMonWidth wideLayouts standardLayouts
      where
        wideLayouts = myGaps $ mySpacing
            $ (suffixed "Wide 3Col" $ ThreeColMid 1 (1/20) (1/2))
        --  ||| (trimSuffixed 1 "Wide BSP" $ emptyBSP)
        --  ||| fullTabs
        standardLayouts = myGaps $ mySpacing
            $ (suffixed "Std 2/3" $ ResizableTall 1 (1/20) (2/3) [])
          ||| (suffixed "Std 1/2" $ ResizableTall 1 (1/20) (1/2) [])

    masterTabbedDynamic =
        named "Master-Tabbed Dynamic"
      $ defaultModifiers
      $ ifWider C.smallMonWidth masterTabbedWide masterTabbedStd
      where
        masterTabbedStd =
            named "Master-Tabbed Standard"
          $ myBigGaps
          $ mastered (1/100) (2/3)
          $ gaps [(U, 0),(D, 0),(L, (fromInteger $ bottom C.innerGap)*2),(R, 0)]
          $ tabbed shrinkText T.tabTheme
        masterTabbedWide =
            named "Master-Tabbed Wide"
          $ myBigGaps
          $ mastered (1/100) (1/4)
          $ gaps [(U, 0),(D, 0),(L, (fromInteger $ bottom C.innerGap)*2),(R, 0)]
          $ mastered (1/100) (2/3)
          $ gaps [(U, 0),(D, 0),(L, (fromInteger $ bottom C.innerGap)*2),(R, 0)]
          $ tabbed shrinkText T.tabTheme

    smartTallTabbedDynamic =
        named "Smart Tall-Tabbed"
      $ defaultModifiers
      $ ifWider C.smallMonWidth wideScreen normalScreen
      where
        wideScreen   = combineTwoP (TwoPane 0.03 (3/4))
                        (smartTall)
                        (smartTabbed)
                        -- (ClassName "Chromium-browser-chromium")
                        (ClassName "Firefox")
        normalScreen = combineTwoP (TwoPane 0.03 (2/3))
                        (smartTall)
                        (smartTabbed)
                        -- (ClassName "Chromium-browser-chromium")
                        (ClassName "Firefox")
        smartTall =
            named "Smart Tall"
          $ mySpacing
          $ myGaps
          $ boringAuto
          $ ifWider C.smallMonWidth tallWideScreen tallNormalScreen
          where
            tallWideScreen = reflectHoriz $ Tall 1 0.03 (2/3)
            tallNormalScreen = Mirror $ Tall 1 0.03 (4/5)
        smartTabbed =
            named "Smart Tabbed"
          $ myGaps
          $ tabbed shrinkText T.tabTheme
