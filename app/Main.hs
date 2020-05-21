{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import qualified Data.Text as T
import Brick
import Brick.Forms
import Brick.Focus
import Brick.Widgets.Edit   as E
import Brick.Widgets.Center as C
import Brick.Widgets.Border as B
import Graphics.Vty as V

import Lens.Micro.TH

import Lib

data User = NameField
          | PasswordField
          deriving (Eq, Ord, Show)

data UserInfo = UserInfo {
  _name :: T.Text
  , _password :: T.Text
  }

makeLenses ''UserInfo

mkForm :: UserInfo -> Form UserInfo e User
mkForm =
  let label s w = padBottom (Pad 1) $
        (vLimit 1 $ hLimit 50 $ str s <+> fill ' ' <+> w)
  in newForm [
    label "Name" @@= editTextField name NameField (Just 1)
    , label "Password" @@= editTextField password PasswordField (Just 2)
    ]

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.black `on` V.yellow) ]

draw :: Form UserInfo e User -> [Widget User]
draw f = [C.vCenter $ C.hCenter form]
  where
    form = B.border $ padTop (Pad 1) $ hLimit 50 $ renderForm f

handleEvent s ev = case ev of
  VtyEvent (V.EvResize {}) -> continue s
  VtyEvent (V.EvKey V.KEsc []) -> halt s
  -- VtyEvent (V.EvKey V.KEnter [])
  _ -> do
    s' <- handleFormEvent ev s
    continue s'

app :: App (Form UserInfo e User) e User
app =
  App { appDraw = draw
      , appHandleEvent = handleEvent
      , appChooseCursor = focusRingCursor formFocus
      , appStartEvent = return
      , appAttrMap = const theMap }

main :: IO ()
main = do
  let buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        pure v

      initialUserInfo = UserInfo {
        _name = ""
        , _password = ""
        }

      f = mkForm initialUserInfo

  initialVty <- buildVty
  f' <- customMain initialVty buildVty Nothing app f

  pure ()
