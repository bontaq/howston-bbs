{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
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

import Lens.Micro
import Lens.Micro.TH
import Network.Wreq
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO)
import Lib

data User = NameField
          | PasswordField
          | LoginField
          | RegisterField
          deriving (Eq, Ord, Show)

data Action = Login | Register
              deriving (Eq, Show)

data UserInfo = UserInfo {
  _name :: T.Text
  , _password :: T.Text
  , _action :: Action
  }

makeLenses ''UserInfo

mkForm :: UserInfo -> Form UserInfo e User
mkForm =
  let label s w = padBottom (Pad 1) $
        (vLimit 1 $ hLimit 50 $ str s <+> fill ' ' <+> (hLimit 25 $ w))
  in newForm [
    label "Name" @@= editTextField name NameField (Just 1)
    , label "Password" @@= editTextField password PasswordField (Just 2)
    , (padTop (Pad 1) . hCenter) @@= setFieldConcat hBox . radioField action
       [ (Login, LoginField, "Login         ")
       , (Register, RegisterField, "Register") ]
    ]

globalDefault = V.green `on` V.rgbColor 255 255 204

theMap :: AttrMap
theMap = attrMap globalDefault
  [ (E.editAttr, V.rgbColor 0 153 153 `on` V.rgbColor 255 255 255)
  , (E.editFocusedAttr, V.rgbColor 0 153 153 `on` V.rgbColor 255 255 255) ]

draw :: Form UserInfo e User -> [Widget User]
draw f = [C.vCenter $ C.hCenter form]
  where
    form = B.borderWithLabel (str "Welcome to Howston BBS") $
      padTop (Pad 2) $ padAll 1 $ hLimit 50 $ renderForm f

postLogin :: UserInfo -> IO ()
postLogin s = do
  let opts = defaults -- & param "foo" .~ ["bar"]
  postWith opts "http://localhost:3000/login" [ "username" := (s ^. name)
                                              , "password" := (s ^. password) ]
  pure ()

handleEvent ::
  Eq n =>
  Form UserInfo e n
  -> BrickEvent n e -> EventM n (Next (Form UserInfo e n))
handleEvent s ev = case ev of
  VtyEvent (V.EvResize {}) -> continue s
  VtyEvent (V.EvKey V.KEsc []) -> halt s
  VtyEvent (V.EvKey V.KEnter []) -> do
    liftIO $ postLogin (formState s)
    continue s
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
        let v16Color = v { outputIface = (V.outputIface v) { contextColorCount = 16 } }
        pure v16Color

      initialUserInfo = UserInfo {
        _name = ""
        , _password = ""
        , _action = Login
        }

      f = mkForm initialUserInfo

  initialVty <- buildVty
  f' <- customMain initialVty buildVty Nothing app f

  pure ()
