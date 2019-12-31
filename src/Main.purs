module Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Hello as Hello
import Web.HTML (window)
import Web.HTML.Window as Window

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  window <- H.liftEffect window
  width <- H.liftEffect $ Window.innerWidth window
  height <- H.liftEffect $ Window.innerHeight window
  let input = { width, height }
  runUI Hello.component input body