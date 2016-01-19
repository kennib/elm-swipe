import Graphics.Element exposing (..)
import Swipe

main : Signal Element
main = Signal.map show Swipe.swipes
