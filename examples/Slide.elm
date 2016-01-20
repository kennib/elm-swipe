import Html exposing (Html, div, h1, h2, text)
import Html.Attributes as Attr exposing (..)
import Swipe exposing (..)

main = Signal.map (slide item) <| Signal.map List.head swipeStates

item = div [style itemStyle] content

itemStyle =
    [ ("position", "relative")
    , ("width", "200px")
    , ("height", "200px")
    , ("padding", "10px")
    , ("text-align", "center")
    , ("background-color", "#222")
    ]

content =
    [ h1 [ style [("color", "white")]] [text "Swipe me"]
    ]

slide : Html -> Maybe SwipeState -> Html
slide item swipeState = case swipeState of
    Just (Start   swipe) -> item
    Just (Swiping swipe) -> div [style <| itemStyle ++ [("left", toString (swipe.x1 - swipe.x0) ++ "px")]] content
    Just (End     swipe) -> div [style itemStyle] <| content ++ [h2 [style [("color", "white")]] [text <| toString swipe.direction]]
    Nothing              -> item
