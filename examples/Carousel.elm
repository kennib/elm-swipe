import Array exposing (Array)
import Html exposing (Html, div, h1, img, text)
import Swipe exposing (..)

carousel : Array Html
carousel = Array.fromList
    [ div [] [h1 [] [text "1 - Elm"]]
    , div [] [h1 [] [text "2 - Oak"]]
    , div [] [h1 [] [text "3 - Birch"]]
    , div [] [h1 [] [text "4 - Spruce"]]
    ]

{- Watch for swipes (as index changes) and then update based on the swipes -}
main = Signal.map view <| Signal.foldp update 0 indexChanges

{- Update the index based on some value, and loop around the edges -}
update : Int -> Int -> Int
update change index = let
        updated = index + change
    in
        if updated < 0 then
            Array.length carousel - 1
        else if updated >= Array.length carousel then
            0
        else
            updated

{- Transform the first of each set of swipes into changes on the carousel's index -}
indexChanges : Signal Int
indexChanges = Signal.map (\swipes -> case swipes of
        swipe::_ -> indexChange swipe
        []       -> 0
    ) <| Signal.dropRepeats swipes

{- Swiping left or right will change the index of the carousel -}
indexChange : Swipe -> Int
indexChange swipe = case swipe.direction of
    Swipe.Up    -> 0
    Swipe.Down  -> 0
    Swipe.Left  -> 1
    Swipe.Right -> -1 

{- Display the current index of the carousel -}
view index = case Array.get index carousel of
    Just item -> item
    Nothing   -> div [] [h1 [] [text "The carousel is empty!"]]
