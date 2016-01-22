import Html exposing (Html, div, h1, h2, ul, li, text)
import Html.Attributes as Attr exposing (..)
import Swipe exposing (..)

type alias AppState a =
    { items : List a
    , favourites : List a
    , animation : Animation
    , favourited : Maybe Bool
    }

type alias Animation =
    { fade : Float
    , position : Int
    }

{- Update the app based on swipes and then view the result -}
main = Signal.map view
    <| Signal.foldp (\swipe state -> update <| updateSwipe swipe state) startState 
    <| Signal.map List.head swipeStates

{- Initially start with a full list of items and no favourites -}
startState : AppState String
startState =
    { items = names
    , favourites = []
    , favourited = Nothing
    , animation = startAnimation
    }

startAnimation : Animation
startAnimation =
    { fade = 0
    , position = 0
    }

names = ["Mario", "Luigi", "Chris", "Robin", "Julia", "Paula"]

{- Show the list of people and favourites -}
view state = div []
    [ div [style itemsLayout] [items state]
    , div [] [favourites state]
    ]

itemsLayout =
    [ ("width", "100%") 
    , ("overflow", "hidden")
    , ("text-align", "center")
    , ("background-color", "#eee")
    ]

items state = div [style itemsStyle] <|
    Maybe.withDefault [text "No more items"] <|
        Maybe.map2 (++)
        (Maybe.map (\items -> List.map (itemView 0 0) items) <| Maybe.map List.reverse <| List.tail state.items)
        (Maybe.map (\item  -> [itemView state.animation.fade state.animation.position item]) <| List.head state.items)

itemView fade position name = div [style <| itemStyle fade position] [h1 [] [text name]]

itemsStyle =
    [ ("display", "inline-block")
    , ("position", "relative")
    , ("height", "200px")
    , ("width", "200px")
    , ("padding", "30px")
    , ("user-select", "none")
    , ("-webkit-user-select", "none")
    , ("text-align", "center")
    ]

itemStyle fade position =
    [ ("position", "absolute")
    , ("display", "inline-block")
    , ("background", "white")
    , ("width", "200px")
    , ("height", "200px")
    , ("margin", "30px")
    , ("text-align", "center")
    , ("top", "0px")
    , ("left",    toString position ++ "px")
    , ("opacity", toString <| 1.0 - fade)
    ]

favourites state = div []
    [ h2 [] [text "Favourites"] 
    , ul []
        <| List.map (\item -> li [] [text item]) state.favourites
    ]

{- Update animation completion -}
update : AppState a -> AppState a
update state = case state.favourited of
    Just _ ->
        { state |
          items = Maybe.withDefault [] <| List.tail state.items
        , favourites = case List.head state.items of
            Just item -> if Maybe.withDefault False state.favourited then
                item :: state.favourites
            else
                state.favourites
            Nothing -> state.favourites
        , animation = startAnimation
        , favourited = Nothing
        }
    Nothing ->
        state

{- Update based on swipe -}
updateSwipe : Maybe SwipeState -> AppState a -> AppState a
updateSwipe swipe state = case swipe of
    Just (Start swipe) ->
        state
    Just (Swiping swipe) ->
        { state |
          animation =
            { fade = toFloat (swipe.x1 - swipe.x0) / 300
            , position = swipe.x1 - swipe.x0
            }
        }
    Just (End swipe) -> if abs (swipe.x1 - swipe.x0) < 100 then
            { state |
              animation =
                { fade = 0
                , position = 0
                }
            , favourited = Nothing
            }
        else
            case swipe.direction of
                Left -> 
                    { state |
                      animation =
                        { fade = 1.0
                        , position = swipe.x1 - swipe.x0
                        }
                    , favourited = Just False
                    }
                Right ->
                    { state |
                      animation =
                        { fade = 1.0
                        , position = swipe.x1 - swipe.x0
                        }
                    , favourited = Just True
                    }
                _ ->
                state
    Nothing ->
        state
