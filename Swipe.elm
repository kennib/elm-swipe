module Swipe (Swipe, swipes, SwipeState(Start, Swiping, End), SwipeStart, swipeStates, Direction(Up, Down, Left, Right)) where
{-| The Swipe package allows you to access swipe gestures from the browser

# Swipes

@docs Swipe, Direction

@docs swipes

# Swipe States

@docs SwipeState, SwipeStart, Swipe

@docs swipeStates
-}

import Time exposing (Time)
import Touch exposing (Touch, touches)

{-| Every `Swipe` has start and end `xy` coordinates. It also has an identifier `id` to distinguish one touch from another.

A swipe also has a `Direction` which indicates the direction the user made contact, moved, and then stopped making contact.

Swipes also have a start time (`t0`) and an end time (`t1`).
-}
type alias Swipe =
    { x0 : Int
    , y0 : Int
    , x1 : Int
    , y1 : Int
    , id : Int
    , direction : Direction
    , t0 : Time
    }

{-| A `SwipeStart` is the initial touch for a swipe when the direction and end-time of the swipe is unknown. -}
type alias SwipeStart =
    { x : Int
    , y : Int
    , id : Int
    , t0 : Time
    }

{-| Swiping can be divided into threo states.
The initial touch of a swipe, currently swiping and end of the swipe. -}
type SwipeState = Start SwipeStart | Swiping Swipe | End Swipe

{-| A swipe can go up, down, left or right. -}
type Direction = Up | Down | Left | Right

{-| The direction of a swipe as determined by differences in `x` and `y` positions -}
direction : Int -> Int -> Maybe Direction
direction dx dy =
    if abs dx > abs dy then
        if dx > 0 then
            Just Right
        else if dx < 0 then
            Just Left
        else
            Nothing
    else
        if dy > 0 then
            Just Down
        else if dy < 0 then
            Just Up
        else
            Nothing

{-| The id of the swipe state -}
swipeId state = case state of
    Start   swipe -> swipe.id
    Swiping swipe -> swipe.id
    End     swipe -> swipe.id

{-| A list of the current state of swipes. -}
swipeStates : Signal (List SwipeState)
swipeStates = let
        -- Update swipe states with the touch of the same id
        updates touches states =
            -- Update swipes with new touch points, if they exist
            List.concatMap (\state ->
                let
                    newTouches = List.filter (\t -> swipeId state == t.id) touches
                in
                    if List.isEmpty newTouches then
                        case state of
                            Start   swipe -> [] -- This was just a touch
                            Swiping swipe -> [End swipe] -- This swipe has ended
                            End     swipe -> [] -- This swipe is done
                    else
                        List.filterMap (\touch -> update touch state) newTouches
            ) states
            ++
            -- Touches not seen yet will become the start of swipes
            (List.map (\touch -> Start
                { x = touch.x
                , y = touch.y
                , id = touch.id
                , t0 = touch.t0
                }
            ) <| List.filter (\touch ->
                not <| List.any (\s -> swipeId s == touch.id) states
            ) touches)
    in
        Signal.foldp updates [] touches

{-| Update a swipe state with a new touch point.
A swipe may end if the user stops swiping. -}
update : Touch -> SwipeState -> Maybe SwipeState
update touch swipeState = case swipeState of
    Start start ->
        case direction (touch.x - start.x) (touch.y - start.y) of
            Just dir -> Just <| Swiping
                { x0 = start.x
                , y0 = start.y
                , x1 = touch.x
                , y1 = touch.y
                , id = start.id
                , direction = dir
                , t0 = start.t0
                }
            Nothing -> Nothing
    Swiping swipe ->
        let
            fromStartDir = direction (touch.x - swipe.x0) (touch.y - swipe.y0)
            fromEndDir = direction (touch.x - swipe.x1) (touch.y - swipe.y1)
            sameDir = case (fromStartDir, fromEndDir) of
                (Just start, Just end) -> start == end
                (_, _) -> False
        in
            if sameDir then
                Just <| Swiping
                    { x0 = swipe.x0
                    , y0 = swipe.y0
                    , x1 = touch.x
                    , y1 = touch.y
                    , id = swipe.id
                    , direction = swipe.direction
                    , t0 = swipe.t0
                    }
            else
                Just <| End
                    { x0 = swipe.x0
                    , y0 = swipe.y0
                    , x1 = touch.x
                    , y1 = touch.y
                    , id = swipe.id
                    , direction = swipe.direction
                    , t0 = swipe.t0
                    }
    End swipe -> Just <| End swipe


{-| A list of completed swipes. -}
swipes : Signal (List Swipe)
swipes = let
        isSwiping swipe = case swipe of
            Start   swipe -> Nothing
            Swiping swipe -> Nothing
            End     swipe -> Just swipe
    in
        Signal.map (List.filterMap isSwiping) swipeStates
