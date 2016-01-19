module Swipe (Swipe, swipes, Direction) where
{-| The Swipe package allows you to access swipe gestures from the browser

# Swipes

@docs Swipe, Direction

To do this we just map the in-built touch signal to a series of swipes

@docs swipes

-}

import Signal exposing (Signal, map)
import Time exposing (Time)
import List exposing (map)
import Touch exposing (Touch, touches)

{-| Every `Swipe` has `xy` coordinates. It also has an identifier `id` to distinguish one touch from another. 

A swipe also has a `Direction` which indicates the direction the user made contact, moved, and then stopped making contact.

Swipes also have a start time (`t0`) and an end time (`t1`).
-}
type alias Swipe =
    { x : Int
    , y : Int
    , id : Int
    , direction : Direction
    , t0 : Time
    , t1 : Time
    }

{-| A swipe can go up, down, left or right. -}
type Direction = Up | Down | Left | Right

{-| A list of ongoing swipes. -}
swipes : Signal (List Swipe)
swipes = let
        f touches = List.map g touches
        g touch =
            { x = touch.x
            , y = touch.y
            , id = touch.id
            , direction = Up
            , t0 = touch.t0
            , t1 = touch.t0
            }
    in
        Signal.map f touches
