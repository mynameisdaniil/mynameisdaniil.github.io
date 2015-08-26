module Main where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import Signal exposing (..)
import List exposing (map)
import Random exposing (Seed, int)
import Signal.Time exposing (..)
import Debug exposing (..)

type alias Model = List String

type Action = NoOp

view : Model -> Html
view model =
  div [] [
    h1 [] [text "Daniil Sobol"],
    p  [] [
      h2 [] [text "Set of random facts about me:"],
      ul [] <| List.map (\line -> li [] [text line]) model
      ],
    h6 [] [a [href "https://github.com/mynameisdaniil/mynameisdaniil.github.io/blob/master/src/Main.elm"][ text "This site is functional and reactive too" ]]
    ]

data = [
  "Master's degree in signal processing",
  "All-time linux user since Ubuntu 8.04",
  "Have expirience in nginx module writing",
  "I have /dev/ops which is recursive link to myself",
  "Have expirience in nodejs native modules writing",
  "Do love syntax of Erlang",
  "Pretty reactive",
  "Very functional",
  "My other CAP is a theorem",
  "I do understand Riak's code",
  "I've lost my ability to explain monads",
  "Can't decide if I dislike Java more than C++ or vise versa",
  "Wrote this line at 3:43 AM",
  "Wrote several lisp interpreters",
  "I've used Ansible back then before it became mainstream",
  "Containers' pioneer (and I'm not talking about Docker)",
  "Can't advise you continuous integration server",
  "Engineer",
  "Good guy"
  ]

model : Time -> Model
model time = shuffle data (Random.initialSeed <| round time)

shuffle : List String -> Seed -> List String
shuffle data seed =
  let len = List.length data
      randomList len seed =
        let (list, _) = (Random.generate (Random.list len (Random.int 0 len)) seed)
        in
           list
  in
      List.map (\(_, line) -> line)
      <| List.sortWith (\(a, _) (b, _) -> compare a b)
      <| List.map2 (\i line -> (i, line)) (randomList len seed) data

main : Signal Html
main = view <~ (model <~ startTime)
