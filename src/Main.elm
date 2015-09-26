module Main where

import Html exposing (..)
import Html.Attributes exposing (..)
import Signal exposing (Signal, (<~))
import List exposing (map, map2, sortWith)
import Random exposing (Seed)
import Signal.Time exposing (Time, startTime)
import Mouse

type alias Model = List String

type Action = NoOp

view : Model -> Html
view model =
  div [] [
    a [href "mailto:sobol.daniil@gmail.com"] [h1 [] [text "Daniil Sobol"]],
    p  [] [
      h2 [] [text "Set of random facts about me:"],
      ul [] <| map (\line -> li [] [text line]) model
      ],
    h6 [] [a [href "https://github.com/mynameisdaniil/mynameisdaniil.github.io/blob/master/src/Main.elm"][ text "This site is functional and reactive too" ]]
    ]

model = [
  "Master's degree in signal processing",
  "Full-time linux user since Ubuntu 8.04",
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
  "Wrote this line (and all other lines) using VIM",
  "RPC? Ask me how",
  "I hate DNS",
  "Engineer",
  "Good guy"
  ]

shuffle : List String -> Seed -> List String
shuffle model seed =
  let len = List.length model
      randomList len seed =
        let (list, _) = (Random.generate (Random.list len (Random.int 0 len)) seed)
        in
           list
  in
      map (\(_, line) -> line)
      <| sortWith (\(a, _) (b, _) -> compare a b)
      <| map2 (\i line -> (i, line)) (randomList len seed) model

update : (Int, Int) -> Model
update (x, y) = shuffle model (Random.initialSeed <| x + y)

main : Signal Html
main = view <~ (update <~ Mouse.position)
