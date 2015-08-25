module Main where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import Signal exposing (..)
import List exposing (map)
import Random exposing (Seed, int)
import Signal.Time exposing (..)

type alias Model = List String

type Action = NoOp

--port getRandomRange : Int -> Int -> Int

update : Action -> Model -> Model
update action model = model

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
  "/dev/ops",
  "Have expirience in nodejs native modules writing",
  "Do love syntax of Erlang",
  "Pretty reactive",
  "Very functional",
  "My other CAP is a theorem",
  "I do understand Riak's code",
  "I lost my ability to explain monads",
  "Can't decide if I dislike Java more than C++ or vise versa"
  ]

model : Time -> Model
model time = shuffle data 0 (Random.initialSeed <| round time)

shuffle : List String -> Int -> Seed -> List String
shuffle data i seed =
  let len = List.length data
  in 
     case i <= len of
       False -> data
       True  ->
         let (pivot, newSeed) = Random.generate (int 0 len) seed
             head = List.take pivot data
             tail = List.drop pivot data
         in
            shuffle (List.append tail head) (i + 1) newSeed



main : Signal Html
main = view <~ (model <~ startTime)
