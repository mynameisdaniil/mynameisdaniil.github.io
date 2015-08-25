import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import Signal exposing (..)

type alias Model = {}

type Action = NoOp

update : Action -> Model -> Model
update action model = model

view : Model -> Html
view model =
  div [] [
    h1 [] [text "Daniil Sobol"],
    h6 [] [a [href "https://github.com/mynameisdaniil/mynameisdaniil.github.io/blob/master/src/Main.elm"][ text "This site is functional and reactive" ]]
    ]

model : Signal Model
model = Signal.constant {}

main : Signal Html
main = view <~ model
