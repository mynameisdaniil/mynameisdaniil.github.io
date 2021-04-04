module Main exposing (main)

import Browser
import Dict exposing (get)
import Html exposing (Html, img, div, text, p, a)
import Html.Attributes exposing (href, src, width, height, style)
import Http
import List exposing (map, map2, foldl, drop, sortWith, length)
import Maybe exposing (withDefault)
import Random exposing (Generator)
import String exposing (concat)
import Task
import Time
import Tuple exposing (pair, first, second)
import Yaml.Decode exposing (map3, fromString, list, dict, string, field)

overhead = 5
email = concat ["sobol", ".", "daniil", "@", "gmail", ".", "com"] -- fuck spammers

type alias Link        = String
type alias Icon        = String
type alias Description = String

type Msg = YamlLoaded (Result Http.Error String) | RandomList (List Int) | None

type alias Model = {facts : List String, social : List (Link, Icon), links: List (Description, Link), randomness: List Int}

tuple_extractor a b = \v -> (withDefault "" <| get a v, withDefault "" <| get b v)

decoder model =
  map3 (\facts social links ->
    {model | facts  = facts,
             social = map (tuple_extractor "link" "icon") social,
             links  = map (tuple_extractor "description" "link") links
    })
  (field "facts" <| list string)
  (field "social" <| list (dict string))
  (field "links" <| list (dict string))

zip : List a -> List b -> List (a, b)
zip = map2 pair

shuffle : List a -> List comparable -> List a
shuffle list comparable = map first <| sortWith (\(_, a) (_, b) -> compare a b) <| zip list comparable

main : Program () Model Msg
main =
  Browser.document { init = init, update = update, view = view, subscriptions = subscriptions }

subscriptions model = Sub.none

init : () -> (Model, Cmd Msg)
init _ =
  let
    initialState = {facts = [], social = [], links = [], randomness = []}
    commands = Cmd.batch [
      Http.get {url = "../contents.yaml", expect = Http.expectString YamlLoaded}
      ]
  in
    (initialState, commands)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    YamlLoaded result ->
      case result of
        Ok yamlStr ->
          case fromString (decoder model) yamlStr of
            Ok updatedModel ->
              let
                  amountOfRandomness = length updatedModel.facts + length updatedModel.social + length updatedModel.links + overhead
                  generator = Random.list amountOfRandomness (Random.int 0 amountOfRandomness)
              in
                  (updatedModel, Random.generate RandomList generator)
            Err e -> (model, Cmd.none)
        Err e -> (model, Cmd.none)
    RandomList randomList ->
        ({model | randomness = randomList}, Cmd.none)
    None -> (model, Cmd.none)

view model = {title = "Hello world", body = body model}

body model =
  let
      email_block = [div [] [text "The best way to contact me: ", a [href <| concat ["mailto:", email]] [text email]]]
      blocks = [display_facts model.facts, display_social model.social, display_links model.links, email_block]
      (shuffled_blocks, residual_randomness) = foldl (\block (acc, randomness) ->
        (shuffle block randomness :: acc, drop (length block) randomness)
        ) ([], model.randomness) blocks
  in
    map (div [style "border-radius" "5pt", style "border" "1pt solid #000000", style "padding" "10pt", style "margin" "10pt", style "display" "block", style "position" "absolute"]) <| shuffle shuffled_blocks residual_randomness

display_facts = map (\str -> p [] [ text str ])

display_social = map (\(link, icon) -> p [] [img [src icon, style "height" "20pt", style "vertical-align" "middle"] [], a [href link, style "height" "14pt", style "vertical-align" "middle"] [text link] ])

display_links = map (\(description, link) -> p [] [ text description, text ": ", a [href link] [text link] ])
