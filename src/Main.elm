module Main exposing (main)

import Browser exposing (Document)
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

overhead = 7

type alias Link        = String
type alias Icon        = String
type alias Description = String

type Msg = YamlLoaded (Result Http.Error String) | RandomList (List Int) | None

type alias Model = {facts : List String, social : List (Link, Icon), links: List (Description, Link), email: List String, randomness: List Int}

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

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

init : () -> (Model, Cmd Msg)
init _ =
  let
    initialState = {facts = [], social = [], links = [], email = ["sobol", ".", "daniil", "@", "gmail", ".", "com"], randomness = []}
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

view : Model -> Document Msg
view model = {title = "Hello world", body = body model}

body : Model -> List (Html msg)
body model =
  let
      blocks = [display_facts model.facts, display_social model.social, display_links model.links, display_email model.email, display_source_code]
      (shuffled_blocks, residual_randomness) = foldl (\block (acc, randomness) ->
        (shuffle block randomness :: acc, drop (length block) randomness)
        ) ([], model.randomness) blocks
  in
    map (div body_style) <| shuffle shuffled_blocks residual_randomness

make_style = map (\(a, b) -> style a b)

body_style = make_style [
  ("border-radius", "5pt"),
  ("border", "1pt solid #000000"),
  ("padding", "10pt"),
  ("margin", "10pt"),
  ("display", "inline-block")
  ]

display_facts = map (\str -> p facts_style [ text str ])

facts_style = make_style [
  ("height", "1.5em")
  ]

display_social = map (\(link, icon) -> p [] [img (src icon :: social_img_style) [], a (href link :: social_link_style) [text link] ])

social_img_style = make_style [
  ("height", "2em"),
  ("vertical-align", "middle")
  ]

social_link_style = make_style [
  ("height", "1.5em"),
  ("vertical-align", "middle")
  ]

display_links = map (\(description, link) -> p links_style [ text description, text ": ", a [href link] [text link] ])

links_style = make_style [
  ("height", "1.5em")
  ]

display_email parts = [div email_style [text "The best way to contact me: ", a [href <| concat <| "mailto:" :: parts] [text <| concat parts]]]

email_style = make_style [
  ("height", "1.5em")
  ]

source_code_link = "https://github.com/mynameisdaniil/mynameisdaniil.github.io/blob/master/src/Main.elm"

display_source_code = [div source_code_style [text "This page is written in Elm: ", a [href source_code_link] [text source_code_link]]]

source_code_style = make_style [
  ("height", "1.5em")
  ]
