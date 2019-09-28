module Main exposing (..)

import Browser
import Html exposing (div, h1, input, main_, p, text)
import Html.Attributes exposing (placeholder, type_)
import Html.Events exposing (onInput)
import Mark


main =
    Browser.sandbox
        { init =
            { searchTerm = "Assi"
            , options = Mark.defaultOptions
            }
        , update = update
        , view = view
        }


type Msg
    = SearchTermChange String


update msg model =
    case msg of
        SearchTermChange val ->
            { model | searchTerm = val }


viewOption options =
    text ""


searchText =
    """"You can't, if you can't feel it, if it never
Rises from the soul, and sways
The heart of every single hearer,
With deepest power, in simple ways.
You'll sit forever, gluing things together,
Cooking up a stew from other's scraps,
Blowing on a miserable fire,
Made from your heap of dying ash.
Let apes and children praise your art,
If their admiration's to your taste,
But you'll never speak from heart to heart,
Unless it rises up from your heart's space." """


view model =
    main_ []
        [ h1 [] [ text "Elm-Mark Demo" ]
        , viewOption model
        , div [] [ input [ type_ "text", onInput SearchTermChange, placeholder "Search the text below" ] [] ]
        , p [] <| Mark.markWith model.options model.searchTerm searchText
        , p [] [ text "Johann Wolfgang von Goethe, Faust, First Part" ]
        ]
