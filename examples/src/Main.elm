module Main exposing (..)

import Browser
import Html
    exposing
        ( blockquote
        , button
        , div
        , h1
        , h2
        , input
        , label
        , main_
        , p
        , small
        , text
        )
import Html.Attributes
    exposing
        ( autofocus
        , class
        , classList
        , href
        , placeholder
        , rel
        , type_
        , value
        )
import Html.Events exposing (onClick, onInput)
import String.Mark as Mark


main =
    Browser.sandbox
        { init =
            { searchTerm = "heart"
            , caseSensitivity = Mark.ignoreCase
            , whitespace = Mark.singleWord
            , minTermLength = 3
            }
        , update = update
        , view = view
        }


type Msg
    = SearchTermChange String
    | CaseChanged Mark.Case
    | WhitespaceChanged Mark.Whitespace
    | MinTermLengthChanged Int
    | NoOp


update msg model =
    case msg of
        SearchTermChange val ->
            { model | searchTerm = val }

        CaseChanged case_ ->
            { model | caseSensitivity = case_ }

        WhitespaceChanged whitespace ->
            { model | whitespace = whitespace }

        MinTermLengthChanged minTermLength ->
            { model | minTermLength = minTermLength }

        NoOp ->
            model


viewOption options =
    text ""


searchText =
    String.join " "
        [ "You can't, if you can't feel it, if it never"
        , "Rises from the soul, and sways"
        , "The heart of every single hearer,"
        , "With deepest power, in simple ways."
        , "You'll sit forever, gluing things together,"
        , "Cooking up a stew from other's scraps,"
        , "Blowing on a miserable fire,"
        , "Made from your heap of dying ash."
        , "Let apes and children praise your art,"
        , "If their admiration's to your taste,"
        , "But you'll never speak from heart to heart,"
        , "Unless it rises up from your heart's space."
        ]


optionButton { clickMsg, name, selected } =
    label
        []
        [ button
            [ class "p-4"
            , onClick clickMsg
            , classList
                [ ( "bg-teal-400 hover:bg-teal-500", selected )
                , ( "bg-gray-200 hover:bg-gray-300", not selected )
                ]
            ]
            [ text name ]
        ]


optionGroup title children =
    div [ class "flex flex-col border-2 border-teal-400" ]
        [ h2 [ class "text-center text-2xl text-teal-400 border-b border-teal-400" ] [ text title ]
        , div [ class "flex justify-around py-4" ] children
        ]


modelToOptions model =
    let
        opts =
            Mark.defaultOptions
    in
    { opts
        | searchType = Mark.normalSearch model.caseSensitivity
        , whitespace = model.whitespace
        , minTermLength = model.minTermLength
    }


view model =
    main_ [ class "flex flex-col items-center" ]
        [ Html.node "link" [ rel "stylesheet", href "https://unpkg.com/tailwindcss@^1.0/dist/tailwind.min.css" ] []
        , Html.node "style" [] [ text """
          blockquote { quotes: "“" "”" "‘" "’"; }
          blockquote:before { content: open-quote; }
          blockquote:after { content: close-quote; }
          .v-gap > * + * { margin-top: 1rem; }
        """ ]
        , h1 [ class "text-6xl text-center" ] [ text "Elm-Mark Demo" ]
        , div [ class "px-4" ]
            [ input
                [ class "text-2xl border-2 border-gray-200 focus:border-gray-500 rounded-sm mb-4 w-full max-w-4xl p-2"
                , type_ "text"
                , onInput SearchTermChange
                , placeholder "Search the text below"
                , autofocus True
                , value model.searchTerm
                ]
                []
            ]
        , div [ class "flex flex-col md:flex-row text-justify" ]
            [ div [ class "w-full px-4 md:border-r-4 md:border-black" ]
                [ blockquote [] <| Mark.markWith (modelToOptions model) model.searchTerm searchText
                , small [ class "block mt-4 text-right italic" ] [ text "Johann Wolfgang von Goethe, Faust, First Part" ]
                ]
            , div [ class "flex flex-col w-full px-4 v-gap" ]
                [ optionGroup "Case sensitivity"
                    [ optionButton
                        { name = "ignore case"
                        , clickMsg = CaseChanged Mark.ignoreCase
                        , selected = model.caseSensitivity == Mark.ignoreCase
                        }
                    , optionButton
                        { name = "match case"
                        , clickMsg = CaseChanged Mark.matchCase
                        , selected = model.caseSensitivity == Mark.matchCase
                        }
                    ]
                , optionGroup "Whitespace"
                    [ optionButton
                        { name = "single word"
                        , clickMsg = WhitespaceChanged Mark.singleWord
                        , selected = model.whitespace == Mark.singleWord
                        }
                    , optionButton
                        { name = "multi word"
                        , clickMsg = WhitespaceChanged Mark.multiWord
                        , selected = model.whitespace == Mark.multiWord
                        }
                    ]
                , optionGroup "Minumum search term length"
                    [ input [ type_ "number", value (String.fromInt model.minTermLength), onInput (String.toInt >> Maybe.map MinTermLengthChanged >> Maybe.withDefault NoOp) ] []
                    ]
                ]
            ]
        ]
