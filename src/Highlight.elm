module Mark exposing (highlight, highlightWith, interweave)

import Html exposing (Html)


{-| not tail recursive yet
interweave [1, 3, 5][2, 4, 6] == [1, 2, 3, 4, 5, 6]
Alternative naive implementation of interweave which is not using concatMap2
-}
interweave : List a -> List a -> List a
interweave xs ys =
    case xs of
        [] ->
            ys

        headX :: tailX ->
            case ys of
                [] ->
                    xs

                headY :: tailY ->
                    headX :: headY :: interweave tailX tailY


partitionByTerm : String -> String -> ( List String, List String )
partitionByTerm term content =
    let
        revPositions =
            String.indexes
                (String.toLower term)
                (String.toLower content)
                |> List.reverse
    in
    partitionByTermHelp revPositions term content ( [], [] )


partitionByTermHelp : List Int -> String -> String -> ( List String, List String ) -> ( List String, List String )
partitionByTermHelp revPositions term content ( misses, matches ) =
    case revPositions of
        [] ->
            ( content :: misses, matches )

        pos :: rest ->
            let
                end =
                    pos + String.length term

                miss =
                    String.dropLeft end content

                match =
                    String.slice pos end content

                newContent =
                    String.dropRight (String.length miss + String.length match) content
            in
            partitionByTermHelp rest term newContent ( miss :: misses, match :: matches )


type Marker
    = Miss String
    | Match String


type Accuracy
    = Partially
    | Complementary
    | Exactly


type alias Options a =
    { searchType : SearchType
    , hitWrapper : String -> a
    , missWrapper : String -> a
    }



--defaultOptions : Options (Html msg)
--defaultOptions =
--{ searchType = Normal CaseIgnore WhitespaceSeparatesWords
--, hitWrapper = Html.text
--, missWrapper = \miss -> Html.mark [] [ Html.text miss ]
--}


type Whitespace
    = WhitespacePartOfTerm
    | WhitespaceSeparatesWords


type Case
    = CaseSensitive
    | CaseIgnore


type SearchType
    = Normal Case Whitespace
    | Custom (String -> List ( Int, Int ))


highlightWith : Options a -> String -> String -> List a
highlightWith { hitWrapper, missWrapper } term content =
    partitionByTerm term content
        |> Tuple.mapBoth (List.map hitWrapper) (List.map missWrapper)
        |> (\( texts, marks ) -> interweave texts marks)


defaultOptions : Options (Html msg)
defaultOptions =
    { searchType = Normal CaseIgnore WhitespaceSeparatesWords
    , hitWrapper = Html.text
    , missWrapper = \miss -> Html.mark [] [ Html.text miss ]
    }


highlight : String -> String -> List (Html msg)
highlight =
    highlightWith defaultOptions
