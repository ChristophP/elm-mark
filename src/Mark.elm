module Mark exposing
    ( Case(..)
    , SearchType(..)
    , Whitespace(..)
    , defaultOptions
    , highlight
    , highlightWith
    )

import Html exposing (Html)


partitionByTerm : Options a -> String -> String -> List a
partitionByTerm options term content =
    let
        revPositions =
            String.indexes
                (String.toLower term)
                (String.toLower content)
                |> List.reverse
    in
    partitionByTermHelp options revPositions term content []


partitionByTermHelp : Options a -> List Int -> String -> String -> List a -> List a
partitionByTermHelp ({ hitWrapper, missWrapper } as options) revPositions term content markers =
    case revPositions of
        [] ->
            wrapAndAddToMarkers content missWrapper markers

        pos :: rest ->
            let
                end =
                    pos + String.length term

                miss =
                    String.dropLeft end content

                hit =
                    String.slice pos end content

                newContent =
                    String.dropRight (String.length miss + String.length hit) content

                newMarkers =
                    wrapAndAddToMarkers hit hitWrapper <|
                        wrapAndAddToMarkers miss missWrapper markers
            in
            partitionByTermHelp options rest term newContent newMarkers


wrapAndAddToMarkers : String -> (String -> a) -> List a -> List a
wrapAndAddToMarkers item wrapper markers =
    case item of
        "" ->
            markers

        _ ->
            wrapper item :: markers


type alias Options a =
    { searchType : SearchType
    , hitWrapper : String -> a
    , missWrapper : String -> a
    }


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
highlightWith options term content =
    partitionByTerm options term content


defaultOptions : Options (Html msg)
defaultOptions =
    { searchType = Normal CaseIgnore WhitespaceSeparatesWords
    , hitWrapper = Html.text
    , missWrapper = \miss -> Html.mark [] [ Html.text miss ]
    }


highlight : String -> String -> List (Html msg)
highlight =
    highlightWith defaultOptions
