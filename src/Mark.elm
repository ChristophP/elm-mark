module Mark exposing
    ( defaultOptions
    , highlight
    , highlightWith
    , ignoreCase
    , matchCase
    , searchCustom
    , searchNormal
    , whitespacePartOfTerm
    , whitespaceSeparatesWords
    )

import Html exposing (Html)
import Internal
    exposing
        ( GetIndexesFn
        , filterLastTwo
        , multiWordGetIndexes
        , stringIndexes
        , stringIndexesIgnoreCase
        )


{-| -}
type alias Options a =
    { searchType : SearchType
    , whitespace : Whitespace
    , minTermLength : Int
    , mapHit : String -> a
    , mapMiss : String -> a
    }


type Case
    = CaseSensitive
    | CaseIgnore


{-| -}
matchCase : Case
matchCase =
    CaseSensitive


{-| -}
ignoreCase : Case
ignoreCase =
    CaseIgnore


type Whitespace
    = WhitespacePartOfTerm
    | WhitespaceSeparatesWords


{-| -}
whitespacePartOfTerm : Whitespace
whitespacePartOfTerm =
    WhitespacePartOfTerm


{-| -}
whitespaceSeparatesWords : Whitespace
whitespaceSeparatesWords =
    WhitespaceSeparatesWords


type SearchType
    = SearchNormal Case
    | SearchCustom (String -> String -> List ( Int, Int ))


{-| Use normal search. You can configure case sensitivity
for the search.
-}
searchNormal : Case -> SearchType
searchNormal =
    SearchNormal


{-| -}
searchCustom : (String -> String -> List ( Int, Int )) -> SearchType
searchCustom =
    SearchCustom


{-| -}
defaultOptions : Options (Html msg)
defaultOptions =
    { searchType = SearchNormal CaseIgnore
    , whitespace = WhitespaceSeparatesWords
    , minTermLength = 3
    , mapHit = Html.text
    , mapMiss = \miss -> Html.mark [] [ Html.text miss ]
    }


pickGetIndexesFn : Options a -> GetIndexesFn
pickGetIndexesFn { searchType, whitespace } =
    case ( searchType, whitespace ) of
        ( SearchNormal CaseIgnore, WhitespacePartOfTerm ) ->
            stringIndexesIgnoreCase

        ( SearchNormal CaseSensitive, WhitespacePartOfTerm ) ->
            stringIndexes

        ( SearchNormal CaseIgnore, WhitespaceSeparatesWords ) ->
            multiWordGetIndexes stringIndexesIgnoreCase

        ( SearchNormal CaseSensitive, WhitespaceSeparatesWords ) ->
            multiWordGetIndexes stringIndexes

        ( SearchCustom getIndexes, WhitespacePartOfTerm ) ->
            getIndexes

        ( SearchCustom getIndexes, WhitespaceSeparatesWords ) ->
            multiWordGetIndexes getIndexes


partitionByTerm : Options a -> String -> String -> List a
partitionByTerm options term content =
    if options.minTermLength < String.length term then
        let
            indexes =
                pickGetIndexesFn options term content
        in
        partitionByTermHelp options (List.reverse indexes) term content []

    else
        [ options.mapMiss content ]


partitionByTermHelp : Options a -> List ( Int, Int ) -> String -> String -> List a -> List a
partitionByTermHelp ({ mapHit, mapMiss } as options) revPositions term content markers =
    case revPositions of
        [] ->
            wrapAndAddToMarkers content mapMiss markers

        ( start, end ) :: rest ->
            let
                miss =
                    String.dropLeft end content

                hit =
                    String.slice start end content

                newContent =
                    String.dropRight (String.length miss + String.length hit) content

                newMarkers =
                    wrapAndAddToMarkers hit mapHit <|
                        wrapAndAddToMarkers miss mapMiss markers
            in
            partitionByTermHelp options rest term newContent newMarkers


wrapAndAddToMarkers : String -> (String -> a) -> List a -> List a
wrapAndAddToMarkers item wrapper markers =
    case item of
        "" ->
            markers

        _ ->
            wrapper item :: markers


{-| -}
highlightWith : Options a -> String -> String -> List a
highlightWith options term content =
    partitionByTerm options term content


{-| -}
highlight : String -> String -> List (Html msg)
highlight =
    highlightWith defaultOptions
