module Mark exposing
    ( defaultOptions
    , ignoreCase
    , mark
    , markWith
    , matchCase
    , multiWord
    , searchCustom
    , searchNormal
    , singleWord
    )

import Html exposing (Html)
import Internal
    exposing
        ( GetIndexesFn
        , applyMinLengthCheck
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
    = SingleWord
    | MultiWord


{-| -}
singleWord : Whitespace
singleWord =
    SingleWord


{-| -}
multiWord : Whitespace
multiWord =
    MultiWord


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
    , whitespace = SingleWord
    , minTermLength = 3
    , mapHit = Html.text
    , mapMiss = \miss -> Html.mark [] [ Html.text miss ]
    }


pickGetIndexesFn : Options a -> GetIndexesFn
pickGetIndexesFn { searchType, whitespace, minTermLength } =
    let
        getIndexes =
            applyMinLengthCheck minTermLength <|
                case searchType of
                    SearchNormal caseSensitivity ->
                        case caseSensitivity of
                            CaseIgnore ->
                                stringIndexesIgnoreCase

                            CaseSensitive ->
                                stringIndexes

                    SearchCustom customGetIndexes ->
                        customGetIndexes
    in
    case whitespace of
        SingleWord ->
            getIndexes

        MultiWord ->
            multiWordGetIndexes getIndexes


markWithHelp : Options a -> List ( Int, Int ) -> String -> String -> List a -> List a
markWithHelp ({ mapHit, mapMiss } as options) revPositions term content markers =
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
            markWithHelp options rest term newContent newMarkers


wrapAndAddToMarkers : String -> (String -> a) -> List a -> List a
wrapAndAddToMarkers item wrapper markers =
    case item of
        "" ->
            markers

        _ ->
            wrapper item :: markers


{-| -}
markWith : Options a -> String -> String -> List a
markWith options term content =
    let
        indexes =
            pickGetIndexesFn options term content
    in
    markWithHelp options (List.reverse indexes) term content []


{-| -}
mark : String -> String -> List (Html msg)
mark =
    markWith defaultOptions
