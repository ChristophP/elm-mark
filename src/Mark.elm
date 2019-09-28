module Mark exposing
    ( mark, markWith
    , Options, defaultOptions, SearchType, normalSearch, customSearch, Case, ignoreCase, matchCase, Whitespace, singleWord, multiWord
    )

{-| This library gives you the tools you need to mark text
according to one or multiple search terms. You can use the default or
configure the search with options.


# Marking

@docs mark, markWith


# Options

@docs Options, defaultOptions, SearchType, normalSearch, customSearch, Case, ignoreCase, matchCase, Whitespace, singleWord, multiWord

-}

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


{-| -}
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


{-| -}
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


{-| Normal search is the easiest to start with. You can configure case
sensitivity. If you need lots of flexibility, use [`customSearch`]
-}
normalSearch : Case -> SearchType
normalSearch =
    SearchNormal


{-| -}
customSearch : (String -> String -> List ( Int, Int )) -> SearchType
customSearch =
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


{-| Customize how marking works. Check out the [`options`](#Options)
to what can be configured.
-}
markWith : Options a -> String -> String -> List a
markWith options term content =
    let
        indexes =
            pickGetIndexesFn options term content
    in
    markWithHelp options (List.reverse indexes) term content []


{-| Highlight search terms within text. This uses the default options.
Use [`markWith`](#markWith) for custom options.


    main =
        Html.p [] <| Mark.mark "ness" "Tennessee"

    -- will render <p>Ten<mark>ness</mark>ee</p>

-}
mark : String -> String -> List (Html msg)
mark =
    markWith defaultOptions
