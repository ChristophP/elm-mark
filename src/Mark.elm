module Mark exposing
    ( caseIgnore
    , caseSensitive
    , defaultOptions
    , highlight
    , highlightWith
    , searchCustom
    , searchNormal
    , whitespacePartOfTerm
    , whitespaceSeparatesWords
    )

import Html exposing (Html)


partitionByTerm : Options a -> String -> String -> List a
partitionByTerm options term content =
    if options.minTermLength < String.length term then
        let
            indexes =
                case options.searchType of
                    SearchNormal CaseIgnore _ ->
                        String.indexes (String.toLower term) (String.toLower content)

                    SearchNormal CaseSensitive _ ->
                        String.indexes term content

                    SearchCustom getIndexes ->
                        getIndexes term content
        in
        partitionByTermHelp options (List.reverse indexes) term content []

    else
        [ options.missWrapper content ]


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


{-| -}
type alias Options a =
    { searchType : SearchType
    , minTermLength : Int
    , hitWrapper : String -> a
    , missWrapper : String -> a
    }


type Case
    = CaseSensitive
    | CaseIgnore


{-| -}
caseSensitive : Case
caseSensitive =
    CaseSensitive


{-| -}
caseIgnore : Case
caseIgnore =
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
    = SearchNormal Case Whitespace
    | SearchCustom (String -> String -> List Int)


{-| Use normal search. You can configure case sensitivity,
the way whitespace is treated and a min wordlength
for the search to generate hits.
-}
searchNormal : Case -> Whitespace -> SearchType
searchNormal =
    SearchNormal


{-| -}
searchCustom : (String -> String -> List Int) -> SearchType
searchCustom =
    SearchCustom


{-| -}
defaultOptions : Options (Html msg)
defaultOptions =
    { searchType = SearchNormal CaseIgnore WhitespaceSeparatesWords
    , minTermLength = 3
    , hitWrapper = Html.text
    , missWrapper = \miss -> Html.mark [] [ Html.text miss ]
    }


{-| -}
highlightWith : Options a -> String -> String -> List a
highlightWith options term content =
    partitionByTerm options term content


{-| -}
highlight : String -> String -> List (Html msg)
highlight =
    highlightWith defaultOptions
