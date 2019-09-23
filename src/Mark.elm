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


partitionByTerm : Options a b -> String -> String -> List b
partitionByTerm options term content =
    let
        indexes =
            case options.searchType of
                SearchNormal CaseIgnore _ _ ->
                    String.indexes (String.toLower term) (String.toLower content)

                SearchNormal CaseSensitive _ _ ->
                    String.indexes term content

                SearchCustom getIndexes ->
                    getIndexes term content
    in
    partitionByTermHelp options (List.reverse indexes) term content []


partitionByTermHelp : Options a b -> List Int -> String -> String -> List b -> List b
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
type alias Options record a =
    { record
        | searchType : SearchType
        , hitWrapper : String -> a
        , missWrapper : String -> a
    }


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


type SearchType
    = SearchNormal Case Whitespace Int
    | SearchCustom (String -> String -> List Int)


{-| Use normal search. You can configure case sensitivity,
the way whitespace is treated and a min wordlength
for the search to generate hits.
-}
searchNormal : Case -> Whitespace -> Int -> SearchType
searchNormal =
    SearchNormal


{-| -}
searchCustom : (String -> String -> List Int) -> SearchType
searchCustom =
    SearchCustom


{-| -}
defaultOptions : Options {} (Html msg)
defaultOptions =
    { searchType = SearchNormal CaseIgnore WhitespaceSeparatesWords 3
    , hitWrapper = Html.text
    , missWrapper = \miss -> Html.mark [] [ Html.text miss ]
    }


{-| -}
highlightWith : Options record a -> String -> String -> List a
highlightWith options term content =
    partitionByTerm options term content


{-| -}
highlight : String -> String -> List (Html msg)
highlight =
    highlightWith defaultOptions
