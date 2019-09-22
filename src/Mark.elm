module Mark exposing
    ( Case(..)
    , SearchType(..)
    , Whitespace(..)
    , highlight
    , highlightWith
    , interweave
    )

import Html exposing (Html)


{-| not tail recursive yet
interweave [1, 3, 5][2, 4, 6] == [1, 2, 3, 4, 5, 6]
Alternative naive implementation of interweave which is not using concatMap2
-}
interweave : List String -> List String -> (String -> a) -> (String -> a) -> List a
interweave xs ys hitWrapper missWrapper =
    case xs of
        [] ->
            List.map missWrapper ys

        headX :: tailX ->
            case ys of
                [] ->
                    List.map hitWrapper xs

                headY :: tailY ->
                    case headX of
                        "" ->
                            missWrapper headY
                                :: interweave tailX tailY hitWrapper missWrapper

                        _ ->
                            hitWrapper headX
                                :: missWrapper headY
                                :: interweave tailX tailY hitWrapper missWrapper


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
highlightWith options term content =
    partitionByTerm options term content



--|> Tuple.mapBoth (List.map hitWrapper) (List.map missWrapper)
--|> (\( hits, misses ) -> interweave hits misses hitWrapper missWrapper)


defaultOptions : Options (Html msg)
defaultOptions =
    { searchType = Normal CaseIgnore WhitespaceSeparatesWords
    , hitWrapper = Html.text
    , missWrapper = \miss -> Html.mark [] [ Html.text miss ]
    }


highlight : String -> String -> List (Html msg)
highlight =
    highlightWith defaultOptions
