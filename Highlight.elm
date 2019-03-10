module Highlight exposing (highlight, interweave)


type Html msg
    = Mark (List (Html msg))
    | Text String


mark _ children =
    Mark children


text =
    Text


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


highlight : String -> String -> List (Html msg)
highlight term content =
    partitionByTerm term content
        |> Tuple.mapBoth (List.map text) (List.map (text >> List.singleton >> mark []))
        |> (\( texts, marks ) -> interweave texts marks)
