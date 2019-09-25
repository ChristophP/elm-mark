module Internal exposing
    ( GetIndexesFn
    , filterLastTwo
    , multiWordGetIndexes
    , stringIndexes
    , stringIndexesIgnoreCase
    )

{-| Like filter but gives you access to the last element that passed
the filter check as well.
-}


filterLastTwo : (a -> a -> Bool) -> List a -> List a
filterLastTwo func list =
    case list of
        [] ->
            list

        x :: tail ->
            filterLastTwoHelp func x tail [ x ]
                |> List.reverse


filterLastTwoHelp : (a -> a -> Bool) -> a -> List a -> List a -> List a
filterLastTwoHelp func val list acc =
    case list of
        [] ->
            acc

        head :: tail ->
            let
                ( newVal, newAcc ) =
                    if func val head then
                        ( head, head :: acc )

                    else
                        ( val, acc )
            in
            filterLastTwoHelp func newVal tail newAcc


type alias GetIndexesFn =
    String -> String -> List ( Int, Int )


addEnd : String -> List Int -> List ( Int, Int )
addEnd term =
    List.map (\start -> ( start, start + String.length term ))


stringIndexes : String -> String -> List ( Int, Int )
stringIndexes term content =
    String.indexes term content |> addEnd term


stringIndexesIgnoreCase : String -> String -> List ( Int, Int )
stringIndexesIgnoreCase term content =
    String.indexes (String.toLower term) (String.toLower content)
        |> addEnd term


multiWordGetIndexes : GetIndexesFn -> GetIndexesFn
multiWordGetIndexes getIndexes term content =
    if not (String.contains " " term) then
        getIndexes term content

    else
        let
            words =
                String.words term

            indexes =
                List.concatMap (\word -> getIndexes word content) words

            sortedIndexes =
                List.sortBy Tuple.second indexes
        in
        filterLastTwo (\( _, end ) ( start, _ ) -> end < start) sortedIndexes
