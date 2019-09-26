module Internal exposing
    ( GetIndexesFn
    , applyMinLengthCheck
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


addEnds : String -> List Int -> List ( Int, Int )
addEnds term =
    List.map (\start -> ( start, start + String.length term ))


applyMinLengthCheck : Int -> GetIndexesFn -> GetIndexesFn
applyMinLengthCheck minLength getIndexes term content =
    if minLength <= String.length term then
        getIndexes term content

    else
        []


stringIndexes : GetIndexesFn
stringIndexes term content =
    String.indexes term content
        |> addEnds term


stringIndexesIgnoreCase : GetIndexesFn
stringIndexesIgnoreCase term content =
    String.indexes (String.toLower term) (String.toLower content)
        |> addEnds term


multiWordGetIndexes : GetIndexesFn -> GetIndexesFn
multiWordGetIndexes getIndexes term content =
    if not (String.contains " " term) then
        getIndexes term content

    else
        String.words term
            |> List.concatMap (\word -> getIndexes word content)
            |> List.sortBy Tuple.first
            |> filterLastTwo (\( _, end ) ( start, _ ) -> end < start)
