module Util exposing (filterLastTwo)

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
