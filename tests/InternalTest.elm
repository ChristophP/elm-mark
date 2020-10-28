module InternalTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Internal
    exposing
        ( applyMinLengthCheck
        , filterLastTwo
        , multiWordGetIndexes
        , stringIndexes
        , stringIndexesIgnoreCase
        )
import Test exposing (..)


basic : Test
basic =
    describe "filterLastTwo case ignore" <|
        [ test "returns the same list if length is zero" <|
            \() ->
                filterLastTwo (<=) []
                    |> Expect.equal []
        , test "returns the same list if length is one" <|
            \() ->
                filterLastTwo (<=) [ 1 ]
                    |> Expect.equal [ 1 ]
        , test "filter based on the last two elements" <|
            \() ->
                filterLastTwo (<=) [ 1, 2, 1, 3, 5, 4 ]
                    |> Expect.equal [ 1, 2, 3, 5 ]
        ]


applyMinLengthCheckTest : Test
applyMinLengthCheckTest =
    describe "applyMinLengthCheck" <|
        [ test "when minlength is not reached returns a getIndexes function that always returns an empty list" <|
            \() ->
                let
                    getIndexes =
                        applyMinLengthCheck 3 (\term content -> [ ( 1, 2 ) ])
                in
                getIndexes "Pe" "Peter" |> Expect.equal []
        , test "when minlength is not reached returns a getIndexes function that indentical to the one that's passed" <|
            \() ->
                let
                    getIndexes =
                        \term content -> [ ( 1, 2 ) ]

                    decoratedGetIndexes =
                        applyMinLengthCheck 3 getIndexes
                in
                decoratedGetIndexes "Pet" "Peter" |> Expect.equal (getIndexes "Pet" "Peter")
        ]


getIndexesFunctionsTest : Test
getIndexesFunctionsTest =
    concat
        [ describe "stringIndexes"
            [ test "gets indexes in case sensitive manner" <|
                \() ->
                    stringIndexes "ete" "Peter Leter SCHMETER" |> Expect.equal [ ( 1, 4 ), ( 7, 10 ) ]
            , test "works with adjacent hits" <|
                \() ->
                    stringIndexes "Peter" "PeterPeter" |> Expect.equal [ ( 0, 5 ), ( 5, 10 ) ]
            ]
        , describe "stringIndexesIgnoreCase"
            [ test "gets indexes in case insensitive manner" <|
                \() ->
                    stringIndexesIgnoreCase "ETE" "Peter Leter" |> Expect.equal [ ( 1, 4 ), ( 7, 10 ) ]
            ]
        ]


multiWordGetIndexesTest : Test
multiWordGetIndexesTest =
    describe "multiWordGetIndexes"
        [ test "returns function that acts like passed callback when only a single word is searched" <|
            \() ->
                let
                    getIndexes =
                        multiWordGetIndexes stringIndexesIgnoreCase
                in
                getIndexes "peter" "Peter Leter SCHMETER"
                    |> Expect.equal (getIndexes "peter" "Peter Leter SCHMETER")
        , test "decorates a getIndexes function to find all words separately" <|
            \() ->
                let
                    getIndexes =
                        multiWordGetIndexes stringIndexesIgnoreCase
                in
                getIndexes "iss ipp" "mississippi"
                    |> Expect.equal [ ( 1, 4 ), ( 4, 7 ), ( 7, 10 ) ]
        ]
