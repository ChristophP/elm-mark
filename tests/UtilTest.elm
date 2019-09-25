module UtilTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Util
    exposing
        ( filterLastTwo
        )


basic : Test
basic =
    describe "filterLastTwo case ignore" <|
        [ test "returns the same list if length is zero" <|
            \() ->
                filterLastTwo (<) []
                    |> Expect.equal []
        , test "returns the same list if length is one" <|
            \() ->
                filterLastTwo (<) [ 1 ]
                    |> Expect.equal [ 1 ]
        , test "filter based on the last two elements" <|
            \() ->
                filterLastTwo (<) [ 1, 2, 1, 3, 5, 4 ]
                    |> Expect.equal [ 1, 2, 3, 5 ]
        ]
