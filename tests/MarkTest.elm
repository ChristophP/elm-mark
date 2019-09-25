module MarkTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Mark
    exposing
        ( caseIgnore
        , caseSensitive
        , highlight
        , highlightWith
        , searchCustom
        , searchNormal
        , whitespacePartOfTerm
        , whitespaceSeparatesWords
        )
import Test exposing (..)


type Mark
    = Hit String
    | Miss String


testOptions =
    { searchType = searchNormal caseIgnore whitespacePartOfTerm
    , minTermLength = 3
    , mapHit = Hit
    , mapMiss = Miss
    }


basic : Test
basic =
    describe "highlightWith basic"
        [ test "splits a string into hits and misses" <|
            \() ->
                highlightWith testOptions "assi" "assi Peter Hassi"
                    |> Expect.equal [ Hit "assi", Miss " Peter H", Hit "assi" ]
        , test "starts with hit when hit is first" <|
            \() ->
                highlightWith testOptions "assi" "assi Peter Hassi"
                    |> (\result ->
                            case List.head result of
                                Just (Hit _) ->
                                    Expect.pass

                                _ ->
                                    Expect.fail "Did not start with a hit."
                       )
        , test "starts with miss when miss is first" <|
            \() ->
                highlightWith testOptions "assi" "Peter assi Hassi"
                    |> (\result ->
                            case List.head result of
                                Just (Miss _) ->
                                    Expect.pass

                                _ ->
                                    Expect.fail "Did not start with a hit."
                       )
        , test "ends with hit when hit is last" <|
            \() ->
                highlightWith testOptions "assi" " Peter Hassi"
                    |> (\result ->
                            case List.head (List.reverse result) of
                                Just (Hit _) ->
                                    Expect.pass

                                _ ->
                                    Expect.fail "Did not end with a hit."
                       )
        , test "ends with miss when miss is last" <|
            \() ->
                highlightWith testOptions "assi" "Peter assi Ha"
                    |> (\result ->
                            case List.head (List.reverse result) of
                                Just (Miss _) ->
                                    Expect.pass

                                _ ->
                                    Expect.fail "Did not end with a miss."
                       )
        ]


caseSensitivity =
    concat
        [ describe "highlightWith case ignore" <|
            let
                options =
                    { testOptions | searchType = searchNormal caseIgnore whitespacePartOfTerm }
            in
            [ test "finds uppercase matches too when searching lowercase" <|
                \() ->
                    highlightWith options "assi" "Peter HaSSi Ha ASSIt aSiS"
                        |> Expect.equal [ Miss "Peter H", Hit "aSSi", Miss " Ha ", Hit "ASSI", Miss "t aSiS" ]
            , test "finds also lowercase matches when searching uppercase" <|
                \() ->
                    highlightWith options "ASSI" "Peter aSSi Ha assiaSsi"
                        |> Expect.equal [ Miss "Peter ", Hit "aSSi", Miss " Ha ", Hit "assi", Hit "aSsi" ]
            ]
        , describe "highlightWith case sensitive" <|
            let
                options =
                    { testOptions | searchType = searchNormal caseSensitive whitespacePartOfTerm }
            in
            [ test "won't find uppercase matches when searching lowercase" <|
                \() ->
                    highlightWith options "assi" "Peter HaSSi Ha ASSIt aSiS"
                        |> Expect.equal [ Miss "Peter HaSSi Ha ASSIt aSiS" ]
            , test "finds exact matches" <|
                \() ->
                    highlightWith options "aSSi" "Peter aSSi Ha assiaSsi"
                        |> Expect.equal [ Miss "Peter ", Hit "aSSi", Miss " Ha assiaSsi" ]
            ]
        ]


minLenghtTests =
    describe "highlightWith minimum search length"
        [ fuzz (Fuzz.intRange 0 8) "won't create any hits when length is not reached" <|
            \minLength ->
                let
                    options =
                        { testOptions | minTermLength = minLength }

                    searchTerm =
                        "assi"

                    result =
                        highlightWith options searchTerm "Peter assi Ha"

                    expectedResult =
                        if minLength < String.length searchTerm then
                            [ Miss "Peter ", Hit "assi", Miss " Ha" ]

                        else
                            [ Miss "Peter assi Ha" ]
                in
                result |> Expect.equal expectedResult
        ]
