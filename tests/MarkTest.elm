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
    { searchType = searchNormal caseIgnore whitespacePartOfTerm 3
    , hitWrapper = Hit
    , missWrapper = Miss
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
                caseIgnoreOptions =
                    { testOptions | searchType = searchNormal caseIgnore whitespacePartOfTerm 3 }
            in
            [ test "finds uppercase matches too when searching lowercase" <|
                \() ->
                    highlightWith caseIgnoreOptions "assi" "Peter HaSSi Ha ASSIt aSiS"
                        |> Expect.equal [ Miss "Peter H", Hit "aSSi", Miss " Ha ", Hit "ASSI", Miss "t aSiS" ]
            , test "finds also lowercase matches when searching uppercase" <|
                \() ->
                    highlightWith caseIgnoreOptions "ASSI" "Peter aSSi Ha assiaSiS"
                        |> Expect.equal [ Miss "Peter ", Hit "aSSi", Miss " Ha ", Hit "assi", Hit "aSiS" ]
            ]
        , describe "highlightWith case sensitive"
            []
        ]



otherTest = describe "highlightWith minimum search length" [
  fuzz (Fuzz.intRange 0 8) "won't create any hits when length is not reached" <| \minLength ->
     let options = { testOptions | searchType  = searchNormal caseIgnore whitespacePartOfTerm minLength   }
         searchTerm = "assi"
         result =  highlightWith testOptions "assi" "Peter assi Ha"
         expectedResult =
          if minLength >= String.length searchTerm then
            [Miss "Peter ", Hit "assi", Miss " Ha"]
          else
            [Miss "Peter assi Ha"]
     in
         result |> Expect.equal expectedResult
  ]