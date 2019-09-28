module MarkTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Mark
    exposing
        ( ignoreCase
        , mark
        , markWith
        , matchCase
        , multiWord
        , searchCustom
        , searchNormal
        , singleWord
        )
import Regex
import Test exposing (..)


type Mark
    = Hit String
    | Miss String


testOptions =
    { searchType = searchNormal ignoreCase
    , whitespace = singleWord
    , minTermLength = 3
    , mapHit = Hit
    , mapMiss = Miss
    }


basic : Test
basic =
    describe "markWith basic"
        [ test "splits a string into hits and misses" <|
            \() ->
                markWith testOptions "assi" "assi Peter Hassi"
                    |> Expect.equal [ Hit "assi", Miss " Peter H", Hit "assi" ]
        , test "starts with hit when hit is first" <|
            \() ->
                markWith testOptions "assi" "assi Peter Hassi"
                    |> (\result ->
                            case List.head result of
                                Just (Hit _) ->
                                    Expect.pass

                                _ ->
                                    Expect.fail "Did not start with a hit."
                       )
        , test "starts with miss when miss is first" <|
            \() ->
                markWith testOptions "assi" "Peter assi Hassi"
                    |> (\result ->
                            case List.head result of
                                Just (Miss _) ->
                                    Expect.pass

                                _ ->
                                    Expect.fail "Did not start with a miss."
                       )
        , test "ends with hit when hit is last" <|
            \() ->
                markWith testOptions "assi" " Peter Hassi"
                    |> (\result ->
                            case List.head (List.reverse result) of
                                Just (Hit _) ->
                                    Expect.pass

                                _ ->
                                    Expect.fail "Did not end with a hit."
                       )
        , test "ends with miss when miss is last" <|
            \() ->
                markWith testOptions "assi" "Peter assi Ha"
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
        [ describe "markWith case ignore" <|
            let
                options =
                    { testOptions | searchType = searchNormal ignoreCase }
            in
            [ test "finds uppercase matches too when searching lowercase" <|
                \() ->
                    markWith options "assi" "Peter HaSSi Ha ASSIt aSiS"
                        |> Expect.equal [ Miss "Peter H", Hit "aSSi", Miss " Ha ", Hit "ASSI", Miss "t aSiS" ]
            , test "finds also lowercase matches when searching uppercase" <|
                \() ->
                    markWith options "ASSI" "Peter aSSi Ha assiaSsi"
                        |> Expect.equal [ Miss "Peter ", Hit "aSSi", Miss " Ha ", Hit "assi", Hit "aSsi" ]
            ]
        , describe "markWith case sensitive" <|
            let
                options =
                    { testOptions | searchType = searchNormal matchCase }
            in
            [ test "won't find uppercase matches when searching lowercase" <|
                \() ->
                    markWith options "assi" "Peter HaSSi Ha ASSIt aSiS"
                        |> Expect.equal [ Miss "Peter HaSSi Ha ASSIt aSiS" ]
            , test "finds exact matches" <|
                \() ->
                    markWith options "aSSi" "Peter aSSi Ha assiaSsi"
                        |> Expect.equal [ Miss "Peter ", Hit "aSSi", Miss " Ha assiaSsi" ]
            ]
        ]


minLenghtTests =
    describe "markWith minimum search length"
        [ fuzz (Fuzz.intRange 0 8) "won't create any hits when length is not reached" <|
            \minLength ->
                let
                    options =
                        { testOptions | minTermLength = minLength }

                    searchTerm =
                        "assi"

                    result =
                        markWith options searchTerm "Peter assi Ha"

                    expectedResult =
                        if minLength <= String.length searchTerm then
                            [ Miss "Peter ", Hit "assi", Miss " Ha" ]

                        else
                            [ Miss "Peter assi Ha" ]
                in
                result |> Expect.equal expectedResult
        ]


multiWords =
    describe "markWith multiword search" <|
        let
            options =
                { testOptions | whitespace = multiWord }
        in
        [ test "does regular search if term doens't contain whitespace" <|
            \() ->
                markWith options "assi" "Hi assi Peter"
                    |> Expect.equal [ Miss "Hi ", Hit "assi", Miss " Peter" ]
        , test "searches multiple words when term has whitespace" <|
            \() ->
                markWith options "assi peter" "Hi assi Peter"
                    |> Expect.equal [ Miss "Hi ", Hit "assi", Miss " ", Hit "Peter" ]
        , test "also workds with 3 words" <|
            \() ->
                markWith options "assi peter hil" "Hill assi Peter"
                    |> Expect.equal [ Hit "Hil", Miss "l ", Hit "assi", Miss " ", Hit "Peter" ]
        , test "minTermLength is still respected" <|
            \() ->
                markWith options "assi peter hi" "Hi assi Peter"
                    |> Expect.equal [ Miss "Hi ", Hit "assi", Miss " ", Hit "Peter" ]
        , test "will disregard hits occuring within previous hits" <|
            \() ->
                markWith options "enn Tenness" "Tennessee"
                    |> Expect.equal [ Hit "Tenness", Miss "ee" ]
        ]


customGlobSearch term content =
    let
        pattern =
            String.replace "*" "\\w*" term
                |> String.replace "?" "\\w"

        regexOptions =
            { caseInsensitive = True
            , multiline = False
            }
    in
    case Regex.fromStringWith regexOptions pattern of
        Just regex ->
            Regex.find regex content
                |> List.map (\{ match, index } -> ( index, index + String.length match ))

        Nothing ->
            []


customLogic =
    concat
        [ describe "markWith searchCustom dummy" <|
            let
                -- normally you could implement logic based on term and content
                -- but we only wanna test that the logic is used here
                getIndexes term content =
                    [ ( 0, 2 ), ( 3, 7 ) ]

                unsortedGetIndexes term content =
                    [ ( 3, 7 ), ( 0, 2 ) ]

                options =
                    { testOptions | searchType = searchCustom getIndexes }

                unsortedOptions =
                    { testOptions | searchType = searchCustom unsortedGetIndexes }

                expectedResult =
                    [ Hit "Hi", Miss " ", Hit "assi", Miss " Peter" ]
            in
            [ test "uses the indexes of the custom search logic" <|
                \() ->
                    markWith options "doesn't matter" "Hi assi Peter"
                        |> Expect.equal expectedResult
            , test "won't find all all matches if indexes are not sorted" <|
                \() ->
                    markWith unsortedOptions "doesn't matter" "Hi assi Peter"
                        |> Expect.notEqual expectedResult
            ]
        , describe "markWith searchCustom real implementation" <|
            let
                options =
                    { testOptions | searchType = searchCustom customGlobSearch }
            in
            [ test "can for example perform glob search with *" <|
                \() ->
                    markWith options "as*" "assi Peter Assissi"
                        |> Expect.equal [ Hit "assi", Miss " Peter ", Hit "Assissi" ]
            , test "can for example perform glob search with ?" <|
                \() ->
                    markWith options "as?i" "assi Peter Assissi"
                        |> Expect.equal [ Hit "assi", Miss " Peter ", Hit "Assi", Miss "ssi" ]
            ]
        , describe "markWith searchCustom with multiWord" <|
            let
                options =
                    { testOptions
                        | whitespace = multiWord
                        , searchType = searchCustom customGlobSearch
                    }
            in
            [ test "also works" <|
                \() ->
                    markWith options "as* Pe?er" "Hi assi Peter"
                        |> Expect.equal [ Miss "Hi ", Hit "assi", Miss " ", Hit "Peter" ]
            ]
        ]
