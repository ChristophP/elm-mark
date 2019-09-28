module Mark exposing
    ( mark, markWith
    , Options, defaultOptions, SearchType, normalSearch, customSearch, Case, ignoreCase, matchCase, Whitespace, singleWord, multiWord
    )

{-| This library gives you the tools you need to mark and highlight text
according to one or multiple search terms. You can use the defaults or
configure the search with options.


# Marking

@docs mark, markWith


# Options

@docs Options, defaultOptions, SearchType, normalSearch, customSearch, Case, ignoreCase, matchCase, Whitespace, singleWord, multiWord

-}

import Html exposing (Html)
import Internal
    exposing
        ( GetIndexesFn
        , applyMinLengthCheck
        , multiWordGetIndexes
        , stringIndexes
        , stringIndexesIgnoreCase
        )


{-| The options you have for configuring the search behavior.
`searchType` is explained [here](#SearchType) and `whitespace` is explained [here](#Whitespace).

`minTermLength` sets a threshold at which search is performed. As your typing
you may not want to show hits for 1 or 2 letters so the default is 3, but you
can set it to whatever you want.

You want to mark your hits and misses somehow. `mapHit` and `mapMiss` let you
do just that. By default misses are plain text and hits will be put into
`<mark>` tags. Customize it, if you want to.

-}
type alias Options a =
    { searchType : SearchType
    , whitespace : Whitespace
    , minTermLength : Int
    , mapHit : String -> a
    , mapMiss : String -> a
    }


{-| Match or ignore case. Use with [`normalSearch`](#normalSearch).
-}
type Case
    = CaseSensitive
    | CaseIgnore


{-| Search will match case (case sensitive).
-}
matchCase : Case
matchCase =
    CaseSensitive


{-| Search will ignore case (case insensitive).
-}
ignoreCase : Case
ignoreCase =
    CaseIgnore


{-| This option lets you configure how whitespace in search terms is treated.
In some case you may want to for the string "functional programming" and want
results to have both of those words in them, in that order.
Other times you may wanna search for "Leonardo Raphael Donatello" and search for
occurances of either one of those.
-}
type Whitespace
    = SingleWord
    | MultiWord


{-| Will treat the search term as ONE. This means "Peter Parker" will match
occurances of "Peter Parker" or "peter parker" in the content(depending on your
case settings). You can also search for whole sentences with this option, like
"To infinity and beyond" and the entire sentence will be marked as a hit if it
occurs in the text you're searching.
-}
singleWord : Whitespace
singleWord =
    SingleWord


{-| Will treat the searchterm as MULTIPLE. This means "Peter Parker" would be
split into the words "Peter" and "Parker". This will run a search for each
word in your search term separately. If hits are overlapping then the one that
starts later in the text will be ignored and not marked.
-}
multiWord : Whitespace
multiWord =
    MultiWord


{-| Which search type are you using? Pick between ease of use and custom
flexibility. You probably wanna start with [`normalSearch`](#normalSearch).
-}
type SearchType
    = SearchNormal Case
    | SearchCustom (String -> String -> List ( Int, Int ))


{-| Normal search is the easiest to start with. You can configure case
sensitivity. If you need lots of flexibility, use [`customSearch`](#customSearch).
-}
normalSearch : Case -> SearchType
normalSearch =
    SearchNormal


{-| Custom search will let you pass your own searching logic. Check first if the
provided options for case sensitivity, minimum search term length and
single/multi line search are enough for you. But maybe you need
glob search, or quoting like Google has, etc. If so, you can provide a function,
which gets the search term and the text to search as arguments. Your function
needs to return a list of index pairs where each marks the start and end index of a
match.
You need to make sure the returned indexes are sane. Sane in this case means the index pairs are ...

1.  **sorted ascendingly**: Meaning for every index pair, the end index is always smaller than the start of the next pair.

    For example: `[(3, 4), (0, 2)]` is not sorted, `[(0, 2), (3, 4)]` is sorted.
    You can verify

2.  **non-overlapping**: The ranges expressed by the pairs don't overlap.

    For example: `[(1, 3), (2, 4)]` overlap, `[(1, 3), (5, 6)]` don't.

You can use funtions like `String.indexes`, `Regex.find` or all the good stuff
in `elm/parser` for your implementation.

When using [`singleWord`](#singleWord)
your function is called only once, but it will be called once for every word when using
[`multiWord`](#multiWord).

-}
customSearch : (String -> String -> List ( Int, Int )) -> SearchType
customSearch =
    SearchCustom


{-| The options used for [`mark`](#mark). The defaults should
work for most simple use cases. If you only want to slightly
tweak these options, you can use them with the record update syntax. The default values
are the following:

    defaultOptions : Options (Html msg)
    defaultOptions =
        { searchType = searchNormal ignoreCase
        , whitespace = singleWord
        , minTermLength = 3
        , mapHit = Html.text
        , mapMiss = \miss -> Html.mark [] [ Html.text miss ]
        }

This means that search will ignore case, the search term will be treated as
a single word (including whitespace) and no matches will be generated if the
search term is under three characters. Also, hits will be transformed into
plain HTML text and misses into `<mark>` tags.

-}
defaultOptions : Options (Html msg)
defaultOptions =
    { searchType = SearchNormal CaseIgnore
    , whitespace = SingleWord
    , minTermLength = 3
    , mapHit = Html.text
    , mapMiss = \miss -> Html.mark [] [ Html.text miss ]
    }


pickGetIndexesFn : Options a -> GetIndexesFn
pickGetIndexesFn { searchType, whitespace, minTermLength } =
    let
        getIndexes =
            applyMinLengthCheck minTermLength <|
                case searchType of
                    SearchNormal caseSensitivity ->
                        case caseSensitivity of
                            CaseIgnore ->
                                stringIndexesIgnoreCase

                            CaseSensitive ->
                                stringIndexes

                    SearchCustom customGetIndexes ->
                        customGetIndexes
    in
    case whitespace of
        SingleWord ->
            getIndexes

        MultiWord ->
            multiWordGetIndexes getIndexes


markWithHelp : Options a -> List ( Int, Int ) -> String -> String -> List a -> List a
markWithHelp ({ mapHit, mapMiss } as options) revPositions term content markers =
    case revPositions of
        [] ->
            wrapAndAddToMarkers content mapMiss markers

        ( start, end ) :: rest ->
            let
                miss =
                    String.dropLeft end content

                hit =
                    String.slice start end content

                newContent =
                    String.dropRight (String.length miss + String.length hit) content

                newMarkers =
                    wrapAndAddToMarkers hit mapHit <|
                        wrapAndAddToMarkers miss mapMiss markers
            in
            markWithHelp options rest term newContent newMarkers


wrapAndAddToMarkers : String -> (String -> a) -> List a -> List a
wrapAndAddToMarkers item wrapper markers =
    case item of
        "" ->
            markers

        _ ->
            wrapper item :: markers


{-| Customize how marking works. Check out the [`options`](#Options)
for what can be configured.
-}
markWith : Options a -> String -> String -> List a
markWith options term content =
    let
        indexes =
            pickGetIndexesFn options term content
    in
    markWithHelp options (List.reverse indexes) term content []


{-| Highlight search terms within text.


    main =
        Html.p [] <| Mark.mark "ness" "Tennessee"

    -- will render <p>Ten<mark>ness</mark>ee</p>

`mark` uses sensible defaults. Check out
what they are in detail [here](#defaultOptions).
It is simply defined as:

    mark : String -> String -> List (Html msg)
    mark =
        markWith defaultOptions

If you need more flexibility, use [`markWith`](#markWith) for custom options.

-}
mark : String -> String -> List (Html msg)
mark =
    markWith defaultOptions
