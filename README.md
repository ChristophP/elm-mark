# Elm Mark

Search Term Highlighting for Elm Apps

![elm-mark-demo](https://raw.githubusercontent.com/ChristophP/elm-mark/master/elm-mark-demo.gif)

Check out the [demo](https://elm-mark-demo.deedop.de/).

## Why do you need it?

Marking keywords in text is harder than it sounds. I had to do it a couple of
times and was amazed with the amount of code needed to do something so seemingly simple.
This package wraps everything you need, to do search term highlighting.
I spent quite some time polishing the API to where it is right now, so I hope
you'll find it pleasant to use and highlight away. :-)

## How to use

```elm
import Html
import Mark

main = Html.p [] <| Mark.mark "ness" "Tennessee"
-- will render <p>Ten<mark>ness</mark>ee</p>
```

## Options and Configuration

There are a bunch of options ranging from no-config to fully customizable.
Options include case sensitiviy, single word/multi word search and search term
length threshold. You can also provide your own custom search logic, should the
available options not meet your needs.
Here's a small example of configuring case sensitivity.

```elm
let
  options = { defaultOptions | searchType = normalSearch matchCase }
in Mark.markWith options "a-search-term" "some text to search"
```

Check out the module documentation to see detailed info about the
configuration options.

## Examples

Check out the repository for `examples` and `tests`, there are also a few code
examples in the module documentation.

## Developing

Developing and running the tests.

```
npm ci
npm run dev
```

