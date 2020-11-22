# Elm Mark

Search Term Highlighting for Elm Apps

![elm-mark-demo](https://raw.githubusercontent.com/ChristophP/elm-mark/master/elm-mark-demo.gif)

Check out the [demo](https://elm-mark-demo.deedop.de/).

**Upgrade hint from version 1.x.x:**
There is only one easy-to-fix breaking change in version 2.
The exposed module name for `elm-mark` is changed from `Mark` to `String.Mark`.
This avoids module naming collisions with other popular packages. Now feel free
to use both `elm-mark` with for example the great
[`mdgriffith/elm-markup`](https://package.elm-lang.org/packages/mdgriffith/elm-markup/latest/). 🎉

## Who's using it?

- The amazing [`ThankU`](https://www.thanku.social) [(www.thanku.social)](https://www.thanku.social): the platform to say thank you and do good.
- others: please open a PR to have your name or company added

## Why do you need it?

Marking keywords in text is harder than it sounds. I had to do it a couple of
times and was amazed with the amount of code needed to do something so seemingly simple.
This package wraps everything you need, to do search term highlighting.
I spent quite some time polishing the API to where it is right now, so I hope
you'll find it pleasant to use and highlight away. :-)

## How to use

```elm
import Html
import String.Mark as Mark

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
## Credits

Thanks to [@layflags](https://github.com/layflags) and [@pehota](https://github.com/pehota) for inspirational thoughts 🤔, code reviews 👓 and
lots of fun working on projects together. ❤
