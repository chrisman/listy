# Elm Listy

This is a (todo) list app written in Elm

## Getting started

there is a [justfile](https://github.com/casey/just)[^1]

1. `just` will start a watcher, and rebuild on file change.

2. Open `/dist/index.html` in a browser.

3. Edit `/src/Main.elm`

`just --list` (or just view/edit the justfile) to see other options.

[^1]: `just` is something new I'm trying out for this project. It's got a lot of advantages over GNU Make. My favorites are: no need for tabs, no need for .PHONYs. No builds, just command running. Run arbitrary language commands with a shebang.

## Dependencies

- [Elm](https://guide.elm-lang.org/install/elm.html): `brew install elm elm-format`
- [just](https://github.com/casey/just): `brew install just`
- [entr](https://eradman.com/entrproject/): `brew install entr`

## Todo

- [ ] edit list items
- [ ] LIST OF LISTS
- [ ] tags?
- [x] Filter list items on Complete, Active, All (Use enum)
- [x] Localstorage
