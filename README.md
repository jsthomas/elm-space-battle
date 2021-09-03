# elm-space-battle

This repo implements a simple browser based game loosely based on the
classic game _Spacewar!_, which was developed at MIT for the PDP-1
in 1962. I worked on this project in September of 2021, while I was at
the [Recurse Center](https://www.recurse.com/), in order to learn more
about front-end programming in Elm.

*Disclaimer*: The code in this repo is not especially polished. Pull
requests, issues, and feedback are all welcome, but I'm sure there are
many refactor opportunities, and I'm not holding this repo up as model
code. In particular, you probably shouldn't reference this project to
learn "idiomatic" Elm programming or game programming.

## How to build the game locally

Instructions for installing Elm can be found
[here](https://guide.elm-lang.org/install/). I'm using version
`0.19.1` of the Elm compiler. Once Elm is installed, you'll need to
compile the project, like this:

```
elm make src/Main.elm --output elm.js
```

This should generate a new file called `elm.js` that is referenced from `index.html`. If the build is successful, you should see:

```
Success! Compiled 1 module.

    Main ───> elm.js
```

Open `index.html` in your browser (e.g. `firefox index.html` at the
command line), and you should see a copy of the game:

## References

- You can play the original (two player) version of _Spacewar!_
  [here](https://spacewar.oversigma.com/).
- I found this general Elm [tutorial](https://elmprogramming.com/)
  useful while working on this project.
- I also referenced the docs for
  [joakin/elm-canvas](https://package.elm-lang.org/packages/joakin/elm-canvas/4.3.0/)
  frequently for this project.
