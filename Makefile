gen:
	twirec -e Book -i . --elm-out src/Gen --elm-version 0.17

build:
	elm package install

html:
	elm make src/Main.elm

js:
	elm make src/Main.elm --output=elm.js
