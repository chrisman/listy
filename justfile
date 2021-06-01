watch:
  find src/* | entr just build

build:
  elm make src/Main.elm --output=dist/main.js

format:
  elm-format --yes src/

prod:
  elm make src/Main.elm --optimize --output=dist/main.js
  uglifyjs elm.js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output dist/main.min.js
  wc dist/main.js dist/main.min.js

clean:
  rm -f dist/main.js dist/main.min.js
