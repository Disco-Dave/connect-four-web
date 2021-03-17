FROM haskell:buster AS build

COPY stack.yaml stack.yaml.lock package.yaml ./

RUN stack build --install-ghc --only-dependencies

COPY . .

RUN stack build --install-ghc --copy-bins --local-bin-path ./dist

FROM debian:buster-slim

COPY --from=build /dist/connect-four-web-exe ./

CMD ./connect-four-web-exe
