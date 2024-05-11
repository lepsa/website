# Get all the deps built for both the exe and tests
FROM haskell:9 as build_base

WORKDIR /opt/website

ARG flags

RUN cabal update
COPY ./Website.cabal /opt/website/Website.cabal
RUN cabal build -f "$flags" --only-dependencies -j$(nproc)

COPY ./app /opt/website/app
COPY ./src /opt/website/src
COPY ./test /opt/website/test
COPY ./CHANGELOG.md /opt/website/CHANGELOG.md
COPY ./LICENSE /opt/website/LICENSE

# Actually build the main binary
FROM build_base as build

WORKDIR /opt/website

ARG flags

RUN cabal install -f "$flags" --installdir=. --install-method=copy exe:Website

# What actually runs, no haskell compiler stuff
FROM debian:buster as web

COPY ./static /opt/website/static
COPY --from=build /opt/website/Website /opt/website/Website

WORKDIR /opt/website

EXPOSE 8080

CMD ["/opt/website/Website"]

# Run tests
FROM build_base as test

WORKDIR /opt/website

ARG flags

CMD cabal -f $flags run Website-test