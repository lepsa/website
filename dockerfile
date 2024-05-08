FROM haskell:9 as build

WORKDIR /opt/website

RUN cabal update
COPY ./Website.cabal /opt/website/Website.cabal
RUN cabal build --only-dependencies -j$(nproc)

COPY ./app /opt/website/app
COPY ./src /opt/website/src
COPY ./test /opt/website/test
COPY ./CHANGELOG.md /opt/website/CHANGELOG.md
COPY ./LICENSE /opt/website/LICENSE

RUN cabal install --installdir=. --install-method=copy exe:Website

FROM debian:buster as run

COPY ./static /opt/website/static
COPY --from=build /opt/website/Website /opt/website/Website

WORKDIR /opt/website

EXPOSE 8080

CMD ["/opt/website/Website"]