FROM haskell:9

WORKDIR /opt/website

RUN cabal update
COPY ./Website.cabal /opt/website/Website.cabal
RUN cabal build --only-dependencies -j$(nproc)

COPY ./app /opt/website/app
COPY ./src /opt/website/src
COPY ./test /opt/website/test
COPY ./favicon.ico /opt/website/favicon.ico
COPY ./htmx.min.js /opt/website/htmx.min.js
COPY ./main.css /opt/website/main.css
COPY ./main.js /opt/website/main.js
COPY ./CHANGELOG.md /opt/website/CHANGELOG.md
COPY ./LICENSE /opt/website/LICENSE

RUN cabal install exe:Website

EXPOSE 8080

CMD ["Website"]