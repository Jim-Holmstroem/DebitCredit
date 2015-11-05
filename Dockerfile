FROM haskell:7.10

RUN cabal update && cabal install \
    snap \
    snaplet-acid-state \
    json \
    time \
    iso8601-time

COPY Site.hs /opt/server/Site.hs

WORKDIR /opt/server

CMD ["runhaskell Site.hs"]
