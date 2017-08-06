FROM fpco/stack-build:lts-8.20 as server
COPY ["stack.yaml", "/src/"]
WORKDIR /src
RUN stack setup
COPY ["package.yaml", "/src/"]
RUN stack build --only-dependencies
RUN mkdir /dist
COPY [".", "/src/"]
RUN stack --local-bin-path /dist build --copy-bins

FROM debian:stretch
RUN apt-get update && env DEBIAN_FRONTEND='noninteractive' apt-get install -y \
 libgmp10 libsqlite3-0 libexpat1 \
 && rm -rf /var/lib/apt/lists/*
COPY --from=server ["/dist/ucdapi", "/usr/local/bin/"]
COPY ["db", "/db"]
ENV SQLITE_DATABASE=/db/UCD.sqlite3
ENTRYPOINT ["/usr/local/bin/ucdapi"]
