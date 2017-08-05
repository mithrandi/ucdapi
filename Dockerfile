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
 libpq5 \
 && rm -rf /var/lib/apt/lists/*
COPY --from=server ["/dist/ucdapi", "/usr/local/bin/"]
ENTRYPOINT ["/usr/local/bin/ucdapi"]
