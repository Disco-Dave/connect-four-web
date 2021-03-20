FROM alpine:3.13 AS build_base

RUN echo "Installing system dependencies for ghcup" &&\
    apk --no-cache add \
        curl \
        gcc \
        g++ \
        gmp-dev \
        ncurses-dev \
        libffi-dev \
        make \
        xz \
        tar \
        perl


ARG GHCUP_VERSION=0.1.14
RUN echo "Downloading and installing ghcup $GHCUP_VERSION" &&\
    curl "https://downloads.haskell.org/~ghcup/$GHCUP_VERSION/x86_64-linux-ghcup-$GHCUP_VERSION" -o /usr/bin/ghcup &&\
    chmod +x /usr/bin/ghcup

ENV PATH="/.ghcup/bin:$PATH"
ENV GHCUP_INSTALL_BASE_PREFIX="/"


ARG GHC_VERSION=8.10.3
RUN echo "Downloading and installing ghc $GHC_VERSION" &&\
    ghcup install ghc --set $GHC_VERSION


ARG STACK_VERSION=2.5.1
RUN echo "Downloading and install stack $STACK_VERSION" &&\
    curl -L "https://github.com/commercialhaskell/stack/releases/download/v$STACK_VERSION/stack-$STACK_VERSION-linux-x86_64.tar.gz" -o /tmp/stack.tar.gz &&\
    tar xf /tmp/stack.tar.gz --directory=/tmp &&\
    mv "/tmp/stack-$STACK_VERSION-linux-x86_64/stack" /usr/bin/stack &&\
    rm -rf /tmp/stack/tar.gz "/tmp/stack-$STACK_VERSION-linux-x86_64"


FROM build_base as build

RUN echo "Installing system dependencies for project" &&\
    apk --no-cache add \
        binutils-gold \
        zlib-dev \
        zlib-static

COPY stack.yaml stack.yaml.lock package.yaml ./
RUN echo "Compiling haskell dependencies for project" &&\
    stack --system-ghc --no-install-ghc build --only-dependencies

COPY . ./
RUN echo "Compiling haskell project" &&\
    stack --system-ghc --no-install-ghc install --flag connect-four-web:static --local-bin-path .


FROM alpine:3.13

COPY --from=build /connect-four-web-exe /connect-four-web-exe

CMD /connect-four-web-exe
