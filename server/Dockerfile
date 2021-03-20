FROM alpine:3.13

RUN echo "Installing system dependencies for ghcup" &&\
    apk --no-cache add \
        bash \
        binutils-gold \
        curl \
        g++ \
        gcc \
        gmp-dev \
        libffi-dev \
        make \
        ncurses-dev \
        perl \
        shadow \
        tar \
        xz


ARG GHCUP_VERSION=0.1.14
RUN echo "Downloading and installing ghcup $GHCUP_VERSION" &&\
    curl "https://downloads.haskell.org/~ghcup/$GHCUP_VERSION/x86_64-linux-ghcup-$GHCUP_VERSION" -o /usr/bin/ghcup &&\
    chmod +x /usr/bin/ghcup

ENV PATH="/.ghcup/bin:$PATH"
ENV GHCUP_INSTALL_BASE_PREFIX="/"


ARG GHC_VERSION=8.10.3
RUN echo "Downloading and installing ghc $GHC_VERSION" &&\
    ghcup install ghc --set $GHC_VERSION


RUN echo "Installing system dependencies for project" &&\
    apk --no-cache add \
        zlib-dev \
        zlib-static
