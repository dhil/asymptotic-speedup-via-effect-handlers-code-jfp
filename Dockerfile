FROM debian:bookworm

ENV DEBIAN_FRONTEND noninteractive
ENV DEBCONF_NONINTERACTIVE_SEEN true

WORKDIR /artifact

## Install dependencies

# Install standard utility + opam + libgmp-dev
RUN apt-get update && \
    apt-get install -y opam git curl libgmp-dev

# Install and setup OCaml environment
RUN opam init -y --disable-sandboxing --bare
RUN echo "test -r $HOME/.opam/opam-init/init.sh && . $HOME/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true" >> $HOME/.profile
RUN opam switch create 5.1.1
RUN eval $(opam env)

## Copy local sources
COPY lib ./lib
COPY bin ./bin
COPY dune-project .
COPY benchmarks.opam .
COPY Makefile .

## Install dependencies
RUN opam install --deps-only -y ./benchmarks.opam

# Final steps
ENV DEBIAN_FRONTEND teletype
CMD ["/bin/bash"]
