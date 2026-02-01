FROM ubuntu:22.04

ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8

# ---- System dependencies ----
RUN apt-get update
RUN apt-get install -y curl git build-essential

# ---- Non-root user ----
RUN useradd -ms /bin/bash user
USER user
WORKDIR /home/user

ENV HOME=/home/user
ENV PATH="$HOME/.local/bin:$PATH"

###############################################################################
# 
#  Haskell (ghcup)
#  FindFast Solution by https://www.github.com/mgrosser3
# 
###############################################################################

USER root
RUN apt-get install -y libgmp-dev
USER user

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 sh

ENV PATH="$HOME/.ghcup/bin/:$PATH"

RUN ghcup install ghc 9.6.7 && ghcup set ghc 9.6.7 \
 && ghcup install cabal 3.12.1.0 && ghcup set cabal 3.12.1.0

RUN git clone https://www.github.com/mgrosser3/findfast "findfast-haskell"

WORKDIR /home/user/findfast-haskell
RUN git checkout feature/v1.0.0
RUN cabal install --overwrite-policy=always --installdir="$HOME/.local/bin"


###############################################################################
# 
#  RUST
#  FindFast Solution by https://www.github.com/ardo314
# 
###############################################################################

WORKDIR /home/user
RUN curl https://sh.rustup.rs -sSf | sh -s -- -y
ENV PATH="$HOME/.cargo/bin:$PATH"
RUN rustup default stable

RUN git clone https://github.com/ardo314/find-fast.git "findfast-rust"
WORKDIR /home/user/findfast-rust
RUN cargo build --release
RUN cp target/release/find-fast "$HOME/.local/bin/find-fast"

###############################################################################
# 
#  Elixir
#  FindFast Solution by https://www.github.com/pherklotz
# 
###############################################################################

USER root
RUN apt-get install -y \
    libssl-dev \
    libncurses5-dev \
    libncursesw5-dev \
    unzip
USER user

# ---- asdf for Erlang & Elixir ----
RUN git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.14.0

ENV ASDF_DIR="$HOME/.asdf"
ENV PATH="$ASDF_DIR/bin:$ASDF_DIR/shims:$PATH"

RUN echo '. "$HOME/.asdf/asdf.sh"' >> ~/.bashrc

# Erlang / Elixir plugins
RUN asdf plugin add erlang https://github.com/asdf-vm/asdf-erlang.git \
 && asdf plugin add elixir https://github.com/asdf-vm/asdf-elixir.git

RUN asdf install erlang 26.2.1 \
 && asdf install elixir 1.18.0-otp-26 \
 && asdf global erlang 26.2.1 \
 && asdf global elixir 1.18.0-otp-26

WORKDIR /home/user
RUN git clone https://github.com/pherklotz/find-fast-elixir.git "findfast-elixir"
WORKDIR /home/user/findfast-elixir
RUN mix escript.build

# ---- sanity check ----
RUN ghc --version \
 && cabal --version \
 && elixir --version \
 && erl -version \
 && cargo --version

USER root
RUN rm -rf /var/lib/apt/lists/*
USER user
