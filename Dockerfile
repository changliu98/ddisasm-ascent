FROM stargazermiao/ddisasm

RUN apt-get update && apt-get install -y curl
RUN curl https://sh.rustup.rs -sSf | bash -s -- -y
RUN echo 'source $HOME/.cargo/env' >> $HOME/.bashrc
