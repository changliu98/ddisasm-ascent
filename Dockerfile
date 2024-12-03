FROM stargazermiao/ddisasm

RUN apt-get update && apt-get install -y curl vim git python3 python3-pip
RUN curl https://sh.rustup.rs -sSf | bash -s -- -y
RUN echo 'source $HOME/.cargo/env' >> $HOME/.bashrc
RUN bash -c "$(curl -fsSL https://raw.githubusercontent.com/ohmybash/oh-my-bash/master/tools/install.sh)"
ENV FUNCTOR=/usr/local/src/ddisasm/build/lib/libfunctors.so
ENV LD_LIBRARY_PATH=$FUNCOR:$LD_LIBRARY_PATH
