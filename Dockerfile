FROM haskell:8

#>>> APT PACKAGES
RUN apt-get update

RUN apt-get install -y \
                      fonts-powerline \
                      git \
                      jq \
                      python3 \
                      python3-pip \
                      software-properties-common \
                      sudo \
                      tree \
                      vim \
                      wget \
                      zsh \
                      zsh-syntax-highlighting \
    && rm -rf /var/lib/apt/lists/*

#>>> ZSH SETUP
# oh-my-zsh, fully loaded terminal (https://github.com/ohmyzsh/ohmyzsh#unattended-install)
RUN sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" "" --unattended

# zsh-autosuggestions (https://github.com/zsh-users/zsh-autosuggestions/blob/master/INSTALL.md#oh-my-zsh)
RUN git clone https://github.com/zsh-users/zsh-autosuggestions /root/.oh-my-zsh/custom/plugins/zsh-autosuggestions

# oh-my-zsh plugins
#   - https://github.com/ohmyzsh/ohmyzsh#enabling-plugins)=
#   - https://github.com/zsh-users/zsh-autosuggestions/blob/master/INSTALL.md#oh-my-zsh
RUN sed -i 's/plugins=(git)/plugins=(git kube-ps1 zsh-autosuggestions)/' ~/.zshrc

ENV LC_ALL=en_US.utf-8
ENV LANG=en_US.utf-8

#>>> WORK BENCHES
#RUN mkdir /opt/app/haskell /opt/app/python

#>>> PYTHON SETUP
WORKDIR /opt/app/python
COPY ./python/requirements.txt .
RUN pip3 install -r requirements.txt

#>>> CABAL SETUP
#WORKDIR /opt/app/haskell
WORKDIR /opt/app/haskell/term
RUN cabal update
#RUN cabal install aeson
#RUN cabal install lens-aeson 
#RUN cabal install bytestring
#RUN cabal install bytestring text
#RUN cabal install happy yesod-bin

# Add just the .cabal file to capture dependencies
COPY ./haskell/term .

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN cabal build --only-dependencies -j4
# RUN cabal install --only-dependencies

# Add and Install Application Code
# COPY . /opt/app
RUN cabal install
# RUN cabal build

#>>> FINAL
WORKDIR /opt/app
ENTRYPOINT [ "/bin/zsh" ]

#>>> Use cabal repl to start development
