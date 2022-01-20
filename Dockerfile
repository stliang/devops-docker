# LTS https://wiki.ubuntu.com/Releases
FROM ubuntu:20.04.3


ARG FLUX2_VERSION=0.25.3
ARG HELM3_VERSION=v3.7.2

# Labels.
LABEL gcloud_version="369.0.0+"
LABEL helm3_version=$HELM3_VERSION
LABEL terraform_version="1.1.3"
LABEL flux2_version=$FLUX2_VERSION

RUN apt-get install -y \
                      curl \
                      git \
                      jq \
                      netcat \
                      net-tools \
                      openssl \
                      python3 \
                      python3-pip \
                      unzip \
                      vim \
                      wget
                      
# gcp gcloud
RUN curl https://sdk.cloud.google.com > install.sh \
    && bash install.sh --disable-prompts \
    && rm -rf install.sh
ENV PATH=${PATH}:/root/google-cloud-sdk/bin

# Download validate.sh
RUN wget https://raw.githubusercontent.com/fluxcd/flux2-kustomize-helm-example/main/scripts/validate.sh

# flux cli (https://github.com/fluxcd/flux2)
RUN wget https://github.com/fluxcd/flux2/releases/download/v${FLUX2_VERSION}/flux_${FLUX2_VERSION}_linux_arm64.tar.gz \
    && tar xvzf flux_${FLUX2_VERSION}_linux_arm64.tar.gz \
    && mv flux /usr/local/bin/flux \
    && rm -rf flux_${FLUX2_VERSION}_linux_arm64.tar.gz
COPY validate.sh /usr/local/bin/validate-flux.sh
RUN chmod 755 /usr/local/bin/validate-flux.sh

# The mounted parent dir that contains all your own 'git clone' repos.
# This dir is going to be mounted via 'docker -v ...'
WORKDIR /GITHUB

ENTRYPOINT [ "/bin/sh" ]
