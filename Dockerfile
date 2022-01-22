# LTS https://wiki.ubuntu.com/Releases
FROM ubuntu:20.04

ARG DEBIAN_FRONTEND=noninteractive

ARG FLUX2_VERSION=0.25.3
ARG HELM3_VERSION=v3.7.2
ARG GOMPLATE_VERSION=v3.9.0

ARG KUBECTL_VERSION=v1.23.0
ARG K9S_VERSION=v0.25.18
ARG KUBESEAL_VERSION=v0.16.0
ARG KUBEVAL_VERSION=0.16.0
ARG KUSTOMIZE_VERSION=v4.4.1
ARG KYML_VERSION=20210610
ARG POPEYE_VERSION=v0.9.6
ARG PLUTO_VERSION=4.2.0
ARG SOPS_VERSION=v3.7.1

ARG TERRAFORM_DOCS_VERSION=v0.16.0
ARG TFLINT_VERSION=v0.33.2
ARG VCLUSTER_VERSION=v0.5.1
ARG YQ_VERSION=v4.16.1

# Labels.
LABEL gcloud_version="369.0.0+"
LABEL helm3_version=$HELM3_VERSION
LABEL terraform_version="1.1.3"
LABEL flux2_version=$FLUX2_VERSION
LABEL k9s_version=$K9S_VERSION
LABEL kubeseal_version=$KUBESEAL_VERSION
LABEL kubeval_version=$KUBEVAL_VERSION
LABEL kustomize_version=$KUSTOMIZE_VERSION

RUN apt-get update && \
    apt-get install -y locales \
    && echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
    && locale-gen en_US.UTF-8

RUN apt-get install -y \
                      curl \
                      dnsutils \
                      fonts-powerline \
                      git \
                      iproute2 \
                      jq \
                      lsof \
                      netcat \
                      net-tools \
                      python3 \
                      python3-pip \
                      software-properties-common \
                      sudo \
                      tree \
                      unzip \
                      vim \
                      wget \
                      zip \
                      zsh \
                      zsh-syntax-highlighting \
    && rm -rf /var/lib/apt/lists/*

                      
# gcp gcloud
RUN curl https://sdk.cloud.google.com > install.sh \
    && bash install.sh --disable-prompts \
    && rm -rf install.sh
ENV PATH=${PATH}:/root/google-cloud-sdk/bin

# Download validate.sh
RUN wget https://raw.githubusercontent.com/fluxcd/flux2-kustomize-helm-example/main/scripts/validate.sh \
    && mv validate.sh /usr/local/bin/validate-flux.sh
RUN chmod 755 /usr/local/bin/validate-flux.sh

# helm (https://helm.sh/docs/helm/helm_install/)
RUN curl -LO https://get.helm.sh/helm-${HELM3_VERSION}-linux-amd64.tar.gz \
    && tar -zxvf helm-${HELM3_VERSION}-linux-amd64.tar.gz \
    && mv linux-amd64/helm /usr/local/bin/helm3 \
    && rm -rf helm-${HELM3_VERSION}-linux-amd64.tar.gz linux-amd64

RUN ln -s /usr/local/bin/helm3 /usr/local/bin/helm

# https://github.com/databus23/helm-diff
RUN mkdir -p ~/.helm/plugins \
    && helm3 plugin install https://github.com/databus23/helm-diff \
    && rm -rf /tmp/helm-diff*
    
# flux cli (https://github.com/fluxcd/flux2)
RUN wget https://github.com/fluxcd/flux2/releases/download/v${FLUX2_VERSION}/flux_${FLUX2_VERSION}_linux_arm64.tar.gz \
    && tar xvzf flux_${FLUX2_VERSION}_linux_arm64.tar.gz \
    && mv flux /usr/local/bin/flux \
    && rm -rf flux_${FLUX2_VERSION}_linux_arm64.tar.gz
    
# gomplate (https://github.com/hairyhenderson/gomplate)
RUN curl -o /usr/local/bin/gomplate -sSL https://github.com/hairyhenderson/gomplate/releases/download/${GOMPLATE_VERSION}/gomplate_linux-amd64 \
    && chmod 755 /usr/local/bin/gomplate
    
# kubectl (https://kubernetes.io/docs/tasks/tools/install-kubectl-linux/)
RUN curl -LO https://dl.k8s.io/release/${KUBECTL_VERSION}/bin/linux/amd64/kubectl \
    && install -o root -g root -m 0755 kubectl /usr/local/bin/kubectl \
    && rm -rf kubectl

# k9s (https://github.com/derailed/k9s)
RUN curl -LO https://github.com/derailed/k9s/releases/download/${K9S_VERSION}/k9s_Linux_x86_64.tar.gz \
    && tar xvzf k9s_Linux_x86_64.tar.gz \
    && mv k9s /usr/local/bin/k9s \
    && rm -rf LICENSE README.md k9s_Linux_x86_64.tar.gz

RUN wget https://github.com/bitnami-labs/sealed-secrets/releases/download/${KUBESEAL_VERSION}/kubeseal-linux-amd64 -O kubeseal \
    && install -m 755 kubeseal /usr/local/bin/kubeseal \
    && rm -rf kubeseal

# kubeval (https://github.com/instrumenta/kubeval)
RUN wget https://github.com/instrumenta/kubeval/releases/download/${KUBEVAL_VERSION}/kubeval-linux-amd64.tar.gz \
    && tar xvzf kubeval-linux-amd64.tar.gz \
    && mv kubeval /usr/local/bin/kubeval \
    && rm -rf LICENSE README.md kubeval-linux-amd64.tar.gz

# kustomize (https://kubectl.docs.kubernetes.io/installation/kustomize/)
RUN wget https://github.com/kubernetes-sigs/kustomize/releases/download/kustomize%2F${KUSTOMIZE_VERSION}/kustomize_${KUSTOMIZE_VERSION}_linux_amd64.tar.gz \
    && tar xvf kustomize_${KUSTOMIZE_VERSION}_linux_amd64.tar.gz \
    && mv kustomize /usr/local/bin/kustomize \
    && rm -rf kustomize_${KUSTOMIZE_VERSION}_linux_amd64.tar.gz

# kyml https://github.com/frigus02/kyml
RUN curl -sfL -o /usr/local/bin/kyml https://github.com/frigus02/kyml/releases/download/v${KYML_VERSION}/kyml_${KYML_VERSION}_linux_amd64 \
    && chmod +x /usr/local/bin/kyml

# popeye (https://github.com/derailed/popeye)
RUN curl -LO https://github.com/derailed/popeye/releases/download/${POPEYE_VERSION}/popeye_Linux_x86_64.tar.gz \
    && tar xvzf popeye_Linux_x86_64.tar.gz \
    && mv popeye /usr/local/bin/popeye \
    && rm -rf LICENSE README.md popeye_Linux_x86_64.tar.gz

# pluto (https://github.com/FairwindsOps/pluto)
RUN curl -LO https://github.com/FairwindsOps/pluto/releases/download/v${PLUTO_VERSION}/pluto_${PLUTO_VERSION}_linux_amd64.tar.gz \
    && tar xvzf pluto_${PLUTO_VERSION}_linux_amd64.tar.gz \
    && mv pluto /usr/local/bin/pluto \
    && rm -rf LICENSE README.md pluto_${PLUTO_VERSION}_linux_amd64.tar.gz

# sops (https://github.com/mozilla/sops)
RUN curl -sSL -o /usr/local/bin/sops https://github.com/mozilla/sops/releases/download/${SOPS_VERSION}/sops-${SOPS_VERSION}.linux \
    && chmod +x /usr/local/bin/sops

# sops (https://github.com/mozilla/sops)
RUN curl -sSL -o /usr/local/bin/sops https://github.com/mozilla/sops/releases/download/${SOPS_VERSION}/sops-${SOPS_VERSION}.linux \
    && chmod +x /usr/local/bin/sops

# terraform-docs (https://github.com/terraform-docs/terraform-docs)
RUN wget https://github.com/terraform-docs/terraform-docs/releases/download/${TERRAFORM_DOCS_VERSION}/terraform-docs-${TERRAFORM_DOCS_VERSION}-linux-amd64.tar.gz \
    && tar xvzf terraform-docs-${TERRAFORM_DOCS_VERSION}-linux-amd64.tar.gz \
    && mv terraform-docs /usr/local/bin/terraform-docs \
    && rm -rf LICENSE README.md terraform-docs-${TERRAFORM_DOCS_VERSION}-linux-amd64.tar.gz

RUN curl -L https://raw.githubusercontent.com/warrensbox/terraform-switcher/release/install.sh | bash

# tflint (https://github.com/terraform-linters/tflint)
RUN wget https://github.com/terraform-linters/tflint/releases/download/${TFLINT_VERSION}/tflint_linux_amd64.zip \
    && unzip tflint_linux_amd64.zip  \
    && mv tflint /usr/local/bin/tflint \
    && rm -rf tflint_linux_amd64.zip

# vcluster (https://www.vcluster.com/)
RUN curl -s -L "https://github.com/loft-sh/vcluster/releases/${VCLUSTER_VERSION}" | sed -nE 's!.*"([^"]*vcluster-linux-amd64)".*!https://github.com\1!p' | xargs -n 1 curl -L -o vcluster && chmod +x vcluster \
    && mv vcluster /usr/local/bin

# yq (https://github.com/mikefarah/yq)
RUN wget https://github.com/mikefarah/yq/releases/download/${YQ_VERSION}/yq_linux_amd64.tar.gz -O - | \
    tar xz && mv yq_linux_amd64 /usr/bin/yq

# The mounted parent dir that contains all your own 'git clone' repos.
# This dir is going to be mounted via 'docker -v ...'
WORKDIR /GITHUB

ENTRYPOINT [ "/bin/zsh" ]
