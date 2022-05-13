# LTS https://wiki.ubuntu.com/Releases
FROM ubuntu:20.04

# avoid interaction when installing software-properties-common
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
                      haskell-platform \
    && rm -rf /var/lib/apt/lists/*

                      
# gcloud, GCP utility
RUN curl https://sdk.cloud.google.com > install.sh \
    && bash install.sh --disable-prompts \
    && rm -rf install.sh
ENV PATH=${PATH}:/root/google-cloud-sdk/bin

# helm, K8S deployment tool (https://helm.sh/docs/helm/helm_install/)
RUN curl -LO https://get.helm.sh/helm-${HELM3_VERSION}-linux-amd64.tar.gz \
    && tar -zxvf helm-${HELM3_VERSION}-linux-amd64.tar.gz \
    && mv linux-amd64/helm /usr/local/bin/helm3 \
    && rm -rf helm-${HELM3_VERSION}-linux-amd64.tar.gz linux-amd64

RUN ln -s /usr/local/bin/helm3 /usr/local/bin/helm

# helm diff between actual and desired states (https://github.com/databus23/helm-diff)
RUN mkdir -p ~/.helm/plugins \
    && helm3 plugin install https://github.com/databus23/helm-diff \
    && rm -rf /tmp/helm-diff*

# validate.sh flux sanity check
RUN wget https://raw.githubusercontent.com/fluxcd/flux2-kustomize-helm-example/main/scripts/validate.sh \
    && mv validate.sh /usr/local/bin/validate-flux.sh
RUN chmod 755 /usr/local/bin/validate-flux.sh

# flux2 cli, gitops (https://github.com/fluxcd/flux2)
RUN wget https://github.com/fluxcd/flux2/releases/download/v${FLUX2_VERSION}/flux_${FLUX2_VERSION}_linux_arm64.tar.gz \
    && tar xvzf flux_${FLUX2_VERSION}_linux_arm64.tar.gz \
    && mv flux /usr/local/bin/flux \
    && rm -rf flux_${FLUX2_VERSION}_linux_arm64.tar.gz
    
# gomplate, template renderer (https://github.com/hairyhenderson/gomplate)
RUN curl -o /usr/local/bin/gomplate -sSL https://github.com/hairyhenderson/gomplate/releases/download/${GOMPLATE_VERSION}/gomplate_linux-amd64 \
    && chmod 755 /usr/local/bin/gomplate
    
# kubectl, K8S cli (https://kubernetes.io/docs/tasks/tools/install-kubectl-linux/)
RUN curl -LO https://dl.k8s.io/release/${KUBECTL_VERSION}/bin/linux/amd64/kubectl \
    && install -o root -g root -m 0755 kubectl /usr/local/bin/kubectl \
    && rm -rf kubectl

# k9s, K8S xterm UI (https://github.com/derailed/k9s)
RUN curl -LO https://github.com/derailed/k9s/releases/download/${K9S_VERSION}/k9s_Linux_x86_64.tar.gz \
    && tar xvzf k9s_Linux_x86_64.tar.gz \
    && mv k9s /usr/local/bin/k9s \
    && rm -rf LICENSE README.md k9s_Linux_x86_64.tar.gz

# kubeseal, K8S secrets in github
RUN wget https://github.com/bitnami-labs/sealed-secrets/releases/download/${KUBESEAL_VERSION}/kubeseal-linux-amd64 -O kubeseal \
    && install -m 755 kubeseal /usr/local/bin/kubeseal \
    && rm -rf kubeseal

# kubeval, K8S manifest validation  (https://github.com/instrumenta/kubeval)
RUN wget https://github.com/instrumenta/kubeval/releases/download/${KUBEVAL_VERSION}/kubeval-linux-amd64.tar.gz \
    && tar xvzf kubeval-linux-amd64.tar.gz \
    && mv kubeval /usr/local/bin/kubeval \
    && rm -rf LICENSE README.md kubeval-linux-amd64.tar.gz

# kustomize, customize K8S manifest (https://kubectl.docs.kubernetes.io/installation/kustomize/)
RUN wget https://github.com/kubernetes-sigs/kustomize/releases/download/kustomize%2F${KUSTOMIZE_VERSION}/kustomize_${KUSTOMIZE_VERSION}_linux_amd64.tar.gz \
    && tar xvf kustomize_${KUSTOMIZE_VERSION}_linux_amd64.tar.gz \
    && mv kustomize /usr/local/bin/kustomize \
    && rm -rf kustomize_${KUSTOMIZE_VERSION}_linux_amd64.tar.gz

# kyml, concatenating, testing, and templating command pipeline builder (https://github.com/frigus02/kyml)
RUN curl -sfL -o /usr/local/bin/kyml https://github.com/frigus02/kyml/releases/download/v${KYML_VERSION}/kyml_${KYML_VERSION}_linux_amd64 \
    && chmod +x /usr/local/bin/kyml

# popeye, utility that scans live Kubernetes cluster and reports potential issues (https://github.com/derailed/popeye)
RUN curl -LO https://github.com/derailed/popeye/releases/download/${POPEYE_VERSION}/popeye_Linux_x86_64.tar.gz \
    && tar xvzf popeye_Linux_x86_64.tar.gz \
    && mv popeye /usr/local/bin/popeye \
    && rm -rf LICENSE README.md popeye_Linux_x86_64.tar.gz

# pluto, find deprecates K8S apiVersions (https://github.com/FairwindsOps/pluto)
RUN curl -LO https://github.com/FairwindsOps/pluto/releases/download/v${PLUTO_VERSION}/pluto_${PLUTO_VERSION}_linux_amd64.tar.gz \
    && tar xvzf pluto_${PLUTO_VERSION}_linux_amd64.tar.gz \
    && mv pluto /usr/local/bin/pluto \
    && rm -rf LICENSE README.md pluto_${PLUTO_VERSION}_linux_amd64.tar.gz

# sops, editor of encrypted files (https://github.com/mozilla/sops)
RUN curl -sSL -o /usr/local/bin/sops https://github.com/mozilla/sops/releases/download/${SOPS_VERSION}/sops-${SOPS_VERSION}.linux \
    && chmod +x /usr/local/bin/sops

# terraform-docs, generate documentation from Terraform modules (https://github.com/terraform-docs/terraform-docs)
RUN wget https://github.com/terraform-docs/terraform-docs/releases/download/${TERRAFORM_DOCS_VERSION}/terraform-docs-${TERRAFORM_DOCS_VERSION}-linux-amd64.tar.gz \
    && tar xvzf terraform-docs-${TERRAFORM_DOCS_VERSION}-linux-amd64.tar.gz \
    && mv terraform-docs /usr/local/bin/terraform-docs \
    && rm -rf LICENSE README.md terraform-docs-${TERRAFORM_DOCS_VERSION}-linux-amd64.tar.gz

# tfswitch, switch between different versions of (https://github.com/warrensbox/terraform-switcher)
RUN curl -L https://raw.githubusercontent.com/warrensbox/terraform-switcher/release/install.sh | bash

# tflint, terraform file linter (https://github.com/terraform-linters/tflint)
RUN wget https://github.com/terraform-linters/tflint/releases/download/${TFLINT_VERSION}/tflint_linux_amd64.zip \
    && unzip tflint_linux_amd64.zip  \
    && mv tflint /usr/local/bin/tflint \
    && rm -rf tflint_linux_amd64.zip

# vcluster, virtual cluster inside namespace (https://www.vcluster.com/)
RUN curl -s -L "https://github.com/loft-sh/vcluster/releases/${VCLUSTER_VERSION}" | sed -nE 's!.*"([^"]*vcluster-linux-amd64)".*!https://github.com\1!p' | xargs -n 1 curl -L -o vcluster && chmod +x vcluster \
    && mv vcluster /usr/local/bin

# yq, YAML processor (https://github.com/mikefarah/yq)
RUN wget https://github.com/mikefarah/yq/releases/download/${YQ_VERSION}/yq_linux_amd64.tar.gz -O - | \
    tar xz && mv yq_linux_amd64 /usr/bin/yq

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

COPY requirements.txt /opt/app/requirements.txt

# The mounted parent dir that contains all your own 'git clone' repos.
# This dir is going to be mounted via 'docker -v ...'
WORKDIR /opt/app

# install Python packages
RUN pip3 install -r requirements.txt

ENTRYPOINT [ "/bin/zsh" ]
