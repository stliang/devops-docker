# LTS https://wiki.ubuntu.com/Releases
FROM ubuntu:20.04


ARG FLUX2_VERSION=0.25.3
ARG HELM3_VERSION=v3.7.2
ARG KUBECTL_VERSION=v1.23.0
ARG K9S_VERSION=v0.25.18
ARG KUBESEAL_VERSION=v0.16.0
ARG KUBEVAL_VERSION=0.16.1
ARG KUSTOMIZE_VERSION=v4.4.1


# Labels.
LABEL gcloud_version="369.0.0+"
LABEL helm3_version=$HELM3_VERSION
LABEL terraform_version="1.1.3"
LABEL flux2_version=$FLUX2_VERSION
LABEL k9s_version=$K9S_VERSION
LABEL kubeseal_version=$KUBESEAL_VERSION
LABEL kubeval_version=$KUBEVAL_VERSION
LABEL kustomize_version=$KUSTOMIZE_VERSION

RUN apt-get update

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
RUN wget https://raw.githubusercontent.com/fluxcd/flux2-kustomize-helm-example/main/scripts/validate.sh \
    && mv validate.sh /usr/local/bin/validate-flux.sh
RUN chmod 755 /usr/local/bin/validate-flux.sh

# flux cli (https://github.com/fluxcd/flux2)
RUN wget https://github.com/fluxcd/flux2/releases/download/v${FLUX2_VERSION}/flux_${FLUX2_VERSION}_linux_arm64.tar.gz \
    && tar xvzf flux_${FLUX2_VERSION}_linux_arm64.tar.gz \
    && mv flux /usr/local/bin/flux \
    && rm -rf flux_${FLUX2_VERSION}_linux_arm64.tar.gz
    
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

# The mounted parent dir that contains all your own 'git clone' repos.
# This dir is going to be mounted via 'docker -v ...'
WORKDIR /GITHUB

ENTRYPOINT [ "/bin/sh" ]
