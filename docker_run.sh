#!/usr/bin/env bash

# Does:
# 1.) Mounts your native local directory where you store your github repos
# 2.) Mounts your native local gcloud config
# 3.) Executes docker run with the mounts

# Assumptions
# 1.) Public cloud provider is GCP
# 2.) This devops-docker is built and published to a container registry such as GCR
# 3.) You specify the local directory of your stored your github repos in $GITHUB_DIR


DOCKER_PLATFORM
DOCKER_CPUS
DOCKER_MEMORY

GCP_DIR
GIT_CONFIG_DIR
KUBE_DIR
ZSH_HISTORY_DIR
GITHUB_DIR

DOCKER_IMAGE
IMAGE_TAG


docker run --rm -it \
        --platform ${DOCKER_PLATFORM} \
        --cpus     ${DOCKER_CPUS} \
        --memory   ${DOCKER_MEMORY} \
        -v ${GCP_DIR}:/root/.config \
        -v ${GIT_CONFIG_DIR}:/root/.gitconfig \
        -v ${KUBE_DIR}:/root/.kube \
        -v ${HOME}/.ssh:/root/.ssh \
        -v ${ZSH_HISTORY_DIR}:/root/.zsh_history \
        -v ${GITHUB_DIR}:/GITHUB \
    ${DOCKER_IMAGE}:${IMAGE_TAG}
