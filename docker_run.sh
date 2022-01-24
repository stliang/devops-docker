#!/usr/bin/env bash

# Does:
# 1.) Mounts your native local directory where you store your github repos
# 2.) Mounts your native local gcloud config
# 3.) Executes docker run with the mounts

# Assumptions
# 1.) Public cloud provider is GCP
# 2.) This devops-docker is built and published to a container registry such as GCR
# 3.) You specify the local directory of your stored your github repos in $GITHUB_DIR


DOCKER_PLATFORM="${DOCKER_PLATFORM:-linux/amd64}"  # Default linux/amd64

DOCKER_CPUS="${DOCKER_CPUS:-1}"                    # Default 1 cpu
DOCKER_MEMORY="${DOCKER_MEMORY:-1g}"               # Default 1GB memory

GCP_DIR="${HOME}/.config"
KUBE_DIR="${HOME}/.kube"
ZSH_HISTORY_FILE="${HOME}/.zsh_history"
GIT_CONFIG_FILE="${HOME}/.gitconfig"
GITHUB_DIR="${GITHUB_DIR:-${HOME}/GITHUB}"

DOCKER_IMAGE="${DOCKER_IMAGE:-stliang/devops}"
IMAGE_TAG="${IMAGE_TAG:-latest}"

for DIR in ${GCP_DIR} ${KUBE_DIR} ${GITHUB_DIR}; do
    if [[ ! -d ${DIR} ]]; then
        echo "Creating ${DIR}"
        mkdir -p ${DIR}
    fi
done

for CONFIG_FILE in ${ZSH_HISTORY_FILE} ${GIT_CONFIG_FILE}; do
    if [ ! -f ${CONFIG_FILE} ]; then
        touch -a ${CONFIG_FILE}
    fi
done


docker run --rm -it \
        --platform ${DOCKER_PLATFORM} \
        --cpus     ${DOCKER_CPUS} \
        --memory   ${DOCKER_MEMORY} \
        -v ${GCP_DIR}:/root/.config \
        -v ${GIT_CONFIG_FILE}:/root/.gitconfig \
        -v ${KUBE_DIR}:/root/.kube \
        -v ${HOME}/.ssh:/root/.ssh \
        -v ${ZSH_HISTORY_FILE}:/root/.zsh_history \
        -v ${GITHUB_DIR}:/GITHUB \
    ${DOCKER_IMAGE}:${IMAGE_TAG}
