#!/bin/bash

set -e

self_clone=$0
cleanup() {
    if [[ -n "${yadm_tempdir}" ]]; then
        echo "Cleaning up '${yadm_tempdir}'"
        rm -rfv ${yadm_tempdir}
    fi
    rm -v ${self_clone}
}
trap cleanup EXIT


dotfiles_repo="https://git.sudo.is/ben/dotfiles.git"
yadm_tempdir=$(mktemp -d -p $HOME -t ".tmpXXXXXXXXXX")
yadm_url="https://github.com/TheLocehiliosan/yadm/raw/master/yadm"

command -v wget >/dev/null && have_wget="true"
command -v curl >/dev/null && have_curl="true"

if [[ "${have_wget}" == "true" ]]; then
    wget $yadm_url -O ${yadm_tempdir}/yadm
elif [[ "${have_curl}" == "true" ]]; then
    curl $yadm_url > ${yadm_tempdir}/yadm
else
    echo "Neither 'curl' nor 'wget' found in PATH."
    exit 2
fi

yadm_repo_path=${HOME}/.local/share/yadm/repo.git
if [[ -d "${yadm_repo_path}" ]]; then
    rm -rfv ${yadm_repo_path}
fi

chmod +x ${yadm_tempdir}/yadm
${yadm_tempdir}/yadm clone ${dotfiles_repo} $@
${yadm_tempdir}/yadm checkout ${HOME}

