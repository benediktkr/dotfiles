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
yadm_url="https://github.com/yadm-dev/yadm/raw/master/yadm"
yadm_url2="https://raw.githubusercontent.com/yadm-dev/yadm/master/yadm"

if command -v wget >/dev/null; then
    wget $yadm_url -O ${yadm_tempdir}/yadm
elif command -v curl >/dev/null; then
    curl $yadm_url2 > ${yadm_tempdir}/yadm
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
#${yadm_tempdir}/yadm bootstrap
echo "Done! You may want to run 'yadm bootstrap' now."

