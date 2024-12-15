#!/bin/bash

if [[ "${OSTYPE}" == "darwin"* ]]; then
    ln -s ${HOME}/.config/rustic ${HOME}/Library/Application\ Support/rustic
fi 