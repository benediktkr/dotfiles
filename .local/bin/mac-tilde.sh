#!/bin/bash
#

usage() {
    echo "$(basename $0) [<restore|reset>] [-h]"
    exit 1
}

if [[ "$1" == "restore" ]]; then
    hidutil property --set '{"UserKeyMapping":[{"HIDKeyboardModifierMappingSrc":0x700000035,"HIDKeyboardModifierMappingDst":0x700000035},{"HIDKeyboardModifierMappingSrc":0x700000064,"HIDKeyboardModifierMappingDst":0x700000064}]}'
elif [[ "$1" == "reset" ]]; then
    hidutil property --set '{"UserKeyMapping":[]}'
elif [[ -z "$1" ]]; then
    hidutil property --set '{"UserKeyMapping":[{"HIDKeyboardModifierMappingSrc":0x700000035,"HIDKeyboardModifierMappingDst":0x700000064},{"HIDKeyboardModifierMappingSrc":0x700000064,"HIDKeyboardModifierMappingDst":0x700000035}]}'
else
    usage
fi
