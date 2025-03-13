#!/bin/sh
sptlrx pipe |\
    stdbuf -oL trans -show-translation N -show-translation-phonetics N -show-alternatives N -show-prompt-message N -show-languages N -no-auto -no-init -no-ansi |\
    stdbuf -oL awk 'NR % 3 == 2 {printf "%s\\n\n", $0}'
