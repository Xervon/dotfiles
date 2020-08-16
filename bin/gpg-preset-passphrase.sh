#!/usr/bin/env bash

keyinfo="$(gpg-connect-agent <<<"KEYINFO --no-ask "$1" Err Pmt Des" | grep -E '^S KEYINFO' | awk '{ print $7; }')"

if ! [[ "x$keyinfo" == "x1" ]]; then
    gxmessage "Please unlock pass to preset gpg passphrase for '$1'"

    pass="$(pass "gpg/$1")";

    /usr/libexec/gpg-preset-passphrase --preset "$1" <<<"$pass";
fi
