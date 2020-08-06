#!/bin/sh

if [ -f "/etc/profile" ]; then
	source "/etc/profile"
fi
if [ -d "/etc/profile.d" ]; then
	for file in /etc/profile.d/*.sh; do
		source "$file";
	done
fi

export PATH="$HOME/.local/bin:$HOME/.cache/dotfiles/bin:$PATH"

export EDITOR="/usr/bin/emacsclient"
export VISUAL="$EDITOR"

export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
