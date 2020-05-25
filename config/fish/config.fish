if type bax 2>&1 >/dev/null
	bax source "/etc/profile"
	for file in /etc/profile.d/*.sh;
		bax source "$file";
	end
end

function fish_greeting
	fortune -as;
	echo;
	neofetch;
end

function l
	command ls -lahtp --color $argv;
end

function e
	command emacsclient -n $argv
end

set -gx SSH_AUTH_SOCK '/var/run/user/'(id -u)'/gnupg/S.gpg-agent.ssh'

set -gx PATH "$HOME/.local/bin" $PATH

set -gx EDITOR /usr/bin/emacsclient
set -gx VISUAL $EDITOR

set -gx FZF_TMUX 1
set -gx FZF_COMPLETE 0
set -gx FZF_LEGACY_KEYBINDINGS 0
set -gx FZF_EDITOR_OPEN_CMD_OPTS "-n"

vf install auto_activation projects global_requirements > /dev/null
