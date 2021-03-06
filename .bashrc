# .bashrc -*- mode: Shell-script;-*-

# Env variables
export EDITOR=emacs
if [ -d "/emacs-24.3/bin" ]; then
  export PATH="/emacs-24.3/bin:$PATH"
fi
if [ -d "$HOME/bin" ]; then
  export PATH="$HOME/bin:$PATH"
fi

# Aliases
alias l="ls"
alias ll="ls -l"
alias clean="rm -f *~ .*~"
alias rclean="find . '(' -name '*~' -or -name '.*~' ')' -exec rm {} \; -print"
alias screen="/usr/bin/screen -dRR"

# Prompt
PS1="\h"
if [ -f /etc/redhat-release ]; then
    if grep -q Fedora /etc/redhat-release; then
	PS1="fedora"
    elif grep -q CentOS /etc/redhat-release; then
        PS1="centos"
    else
        PS1="redhat"
    fi
elif [ -f /etc/lsb-release ] && grep -iq ubuntu /etc/lsb-release; then
    PS1="ubuntu"
elif [ -f /etc/debian_version ]; then
    PS1="debian"
elif [ -f /etc/SUSE-release ]; then
    PS1="suse"
fi
if [[ "$USER" = "vagrant" ]]; then
    PS1="vg:$PS1"
fi
if [[ "$TERM" = "screen" ]]; then
    PS1="sc:$PS1"
fi
export PS1="$PS1% "

# OSX
if [[ "`uname`" = "Darwin" ]]; then
  # alias override
  export LSCOLORS=ExGxBxDxCxEgEdxbxgxcxd
  alias ls="ls -G"
  alias vg="vagrant"
  # emacs override
  export EDITOR="/Applications/Emacs.app/Contents/MacOS/Emacs"
  alias emacs="$EDITOR"
fi

# Ruby
if [ -d "$HOME/.rbenv" ]; then
  export PATH="$HOME/.rbenv/bin:$PATH"
  export RUBY_CONFIGURE_OPTS="--enable-shared"
  eval "$(rbenv init -)"
fi

# Slice
if [ -d "$HOME/.slice" ]; then
    source "$HOME/.slice/bin/activate"
    source "$HOME/.slice/`whoami`-openrc.sh"
fi

# VM tool
if [ -f "$HOME/bin/vm" ]; then
    eval "$($HOME/bin/vm --init)"
fi
