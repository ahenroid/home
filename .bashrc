# .bashrc

# Env variables
export EDITOR=emacs
export PS1="\h% "

# Aliases
alias l="ls"
alias ll="ls -l"
alias clean="rm -f *~ .*~"
alias rclean="find . '(' -name '*~' -or -name '.*~' ')' -exec rm {} \; -print"

# Vagrant
if [[ "$USER" == "vagrant" ]]; then
   PS1=""
   if [ -f /etc/redhat-release ]; then
      if [[ `grep Fedora /etc/redhat-release` ]]; then
        PS1="fedora"
      else
        PS1="redhat"
      fi
   elif [ -f /etc/debian_version ]; then
      PS1="debian"
   elif [ -f /etc/SUSE-release ]; then
      PS1="suse"
   fi
   export PS1="vg:$PS1% "
fi

# OSX
if [[ "`uname`" == "Darwin" ]]; then
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
  eval "$(rbenv init -)"
fi

