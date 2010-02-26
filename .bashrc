
# listing aliases
alias lx='ls -lXB'               # sort by extension
alias lk='ls -lSr'               # sort by size
alias lm='ls -alh |more'         # pipe through 'more'
alias lsd='ls -lh | grep "^d"'   # list only directories
alias lsl='ls -lh | grep "^l"'   # list only links
# list long, human readable, ignore implied~,
# ignore compiled python files
alias ll='ls -hlB --group-directories-first --hide=*.pyc'
# list including .dotfiles
alias lsa='ls -lAh --group-directories-first'
# Disk usage
alias du="du -h"


# PATH and SOURCE stuff here
export PATH=$PATH:~/bin
export RUBYOPT=rubygems 
export PYTHONPATH=/home/david/programming/python/google_appengine/:/home/david/programming/python/google_appengine/lib/:/home/david/programming/python/genlog:
alias reload='source ~/.bashrc'

# Default editor
export EDITOR="emacs -nw -q"

# emacs modes 
alias gemacs="emacs-snapshot-gtk"
alias nemacs="emacs -nw"
alias emacs="emacs-snapshot-gtk"

# ssh shortcuts
alias webserver='ssh primrose.co.uk'
alias databaseserver='ssh 78.136.27.15'
alias happenup='ssh happenup@happenup.com'
alias massive='ssh massive'
alias cdmassive='DIR=`pwd` && ssh -t massive "cd $DIR; bash"'
alias pmassive='ssh -p 56 massive'

# moving aliases
alias ..='cd ..'
alias ...='cd ../..'
alias downloads='cd ~/downloads'
alias cdpython='cd ~/programming/python'
alias cdperl='cd ~/programming/perl'
alias cdshell='cd ~/programming/shell'
alias cdlisp='cd ~/programming/lisp'
alias cdruby='cd ~/programming/ruby'
alias cdhaskell='cd ~/programming/haskell'
alias cdjs='cd ~/programming/javascript'


# apt aliases
alias ainstall='sudo apt-get install'

# Misc 
alias rtfm='man'


## Bash Functions ##

# Makes directory then moves into it
function mkcdr {
    mkdir -p -v $1
    cd $1
}

# Easy extract
extract () {
  if [ -f $1 ] ; then
      case $1 in
          *.tar.bz2)   tar xvjf $1    ;;
          *.tar.gz)    tar xvzf $1    ;;
          *.bz2)       bunzip2 $1     ;;
          *.rar)       rar x $1       ;;
          *.gz)        gunzip $1      ;;
          *.tar)       tar xvf $1     ;;
          *.tbz2)      tar xvjf $1    ;;
          *.tgz)       tar xvzf $1    ;;
          *.zip)       unzip $1       ;;
          *.Z)         uncompress $1  ;;
          *.7z)        7z x $1        ;;
          *)           echo "don't know how to extract '$1'..." ;;
      esac
  else
      echo "'$1' is not a valid file!"
  fi
}


#### Boilerplate stuff added by various os defaults ####

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
# don't overwrite GNU Midnight Commander's setting of `ignorespace'.
export HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups
# ... or force ignoredups and ignorespace
export HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# Alias definitions.

#if [ -f ~/.bash_aliases ]; then
#    . ~/.bash_aliases
#fi

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    eval "`dircolors -b`"
    alias ls='ls --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi
