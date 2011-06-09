# David's .bashrc

# Where am I ?
UNAME=uname

# Reload this file due to frequent edits
alias reload='source ~/.bashrc'

#### PATH ####
export PATH=$PATH:~/bin:~/opt/android/tools:~/builds/chrome-linux:~/local/node/bin
export PATH=$PATH:~/builds/chrome-linux
export RUBYOPT=rubygems
export PYTHONPATH=/home/david/programming/python/google_appengine/:/home/david/programming/python/google_appengine/lib/:/home/david/programming/python/genlog:../:../../:
export PYTHONSTARTUP=~/.pythonstartup
export INFOPATH=/home/david/emacs/info
export ONZOHOME=~/src/onzo/backend/
export LD_LIBRARY_PATH=`pwd`

#### Defaults ####
# Default editor
export EDITOR="emacs -nw -q"


#### Aiases ####
# listing aliases
if [ $UNAME='Darwin' ];
then
    alias ll='ls -l | grep -v -E ".pyc$|~$"'
else
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
    alias lz="ls -lZ"                # SELinux display
fi

## directory aliases
alias mkdir='mkdir -p'  #Make intermediaries
# Disk usage
alias du="du -h"
# Grepping
alias h="history | grep"
#enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    eval "`dircolors -b`"
    alias ls='ls --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# emacs modes
alias gemacs="emacs-snapshot-gtk"
alias nemacs="emacs -nw"

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

# local aliases
if [ -f ~/.bash_aliases ]; then
   . ~/.bash_aliases
fi

#### Bash Functions ####
function ginit {
    mkdir $1
    cd $1
    git init
}

# Killlist
function killps {
    ps aux | grep $1 | grep -v grep | awk {'print $2'} | xargs kill -9 2> /dev/null
}

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

# A frequent source of complaint from cat
# is that I've just asked it to cat a directory.
# If I have, just DWIM and do it happily.
function catorls () {
    if [ -d $1 ];
    then
        ls -l $1
    else
        cat $1
    fi
}
alias cat="catorls"

# don't put duplicate lines in the history. See bash(1) for more options
# don't overwrite GNU Midnight Commander's setting of `ignorespace'.
export HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups
# ... or force ignoredups and ignorespace
export HISTCONTROL=ignoreboth


#### Colours ^ Prompt ####
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac


### Completion ###
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

_django_completion()
{
    COMPREPLY=( $( COMP_WORDS="${COMP_WORDS[*]}" \
                   COMP_CWORD=$COMP_CWORD \
                       DJANGO_AUTO_COMPLETE=1 $1 ) )
}
complete -F _django_completion -o default django-admin.py manage.py django-admin django

_python_django_completion()
{
    if [[ ${COMP_CWORD} -ge 2 ]]; then
        PYTHON_EXE=$( basename -- ${COMP_WORDS[0]} )
        echo $PYTHON_EXE | egrep "python([2-9]\.[0-9])?" >/dev/null 2>&1
        if [[ $? == 0 ]]; then
            PYTHON_SCRIPT=$( basename -- ${COMP_WORDS[1]} )
            echo $PYTHON_SCRIPT | egrep "manage\.py|django-admin(\.py)?" >/dev/null 2>&1
            if [[ $? == 0 ]]; then
                COMPREPLY=( $( COMP_WORDS="${COMP_WORDS[*]:1}" \
                               COMP_CWORD=$(( COMP_CWORD-1 )) \
                               DJANGO_AUTO_COMPLETE=1 ${COMP_WORDS[*]} ) )
            fi
        fi
    fi
}

# Support for multiple interpreters.
unset pythons
if command -v whereis &>/dev/null; then
    python_interpreters=$(whereis python | cut -d " " -f 2-)
    for python in $python_interpreters; do
        pythons="${pythons} $(basename -- $python)"
    done
    pythons=$(echo $pythons | tr " " "\n" | sort -u | tr "\n" " ")
else
    pythons=python
fi

complete -F _python_django_completion -o default $pythons

_fab_completion() {
    COMPREPLY=()

    local cur="${COMP_WORDS[COMP_CWORD]}"

    tasks=$(fab --list|awk '{print $1}'|grep -v Available)
    COMPREPLY=( $(compgen -W "${tasks}" -- ${cur}) )
}

complete -F _fab_completion fab


# Goodbye Grep
which ack > /dev/null
if [ $? -ne 0 ] ; then
    which ack-grep > /dev/null
    if [ $? -ne 0 ] ; then
        echo "Where the ack has ack gone?"
    else
        alias ack="ack-grep"
    fi
fi

# keybindings
bind '"\C-f\C-g": "find . | grep "';
bind '"\C-f\C-x": "find . | xargs grep "'
bind '"\C-p\C-a": "ps aux | grep "'