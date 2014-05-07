# zsh configuration file
#

fpath=(~/.zsh/completion $fpath)

HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
set PROMPT_SUBST

stty stop undef

export EDITOR=vim
export GOPATH=$HOME/Code/Go

source ~/.zsh/prompt.zsh
source ~/.zsh/aliases.zsh

# import giant list of ls colors:
if [ "$TERM" = "rxvt-unicode-256color" ] || [ "$TERM" = "st-256color" ] || [ "$TERM" = "screen-256color" ]; then
    eval $(dircolors -b ~/.dircolors)
fi

export LESS='-Ri'

setopt incappendhistory
setopt histignoredups
setopt histignorespace
setopt extended_history

setopt correct
setopt autocd
setopt extendedglob
setopt nomatch
setopt histignorespace
unsetopt beep
bindkey -e
setopt hashlistall
autoload -U edit-command-line
zle -N edit-command-line
bindkey '^xe' edit-command-line
bindkey '^x^e' edit-command-line

# Completion

autoload -Uz compinit

fpath=(~/.zsh/compdef $fpath)

compinit -u
compdef _ack ack

zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zcache
zstyle ':completion:*' verbose yes
zstyle ':completion:*:default' list-colors $LS_COLORS # ls-style colors in completion
zstyle ':compinstall' filename "$HOME/.zshrc"
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}' # complete upper-case from lower-case
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:*:kill:*:processes' command 'ps --forest -A -o pid,user,cmd'
zstyle ':completion:*:ssh:*' tag-order \
zstyle ':completion::(^approximate*):*:functions' ignored-patterns '_*'    # Ignore completion functions for commands you don't have:
zstyle ":completion:*:commands" rehash 1
zstyle ':completion:*:*:vim:*:*files' ignored-patterns '*.o' '*.hi'

if [ -f "/usr/bin/fasd" ];
then
    eval "$(fasd --init auto)"
    alias v='f -e vim' # quick opening files with vim
    bindkey '^X^A' fasd-complete
    c() {
      if [ -x "$@" ]; then
        cd "$@"
      else
        fasd_cd -d "$@"
      fi
    }
fi

alias note="python ~/Work/note/note.py"
