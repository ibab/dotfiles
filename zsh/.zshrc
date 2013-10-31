# zsh configuration file
#

fpath=(~/.zsh/completion $fpath)

HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000

stty stop undef

export EDITOR=vim
export GOPATH=$HOME/Code/Go

if [ "$COLORTERM" = "gnome-terminal" ]; then
    export TERM=xterm-256color
fi

if [ "$TERM" = "rxvt-unicode-256color" ]; then
    export TERMINAL='urxvt -e'
fi

# import giant list of ls colors:
if [ "$TERM" = "rxvt-unicode-256color" ] || [ "$COLORTERM" = "gnome-terminal" ]; then
    eval `dircolors -b ~/.dircolors`
fi

export LESS='-R'

setopt incappendhistory
setopt histignoredups
setopt histignorespace

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

alias ip='ip -4' # for now
alias ccat='pygmentize -f terminal256'
alias ipython3=ipython
alias ls='ls --color=auto -Fh --group-directories-first'
alias ll='ls -Fhla --color=force | less -R'
alias -- '+x'='chmod +x'
alias -- '-x'='chmod -x'
alias ssh='ssh -q'
alias tb='ls ~/.local/share/Trash/files'
alias wat='aplay ~/Documents/wat.wav > /dev/null 2>&1 &|'
alias git='noglob git'
alias curl='noglob curl'

function gvim() {
    /usr/bin/gvim "+cd $(pwd)" $@ > /dev/null &|
}

alias open='mimeo'

alias spawn='urxvt -e "cd $(pwd); vim"'

function emacs {
    /usr/bin/emacsclient -c $@ &|

}

alias mail=mutt
alias music=ncmpcpp
alias pylab='ipython --pylab'

if [ -f "/usr/bin/systemctl" ]
then
    alias start='sudo systemctl start'
    alias restart='sudo systemctl restart'
    alias stop='sudo systemctl stop'
    alias enable='sudo systemctl enable'
    alias status='systemctl status'
    alias disable='sudo systemctl disable'
    alias is-enabled='systemctl is-enabled'
    alias userctl='systemctl --user'
fi

if [ -f "/usr/bin/root" ]
then
    alias root='root -l'
fi

jobsprompt="%(1j.%F{yellow}[%j]%f.)"
promptmarker="%F{green}%#%f"

PS1="$jobsprompt$promptmarker "
PS2="| "

function precmd {
    vcs_info 'prompt'
    RPS1="${vcs_info_msg_0_} %{%B%F{blue}%}%~%{%f%b%}"
}

if [ "$USER" != "igor" ] && [ "$USER" != "ibabuschkin" ] && [ "$USER" != "ibabusch" ];
then
    PS1="%{%F{green}%}(%n)%f$PS1"
fi

# Add yellow marker when connected over SSH
if [ -n "$SSH_CONNECTION" ]; then
    PS1="%{%F{yellow}%}(%m)%f$PS1"
fi

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

users 'hosts:-host hosts:-domain:domain hosts:-ipaddr"IP\ Address *'

## vcs funtions (for dynamic right prompt)
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git hg
zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' stagedstr '%F{2}M%f'
zstyle ':vcs_info:git:*' unstagedstr '%F{1}M%f'

zstyle ':vcs_info:git:*' formats '%c%u %F{2}(%b)%f'
zstyle ':vcs_info:git:*' actionformats '%c%u %F{3}[%a]%f%F{2}(%b)%f'

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


PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
