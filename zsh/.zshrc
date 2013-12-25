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

if [ "$COLORTERM" = "gnome-terminal" ]; then
    export TERM=xterm-256color
fi

if [ "$TERM" = "rxvt-unicode-256color" ]; then
    export TERMINAL='urxvt -e'
fi

# import giant list of ls colors:
if [ "$TERM" = "rxvt-unicode-256color" ] || [ "$TERM" = "xterm-256color" ]; then
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

# Aliases

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
alias t='tree -F --noreport'
alias mail=mutt
alias music=ncmpcpp
alias pylab='ipython --pylab'
alias open='mimeo'
alias spawn='urxvt -e "cd $(pwd); vim"'

if [ -f "/usr/bin/root" ]
then
    alias root='root -l'
fi

function emacs {
    /usr/bin/emacsclient -c $@ &|
}

# Prompt

if [[ "$TERM" =~ ".*-256color" ]]; then
    local primary="113"
    local dark="235"
    local light="237"

    jobsprompt="%(1j.%F{yellow}[%j]%f.)"
    promptmarker="%K{$light}%F{$dark}%K{$light}%k%F{$light}%f"
    foldersegment="%F{$dark}%K{$dark}%F{$primary}%~ %F{232}%f%k"
    usernamesegment="%K{$dark}%F{$primary}%n%f%k"
    sshsegment="%K{$dark}%F{223}@%m%f%k"
    PS2="%K{$light} %k  "
else
    jobsprompt="%(1j.%F{yellow}[%j]%f.)"
    promptmarker="%F{green}%#%f"
    foldersegment="%{%B%F{blue}%}%~%{%f%b%}"
    usernamesegment="%{%F{green}%}(%n)%f"
    sshsegment="%{%F{yellow}%}(%m)%f"
    PS2="| "
fi

PS1="$jobsprompt$promptmarker "

function precmd {
    vcs_info
    RPS1="${vcs_info_msg_0_} $foldersegment"
}

# Add yellow marker when connected over SSH
if [ -n "$SSH_CONNECTION" ]; then
    PS1="$sshsegment$PS1"
fi

if [ "$USER" != "igor" ] && [ "$USER" != "ibabuschkin" ] && [ "$USER" != "ibabusch" ];
then
    PS1="$usernamesegment$PS1"
fi

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

#alias note="python ~/note/note.py"
