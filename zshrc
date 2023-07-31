# zsh configuration file.

fpath=(~/.zsh/completion $fpath)

HISTFILE="${HOME}/.histfile"
HISTSIZE=100000
SAVEHIST=100000
set PROMPT_SUBST

stty stop undef

## Prompt settings.
if [[ "$TERM" =~ ".*-256color" ]]; then
    local primary="113"
    local dark="236"
    local normal="234"
    local black="0"
    local light="237"

    jobsprompt="%(1j.%F{221}(%j)%f .)"
    promptmarker="%F{$primary}%B▸%f%b"
    foldersegment="%F{$dark}%K{$dark}%F{$primary}%~ %k%F{$dark}%S%s%f"
    usernamesegment="%F{$primary}%n%f"
    sshsegment="%F{221}@%m%f"
    vcssegment="%c%u %F{$primary}%f%K{$primary}%F{$dark} %b%F{$dark}  "
    vcsactionsegment="%K{$black} %c%u %F{$primary}%K{$primary}%F{$dark} %a %F{$dark  %b%F{$dark}  "
    PS2="%F{$primary}┃%f "
else
    jobsprompt="%(1j.%F{yellow}[%j]%f.)"
    promptmarker="%F{green}%#%f"
    foldersegment="%{%B%F{blue}%}%~%{%f%b%}"
    usernamesegment="%{%F{green}%}(%n)%f"
    sshsegment="%{%F{yellow}%}(%m)%f"
    vcssegment="%c%u %F{2}%b %f"
    vcsactionsegment="%c%u %f{3}[%a]%f%f{2}(%b)%f"
    PS2="| "
fi

PS1="$jobsprompt$promptmarker "

# Refresh environment variables in tmux.
if [ -n "$TMUX" ]; then
    function refresh {
        sshauth=$(tmux show-environment | grep "^SSH_AUTH_SOCK")
        if [ $sshauth ]; then
            export $sshauth
        fi
        display=$(tmux show-environment | grep "^DISPLAY")
        if [ $display ]; then
            export $display
        fi
    }
else
    function refresh { }
fi

function preexec {
    # Refresh environment if inside tmux
    refresh
}

function precmd {
    # Update prompt
    vcs_info
    RPS1="${vcs_info_msg_0_}$foldersegment"
}

# Add yellow marker when connected over SSH
if [ -n "$SSH_CONNECTION" ]; then
    PS1="$sshsegment $PS1"
    padding=""
else
    padding=" "
fi

if [ "$USER" != "igor" ] && 
   [ "$USER" != "ibabuschkin" ] &&
   [ "$USER" != "ibab" ] &&
   [ "$USER" != "ibabusch" ]
then
    PS1="$usernamesegment$padding$PS1"
fi

# vcs functions (for dynamic right prompt)
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' stagedstr '%F{2}M%f'
zstyle ':vcs_info:git:*' unstagedstr '%F{3}M%f'
zstyle ':vcs_info:git:*' formats "$vcssegment"
zstyle ':vcs_info:git:*' actionformats "$vcsactionsegment"
zstyle ':vcs_info:git*+set-message:*' hooks git-untracked

function +vi-git-untracked(){
    if [[ $(git rev-parse --is-inside-work-tree 2> /dev/null) == 'true' ]] && \
        git status --porcelain | fgrep '??' &> /dev/null ; then
        hook_com[unstaged]+='%F{1}?%f'
    fi
}

## Commands.
alias ls='ls --color=auto -Fh --group-directories-first'
alias -- '+x'='chmod +x'
alias -- '-x'='chmod -x'
if [ -f "/usr/bin/nvim" ]; then
    alias vim=nvim
    alias vimdiff="nvim -d"
fi

# ff stands for find file.
function ff {
    find . -iname "*$1*"
}

# Attaches to a tmux session or starts a new one
function att {
  ssh $1 -t LANG=$LANG tmux attach-session -t main
  if [ $? -eq 1 ]; then
      echo "Starting new session"
      ssh $1 -t LANG=$LANG tmux new-session -s main
  fi
}

# import giant list of ls colors:
if [ "$TERM" = "xterm-256color" ]; then
    eval $(dircolors -b ~/.dircolors)
fi

export LESS='-Ri'

setopt incappendhistory
setopt histignoredups
setopt histignorespace
setopt extended_history

setopt correct
#setopt autocd
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

alias vim=nvim
alias k=kubectl
export PATH="${HOME}/usr/local/opt/coreutils/libexec/gnubin:$PATH"
export MANPATH="/usr/local/opt/coreutils/libexec/gnuman:$MANPATH"
export PATH="${HOME}/.bin:$PATH"
export PATH="${HOME}/.cargo/bin:$PATH"
export PATH="${HOME}/.miniconda3/bin:$PATH"
if [ -f "${HOME}/.miniconda3/etc/profile.d/conda.sh" ]; then
    . "${HOME}/.miniconda3/etc/profile.d/conda.sh"
else
    export PATH="${HOME}/.miniconda3/bin:$PATH"
fi
