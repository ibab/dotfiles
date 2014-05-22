
termtitle="%n@%m:%~"
if [ $TERM != "linux" ]; then
    print -Pn "\e]2;$termtitle\a"
fi

if [[ "$TERM" =~ ".*-256color" ]]; then
    local primary="113"
    local dark="236"
    local normal="234"
    local black="0"
    local light="237"

    jobsprompt="%(1j.%F{221}(%j)%f .)"
    #promptmarker="%K{$dark}%F{$black}%F{113} %#%F{$dark}%k%f"
    promptmarker="%F{$primary}%B%#%f%b"
    foldersegment="%F{$dark}%K{$dark}%F{$primary}%~ %F{$black}%f%k"
    usernamesegment="%K{$black}%F{$primary}%n%f%k"
    sshsegment="%F{221}@%m%f"
    vcssegment=" %K{$black} %c%u %F{$primary}%f%K{$primary}%F{$dark} %b%F{$dark}  "
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

function precmd {
    vcs_info
    RPS1="${vcs_info_msg_0_}$foldersegment"
    if [ $TERM != "linux" ]; then
        print -Pn "\e]2;$termtitle\a" 2>/dev/null
    fi
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
   [ "$USER" != "ibabusch" ]
then
    PS1="$usernamesegment$padding$PS1"
fi

## vcs functions (for dynamic right prompt)
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

