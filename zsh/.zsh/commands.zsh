
alias ccat='pygmentize -f terminal256'
alias ipython3=ipython
alias ls='ls --color=auto -Fh --group-directories-first'
alias ll='ls -1Fha --color=force | less -R'
alias -- '+x'='chmod +x'
alias -- '-x'='chmod -x'
function ssh {
    if [[ $1 == hepgpu1 ]]; then
      SSHPASS=$(pass hepgpu1) sshpass -e ssh -q $*
    else
      ssh -q $*
    fi
}
alias tb='ls ~/.local/share/Trash/files'
alias wat='aplay ~/Documents/wat.wav > /dev/null 2>&1 &|'
if [ -n "/usr/bin/hub" ]; then
    alias git=hub
fi
alias mail=mutt
alias music=ncmpcpp
function open {
  xdg-open $1 > /dev/null 2>&1 &|
}
alias spawn='urxvt -e "cd $(pwd); vim"'
alias userctl='systemctl --user'
alias mk='make 2>&1 | tee build.log | less -RiMS +F'
alias journalctl='journalctl -e -b'
if [ -f "/usr/bin/nvim" ]; then
    alias vim=nvim
    alias vimdiff="nvim -d"
fi
# ff stands for find file and is a command I regularly use when searching for
# files
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

alias root='root -l'

function emacs {
    /usr/bin/emacsclient -c $@ &|
}

function run {
    gaudirun.py $1 | tee $1.log
}

