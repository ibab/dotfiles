
alias ip='ip -4' # for now
alias ccat='pygmentize -f terminal256'
alias ipython3=ipython
alias ls='ls --color=auto -Fh --group-directories-first'
alias ll='ls -1Fha --color=force | less -R'
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
function open {
  xdg-open $1 > /dev/null 2>&1 &|
}
alias spawn='urxvt -e "cd $(pwd); vim"'
alias userctl='systemctl --user'
alias mk='make 2>&1 | tee build.log | less -RiMS +F'
alias journalctl='journalctl -b'

# Attaches to a tmux session or starts a new one
function att {
  ssh $1 -t LANG=$LANG tmux attach-session
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

