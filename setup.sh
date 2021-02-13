#!/bin/sh

set -e

# zsh.
ln -s "${PWD}/zshrc" ~/.zshrc

# Gnutils.
brew install coreutils findutils gnu-tar gnu-sed gawk gnutls gnu-indent gnu-getopt grep
ln -s "${PWD}/dircolors" ~/.dircolors

# Vim.
brew install neovim
sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
       https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
mkdir -p ~/.config/nvim/ && ln -s "${PWD}/vimrc" ~/.config/nvim/init.vim && ln -s ${PWD}/vim/* ~/.config/nvim/
nvim '+PluginInstall --sync' +q

# Tmux.
brew install tmux
ln -s "${PWD}/tmux.conf" ~/.tmux.conf

# Git.
ln -s "${PWD}/gitconfig" ~/.gitconfig

# VS Code.
cat vscode/extensions.list | xargs -n 1 code --install-extension
ln -s "${PWD}/vscode/settings.json" "${HOME}/Library/Application Support/Code/User/settings.json"
