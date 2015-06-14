# My configuration files

This repository contains my unix configuration files.
Each directory groups together config files for a single application or purpose. 
The config files in each directory belong into `$HOME`.

I use [GNU stow](https://www.gnu.org/software/stow/) to link them into my home directory.
For example, to set up vim on a new machine, I execute

```bash
git clone git@github.com:ibab/dotfiles ~/.dotfiles
cd ~/.dotfiles
stow vim
```

I also manage sensitive files like my email config and SSH keys through this repo, but they are stored on a separate server and accessed with [`git-annex`](https://git-annex.branchable.com/).
To set these up, I run
```bash
git annex enableremote $SECUREREMOTE
git annex get
```
on the server, where `SECUREREMOTE` is the name of the git-annex remote that stores my sensitive files.

## Screenshot

![Screenshot](screenshot.jpg)

