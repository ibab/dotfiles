" Vim configuration file.

set nocompatible
set shell=/bin/bash

filetype off
call plug#begin('~/.vim/plugged')
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-markdown'
Plug 'tpope/vim-dispatch'
Plug 'sjl/gundo.vim'
Plug 'airblade/vim-gitgutter'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'pbrisbin/html-template-syntax'
Plug 'itchyny/calendar.vim'
Plug 'jayflo/vim-skip'
Plug 'junegunn/limelight.vim'
Plug 'google/vim-jsonnet'
call plug#end()

let g:jsonnet_fmt_options="-n 4"

let g:dispatch_handlers = [
      \ 'tmux',
      \ 'screen',
      \ 'windows',
      \ 'iterm',
      \ 'headless',
      \ 'x11',
      \ ]

let g:tex_conceal = ""

let g:airline_powerline_fonts=1
let g:airline_section_z = "%3l/%L,%2c"
let g:airline_section_warning=""
let g:airline#extensions#hunks#enabled = 0
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#buffer_nr_show = 1
let g:airline#extensions#tabline#buffer_nr_format = '%s:'
let g:airline#extensions#tabline#buffer_min_count = 2
let g:airline#extensions#tabline#fnamemod = ':t'
let g:airline#extensions#tagbar#enabled = 0
let g:airline_theme="wombat"
let g:airline_mode_map = {
    \ '__' : '-',
    \ 'n'  : '∅',
    \ 'i'  : 'Ϟ',
    \ 'R'  : 'R',
    \ 'c'  : 'C',
    \ 'v'  : '♐',
    \ 'V'  : '♐',
    \ '' : '♐',
    \ 's'  : 'S',
    \ 'S'  : 'S',
    \ '' : 'S',
    \ }
let g:airline#extensions#bufferline#overwrite_variables = 1
set ttimeoutlen=50

let g:gitgutter_sign_added = '│'
let g:gitgutter_sign_modified = '│'
let g:gitgutter_sign_removed = '_'
let g:gitgutter_sign_modified_removed = '_'

let g:syntastic_mode_map = { 'mode' : 'passive',
                            \ 'active_filetypes' : [],
                            \ 'passive_filetypes' : [] }

let g:pandoc_no_folding = 1

set foldlevelstart=20

let g:surround_indent=1
let g:surround_108 = "\\begin{\1environment: \1}\r\\end{\1\1}"

" Standard settings
set enc=utf-8
filetype plugin on
filetype indent on
set cinoptions+=L0
syntax enable
set backspace=eol,start,indent
set mouse=a
set completeopt=menu

" Mapleaders
let mapleader=" "
let maplocalleader=","

" Custom colorscheme
colorscheme desert256

" Statusbar
set stl=[%1*%n%0*]\  "buffer number
set stl+=%F\         "tail of the filename
set stl+=%m          "modified flag
set stl+=%2*%r%0*    "read only flag (red)
set stl+=%{fugitive#statusline()}\ 
set stl+=%y\          "filetype
set stl+=%=          "left/right separator
set stl+=%l/%L,\     "cursor line/total lines
set stl+=%2c\        "cursor column
set stl+=[%P]        "percent through file

" Statusbar always active
set laststatus=2

" Default indentation
set autoindent
set shiftwidth=4
set tabstop=4
set expandtab

" Keep selection when changing indentation
" (with temporary hack for tagging the cursor along)
vnoremap < <gv4h
vnoremap > >gv4l

" Wrapping
set nowrap

" Searching
set showmatch
set hlsearch
set ignorecase
set smartcase
set magic
set grepprg=grep\ -nH\ $*
set incsearch

cnoremap <C-a> <Home>
cnoremap <C-e> <End>

" Quiet output
set noshowcmd
set noshowmode
set noruler
set noerrorbells
set shortmess+=asIc

" Scrolling
set scrolloff=12
set sidescrolloff=3
set sidescroll=1

" Hidden character toggling
"set nolist
set list
set listchars=tab:»\ ,eol:¬,trail:⋅,extends:❯,precedes:❮
noremap <silent><leader>l :set list!<CR>
autocmd Filetype python map <buffer> <silent><leader>i :call system("ipython kernel &")<CR>:sleep 1<CR>:IPython<CR>

" Show linebreaks
set showbreak=↪

" Highlight current line
noremap <leader>h :set cursorline!<CR>

" Toggle line numbers
noremap <leader>n :set number!<CR>

" Quick window splitting
nnoremap <leader>w <C-w>v<C-w>l

" Faster window navigation with alt
" (even in the terminal)
nnoremap h <C-w>h
nnoremap j <C-w>j
nnoremap k <C-w>k
nnoremap l <C-w>l
nnoremap <M-h> <C-w>h
nnoremap <M-j> <C-w>j
nnoremap <M-k> <C-w>k
nnoremap <M-l> <C-w>l

" Window dimensions for splits
set noequalalways

" Y works like D
noremap Y y$

" Clear search
noremap <silent><Leader>/ :nohls<CR>

set backupdir=~/.config/nvim/backup
set noswapfile

" Undo settings
set undofile
set undodir=~/.config/nvim/undodir
set undolevels=1000
set undoreload=10000

" Gundo settings
let g:gundo_width = 55
noremap <F5> :GundoToggle<CR>

" File menu
set wildmenu
set wildignorecase
set wildignore=*.o,*.obj,*.bak,*.exe,*.so,*.class,*.hi,*.jpg,*.png,*.zip,*.tar,*.tar.gz
set wildmode=list:longest,full

" Ignored in completion
set suffixes=.bak,~,.h,.info,.swp,.obj,.class

" Fugitive settings
au bufwritepost fugitive://* set bufhidden=delete

" NERDTree settings
nmap <silent> <F7> :NERDTreeToggle<CR>
let NERDTreeShowHidden=0

" sudo write this
cmap W! silent w !sudo tee % >/dev/null

" More convenient copy/paste for the + register
noremap <Leader>y "+y
noremap <leader>Y "+Y
noremap <leader>p "+p
noremap <leader>P "+P

" Execute Python and show output
function! ReadPyResults_buffer()
    exe join(['w | new | setl bt=nofile | r !/usr/bin/env python ', expand('%:p')], '')
endfunction
nnoremap <Leader>r :execute ReadPyResults_buffer()<CR>

"When editing a file, always jump to the last cursor position
autocmd BufReadPost *
\ if line("'\"") > 0 && line ("'\"") <= line("$") |
\   exe "normal! g'\"" |
\ endif
