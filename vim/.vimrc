
set nocompatible
set shell=/bin/bash

filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()



Plugin 'gmarik/Vundle.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-unimpaired'
Plugin 'sjl/gundo.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'JuliaLang/julia-vim'
"Plugin 'vim-pandoc/vim-pandoc'
Plugin 'tpope/vim-markdown'
Plugin 'SirVer/ultisnips'
"Plugin 'lukerandall/haskellmode-vim'
Plugin 'saltstack/salt-vim'
Plugin 'chriskempson/base16-vim'
"Plugin 'jhenahan/idris-vim'
Plugin 'mattn/emmet-vim'
"Plugin 'airblade/vim-gitgutter'
Plugin 'junegunn/vim-easy-align'
Plugin 'bling/vim-airline'
" Disable backups for sensitive data
Plugin 'https://gist.github.com/5890634.git'
Plugin 'tpope/vim-dispatch'
Plugin 'pbrisbin/html-template-syntax'
Plugin 'itchyny/calendar.vim'
Plugin 'jayflo/vim-skip'
Plugin 'Valloric/YouCompleteMe'
"Plugin 'jalcine/cmake.vim'
Plugin 'takac/vim-hardtime'
Plugin 'eagletmt/neco-ghc'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'ledger/vim-ledger'
Plugin 'rhysd/vim-clang-format'
Plugin 'kana/vim-operator-user'

call vundle#end()


let g:clang_format#style_options = {
            \ "AccessModifierOffset" : -4,
            \ "AlwaysBreakTemplateDeclarations" : "true",
            \ "Standard" : "C++11"}

autocmd FileType c,cpp,objc map <buffer><Leader>x <Plug>(operator-clang-format)

let g:calendar_google_calendar = 1
let g:calendar_google_task = 1


nnoremap <silent> _t :GhcModType<CR>

command! -nargs=1 Silent
\ | execute ':silent !'.<q-args>
\ | execute ':redraw!'

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


let g:syntastic_mode_map = { 'mode' : 'passive',
                            \ 'active_filetypes' : [],
                            \ 'passive_filetypes' : [] }

let g:pandoc_no_folding = 1

let g:haddock_browser = "firefox"
let g:haddock_docdir = ""

let g:pad_dir = "~/.pad"

set foldlevelstart=20

let g:ycm_confirm_extra_conf = 0
let g:ycm_min_num_of_chars_for_completion = 4
let g:ycm_filetype_whitelist = { 'c': 1, 'cpp': 1 }
let g:ycm_complete_in_comments_and_strings = 0
let g:ycm_key_list_select_completion = ['<TAB>', '<Down>']
"let g:ycm_key_invoke_completion = '<c-h>'
let g:ycm_global_ycm_extra_conf = '~/.vim/ycm.py'
let g:ycm_enable_diagnostic_signs = 0
let g:ycm_enable_diagnostic_highlighting = 1
let g:ycm_auto_trigger = 1


let g:UltiSnipsExpandTrigger="<c-j>"
let g:UltiSnipsJumpForwardTrigger="<c-j>"
let g:UltiSnipsJumpBackwardTrigger="<c-k>"
let g:UltiSnipsEditSplit="vertical"
let g:UltiSnipsSnippetsDir="~/.vim/snippets"
let g:UltiSnipsSnippetDirectories=["UltiSnips"]

let g:surround_indent=1

au BufNewFile,BufRead wscript* set filetype=python

let g:jedi#show_function_definition = 0

nnoremap <F11> :Make!<CR>

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

" Spell check languages
set spelllang=en,de
set spellfile=~/.vim/spell/spell.UTF-8.add

" Custom colorscheme
colorscheme desert256

" Statusbar
set stl=[%1*%n%0*]\  "buffer number
set stl+=%F\         "tail of the filename
" set stl+=[%{strlen(&fenc)?&fenc:'none'}]\  "file encoding
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

if has("cscope")
    set csto=0
    set cst
    set nocsverb
    " add any database in current directory
    if filereadable("cscope.out")
        cs add cscope.out
    " else add database pointed to by environment
    elseif $CSCOPE_DB != ""
        cs add $CSCOPE_DB
    endif
    set csverb
endif

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

" Can't live without these
cnoremap <C-a> <Home>
cnoremap <C-e> <End>

" Finding merge conflicts quickly
nmap <silent> <leader>m <ESC>/\v^[<=>]{7}( .*\|$)<CR>

" Conveniently inserting spaces
nmap <silent> <leader><space> i<space><ESC>l

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

" Don't know if this does anything
set ttyfast

" Automatic reload
set autoread

" Don't force me to save when switching buffers
"set hidden

" GUI options
set guifont=DejaVu\ Sans\ Mono\ for\ Powerline\ 10
set guioptions-=m  " Remove menubar
set guioptions-=T  " Remove toolbar
set guioptions-=r  " Remove right-hand scrollbar
set guioptions-=L  " Remove left-hand scrollbar

" Hidden character toggling
set nolist
set listchars=tab:»\ ,eol:¬,trail:⋅,extends:❯,precedes:❮
noremap <silent><leader>l :set list!<CR>
autocmd Filetype python map <buffer> <silent><leader>i :call system("ipython kernel &")<CR>:sleep 1<CR>:IPython<CR>

" Show linebreaks
set showbreak=↪

" select EVERYTHING!
map <Leader>a ggVG

map <Leader>j :make<CR>

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

set backupdir=~/.vim/backup
set noswapfile

" Undo settings
set undofile
set undodir=~/.vim/undodir
set undolevels=1000
set undoreload=10000

" EasyMotion settings
let g:EasyMotion_leader_key = "<Leader>"

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
