" use vim settings, instead of vi settings
" must be first
set nocompatible

" turn off vim's backup files and other annoying settings
set nobackup
set noswapfile
set noerrorbells
set nowrap

" prevent delays when switching to insert mode (in default /etc/vimrc on
" fedora/amzn)
set timeout
set timeoutlen=100

" searching: highlight, match while typing, case-insensitive
set hlsearch
set incsearch
set ignorecase

" disables backspace in normal mode
"set backspace=indent,eol,start

" show a few lines around the cursor while moving around
set scrolloff=10

" tabs should be 4 spaces by default, but 2 spaces in some file types
set expandtab shiftwidth=4 tabstop=4 softtabstop=4
autocmd FileType toml setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2
autocmd FileType yaml setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2

" always enable autoindent
set ai

" keep undo history between sessions
set undofile
"set undodir=~/.cache/vim/undo

" hide header in file explorer
let g:netrw_banner=0

" enable syntax highlighting
syntax on
filetype on
filetype plugin on
"filetype indent on

" show relative line numbers, and absolute line number current line
" uncommented so it doensnt get loaded as default, toggled with the
" setnnoremap key binding
"set number
"set relativenumber

" default to using path of the currently open file:
set autochdir

" change colors when switching modes
"  from: https://vim.fandom.com/wiki/Change_statusline_color_to_show_insert_or_normal_mode
set laststatus=2
hi StatusLine ctermfg=DarkBlue ctermbg=Black
au InsertLeave * hi StatusLine term=reverse ctermfg=DarkBlue ctermbg=Black
au InsertEnter * hi StatusLine term=reverse ctermfg=DarkGreen ctermbg=Black

set background=dark

" load everforest colorscheme (after StatusLine colors are set, overrides
" those)
"let g:colors_override = 'soft'
"let g:everforest_better_performance='1'
let g:everforest_background = 'soft'
let g:everforest_disable_italic_comment='1'
colorscheme everforest

" set syntax highlighting for files with non-standard names
"  from: https://ls3.io/posts/jenkinsfile_vim_highlighting/
au BufNewFile,BufRead Jenkinsfile setf groovy
au BufNewFile,BufRead *.py.j2 setf python
au BufNewFile,BufRead *.yml.j2 setf yaml
au BufNewFile,BufRead *.yaml.j2 setf yaml
au BufNewFile,BufRead *.euw setf yaml
au BufNewFile,BufRead *.use setf yaml
au BufNewFile,BufRead *.sh.j2 setf sh
au BufNewFile,BufRead *.conf.j2 setf nginx
au BufNewFile,BufRead *.groovy.j2 setf groovy
" jinja syntax isnt built into vim, and is read from ~/.vim/syntax/jinja.vim
" using 'setf jinja' doesnt work since that has not been loaded yet (probably)
" but if 'filetype' has been set, the syntax highlight gets applied when it
" has been loaded.
au BufNewFile,BufRead *.hbs set filetype=jinja

" always remove trailing whitespace
autocmd BufWritePre * :%s/\s\+$//e

" jump to last position in a file
autocmd BufReadPost *
\ if line("'\"") > 0 && line ("'\"") <= line("$") |
\   exe "normal! g'\"" |
\ endif

" disable <del> i normal mode
nnoremap <del> <nop>
nnoremap <space> <nop>
nnoremap <backspace> <nop>

" normal mode key bindigs
nnoremap 1 <c-w>w
nnoremap 2 :set relativenumber!<cr>:set number!<cr>
nnoremap 3 <c-w>s
nnoremap 4 :b#<cr>
nnoremap 8 :25Lexplore!<cr>
nnoremap t :term<cr>

" terminal mode key bindings
tnoremap <esc> <c-\><c-n>
tnoremap <c-esc> <esc>
