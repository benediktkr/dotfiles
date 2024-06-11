" this line must be first
set nocompatible

set nobackup
set noswapfile
set noerrorbells
set nowrap
set scrolloff=10
set autochdir
" auto-indent
set ai

" prevent delays when switching to insert mode (in default /etc/vimrc on fedora/amzn)
set timeout
set timeoutlen=100

" searching: highlight, match while typing, case-insensitive
set hlsearch
set incsearch
set ignorecase

" tabs should be 4 spaces by default, but 2 spaces in some file types
set expandtab shiftwidth=4 tabstop=4 softtabstop=4
autocmd FileType toml setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2
autocmd FileType yaml setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2
autocmd FileType lua setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2

" remove trailing whitespaces
autocmd BufWritePre * :%s/\s\+$//e

" jump to last position in a file (missing from /etc/vimrc on debian-based distros)
autocmd BufReadPost *
\ if line("'\"") > 0 && line ("'\"") <= line("$") |
\   exe "normal! g'\"" |
\ endif

" keep undo history between sessions
set undofile
set undodir=~/.cache/vim/undo

" hide header and the './' entry in file explorer
let g:netrw_banner=0
let g:netrw_list_hide= '\~$,\.so$,\.swp$,\.zip$,\.pyc$,^__pycache__\/$,\.git[a-z]*\/$,\.tfplan$,\.terraform\/$,\.lock\.hcl$,\.DS_Store\/$,^\.\=/\=$'

" syntax highlighting
syntax on
filetype on
filetype plugin on
"filetype indent on

set background=dark
let g:everforest_background = 'soft'
let g:everforest_disable_italic_comment='1'
colorscheme everforest

" change colors when switching modes (looks nicer if 'everforest' has been loaded first)
" https://vim.fandom.com/wiki/Change_statusline_color_to_show_insert_or_normal_mode
set laststatus=2
hi StatusLine ctermfg=DarkBlue ctermbg=Black
au InsertLeave * hi StatusLine term=reverse ctermfg=DarkBlue ctermbg=Black
au InsertEnter * hi StatusLine term=reverse ctermfg=DarkGreen ctermbg=Black

" set syntax highlighting for files with non-standard names
au BufNewFile,BufRead Jenkinsfile setf groovy
" Instead of '0*.{conf.,j2}':
au BufNewFile,BufRead *.conf.j2 setf nginx

au BufNewFile,BufRead *.yml.j2 setf yaml
au BufNewFile,BufRead *.yaml.j2 setf yaml
au BufNewFile,BufRead *.xml.j2 setf xml
au BufNewFile,BufRead *.use setf yaml
au BufNewFile,BufRead *.sh.j2 setf sh
au BufNewFile,BufRead *.py.j2 setf python
au BufNewFile,BufRead *.euw setf yaml
au BufNewFile,BufRead *.groovy.j2 setf groovy
" jinja syntax isnt built into vim, and is read from ~/.vim/syntax/jinja.vim.  using 'setf jinja'
" doesnt work (i think it hasnt been loaded at this point). instead set 'filetype'
au BufNewFile,BufRead *.hbs set filetype=jinja

" disable del/space/backspace in normal mode
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
