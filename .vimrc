" use vim settings, instead of vi settings
" must be first
set nocompatible

set background=dark

" turn off vim's backup files and other annoying settings
set nobackup
set noswapfile
set noerrorbells
set nowrap

" highlight searching
set hlsearch

" disables backspace in normal mode
set backspace=indent,eol,start

" show a few lines around the cursor while moving around
set scrolloff=5


" tabs should be spaces
set expandtab shiftwidth=4 tabstop=4 softtabstop=4

" always enable autoindent
set ai

" enable syntax highlighting
syntax on
filetype on
filetype plugin on
filetype indent on

" show relative line numbers, and absolute line number current line
set number
set relativenumber

" change colors when switching modes
"  from: https://vim.fandom.com/wiki/Change_statusline_color_to_show_insert_or_normal_mode
set laststatus=2
hi StatusLine ctermfg=DarkBlue ctermbg=Black
au InsertLeave * hi StatusLine term=reverse ctermfg=DarkBlue ctermbg=Black
au InsertEnter * hi StatusLine term=reverse ctermfg=DarkGreen ctermbg=Black

" set syntax highlighting for files with non-standard names
"  from: https://ls3.io/posts/jenkinsfile_vim_highlighting/
au BufNewFile,BufRead Jenkinsfile setf groovy
au BufNewFile,BufRead *.py.j2 setf python
au BufNewFile,BufRead *.yml.j2 setf yaml
au BufNewFile,BufRead *.yaml.j2 setf yaml
au BufNewFile,BufRead *.sh.j2 setf sh

" always remove trailing whitespace
autocmd BufWritePre * :%s/\s\+$//e

" normal mode key bindigs
nnoremap <esc>l :set relativenumber!<cr>:set number!<cr>
