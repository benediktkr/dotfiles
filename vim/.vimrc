" always enable syntax highlighting
syntax on

" truncate short messages to supress "press enter to continue" when using scp
"  from: https://stackoverflow.com/questions/12422468/vim-netrw-asking-to-press-enter-or-type-command-to-continue
set shortmess+=T
set cmdheight=2

" add color to the statusline to show insert vs normal mode
"  normal mode: green
"  insert mode: orange
"  docs: https://vimdoc.sourceforge.net/htmldoc/syntax.html#highlight-ctermbg

" always show status line, and color it blue since vim starts in normal mode
set laststatus=2
hi StatusLine ctermfg=DarkBlue ctermbg=Black

" change colors when switching modes
"  from: https://vim.fandom.com/wiki/Change_statusline_color_to_show_insert_or_normal_mode
au InsertLeave * hi StatusLine term=reverse ctermfg=DarkBlue ctermbg=Black
au InsertEnter * hi StatusLine term=reverse ctermfg=DarkGreen ctermbg=Black

" auto indent after enter/c-j
set autoindent

" from: https://superuser.com/a/397009
"set backspace=indent,eol,start

" set syntax highlighting for files with non-standard names
"  from: https://ls3.io/posts/jenkinsfile_vim_highlighting/
au BufNewFile,BufRead Jenkinsfile setf groovy
au BufNewFile,BufRead *.py.j2 setf python
au BufNewFile,BufRead *.yml.j2 setf yaml
au BufNewFile,BufRead *.yaml.j2 setf yaml
au BufNewFile,BufRead *.sh.j2 setf sh

" remove trailing whitespace from files
autocmd BufWritePre *.py :%s/\s\+$//e
autocmd BufWritePre *.py.j2 :%s/\s\+$//e
autocmd BufWritePre *.sh :%s/\s\+$//e
autocmd BufWritePre *.sh.j2 :%s/\s\+$//e
autocmd BufWritePre *.j2 :%s/\s\+$//e
autocmd BufWritePre *.txt :%s/\s\+$//e
autocmd BufWritePre *.md :%s/\s\+$//e
autocmd BufWritePre *.conf :%s/\s\+$//e
autocmd BufWritePre *.ini :%s/\s\+$//e
autocmd BufWritePre Dockerfile* :%s/\s\+$//e
autocmd BufWritePre Jenkinsfile :%s/\s\+$//e
autocmd BufWritePre .vimrc :%s/\s\+$//e
autocmd BufWritePre 0*-* :%s/\s\+$//e

" jump to last position when opening a file
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

" from /etc/vim/vimrc
set background=dark        " if using dark bg within edit area
filetype plugin indent on  " indent rules according to detected filetype

set showcmd            " Show (partial) command in status line.
set showmatch          " Show matching brackets.
set ignorecase         " Do case insensitive matching
"set smartcase          " Do smart case matching
set incsearch          " Incremental search
"set autowrite          " Automatically save before commands like :next and :make
set hidden             " Hide buffers when they are abandoned

" borrowed from matt
"  sets tabs to be spaces
set expandtab shiftwidth=4 tabstop=4 softtabstop=4

" borrowed from lars
set noerrorbells
set nowrap
set nobackup
set noswapfile
set background=dark

"disable prehistoric freezing feature
silent !stty -ixon

" keybindings
"  c/n/i:
"   cnoremap: keybindings command-line mode (when writing the :commands below status line)
"   inoremap: keybindings for insert mode
"   nnoremap: keybindings for normal mode
"   overview in stackoverflow answer: https://stackoverflow.com/a/71194964
"  docs: https://vimdoc.sourceforge.net/htmldoc/map.html
"  based on: https://www.monolune.com/articles/readline-keybindings-for-vim-commands/

"  free up c-c
"nnoremap <c-c> <nop>
"inoremap <c-c> <nop>

"  unmap c-d and m-d in normal mode
"nnoremap <c-d> <nop>
"nnoremap <alt-d> <nop>


"  for :commands below status line
"cnoremap <c-g> <c-c>

"  normal mode
"nnoremap <c-g> <c-c>
"nnoremap <c-x><c-c> :q<cr>
"nnoremap <c-x><c-d> :wq!<cr>
"nnoremap <c-x><c-s> :update<cr>
"nnoremap <c-a> <home>
"nnoremap <c-e> <end>
"nnoremap <c-p> <up>
"nnoremap <c-n> <down>
"nnoremap <c-b> <left>
"nnoremap <c-f> <right>
"noremap <esc>b <s-left>
"nnoremap <esc>f <s-right>
"nnoremap <c-c>l :set number!<cr>
"nnoremap <c-l> :set number!<cr>
"nnoremap <c-c>L :set relativenumber!<cr>
"nnoremap <esc>l :set relativenumber!<cr>
"nnoremap <esc>l :set number!<cr>
"nnoremap <esc>L :set relativenumber!<cr>
nnoremap <esc>l :set relativenumber!<cr>:set number!<cr>
"nnoremap <c-x><c-f> :edit<space>
"nnoremap <c-x><c-b> :ls<cr>:b<space>
"nnoremap <c-x>b :ls<cr>

"  insert mode
"inoremap <c-x><c-c> <esc>:q<cr>
"inoremap <c-x><c-d> <esc>:wq!<cr>
"inoremap <c-x><c-s> <esc>:update<cr>i
"inoremap <c-x><c-s> <esc>:update<cr>
"inoremap <c-p> <up>
"inoremap <c-n> <down>
"inoremap <c-a> <home>
"inoremap <c-e> <end>
"inoremap <c-b> <left>
"inoremap <c-f> <right>
"inoremap <c-j> <enter>
"inoremap <c-d> <del>
"inoremap <esc>b <s-left>
"inoremap <esc>f <s-right>
"inoremap <esc><backspace> <c-w>
"inoremap <c-k> <esc>d$<cr>i
"inoremap <c-_> <esc>u<cr>i


"inoremap <c-k> <c->estrpart(getcmdline(), 0, getcmdpos() - 1)<cr>
"nnoremap <c-k> <c->estrpart(getcmdline(), 0, getcmdpos() - 1)<cr>





