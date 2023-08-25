" add color to the statusline to show insert vs normal mode
"" colors:
""" normal mode: green
""" insert mode: orange
"" docs: https://vimdoc.sourceforge.net/htmldoc/syntax.html#highlight-ctermbg

" always show status line, and color it blue since vim starts in normal mode
set laststatus=2
hi StatusLine ctermfg=DarkBlue ctermbg=Black

" change colors when switching modes
"" from: https://vim.fandom.com/wiki/Change_statusline_color_to_show_insert_or_normal_mode
au InsertLeave * hi StatusLine term=reverse ctermfg=DarkBlue ctermbg=Black
au InsertEnter * hi StatusLine term=reverse ctermfg=DarkGreen ctermbg=Black

" borrowed from matt
"" sets tabs to be spaces
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
"" c/n/i:
""" cnoremap: keybindings command-line mode (when writing the :commands below status line)
""" inoremap: keybindings for insert mode
""" nnoremap: keybindings for normal mode
""" overview in stackoverflow answer: https://stackoverflow.com/a/71194964
"" docs: https://vimdoc.sourceforge.net/htmldoc/map.html
"" based on: https://www.monolune.com/articles/readline-keybindings-for-vim-commands/

"" free up c-c
nnoremap <c-c> <nop>
inoremap <c-c> <nop>

"" :commands below status line
cnoremap <c-g> <c-c>

"" normal mode
nnoremap <c-g> <c-c>
nnoremap <c-x><c-c> :q<cr>
nnoremap <c-x><c-d> :wq!<cr>
nnoremap <c-x><c-s> :update<cr>
nnoremap <c-a> <home>
nnoremap <c-e> <end>
nnoremap <c-p> <up>
nnoremap <c-n> <down>
nnoremap <c-b> <left>
nnoremap <c-f> <right>
nnoremap <esc>b <s-left>
nnoremap <esc>f <s-right>
nnoremap <c-k> <c->estrpart(getcmdline(), 0, getcmdpos() - 1)<cr>
nnoremap <c-c>l :set number!<cr>
nnoremap <c-l> :set number!<cr>
nnoremap <esc>l :set relativenumber!<cr>
nnoremap <c-c>L :set relativenumber!<cr>

"" insert mode
inoremap <c-x><c-c> <esc>:q<cr>
inoremap <c-x><c-d> <esc>:wq!<cr>
inoremap <c-x><c-c> <esc>:quit<cr>
inoremap <c-x><c-s> <esc>:update<cr>i
inoremap <c-p> <up>
inoremap <c-n> <down>
inoremap <c-a> <home>
inoremap <c-e> <end>
inoremap <c-b> <left>
inoremap <c-f> <right>
inoremap <c-j> <enter>
inoremap <c-d> <del>
inoremap <esc>b <s-left>
inoremap <esc>f <s-right>
inoremap <esc><backspace> <c-w>
inoremap <c-k> <c->estrpart(getcmdline(), 0, getcmdpos() - 1)<cr>

""" on macs, after changing the keybindings to make some sense on the os level, this
""" doesnt work. the alt key is command, which cant be remapped in regular vim
""" so just get used to using capslock (esc) instead.
""" will get better once i have the kinesis keyboard.
inoremap <alt-b> <s-left>
inoremap <alt-f> <s-right>
