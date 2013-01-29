" Let backspace work normally, i.e. allow deleting everything
set backspace=indent,eol,start

" Indentation: automatic and smart, for C-like languages anyway
set autoindent
set cindent

" Auto save before certain dangerous commands, otherwise confirm
set autowrite
set confirm

" Incrementally search through the file as you type, highlighting as you go
" Searches are also case insensitive if entered in lower-case
" Searches wrap at EOF
set incsearch
set hlsearch
set ignorecase
set smartcase
set wrapscan

" Tabs are no good, use spaces instead
set expandtab
set tabstop=2
set shiftwidth=2
set softtabstop=2

" Except for makefiles; let's just use tabs
autocmd FileType make set noexpandtab

" Let us know that we are in command/insert mode, and show commands as we type
set showmode
set showcmd

" Show line numbers and cursor position (rows and cols)
set number
set ruler

" Show tabs and trailing whitespace
set listchars=tab:>·,trail:·
set list

" Error bells are the worst
set noerrorbells

" When the cursor is on a bracket/paren/brace, highlight the other one briefly
set showmatch
set matchtime=3

" Spell checking is nice, and so is being able to add words to the dictionary
set spellfile=~/.vim/spellfile.en.add
setlocal spell spelllang=en_gb

" A nice, dark colour scheme, so make things visible and highlight syntax
set t_Co=256
set background=dark
syntax on

" Remove the GUI toolbar, make options text instead of popups, enable the
" mouse, and use a good font
set guioptions-=T
set guioptions+=c
set mouse=a
set guifont=Inconsolata\ 13

" Windows should always have a status line
set laststatus=2

" I really don't want anything to get folded
set nofoldenable

" When splitting a window, put the new window below the original one
set splitbelow

" Configure what gets saved in the .viminfo history file
set viminfo='0,\"0,%,/0,:10,@0,h

" Remap the default motion keys so long, wrapped, but unsplit lines can be
" navigated as independent lines
noremap j gj
noremap k gk

" Complete my parens etc for me
inoremap ( ()<ESC>i
inoremap [ []<ESC>i
inoremap { {}<ESC>i
