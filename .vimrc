" Plugins {{{

set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'

" ultisnips {{{

Plugin 'SirVer/ultisnips'

let g:UltiSnipsExpandTrigger='<Tab>'
let g:UltiSnipsJumpForwardTrigger='<Tab>'
let g:UltiSnipsJumpBackwardTrigger='<S-Tab>'
let g:UltiSnipsSnippetDirectories=[$HOME.'/.vim/ultisnips']

" }}}

" vimtex {{{

Plugin 'lervag/vimtex'

let g:vimtex_view_general_viewer='okular'
let g:vimtex_view_general_options='--unique file:@pdf\#src:@line@tex'

" }}}

" ale {{{

Plugin 'dense-analysis/ale'

let g:ale_set_quickfix=1
let g:ale_lint_on_enter=0
let g:ale_lint_on_save=1

" }}}

call vundle#end()
filetype plugin indent on

" }}}

" General Settings {{{

set encoding=utf-8
set foldmethod=marker
syntax enable

" enable :find to search in subfolders
set path+=**

" enable autocompletion menu for :find
set wildmenu

" tabs
set tabstop=4
set shiftwidth=4
set smartindent
set expandtab

" color tweaking
hi LineNr ctermfg=gray
hi Search ctermfg=black ctermbg=yellow
hi Folded ctermfg=black
hi Visual ctermfg=black

" netrw
let g:netrw_browse_split=2
let g:netrw_liststyle=3
let g:netrw_banner=0

" }}}

" Keymappings {{{

nnoremap ; :

" toggle folds
nnoremap <CR> za

" visual block mode
nnoremap q <C-v>

" buffer resize mode
nnoremap <silent> r :call BufferResize()<CR>

" toggle search highlighting
nnoremap <silent> ,s :set invhlsearch<CR>

" toggle line numbering
nnoremap <silent> ,m :set invnumber<CR>

" toggle quickfix window
nnoremap <silent> ,z :call ToggleQuickFix()<CR>

" toggle netrw
nnoremap <silent> ,e :Lexplore<CR>

" toggle ale
nnoremap <silent> ,a :ALEToggle<CR>

" quickfix list error navigation
nnoremap <silent> <C-n> :cn<CR>
nnoremap <silent> <C-p> :cp<CR>

" BufferResize(): resize buffers {{{

fun! BufferResize()
    echo "-- BUFFER RESIZE --"
    nnoremap <Up> 1<C-w>+
    nnoremap <Down> 1<C-w>-
    nnoremap <Left> 1<C-w>>
    nnoremap <Right> 1<C-w><
    nnoremap <silent> x :call Restore()<CR>
endfun

fun! Restore()
    nunmap <Up>
    nunmap <Down>
    nunmap <Left>
    nunmap <Right>
    nunmap x
    echo ""
endfun

" }}}

" ToggleQuickFix(): toggle quickfix window {{{

fun! ToggleQuickFix()
    if empty(filter(getwininfo(), 'v:val.quickfix'))
        copen
    else
        cclose
    endif
endfun

" }}}

" }}}
