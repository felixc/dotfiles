#!/usr/bin/zsh

# Predicate: Does the specified command exist?
existsp() {
  command -v $1 > /dev/null
}

# Helper to add hook functions
autoload -U add-zsh-hook

# Tab completion
autoload -Uz compinit
compinit

# History file parameters
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

# Immediately append to the history file, rather than overwriting at exit
setopt inc_append_history

# Do not save duplicate history entries, even if they are not consecutive
setopt hist_ignore_all_dups

# Do not save commands in history if the first character is whitespace
setopt hist_ignore_space

# Share history between terminals
setopt share_history

# Changing directories can easily be undone
DIRSTACKSIZE=5
setopt auto_pushd
setopt pushd_silent
alias dh="dirs -v"

# Just specifying a directory name implies we want to switch to it
setopt auto_cd

# No beeps
setopt no_beep

# Don't accidentally overwrite files
setopt no_clobber

# If a line ends with an unbalanced ' then ignore it (enter key is too close)
setopt sun_keyboard_hack
KEYBOARD_HACK=\'

# Allows the use of # comments in interactive shells
setopt interactive_comments

# Look up commands in subdirs under $PATH; allows for commands with slashes
setopt PATH_DIRS

# Filename generation pattern options
setopt extended_glob nomatch

# Allow the prompt to contain variables for substitution
setopt prompt_subst

# Print non-zero exit statuses
setopt print_exit_value

# Emacs-style shortcuts
bindkey -e

# Allow editing the command line in $EDITOR with Ctrl-X-E
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey "^X^E" edit-command-line

# When jumping/deleting words, treat punctuation as word boundaries
WORDCHARS=""

# Sane defaults for various commands
alias df="df --human-readable --exclude-type tmpfs --exclude-type=devtmpfs"
alias grep="grep --extended-regexp"
alias ls="ls --human-readable --hide '__pycache__' --hide 'target'"
alias rg="rg --smart-case --search-zip"

# Enable colours for commands if possible
if existsp dircolors; then
  alias ls="ls --human-readable --hide '__pycache__' --hide 'target' --color=auto"
  alias grep="grep --extended-regexp --color=auto"
fi

# Saner ripgrep
if existsp rg; then
  alias rg="rg --smart-case --follow --hidden --ignore-file ~/cfg/ripgrepignore"
fi

# Provide nicer name-based access to colours
autoload -Uz colors && colors
zstyle ':completion:*' list-colors 'reply=( "=(#b)(*$VAR)(?)*=00=$color[green]=$color[bg-green]" )'
zstyle ':completion:*' rehash true

# VCS info configuration
autoload -Uz vcs_info
zstyle ":vcs_info:*" enable git hg
zstyle ":vcs_info:*" check-for-changes true
zstyle ":vcs_info:*" stagedstr "%F{cyan}!%f"
zstyle ":vcs_info:*" unstagedstr "%F{yellow}!%f"
zstyle ":vcs_info:*" actionformats " %F{blue}[%F{green}%b%F{3}|%F{red}%a%F{blue}]%f"
zstyle ":vcs_info:*" formats " %F{magenta}[%F{green}%b%u%c%F{magenta}]%f"

add-zsh-hook precmd vcs_info

# Prompt configuration
hostcols=(
    yellow
    cyan
    green
    blue
)
hosthash=0
foreach char (${(ws::)$(hostname)})
  hosthash=$(( $hosthash % ((2**63 - 1) / 65599) * 65599 + #char ))
end
hosthash=$(( $hosthash % $#hostcols + 1))
hostcolour=$hostcols[$hosthash]

P_USER="%(!~%F{red}~%F{green})%B%n%b%f"
P_HOST="%F{$hostcolour}%B%m%b%f"
P_PWD="%F{blue}%B%4(~.…/.)%3~%b%f"
PROMPT='$P_USER%F{green}@%f$P_HOST:$P_PWD${vcs_info_msg_0_} %F{green}$%f '
RPROMPT="%F{red}%T%f"

# Every 30 seconds, redraw the prompt (to update the clock)
TMOUT=30
TRAPALRM () {
  zle reset-prompt
}

# Make 'less' do magic with all kinds of files, and make it colourful
existsp lessfile && eval "$(lessfile)"
export LESS_TERMCAP_md=$'\E[01;38;5;2m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_us=$'\E[04;38;5;12m'
export LESS_TERMCAP_ue=$'\E[0m'

# Put the current directory in the terminal title bar
function pwd_to_title {
  [[ -o interactive ]] || return
  print -Pn "\e]2;xterm: %~\a"
}
add-zsh-hook chpwd pwd_to_title
pwd_to_title

# Automatically activate the virtualenv corresponding to the current directory
function auto_activate_venv {
  # First, check for any virtualenvs in the local directory
  local venv_locations=(".venv" ".virtualenv")
  for venv_location in $venv_locations; do
    if [ -f "$venv_location/bin/activate" ]; then
      source "$venv_location/bin/activate"
      return
    fi
  done

  # If no local virtualenvs are found, we'll look in our standard location
  local relative="${PWD#$HOME}"
  if [ "${relative}" = "${PWD}" ] || [ "${relative}" = "" ]; then return; fi
  while true; do
    local venv="$HOME/.venv${relative}"
    local activate="${venv}/bin/activate"
    if [ -f "${activate}" ]; then
      source "${activate}"
      return
    else
      relative=$(dirname "${relative}")
      if [ "${relative}" = "/" ]; then break; fi
    fi
  done

  # Finally, if there are no virtualenvs anywhere, we should ensure we
  # deactivate any that might be currently in use.
  # TODO: Should we do this before activating a different one?
  existsp deactivate && deactivate
}
add-zsh-hook chpwd auto_activate_venv

function venv_pwd {
  local relative="${PWD#$HOME}"
  if [ "${relative}" = "${PWD}" ] || [ "${relative}" = "" ]; then return; fi
  local venv_dir="$HOME/.venv${relative}"
  mkdir -p "$venv_dir"
  python3 -m venv "$venv_dir"
  auto_activate_venv
  pip install --upgrade pip wheel
}

# Make cat perform syntax highlighting
function pygmentize_cat {
  for arg in "$@"; do
    pygmentize -g "${arg}" 2>/dev/null || /bin/cat "${arg}"
  done
}
existsp pygmentize && alias cat=pygmentize_cat
