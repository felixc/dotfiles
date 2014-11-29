# Predicate: Does the specified command exist?
existsp() {
  command -v $1 > /dev/null
}

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

# Filename generation pattern options
setopt extended_glob nomatch

# Allow the prompt to contain variables for substitution
setopt prompt_subst

# Emacs-style shortcuts
bindkey -e

# Enable colors for certain commands, or at least make them nicer
if existsp dircolors; then
  alias ls="ls --color=auto --human-readable --hide '__pycache__'"
  alias grep="grep -E --color=auto"
else
  alias ls="ls --human-readable --hide '__pycache__'"
  alias grep="grep -E"
fi

# Provide nicer name-based access to colors
autoload -Uz colors && colors
zstyle ':completion:*' list-colors 'reply=( "=(#b)(*$VAR)(?)*=00=$color[green]=$color[bg-green]" )'
zstyle ':completion:*' rehash true

# VCS info configuration
autoload -Uz vcs_info
precmd_functions=("${precmd_functions[@]}" vcs_info)
zstyle ":vcs_info:*" enable git hg
zstyle ":vcs_info:*" check-for-changes true
zstyle ":vcs_info:*" stagedstr "%F{cyan}!%f"
zstyle ":vcs_info:*" unstagedstr "%F{yellow}!%f"
zstyle ":vcs_info:*" actionformats " %F{blue}[%F{green}%b%F{3}|%F{red}%a%F{blue}]%f "
zstyle ":vcs_info:*" formats " %F{magenta}[%F{green}%b%u%c%F{magenta}]%f "

# Default prompt: user@hostname:/path/$
PROMPT='%F{green}%B%n@%m%b%f:%F{blue}%B%4~%b%f${vcs_info_msg_0_}$ '
RPROMPT="%{%F{red}%}%t%f"

if [[ ! -z $SCHROOT_CHROOT_NAME ]]; then
  PROMPT='%F{green}%B%n@%m%F{red}[$SCHROOT_CHROOT_NAME]%f%b%f:%F{blue}%B%4~%b%f${vcs_info_msg_0_}$ '
fi

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
chpwd_functions=("${chpwd_functions[@]}" pwd_to_title)
pwd_to_title

# Automatically activate the virtualenv corresponding to the current directory
function auto_activate_venv {
  local relative="${PWD#$HOME}"
  if [ "${relative}" = "${PWD}" ] || [ "${relative}" = "" ]; then return; fi
  while true; do
    local venv="$HOME/.venv${relative}"
    local activate="${venv}/bin/activate"
    if [ -f "${activate}" ]; then
      source "${activate}";
      return;
    else
      relative=$(dirname "${relative}")
      if [ "${relative}" = "/" ]; then break; fi
    fi
  done
  existsp deactivate && deactivate
}
chpwd_functions=("${chpwd_functions[@]}" auto_activate_venv)
auto_activate_venv

function venv_pwd {
  local relative="${PWD#$HOME}"
  if [ "${relative}" = "${PWD}" ] || [ "${relative}" = "" ]; then return; fi
  local venv_dir="$HOME/.venv${relative}"
  mkdir -p "$venv_dir"
  virtualenv -p /usr/bin/python3 "$venv_dir"
  auto_activate_venv
}

# Make cat perform syntax highlighting
function pygmentize_cat {
  for arg in "$@"; do
    pygmentize -g "${arg}" 2>/dev/null || /bin/cat "${arg}"
  done
}
existsp pygmentize && alias cat=pygmentize_cat
