#!/bin/sh

set -eux


# We'll use this to specialize our installation.
hostname=$(hostname --short)


# First we have to set up our installation sources.
tee /etc/apt/sources.list > /dev/null << EOF
deb      http://deb.debian.org/debian/  buster            main contrib non-free
deb-src  http://deb.debian.org/debian/  buster            main contrib non-free

deb      http://security.debian.org/    buster/updates    main contrib non-free
deb-src  http://security.debian.org/    buster/updates    main contrib non-free

deb      http://deb.debian.org/debian/  buster-updates    main contrib non-free
deb-src  http://deb.debian.org/debian/  buster-updates    main contrib non-free

deb      http://deb.debian.org/debian/  buster-backports  main contrib non-free

deb      http://deb.debian.org/debian/  unstable          main contrib non-free
deb-src  http://deb.debian.org/debian/  unstable          main contrib non-free

deb      http://deb.debian.org/debian/  experimental      main contrib non-free
deb-src  http://deb.debian.org/debian/  experimental      main contrib non-free
EOF


#
# Track stable but make packages from Unstable and Experimental available
#
tee /etc/apt/preferences.d/non-stable-repo-priorities > /dev/null << EOF
Package: *
Pin: release a=unstable
Pin-Priority: -1

Package: *
Pin: release a=experimental
Pin-Priority: -1
EOF


# To start, get into a clean and ready state.
apt update
apt upgrade


# Base packages used on all systems.
apt install \
  anacron apt-transport-https bc curl daemontools debian-keyring \
  firmware-linux git git-annex htop iotop iperf make moreutils rsync \
  smartmontools sudo tmux unzip util-linux vim-nox zsh

apt install \
  --target-release unstable \
    ripgrep

if lscpu | grep -q "GenuineIntel"; then
  apt install \
    --target-release stretch-backports \
      intel-microcode
elif lscpu | grep -q "AuthenticAMD"; then
  apt install \
    --target-release stretch-backports \
      amd64-microcode
fi

if [ "$(dpkg --print-architecture)" = "amd64" ]; then
  apt install \
    --target-release stretch-backports \
      linux-image-amd64
fi


# Packages used only on servers/headless systems.
if \
  [ "$hostname" = "vostok" ] || \
  [ "$hostname" = "voskhod" ] || \
  [ "$hostname" = "molniya" ] || \
  [ "$hostname" = "salyut" ]
then
  apt install \
    --no-install-recommends \
      emacs-nox
fi

# Packages for systems where I want to keep track of HDD state.
if \
  [ "$hostname" = "molniya" ] || \
  [ "$hostname" = "mir" ]
then
  apt install exim4 smartmontools
fi


# Packages for non-server machines.
if \
  [ "$hostname" = "mir" ] || \
  [ "$hostname" = "zond" ]
then
  apt install \
    alsa-utils build-essential chromium compton dzen2 emacs eog evince feh \
    ffmpeg fonts-inconsolata fonts-liberation gdb gimp git-email git-extras \
    gksu gnome-disk-utility gnome-screenshot gnupg-agent gnupg2 gparted \
    imagemagick inkscape ipython3 irssi irssi-scripts keychain lbdb ledger \
    libghc-xmonad-contrib-dev libghc-xmonad-dev libsecret-tools lightdm mpv \
    msmtp msmtp-gnome numlockx pass pavucontrol pulseaudio pylint3 python \
    python3 python3-flake8 python3-jedi python3-venv rxvt-unicode-256color \
    scdaemon shellcheck strace ttf-bitstream-vera ttf-dejavu \
    ttf-mscorefonts-installer ttf-xfree86-nonfree unicode-screensaver unifont \
    vlc wmctrl xbindkeys xsel xinit xlsx2csv xmonad xorg xscreensaver \
    xscreensaver-data-extra xscreensaver-gl xscreensaver-gl-extra \
    xscreensaver-screensaver-bsod xserver-xorg-input-all

  apt install \
    --no-install-recommends \
      gnome-control-center

  apt install \
    --target-release unstable \
      firefox
fi


# Desktop packages.
if [ "$hostname" = "mir" ]; then
  apt install \
    cups darktable fonts-cantarell fonts-dejavu fonts-dejavu-extra \
    fonts-ebgaramond fonts-ebgaramond-extra fonts-lato fonts-linuxlibertine \
    fonts-ocr-a fonts-opensymbol fonts-sil-charis fonts-sil-gentium \
    fonts-vollkorn fonts-yanone-kaffeesatz geeqie gnome-font-viewer libdvdcss2 \
    mutt notmuch notmuch-mutt offlineimap signing-party texlive \
    texlive-bibtex-extra texlive-font-utils texlive-fonts-extra \
    texlive-fonts-recommended texlive-pictures texlive-pstricks texlive-xetex \
    xsane

  apt install \
    --target-release stretch-backports \
      linux-headers-amd64 nvidia-driver
fi


# Laptop packages.
if [ "$hostname" = "zond" ]; then
  apt install \
    acpi firmware-iwlwifi firmware-realtek laptop-mode-tools xbacklight \
    xserver-xorg-input-synaptics
fi


# Home server packages.
if [ "$hostname" = "molniya" ]; then
  apt install \
    apcupsd hddtemp nfs-common nfs-kernel-server openjdk-8-jre-headless samba \
    sane-utils tarsnap task-print-server unifi

  apt install \
    --target-release stretch-backports \
    netdata
fi


# Media centre packages.
if [ "$hostname" = "salyut" ]; then
  apt install \
    fbi kodi kodi-visualization-shadertoy nfs-common

  apt autoremove --purge \
    avahi-daemon bluetooth dhcpcd5 pi-bluetooth wpasupplicant
fi


# Web server packages.
if \
  [ "$hostname" = "vostok" ] || \
  [ "$hostname" = "voskhod" ]
then
  apt install \
    --target-release stretch-backports \
      certbot nginx-extras

  apt autoremove --purge \
    awscli 'google-*' 'python*-boto*'
fi


# Finally, clean up after ourselves.
apt autoremove --purge
apt purge $(dpkg -l | awk '/^rc/ { print $2 }')
apt clean
