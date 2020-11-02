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


# Track stable but make packages from Unstable and Experimental available
tee /etc/apt/preferences.d/non-stable-repos > /dev/null << EOF
Package: *
Pin: release a=unstable
Pin-Priority: -1

Package: *
Pin: release a=experimental
Pin-Priority: -1
EOF


# Machines for everyday personal use need to support i386 as well.
if \
  [ "$hostname" = "mir" ] || \
  [ "$hostname" = "zond" ]
then
  dpkg --add-architecture i386
fi


# To start, get into a clean and ready state.
apt update
apt upgrade


# Base packages used on all systems.
apt install \
  bc curl daemontools debian-keyring firmware-linux git make moreutils ripgrep \
  rsync sudo tmux unzip util-linux vim-nox zsh

if lscpu | grep -q "GenuineIntel"; then
  apt install \
    --target-release buster-backports \
      intel-microcode
elif lscpu | grep -q "AuthenticAMD"; then
  apt install \
    --target-release buster-backports \
      amd64-microcode
fi

if [ "$(dpkg --print-architecture)" = "amd64" ]; then
  if \
    [ "$hostname" = "vostok" ] || \
    [ "$hostname" = "voskhod" ]
  then
    apt install \
      --target-release buster-backports \
      linux-image-cloud-amd64
  else
    apt install \
      --target-release buster-backports \
      linux-image-amd64
  fi
fi

tee /etc/apt/preferences.d/backports-core-packages > /dev/null << EOF
Package: amd64-microcode intel-microcode linux-image-amd64 linux-image-cloud-amd64
Pin: release a=buster-backports
Pin-Priority: 500
EOF


# Packages used only on servers/headless systems.
if \
  [ "$hostname" = "vostok" ] || \
  [ "$hostname" = "voskhod" ] || \
  [ "$hostname" = "molniya" ] || \
  [ "$hostname" = "salyut" ]
then
  apt install openssh-server

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
    alsa-utils anacron build-essential chromium dzen2 emacs eog evince feh \
    ffmpeg fonts-inconsolata fonts-liberation gdb gimp git-email git-extras \
    gnome-disk-utility gnome-screenshot gnupg-agent gnupg2 gparted \
    gvfs-backends imagemagick inkscape ipython3 irssi irssi-scripts keychain \
    lbdb ledger libghc-xmonad-contrib-dev libghc-xmonad-dev libsecret-tools \
    libssl-dev lightdm net-tools msmtp msmtp-gnome nfs-common numlockx pass \
    pavucontrol pulseaudio pylint3 python python-ledger python3 python3-flake8 \
    python3-venv rxvt-unicode scdaemon shellcheck steam strace \
    ttf-bitstream-vera ttf-dejavu ttf-mscorefonts-installer \
    ttf-xfree86-nonfree unicode-screensaver unifont virtualenv vlc wmctrl \
    xbindkeys xsel xinit xlsx2csv xmonad xorg xscreensaver \
    xscreensaver-data-extra xscreensaver-gl xscreensaver-gl-extra \
    xscreensaver-screensaver-bsod xserver-xorg-input-all

  apt install \
    --no-install-recommends \
      nautilus
fi


# Desktop packages.
if [ "$hostname" = "mir" ]; then
  apt install \
    cups darktable fonts-cantarell fonts-dejavu fonts-dejavu-extra \
    fonts-ebgaramond fonts-ebgaramond-extra fonts-lato fonts-linuxlibertine \
    fonts-ocr-a fonts-opensymbol fonts-sil-charis fonts-sil-gentium \
    fonts-vollkorn fonts-yanone-kaffeesatz geeqie gnome-font-viewer \
    neomutt notmuch notmuch-mutt offlineimap python-keyring signing-party \
    texlive texlive-bibtex-extra texlive-font-utils texlive-fonts-extra \
    texlive-fonts-recommended texlive-pictures texlive-pstricks texlive-xetex \
    xsane

  apt install \
    --target-release buster-backports \
      linux-headers-amd64 nvidia-driver nvidia-driver-libs nvidia-driver-libs:i386

  tee /etc/apt/preferences.d/backports-nvidia-driver > /dev/null <<- EOF
	Package: linux-headers-amd64 nvidia-driver nvidia-driver-libs
	Pin: release a=buster-backports
	Pin-Priority: 500
	EOF

  apt autoremove --purge \
    wpasupplicant yelp
fi


# Laptop packages.
if [ "$hostname" = "zond" ]; then
  apt install \
    acpi firmware-iwlwifi firmware-realtek laptop-mode-tools xbacklight \
    xserver-xorg-input-synaptics xserver-xorg-video-intel

  apt autoremove --purge \
    yelp
fi


# Home server packages.
if [ "$hostname" = "molniya" ]; then
  apt install \
    apcupsd hddtemp netdata nfs-common nfs-kernel-server \
    openjdk-8-jre-headless samba sane-utils tarsnap task-print-server unifi

  apt install \
    --no-install-recommends --target-release buster-backports \
      youtube-dl

  tee /etc/apt/preferences.d/youtube-dl > /dev/null <<- EOF
	Package: youtube-dl
	Pin: release a=buster-backports
	Pin-Priority: 500
	EOF

  apt autoremove --purge \
    bluetooth wpasupplicant yelp
fi


# Media centre packages.
if [ "$hostname" = "salyut" ]; then
  apt install \
    fbi kodi kodi-visualization-shadertoy nfs-common

  apt autoremove --purge \
    bluetooth dhcpcd5 pi-bluetooth wpasupplicant
fi


# Web server packages.
if \
  [ "$hostname" = "vostok" ] || \
  [ "$hostname" = "voskhod" ]
then
  apt install \
    certbot nginx-extras

  apt autoremove --purge \
    awscli 'google-*' 'python*-boto*'
fi


# Configure unattended upgrades
apt install unattended-upgrades
sed -Ei \
  's|//      "o=Debian Backports,a=\$\{distro_codename\}-backports,l=Debian Backports";|        "o=Debian Backports,a=${distro_codename}-backports,l=Debian Backports";|' \
  /etc/apt/apt.conf.d/50unattended-upgrades


# Finally, clean up after ourselves.
apt autoremove --purge
apt purge $(dpkg -l | awk '/^rc/ { print $2 }')
apt clean
