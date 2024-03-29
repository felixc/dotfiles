#!/bin/sh

set -eux


# We'll use this to specialize our installation.
hostname=$(hostname --short)
architecture=$(dpkg --print-architecture)

# First we have to set up our installation sources.
tee /etc/apt/sources.list > /dev/null << EOF
deb      http://deb.debian.org/debian/  bullseye            main contrib non-free
deb-src  http://deb.debian.org/debian/  bullseye            main contrib non-free

deb      http://security.debian.org/    bullseye-security   main contrib non-free
deb-src  http://security.debian.org/    bullseye-security   main contrib non-free

deb      http://deb.debian.org/debian/  bullseye-updates    main contrib non-free
deb-src  http://deb.debian.org/debian/  bullseye-updates    main contrib non-free

deb      http://deb.debian.org/debian/  bullseye-backports  main contrib non-free

deb      http://deb.debian.org/debian/  unstable            main contrib non-free
deb-src  http://deb.debian.org/debian/  unstable            main contrib non-free

deb      http://deb.debian.org/debian/  experimental        main contrib non-free
deb-src  http://deb.debian.org/debian/  experimental        main contrib non-free
EOF


# Track stable but make packages from Unstable and Experimental available.
tee /etc/apt/preferences.d/non-stable-repos > /dev/null << EOF
Package: *
Pin: release a=unstable
Pin-Priority: -1

Package: *
Pin: release a=experimental
Pin-Priority: -1
EOF


# The home server needs to install some tools from non-standard sources.
if [ "$hostname" = "molniya" ]; then
  if { \
       [ ! -f /usr/share/keyrings/tarsnap-archive.gpg ] || \
       [ ! -f /usr/share/keyrings/tailscale-archive.gpg ] || \
       [ ! -f /usr/share/keyrings/google-archive.gpg ];
     } && { \
       ( ! command -v curl > /dev/null ) || \
       ( ! command -v gpg > /dev/null ) \
     }
  then
    apt install --yes curl gnupg
  fi

  if [ ! -f /usr/share/keyrings/tarsnap-archive.gpg ]; then
    curl -fsSL https://pkg.tarsnap.com/tarsnap-deb-packaging-key.asc \
      | gpg --dearmor > /usr/share/keyrings/tarsnap-archive.gpg
  fi
  tee /etc/apt/preferences.d/limit-tarsnap-repo > /dev/null <<-EOF
	Package: *
	Pin: origin pkg.tarsnap.com
	Pin-Priority: 100
	EOF
  tee /etc/apt/sources.list.d/tarsnap.list > /dev/null <<- EOF
	deb [signed-by=/usr/share/keyrings/tarsnap-archive.gpg] http://pkg.tarsnap.com/deb/bullseye ./
	EOF

  if [ ! -f /usr/share/keyrings/tailscale-archive.gpg ]; then
    curl -fsSL https://pkgs.tailscale.com/stable/debian/bullseye.gpg \
      | gpg --dearmor > /usr/share/keyrings/tailscale-archive.gpg
  fi
  tee /etc/apt/preferences.d/limit-tailscale-repo > /dev/null <<-EOF
	Package: *
	Pin: origin pkgs.tailscale.com
	Pin-Priority: 100
	EOF
  tee /etc/apt/sources.list.d/tailscale.list > /dev/nul <<- EOF
	deb [signed-by=/usr/share/keyrings/tailscale-archive.gpg] https://pkgs.tailscale.com/stable/debian bullseye main
	EOF

  if [ ! -f /usr/share/keyrings/google-archive.gpg ]; then
    curl -fsSL https://packages.cloud.google.com/apt/doc/apt-key.gpg \
      | gpg --dearmor > /usr/share/keyrings/google-archive.gpg
  fi
  tee /etc/apt/preferences.d/limit-google-repo > /dev/null <<-EOF
	Package: *
	Pin: origin packages.cloud.google.com
	Pin-Priority: 100
	EOF
  tee /etc/apt/sources.list.d/google.list > /dev/nul <<- EOF
	deb [signed-by=/usr/share/keyrings/google-archive.gpg] https://packages.cloud.google.com/apt coral-edgetpu-stable main
	EOF
fi


# Machines for everyday personal use need to support i386 as well (for games).
if \
  [ "$hostname" = "mir" ] || \
  [ "$hostname" = "zond" ]
then
  dpkg --add-architecture i386
fi


# To start, get into a clean and ready state.
apt update
apt --yes upgrade


# Base packages used on all systems.
apt install --yes \
  bc curl daemontools debian-keyring dnsutils firmware-linux git make \
  moreutils ripgrep rsync sudo tmux ufw unzip util-linux vim-nox zsh

tee /etc/apt/preferences.d/backports-core-packages > /dev/null <<- EOF
	Package: amd64-microcode intel-microcode linux-image-amd64 linux-image-cloud-amd64
	Pin: release a=bullseye-backports
	Pin-Priority: 500
	EOF

if lscpu | grep -q "GenuineIntel"; then
  apt install --yes intel-microcode
elif lscpu | grep -q "AuthenticAMD"; then
  apt install --yes amd64-microcode
fi

if [ "$architecture" = "amd64" ]; then
  if \
    [ "$hostname" = "vostok" ] || \
    [ "$hostname" = "voskhod" ]
  then
    apt install --yes linux-image-cloud-amd64
  else
    apt install --yes linux-image-amd64
  fi
fi


# Packages used only on servers/headless systems.
if \
  [ "$hostname" = "vostok" ] || \
  [ "$hostname" = "voskhod" ] || \
  [ "$hostname" = "molniya" ]
then
  apt install --yes emacs-nox openssh-server
fi

# Packages for systems where I want to keep track of HDD state.
if \
  [ "$hostname" = "molniya" ] || \
  [ "$hostname" = "mir" ]
then
  apt install --yes smartmontools
fi

# Packages for non-server machines.
if \
  [ "$hostname" = "mir" ] || \
  [ "$hostname" = "zond" ]
then
  apt install --yes \
    alsa-utils anacron borgbackup build-essential chromium dunst dzen2 emacs \
    entr eog evince feh ffmpeg fonts-dejavu fonts-inconsolata fonts-liberation \
    fonts-symbola gdb gimp git-email git-extras gnome-disk-utility \
    gnome-screenshot gnupg-agent gnupg2 gparted gron gvfs-backends imagemagick \
    inkscape ipython3 irssi irssi-scripts keychain lbdb \
    libghc-xmonad-contrib-dev libghc-xmonad-dev libsecret-tools libssl-dev \
    lightdm net-tools msmtp ncal nemo nfs-common numlockx pass pavucontrol \
    picom pulseaudio pylint3 python3 python3-flake8 python3-venv rxvt-unicode \
    scdaemon shellcheck steam strace ttf-bitstream-vera \
    ttf-mscorefonts-installer ttf-xfree86-nonfree unicode-screensaver unifont \
    virtualenv vlc w3m wmctrl xbindkeys xsel xinit xlsx2csv xmonad xorg \
    xscreensaver xscreensaver-data-extra xscreensaver-gl xscreensaver-gl-extra \
    xscreensaver-screensaver-bsod xserver-xorg-input-all

  apt autoremove --purge \
    yelp avahi-daemon xdg-desktop-portal gnome-online-accounts
fi


# Desktop packages.
if [ "$hostname" = "mir" ]; then
  apt install --yes \
    brotli darktable fonts-cantarell fonts-dejavu fonts-dejavu-extra \
    fonts-ebgaramond fonts-ebgaramond-extra fonts-lato fonts-linuxlibertine \
    fonts-ocr-a fonts-opensymbol fonts-sil-charis fonts-sil-gentium \
    fonts-vollkorn fonts-yanone-kaffeesatz geeqie gnome-font-viewer goaccess \
    ledger libdvd-pkg neomutt notmuch notmuch-mutt offlineimap3 \
    par2 python3-keyring signing-party texlive texlive-bibtex-extra \
    texlive-font-utils texlive-fonts-extra texlive-fonts-recommended \
    texlive-pictures texlive-pstricks texlive-xetex

  apt install --yes \
    firmware-amd-graphics libgl1-mesa-dri libgl1-mesa-dri:i386 libglx-mesa0 \
    libglx-mesa0:i386 mesa-vulkan-drivers mesa-vulkan-drivers:i386 \
    mesa-va-drivers mesa-vdpau-drivers xserver-xorg-video-amdgpu

  apt autoremove --purge \
    wpasupplicant modemmanager
fi


# Laptop packages.
if [ "$hostname" = "zond" ]; then
  apt install --yes \
    acpi firmware-iwlwifi firmware-realtek laptop-mode-tools xbacklight \
    xserver-xorg-input-synaptics xserver-xorg-video-intel
fi


# Home server packages.
if [ "$hostname" = "molniya" ]; then
  apt install --yes \
    apcupsd borgbackup certbot kodi kodi-vfs-libarchive libcec6 lm-sensors \
    mdadm minidlna netdata nfs-common nfs-kernel-server nginx-light podman \
    python3-certbot-dns-cloudflare rtorrent tailscale tarsnap \
    xserver-xorg-video-amdgpu

  tee /etc/apt/preferences.d/youtube-dl > /dev/null <<- EOF
	Package: youtube-dl
	Pin: release a=bullseye-backports
	Pin-Priority: 500
	EOF

  apt install \
    --no-install-recommends \
      youtube-dl

  apt autoremove --purge \
    bluetooth dhcpcd5 wpasupplicant yelp
fi


# Web server packages.
if \
  [ "$hostname" = "vostok" ] || \
  [ "$hostname" = "voskhod" ]
then
  apt install --yes \
    certbot nginx-extras python3-certbot-dns-cloudflare

  apt autoremove --purge \
    awscli 'google-*' 'python*-boto*'
fi


# Media player
if [ "$hostname" = "almaz" ]; then
  apt install --yes mopidy

  apt autoremove --purge \
    avahi-daemon bluetooth dhcpcd5 ifupdown wpasupplicant yelp
fi


# Configure unattended upgrades
apt install --yes unattended-upgrades
sed -Ei \
  's|//      "o=Debian Backports,a=\$\{distro_codename\}-backports,l=Debian Backports";|        "o=Debian Backports,a=${distro_codename}-backports,l=Debian Backports";|' \
  /etc/apt/apt.conf.d/50unattended-upgrades


# Finally, clean up after ourselves.
apt autoremove --purge
apt purge $(dpkg -l | awk -v ORS=" " '/^rc/ { print $2 } END { printf("\n"); }')
apt clean
