{
  pkgs,
  inputs,
  ...
}: {
  nixpkgs.overlays = [
    (self: super: {
      mpv = super.mpv.override {
        scripts = with self.mpvScripts; [ mpris uosc ];
      };
    })
  ];
  environment.systemPackages = with pkgs; [
    (hyprland.override {enableXWayland = true;})
    foot
    waybar
    git
    spotify-player
    xdg-desktop-portal-hyprland
    xdg-desktop-portal-gtk
    kdePackages.xdg-desktop-portal-kde
    eww
    mpv
    sptlrx
    btop
    rofi-wayland
    bat
    direnv
    eza
    zoxide
    fzf
    starship
    zathura
    blueman
    fishPlugins.autopair
    fishPlugins.puffer

    xorg.xrdb

    (papirus-icon-theme.override {color = "teal";})
    phinger-cursors
    nerd-fonts.symbols-only
    noto-fonts-emoji

    aria2
    curl
    wget
    yt-dlp
    ffmpeg
    imagemagick

    dust
    duf
    fd
    file
    ripgrep
    xcp

    gcc
    xdg-utils
    timg
    playerctl
    translate-shell
    pulsemixer

    grimblast
    cliphist
    wl-clipboard
    swww
    eww

    microfetch
    nurl
    sops

    zip
    unzip
    rar
    unrar
    p7zip

    calibre
    vesktop
    qbittorrent
    xfce.thunar
    qalculate-qt
    nsxiv

    heroic
    prismlauncher
    protonup-qt
    # yuzu
    # onscripter-en
    # desmume
    # mgba
    # snes9x-gtk
    # pcsx2

    inputs.zen-browser.packages."${system}".default
    brave

    libnotify
    man-pages
    man-pages-posix

    python3
    zig
    clojure
    leiningen
    zls

    emacs30-pgtk
    emacs-lsp-booster
  ];
}
