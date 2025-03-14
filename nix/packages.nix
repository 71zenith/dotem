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
  fonts.packages = with pkgs; [
    nerd-fonts.symbols-only
    noto-fonts-emoji
  ];
  environment.systemPackages = with pkgs; [
    hyprland
    foot
    waybar
    rofi-wayland
    eww

    spotify-player
    sptlrx
    btop
    bat
    direnv
    eza
    zoxide
    fzf
    starship
    fishPlugins.autopair
    fishPlugins.puffer

    zathura
    mpv
    blueman

    stow
    git

    adw-gtk3
    (papirus-icon-theme.override {color = "teal";})
    phinger-cursors

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
    xorg.xrdb
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
