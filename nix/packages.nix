{pkgs, inputs, ...}: {
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
    inputs.zen-browser.packages."${system}".default

    foot
    waybar
    eww
    (rofi-wayland.override {plugins = [pkgs.rofi-calc];})
    zathura
    mpv
    blueman

    grimblast
    cliphist
    wl-clipboard
    swww
    eww

    spotify-player
    sptlrx
    btop
    starship
    dust
    duf
    fzf
    eza
    bat

    direnv
    zoxide
    stow
    git
    fd
    ripgrep
    xcp
    fishPlugins.autopair
    fishPlugins.puffer

    adw-gtk3
    (papirus-icon-theme.override {color = "teal";})
    phinger-cursors

    aria2
    curl
    wget
    yt-dlp
    ffmpeg
    imagemagick

    gcc
    file
    xdg-utils
    timg
    playerctl
    translate-shell
    pulsemixer

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
    nemo-with-extensions
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

    xorg.xrdb
    man-pages
    man-pages-posix

    python3
    zig
    clojure
    leiningen
    zls

    neovim
    emacs30-pgtk
    emacs-lsp-booster
  ];
}
