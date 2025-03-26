{pkgs, inputs, ...}: let
  yuzu = pkgs.callPackage ./yuzu.nix {};
in {
  nixpkgs.overlays = [
    (self: super: {
      mpv = super.mpv.override {
        scripts = with self.mpvScripts; [ mpris uosc ];
      };
      hyprmag = super.hyprpicker.overrideAttrs {
        src = super.fetchFromGitHub {
          owner = "SIMULATAN";
          repo = "hyprmag";
          rev = "a7ca558d357e2a0d34373620f89d2daf210744dc";
          hash = "sha256-Ntb+lvscDzOAGOJo+xOtjRrjdIDW6HpAtehlh5VipV8=";
        };
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
    (rofi-wayland.override {plugins = [pkgs.rofi-calc];})
    hyprmag
    zathura
    mpv
    blueman

    grimblast
    cliphist
    wl-clipboard
    swww

    spotify-player
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
    nvd
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
    yuzu
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
    clojure
    leiningen
    odin

    neovim
    emacs30-pgtk
    emacs-lsp-booster
  ];
}
