{pkgs, config, inputs, ...}: {
  imports = [
    ./boot.nix
    ./hardware.nix
    ./packages.nix
    inputs.nix-gaming.nixosModules.platformOptimizations
  ];
  nixpkgs = {
    hostPlatform = "x86_64-linux";
    config.allowUnfree = true;
  };
  sops = {
    defaultSopsFile = ./secrets/secrets.yaml;
    age.sshKeyPaths = ["/home/zen/.ssh/id_ed25519"];
    secrets = {
      root_pass.neededForUsers = true;
      user_pass.neededForUsers = true;
      ssh_public = {};
      vpn_private_jp = {};
      vpn_private_us = {};
      vpn_private_nl = {};
      spot_username = {};
      spot_auth_data = {};
      spot_client_id = {owner = "zen";};
    };
    templates."credentials.json" = {
      content = builtins.toJSON {
        username = config.sops.placeholder.spot_username;
        auth_type = 1;
        auth_data = config.sops.placeholder.spot_auth_data;
      };
      owner = "zen";
      path = "/home/zen/.cache/spotify-player/credentials.json";
    };
  };
  nix = {
    settings = {
      experimental-features = ["nix-command" "flakes"];
      warn-dirty = false;
      trusted-users = ["@wheel"];
      log-lines = 50;
      http-connections = 50;
    };
    nixPath = ["nixpkgs=${inputs.nixpkgs}"];
    registry.nixpkgs.flake = inputs.nixpkgs;
    gc.automatic = true;
    optimise.automatic = true;
  };
  boot = {
    loader = {
      systemd-boot = {
        enable = true;
        consoleMode = "max";
      };
      efi.canTouchEfiVariables = true;
    };
    kernelPackages = pkgs.linuxPackages_xanmod;
  };
  documentation = {
    enable = true;
    dev.enable = true;
  };
  services = {
    greetd = {
      enable = true;
      settings = {
        default_session = {
          command = "uwsm start default";
          user = "zen";
        };
      };
    };
    audiobookshelf = {
      enable = true;
      openFirewall = true;
      group = "users";
    };
    udisks2.enable = true;
    gvfs.enable = true;
  };
  systemd.coredump.extraConfig = "Storage=none";
  environment = {
    etc.anime4k.source = pkgs.anime4k;
    sessionVariables.PATH = ["/home/zen/.config/scripts"];
    pathsToLink = ["/share/xdg-desktop-portal" "/share/applications"];
    variables.FREETYPE_PROPERTIES = "cff:no-stem-darkening=0 autofitter:no-stem-darkening=0";
  };
  security.sudo.wheelNeedsPassword = false;
  time.timeZone = "Asia/Kolkata";
  i18n = {
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings.LC_TIME = "en_IN";
    inputMethod = {
      enable = true;
      type = "fcitx5";
      fcitx5 = {
        addons = with pkgs;[fcitx5-mozc fcitx5-fluent];
        waylandFrontend = true;
      };
    };
  };
  console = {
    earlySetup = true;
    font = "${pkgs.terminus_font}/share/consolefonts/ter-132n.psf.gz";
    useXkbConfig = true;
  };
  programs = {
    fish.enable = true;
    hyprland = {
      enable = true;
      withUWSM = true;
    };
    gamescope.enable = true;
    steam = {
      enable = true;
      protontricks.enable = true;
      platformOptimizations.enable = true;
    };
    nh = {
      enable = true;
      flake = "/home/zen/dotem/nix";
    };
  };
  users.users = {
    "root" = {
      hashedPasswordFile = config.sops.secrets.root_pass.path;
    };
    "zen" = {
      isNormalUser = true;
      shell = pkgs.fish;
      homeMode = "770";
      openssh.authorizedKeys.keyFiles = [config.sops.secrets.ssh_public.path];
      hashedPasswordFile = config.sops.secrets.user_pass.path;
      extraGroups = ["wheel" "libvirtd" "input"];
    };
  };
  system.stateVersion = "25.05";
}
