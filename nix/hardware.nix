{
  config,
  inputs,
  ...
}: {
  imports = [inputs.nix-gaming.nixosModules.pipewireLowLatency];
  services = {
    xserver = {
      xkb = {
        layout = "us";
        variant = "";
        options = "caps:escape,altwin:swap_lalt_lwin";
      };
      videoDrivers = ["nvidia"];
    };

    pipewire = {
      enable = true;
      alsa.enable = true;
      pulse.enable = true;
      lowLatency.enable = true;
    };
  };
  hardware = {
    bluetooth = {
      enable = true;
      powerOnBoot = true;
    };

    graphics = {
      enable = true;
      enable32Bit = true;
    };

    nvidia = {
      modesetting.enable = true;
      open = true;
      powerManagement.enable = true;
      forceFullCompositionPipeline = true;
      nvidiaSettings = true;
      package = config.boot.kernelPackages.nvidiaPackages.beta;
    };

    cpu.amd.updateMicrocode = true;
  };
  networking = {
    hostName = "izanagi";
    wireless.enable = false;
    useNetworkd = true;

    stevenBlackHosts.enable = true;

    wg-quick.interfaces = {
      jp = {
        autostart = false;
        address = ["10.2.0.2/32"];
        dns = ["10.2.0.1"];
        privateKeyFile = config.sops.secrets.vpn_private_jp.path;
        peers = [
          {
            publicKey = "5fFhuzIQPu8C4tySJuCJYg/13g75APFtMnqn3oeCpxk=";
            allowedIPs = ["0.0.0.0/0"];
            endpoint = "193.148.16.2:51820";
            persistentKeepalive = 25;
          }
        ];
      };
      us = {
        autostart = false;
        address = ["10.2.0.2/32"];
        dns = ["10.2.0.1"];
        privateKeyFile = config.sops.secrets.vpn_private_us.path;
        peers = [
          {
            publicKey = "ksK3faRBQlFLul2FcKPphBR9LYR+6/FbP1etg0T2liA=";
            allowedIPs = ["0.0.0.0/0"];
            endpoint = "37.19.221.198:51820";
            persistentKeepalive = 25;
          }
        ];
      };
      nl = {
        autostart = false;
        address = ["10.2.0.2/32"];
        dns = ["10.2.0.1"];
        privateKeyFile = config.sops.secrets.vpn_private_nl.path;
        peers = [
          {
            publicKey = "jA3Pf5MWpHk8STrLXVPyM28aV3yAZgw9nEGoIFAyxiI=";
            allowedIPs = ["0.0.0.0/0"];
            endpoint = "185.177.124.190:51820";
            persistentKeepalive = 25;
          }
        ];
      };
    };
  };
}
