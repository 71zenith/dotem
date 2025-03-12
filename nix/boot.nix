{modulesPath, ...}: {
  imports = [(modulesPath + "/installer/scan/not-detected.nix")];
  boot = {
    initrd.availableKernelModules = ["xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod"];
    kernelModules = ["kvm-amd"];
  };
  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/a712d0ab-a784-4a07-bcfa-c538173722c2";
      fsType = "btrfs";
      options = ["subvol=@" "compress=zstd"];
    };
    "/boot" = {
      device = "/dev/disk/by-uuid/43BE-E69D";
      fsType = "vfat";
    };
    "/home/zen/mnt" = {
      device = "/dev/disk/by-uuid/d574db64-7098-4a82-8837-3f4f43f3b003";
      fsType = "ext4";
      options = ["noauto"];
    };
  };
  swapDevices = [{device = "/dev/disk/by-uuid/9ff583e8-b9bb-46f3-a68d-0883d3f71fe6";}];
}
