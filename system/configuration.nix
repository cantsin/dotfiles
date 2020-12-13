{ config, pkgs, ... }:

let
  lib = import <nixpkgs/lib>;
  settings = import ./settings.nix;
  secureBoot = builtins.elem settings.hostName [ "satori" ];
in {
  nix = {
    package = pkgs.nixFlakes;
    extraOptions = lib.optionalString (config.nix.package == pkgs.nixFlakes)
      "experimental-features = nix-command flakes";
  };

  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./firefox.nix
  ] ++ (builtins.filter builtins.pathExists [
    ./system-vpn.nix
    ./system-hosts.nix
    ./system-experimental.nix
  ]);

  boot.initrd.luks.devices = if secureBoot then {
    name = "root";
    device = "/dev/nvme0n1p2";
    preLVM = true;
  } else
    { };

  environment.variables.TERMINAL = "st";
  environment.variables.EDITOR = "emacs";

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.configurationLimit = 25;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernelParams = [ "amd_iommu=pt" "iommu=soft" "amdgpu.dpm=0" ];

  networking.hostName = settings.hostName;
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;

  # Select internationalisation properties.
  console = {
    font = "Lat2-Terminus16";
    # keyMap = "us";
    useXkbConfig = true;
  };
  i18n.defaultLocale = "en_US.UTF-8";

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    curl
    emacs
    git
    gnumake
    rsync
    xterm
    zsh

    # icons
    hicolor-icon-theme
    tango-icon-theme
  ];

  fonts = {
    fontDir.enable = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      anonymousPro
      dejavu_fonts
      font-awesome-ttf
      freefont_ttf
      google-fonts
      inconsolata
      liberation_ttf
      nerdfonts
      source-code-pro
      terminus_font
      ttf_bitstream_vera
      ubuntu_font_family
    ];
  };

  programs.ssh.startAgent = true;

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.permitRootLogin = "no";
  services.openssh.passwordAuthentication = false;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = false;

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers = [ pkgs.hplip ];
  # http://localhost:631/ -- HP Color LaserJet MFP M477fdw

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;
  hardware.enableRedistributableFirmware = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "ctrl:nocaps";

  services.xserver.desktopManager.xterm.enable = false;

  # i3 or sway.
  programs.sway.enable = !settings.i3;
  services.xserver.windowManager.i3.enable = settings.i3;
  services.xserver.displayManager.defaultSession =
    if settings.i3 then "none+i3" else "sway";

  services.xserver.displayManager.gdm = {
    enable = true;
    wayland = true;
  };

  # Enable touchpad support.
  services.xserver.libinput.enable = true;
  services.xserver.libinput.disableWhileTyping = true;

  services.xserver.videoDrivers = [ "amdgpu" ];

  virtualisation.docker.enable = true;
  # virtualisation.virtualbox.host.enable = true;

  services.nixops-dns = {
    enable = true;
    user = "james";
  };

  # android
  programs.adb.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.james = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [
      "wheel"
      "networkmanager"
      "docker"
      "video"
      "audio"
      "dialout"
      "adbusers"
    ];
    shell = pkgs.zsh;
  };
  users.extraGroups.vboxusers.members = [ "james" ];

  system.stateVersion = "19.03";

  system.autoUpgrade.enable = true;
}
