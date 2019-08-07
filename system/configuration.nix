{ config, pkgs, ... }:

let
  hostName = (import ./hostname.nix).hostName;
  useNvidia = builtins.elem hostName [ "zen" ];
  secureBoot = builtins.elem hostName [ "satori" ];
in {
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ] ++ (builtins.filter builtins.pathExists [
    ./system-vpn.nix
    ./system-hosts.nix
    ./system-experimental.nix
  ]);

  boot.initrd.luks.devices = if secureBoot then [{
    name = "root";
    device = "/dev/nvme0n1p2";
    preLVM = true;
  }] else
    [ ];

  environment.variables.TERMINAL = "st";
  environment.variables.EDITOR = "emacs";

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = hostName;
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

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
  ];

  fonts = {
    enableFontDir = true;
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
  # sound.enable = true;
  # hardware.pulseaudio.enable = true;
  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "eurosign:e";
  services.xserver.windowManager.default = "i3";
  services.xserver.windowManager.i3.enable = true;
  services.xserver.desktopManager.xterm.enable = false;
  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.displayManager.lightdm.background = "black";
  services.xserver.displayManager.lightdm.extraSeatDefaults = ''
    greeter-hide-users=false
  '';

  # Enable touchpad support.
  services.xserver.libinput.enable = true;
  services.xserver.libinput.disableWhileTyping = true;

  # fix dconf/dbus errors
  services.dbus.packages = with pkgs; [ gnome3.dconf ];

  # conditional nvidia support
  nixpkgs.config.allowUnfree = useNvidia;
  services.xserver.videoDrivers = if useNvidia then [ "nvidia" ] else [ ];

  virtualisation.docker.enable = true;
  virtualisation.virtualbox.host.enable = true;

  services.nixops-dns = {
    enable = true;
    user = "james";
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.james = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" "networkmanager" "docker" ];
    shell = pkgs.zsh;
  };
  users.extraGroups.vboxusers.members = [ "james" ];

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?

  system.autoUpgrade.enable = true;
}
