{ pkgs, ... }:

let
  lib = import <nixpkgs/lib>;
  sysconfig = (import <nixpkgs/nixos> { }).config;
  hostname = sysconfig.networking.hostName;
  darkred = "#cc241d";
  darkwhite = "#a89984";
  white = "#ebdbb2";
  black = "#282828";
  darkblack = "#1d2021";
  dotfiles = "/home/james/.dotfiles";

in {
  home.packages = with pkgs; [ grim slurp wl-clipboard ];
  xsession.windowManager.command = "${pkgs.waybar}/bin/waybar";
  xsession.preferStatusNotifierItems = true; # enable nm-applet
  wayland.windowManager.sway = {
    enable = true;
    config = rec {
      input = { "*" = { xkb_options = "ctrl:nocaps"; }; };
      output = {
        "eDP-1" = { scale = "2"; };
        "DP-2" = { scale = "2"; };
        "DP-3" = { position = "0 0"; };
        "DVI-D-1" = { position = "2560 0"; };
        "*" = { bg = "${dotfiles}/backgrounds/${hostname}/bg.jpg fill"; };
      };
      focus.followMouse = true;
      left = "j";
      down = "k";
      up = "l";
      right = "semicolon";
      terminal = "st tmux";
      menu = ''
        ${pkgs.bemenu}/bin/bemenu-run -i --fn "Triplicate T3c 16" --tb="#ff8700" --tf="#1d2021" --hf="#ff8700" -p "exec"'';
      modifier = "Mod4";
      fonts = [ "Triplicate T3c" "Liberation Mono 16" ];
      bars = [{ command = "${pkgs.waybar}/bin/waybar"; }];
      keybindings = lib.mkOptionDefault rec {
        "Print" = ''exec grim -g "$(slurp)"'';
        "Mod4+p" = "exec ${menu}";
      };
      colors = {
        focused = {
          border = "${black}";
          background = "${black}";
          text = "${white}";
          indicator = "${darkblack}";
          childBorder = "${darkblack}";
        };
        unfocused = {
          border = "${black}";
          background = "${black}";
          text = "${darkwhite}";
          indicator = "${darkblack}";
          childBorder = "${darkblack}";
        };
        focusedInactive = {
          border = "${black}";
          background = "${black}";
          text = "${darkwhite}";
          indicator = "${darkblack}";
          childBorder = "${darkblack}";
        };
        urgent = {
          border = "${darkred}";
          background = "${darkred}";
          text = "${black}";
          indicator = "${darkred}";
          childBorder = "${darkred}";
        };
        background = "${black}";
      };
    };
  };
  home.file.".config/waybar/config".source = "${dotfiles}/home/waybar.config";
  home.file.".config/waybar/style.css".source = "${dotfiles}/home/waybar.css";
  services.redshift = {
    enable = true;
    tray = true;
    package = pkgs.redshift-wlr;
    latitude = "37.733795";
    longitude = "-122.446747";
    provider = "manual";
  };
  programs.mako = {
    enable = true;
    font = "Triplicate T3c 12";
    textColor = darkwhite;
    backgroundColor = darkblack;
    borderColor = black;
    defaultTimeout = 5000;
  };
  # systemd.user.services.mako = {
  #   Unit = {
  #     Description = "Mako notification daemon";
  #     PartOf = [ "graphical-session.target" ];
  #   };
  #   Install = { WantedBy = [ "graphical-session.target" ]; };
  #   Service = {
  #     Type = "simple";
  #     BusName = "org.freedesktop.Notifications";
  #     ExecStart = "${pkgs.mako}/bin/mako";
  #     RestartSec = 5;
  #     Restart = "always";
  #   };
  # };
}
