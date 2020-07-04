{ pkgs, ... }:

let
  sysconfig = (import <nixpkgs/nixos> { }).config;
  hostname = sysconfig.networking.hostName;
  darkred = "#cc241d";
  darkwhite = "#a89984";
  white = "#ebdbb2";
  black = "#282828";
  darkblack = "#1d2021";
  dotfiles = "/home/james/.dotfiles";

in {
  xsession.windowManager.command = "${pkgs.waybar}/bin/waybar";
  wayland.windowManager.sway = {
    enable = true;
    config = {
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
      modifier = "Mod4";
      fonts = [ "Triplicate T3c" "Liberation Mono 16" ];
      bars = [{ command = "${pkgs.waybar}/bin/waybar"; }];
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
}
