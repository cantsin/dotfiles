{ pkgs, ... }:

let
  darkred = "#cc241d";
  darkwhite = "#a89984";
  white = "#ebdbb2";
  black = "#282828";
  darkblack = "#1d2021";

in {
  xsession.windowManager.command = "${pkgs.waybar}/bin/waybar";
  wayland.windowManager.sway = {
    enable = true;
    config = {
      left = "j";
      down = "k";
      up = "l";
      right = "semicolon";
      terminal = "st tmux";
      modifier = "Mod4";
      fonts = [ "Triplicate T3c" "Liberation Mono 16" ];
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
}
