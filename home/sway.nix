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
  home.packages = with pkgs; [
    grim
    kanshi
    mako
    slurp
    swayidle
    swaylock-effects
    wf-recorder
    wl-clipboard
    xwayland
  ];
  xsession.windowManager.command = "${pkgs.waybar}/bin/waybar";
  xsession.preferStatusNotifierItems = true; # enable nm-applet
  wayland.windowManager.sway = {
    enable = true;
    config = rec {
      input = { "*" = { xkb_options = "ctrl:nocaps"; }; };
      output = {
        "eDP-1" = { scale = if hostname == "mu" then "1" else "2"; };
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
        "Mod4+Print" = ''
          exec wf-recorder -g "$(slurp)" -f ~/recording_$(date +"%Y-%m-%d_%H:%M:%S.mp4")'';
        "Mod4+Backspace" = "exec pkill -INT wf-recorder";
        "Mod4+p" = "exec ${menu}";
        "Mod4+1" = "workspace $ws1";
        "Mod4+2" = "workspace $ws2";
        "Mod4+3" = "workspace $ws3";
        "Mod4+4" = "workspace $ws4";
        "Mod4+5" = "workspace $ws5";
        "Mod4+6" = "workspace $ws6";
        "Mod4+7" = "workspace $ws7";
        "Mod4+8" = "workspace $ws8";
        "Mod4+9" = "workspace $ws9";
        "Mod4+Shift+1" = "move container to workspace $ws1";
        "Mod4+Shift+2" = "move container to workspace $ws2";
        "Mod4+Shift+3" = "move container to workspace $ws3";
        "Mod4+Shift+4" = "move container to workspace $ws4";
        "Mod4+Shift+5" = "move container to workspace $ws5";
        "Mod4+Shift+6" = "move container to workspace $ws6";
        "Mod4+Shift+7" = "move container to workspace $ws7";
        "Mod4+Shift+8" = "move container to workspace $ws8";
        "Mod4+Shift+9" = "move container to workspace $ws9";
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
    extraConfig = ''
      set $ws1 α
      set $ws2 β
      set $ws3 γ
      set $ws4 δ
      set $ws5 ε
      set $ws6 ζ
      set $ws7 η
      set $ws8 θ
      set $ws9 ι
      set $ws10 κ
      exec "swayidle -w timeout 300 'swaylock --screenshots --effect-blur 5x5' timeout 600 'swaymsg \"output * dpms off\"' resume 'swaymsg \"output * dpms on\"' before-sleep 'swaylock --screenshots --effect-blur 5x5'"
    '';
  };
  home.file.".config/waybar/config".source = "${dotfiles}/home/waybar/config";
  home.file.".config/waybar/style.css".source =
    "${dotfiles}/home/waybar/style.css";
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
  systemd.user.services.mako = {
    Unit = {
      Description = "Mako notification daemon";
      PartOf = [ "graphical-session.target" ];
    };
    Install = { WantedBy = [ "graphical-session.target" ]; };
    Service = {
      Type = "simple";
      BusName = "org.freedesktop.Notifications";
      ExecStart = "${pkgs.mako}/bin/mako";
      RestartSec = 5;
      Restart = "always";
    };
  };
}
