{ pkgs, ... }: {
  services.polybar = import ./polybar.nix pkgs;
  services.flameshot.enable = true;
  xsession.enable = true;
  xsession.initExtra = "xmodmap ~/.Xmodmap";
  xsession.windowManager.i3 = {
    enable = true;
    extraConfig = ''
      set $ws1 "α"
      set $ws2 "β"
      set $ws3 "γ"
      set $ws4 "δ"
      set $ws5 "ε"
      set $ws6 "ζ"
      set $ws7 "η"
      set $ws8 "θ"
      set $ws9 "ι"
      set $ws10 "κ"
      set $darkred     #cc241d
      set $darkwhite   #a89984
      set $white       #ebdbb2
      set $black       #282828
      set $darkblack   #1d2021
      set $transparent #00000000
    '';
    config = {
      modifier = "Mod4";
      fonts = [ "Triplicate T3c" "Liberation Mono 16" ];
      bars = [ ];
      colors = {
        focused = {
          border = "$black";
          background = "$black";
          text = "$white";
          indicator = "$darkblack";
          childBorder = "$darkblack";
        };
        unfocused = {
          border = "$black";
          background = "$black";
          text = "$darkwhite";
          indicator = "$darkblack";
          childBorder = "$darkblack";
        };
        focusedInactive = {
          border = "$black";
          background = "$black";
          text = "$darkwhite";
          indicator = "$darkblack";
          childBorder = "$darkblack";
        };
        urgent = {
          border = "$darkred";
          background = "$darkred";
          text = "$black";
          indicator = "$darkred";
          childBorder = "$darkred";
        };
        background = "$black";
      };
      window.commands = [
        {
          criteria = { class = "splash"; };
          command = "floating enable";
        }
        {
          criteria = { workspace = "κ"; };
          command = "layout stacked";
        }
      ];
      keybindings = {
        "Print" = "exec flameshot gui";
        "Mod4+Return" = "exec i3-sensible-terminal";
        "Mod4+p" =
          "exec dmenu_run -fn 'Triplicate T3c-16'  -sb '#ff8700' -sf '#1d2021'";
        "Mod4+x" =
          "exec ${pkgs.i3lock-fancy}/bin/i3lock-fancy -pf Triplicate-T3c";
        "Mod4+Shift+q" = "kill";
        "Mod4+h" = "split h";
        "Mod4+v" = "split v";
        "Mod4+f" = "fullscreen toggle";
        "Mod4+s" = "layout stacking";
        "Mod4+w" = "layout tabbed";
        "Mod4+e" = "layout toggle split";
        "Mod4+Shift+space" = "floating toggle";
        "Mod4+space" = "focus mode_toggle";
        "Mod4+a" = "focus parent";
        "Mod4+Shift+c" = "reload";
        "Mod4+Shift+r" = "restart";
        "Mod4+r" = "mode resize";
        "Mod4+j" = "focus left";
        "Mod4+k" = "focus down";
        "Mod4+l" = "focus up";
        "Mod4+semicolon" = "focus right";
        "Mod4+Shift+j" = "move left";
        "Mod4+Shift+k" = "move down";
        "Mod4+Shift+l" = "move up";
        "Mod4+Shift+semicolon" = "move right";
        "Mod4+1" = "workspace $ws1";
        "Mod4+2" = "workspace $ws2";
        "Mod4+3" = "workspace $ws3";
        "Mod4+4" = "workspace $ws4";
        "Mod4+5" = "workspace $ws5";
        "Mod4+6" = "workspace $ws6";
        "Mod4+7" = "workspace $ws7";
        "Mod4+8" = "workspace $ws8";
        "Mod4+9" = "workspace $ws9";
        "Mod4+0" = "workspace $ws10";
        "Mod4+Shift+1" = "move container to workspace $ws1";
        "Mod4+Shift+2" = "move container to workspace $ws2";
        "Mod4+Shift+3" = "move container to workspace $ws3";
        "Mod4+Shift+4" = "move container to workspace $ws4";
        "Mod4+Shift+5" = "move container to workspace $ws5";
        "Mod4+Shift+6" = "move container to workspace $ws6";
        "Mod4+Shift+7" = "move container to workspace $ws7";
        "Mod4+Shift+8" = "move container to workspace $ws8";
        "Mod4+Shift+9" = "move container to workspace $ws9";
        "Mod4+Shift+0" = "move container to workspace $ws10";
        "Mod4+Right" = "move workspace to output right";
        "Mod4+Left" = "move workspace to output left";
        "XF86MonBrightnessUp" = "exec brightnessctl set +10%";
        "XF86MonBrightnessDown" = "exec brightnessctl set 10%-";
      };
      startup = [{
        command = "systemctl --user restart polybar";
        always = true;
        notification = false;
      }];
    };
  };
  services.screen-locker = {
    enable = true;
    lockCmd = "${pkgs.i3lock-fancy}/bin/i3lock-fancy -pf Triplicate-T3c";
  };
}
