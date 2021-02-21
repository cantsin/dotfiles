{ pkgs, ... }: {
  services.polybar = import ./polybar.nix pkgs;
  services.flameshot.enable = true;
  services.redshift = {
    enable = true;
    tray = true;
    latitude = "37.733795";
    longitude = "-122.446747";
    provider = "manual";
  };
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
        "F5" = "exec pkill -f screenkey";
        "Mod4+F5" = "exec ${pkgs.screenkey}/bin/screenkey";
        "Mod4+Return" = "exec i3-sensible-terminal";
        "Mod4+p" =
          "exec dmenu_run -fn 'Triplicate T3c-16'  -sb '#ff8700' -sf '#1d2021'";
        "Mod4+x" = "exec ${pkgs.i3lock}/bin/i3lock -c 1d2021";
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
      startup = [
        {
          command = "${pkgs.autorandr}/bin/autorandr -c";
          always = true;
          notification = false;
        }
        {
          command = "systemctl --user restart polybar";
          always = true;
          notification = false;
        }
      ];
    };
  };
  services.screen-locker = {
    enable = true;
    lockCmd = "${pkgs.i3lock}/bin/i3lock -c 1d2021";
  };
  programs.autorandr = {
    enable = true;
    profiles = {
      desk = {
        fingerprint = {
          "DVI-D-0" =
            "00ffffffffffff0010ac63404c3830312e16010380402878ea8d85ad4f35b1250e5054a54b00714f81008180a940d100d14001010101e26800a0a0402e603020360081912100001a000000ff005048354e593242443130384c0a000000fc0044454c4c2055333031310a2020000000fd0031561d711c000a2020202020200086";
          "DisplayPort-1" =
            "00ffffffffffff0009d1258045540000171d0104b54628783e87d1a8554d9f250e5054a56b80818081c08100a9c0b300d1c0010101014dd000a0f0703e8030203500c48f2100001a000000ff0039364b30303132353031390a20000000fd00324c1e8c3c000a202020202020000000fc0042656e5120504433323030550a01f7020324f15161605f5e5d101f22212005140413120301230907078301000065030c002000a36600a0f0701f8030203500c48f2100001a565e00a0a0a029502f203500c48f2100001a00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000b6";
          "DisplayPort-2" =
            "00ffffffffffff0010ac65404c3432382e160104b54028783a8d85ad4f35b1250e5054a54b00714f81008180a940d100d14001010101e26800a0a0402e603020360081912100001a000000ff005048354e593242443832344c0a000000fc0044454c4c2055333031310a2020000000fd0031561d711c000a20202020202001f302031df15090050403020716010611121513141f20230d7f07830f0000023a801871382d40582c450081912100001e011d8018711c1620582c250081912100009e011d007251d01e206e28550081912100001e8c0ad08a20e02d10103e9600819121000018000000000000000000000000000000000000000000000000000077";
        };
        config = {
          "DisplayPort-2" = {
            enable = true;
            mode = "2560x1600";
            position = "0x0";
            rotate = "normal";
            primary = true;
          };
          "DVI-D-0" = {
            enable = true;
            mode = "2560x1600";
            position = "2560x0";
            rotate = "normal";
          };
          "DisplayPort-1" = {
            enable = true;
            mode = "3840x2160";
            position = "5120x0";
            rotate = "normal";
          };
        };
      };
    };
  };

}
