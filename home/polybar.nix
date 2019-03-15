{ pkgs, ... }:
let alpha = "99";
in
{
  enable = true;
  package = pkgs.polybar.override {
    i3Support = true;
    githubSupport = true;
  };
  config = {
    "color" = {
      green= "#98971a";
      yellow = "#d79921";
      darkred = "#cc241d";
      darkwhite = "#a89984";
      white = "#ebdbb2";
      transparent = "#${alpha}000000";
    };
    "settings" = {
       pseudo-transparency = true;
    };
    "bar/status" = {
       bottom = true;
       width = "100%";
       background = "\${color.transparent}";
       font-0 = "Triplicate T3c:pixelsize=10";
       font-1 = "FuraCode Nerd Font:pixelsize=10:style=Light";
       modules-left = "i3";
       modules-center = "mail github";
       modules-right = "wlan eth battery date";
       height = 48;
       dpi = 192;
       underline-size = 2;
       separator = "  ";
       tray-padding = 2;
       tray-maxsize = 24;
       tray-position = "right";
       tray-background = "\${color.transparent}";
       line-color = "\${color.transparent}";
       line-size = 2;
       # wm-restack = "i3";
       # override-redirect = true;
    };
    "module/wlan" = {
      type = "internal/network";
      interface = "wlp4s0";
      format-connected = "<label-connected>";
      label-connected = " %essid%/%local_ip%";
      label-connected-foreground = "\${color.green}";
      label-disconnected = "ﲁ";
      label-disconnected-foreground = "\${color.darkred}";
    };
    "module/eth" = {
      type = "internal/network";
      interface = "eno1";
      format-connected = "<label-connected>";
      label-connected = " %local_ip%";
      label-connected-foreground = "\${color.green}";
    };
    "module/mail" = {
      type = "custom/script";
      interval = 300;
      exec = "${pkgs.notmuch}/bin/notmuch count tag:unread";
      label = " %output%";
      label-foreground = "\${color.darkwhite}";
    };
    "module/github" = {
      type = "internal/github";
      token = "\${file:~/.github-access-token}";
      format = "<label>";
      label = " %notifications%";
      label-foreground = "\${color.darkwhite}";
      empty-notifications = true;
    };
    "module/battery" = {
      type = "internal/battery";
      battery = "BAT0";
      adapter = "AC";
      full-at = 98;
      label-charging-foreground = "\${color.green}";
      label-charging = " %percentage%%";
      label-discharging = " %percentage%%";
      label-discharging-foreground = "\${color.yellow}";
      label-full = "";
      label-full-foreground = "\${color.green}";
      ramp-capacity-0 = "";
      ramp-capacity-1 = "";
      ramp-capacity-2 = "";
      ramp-capacity-3 = "";
      ramp-capacity-4 = "";
    };
    "module/i3" = {
      type = "internal/i3";
      format = "<label-state> <label-mode>";
      index-sort = true;
      wrapping-scroll = true;
      # pin-workspaces = true;
      label-mode-padding = 4;
      label-focused = "%name%";
      label-focused-padding = 1;
      label-focused-foreground = "\${color.white}";
      label-focused-background = "\${color.transparent}";
      label-focused-underline = "\${color.white}";
      label-unfocused = "%name%";
      label-unfocused-padding = 1;
      label-unfocused-foreground = "\${color.darkwhite}";
      label-unfocused-background = "\${color.transparent}";
      label-unfocused-underline = "\${color.transparent}";
      label-visible = "%name%";
      label-visible-padding = 1;
      label-visible-foreground = "\${color.darkwhite}";
      label-visible-background = "\${color.transparent}";
      label-visible-underline = "\${color.transparent}";
      label-urgent = "%name%";
      label-urgent-padding = 1;
      label-urgent-foreground = "\${color.transparent}";
      label-urgent-background = "\${color.darkred}";
      label-urgent-underline = "\${color.transparent}";
    };
    "module/date" = {
      type = "internal/date";
      interval = 5;
      date = "%A, %B %d";
      time = "%H:%M:%S";
      label = " %date%  %time%";
      label-foreground = "\${color.white}";
    };
  };
  script = "polybar status &";
}
