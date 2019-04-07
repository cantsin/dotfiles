{ config, pkgs, ... }:

let
  dotfiles = "/home/james/.dotfiles";
in
{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # patch suckless terminal
  nixpkgs.overlays = [(self: super: {
    st = super.st.override {
      conf = builtins.readFile ./st-config.h;
      patches = [./st-badweight.patch] ++ builtins.map super.fetchurl [{
        url = "https://st.suckless.org/patches/bold-is-not-bright/st-bold-is-not-bright-20190127-3be4cf1.diff";
        sha256 = "1cpap2jz80n90izhq5fdv2cvg29hj6bhhvjxk40zkskwmjn6k49j";
      }
      {
        url = "http://st.suckless.org/patches/anysize/st-anysize-0.8.1.diff";
        sha256 = "03z5vvajfbkpxvvk394799l94nbd8xk57ijq17hpmq1g1p2xn641";
      }
      {
        url = "https://st.suckless.org/patches/visualbell/st-visualbell-0.8.1.diff";
        sha256 = "1cr8vk8yjlpg6wj4p05lb37q111pih6rmrq2cmnmp1rkw3mnq1f4";
      }];
    };
  })];

  home.packages = with pkgs; [
    arandr
    bc
    bind
    direnv
    evince
    feh
    file
    htop
    ispell
    nmap
    pstree
    ripgrep
    st
    tig
    tmux
    xlibs.xmodmap
    zip

    # convenience
    nix-prefetch-scripts
    nix-zsh-completions

    # applets
    networkmanagerapplet

    # notification system
    notify-osd-customizable
  ];

  programs.git = {
    package = pkgs.gitAndTools.gitFull;
    enable = true;
    userName = "James Tranovich";
    userEmail = "jtranovich@gmail.com";
    aliases = {
      co = "checkout";
    };
    extraConfig = {
      core = {
        editor = "emacs";
      };
      pull = {
        rebase = true;
      };
      rebase = {
        autostash = true;
      };
    };
  };

  programs.zsh.enable = true;
  programs.zsh.sessionVariables = {
    BC_ENV_ARGS = "$HOME/.config/bc";
    ZSH_CUSTOM = "$HOME/.zsh-custom";
    ZSH_THEME = "cantsin";
    ZSH_TMUX_AUTOSTART = 1;
    ZSH_TMUX_AUTOCONNECT = 0;
  };
  programs.zsh.initExtra = ''
    command_not_found_handler() { command-not-found "$1" }
    nixify() {
      if [ ! -e ./.envrc ]; then
        echo "use nix" > .envrc
        direnv allow
      fi
      if [ ! -e default.nix ]; then
        cat > default.nix <<'EOF'
    with import <nixpkgs> {};
    stdenv.mkDerivation {
      name = "env";
      buildInputs = [
        bashInteractive
      ];
    }
    EOF
        emacs default.nix
      fi
    }
    eval "$(direnv hook zsh)"
  '';

  programs.zsh.oh-my-zsh = {
    enable = true;
    plugins = ["git" "docker" "tmux"];
  };

  programs.emacs = {
    enable = true;
    extraPackages = import ./emacs-packages.nix { inherit pkgs; };
  };

  programs.tmux = {
    enable = true;
    extraConfig = "
      set -g default-terminal 'screen-256color'
      set-option -g status-bg colour237
      set-option -g status-fg yellow
      set-option -g status-attr default
      set-option -g pane-border-fg colour237
      set-option -g pane-active-border-fg brightgreen
      set-option -g message-bg colour237
      set-option -g message-fg white
      set-option -g display-panes-active-colour blue
      set-option -g display-panes-colour brightred
      set-window-option -g mode-keys emacs
      set-window-option -g window-status-fg colour75
      set-window-option -g window-status-bg default
      set-window-option -g window-status-attr dim
      set-window-option -g window-status-current-fg white
      set-window-option -g window-status-current-bg default
      set-window-option -g window-status-bell-style fg=colour237,bg=red
      set -g prefix C-o
      unbind-key -n C-a
      set -g mouse on
      set -g status-left ''
      set -g status-right ''
    ";
  };

  programs.firefox = {
    enable = true;
    enableAdobeFlash = true; # only for https://anywhere.convorelay.com/
    enableGoogleTalk = true;
  };

  xsession = {
    enable = true;
    windowManager.i3 = import ./i3.nix pkgs;
    initExtra = ''
      xmodmap ~/.Xmodmap
    '';
  };

  programs.jq.enable = true;
  programs.browserpass.enable = true;
  services.emacs.enable = true;
  services.flameshot.enable = true;
  services.syncthing.enable = true;
  # qsyncthingtray is currently broken. in the meantime, go to http://127.0.0.1:8384/ directly
  # services.syncthing.tray = true;
  services.network-manager-applet.enable = true;
  services.polybar = import ./polybar.nix pkgs;
  services.redshift = {
    enable = true;
    tray = true;
    latitude = "37.733795";
    longitude = "-122.446747";
    provider = "manual";
  };

  services.screen-locker = {
    enable = true;
    lockCmd = "${pkgs.i3lock-fancy}/bin/i3lock-fancy -pf Triplicate-T3c";
  };

  home.file = {
    ".Xmodmap" = {
      text = ''
        remove Lock = Caps_Lock
        remove Control = Control_L
        keysym Caps_Lock = Control_L
        add Lock = Caps_Lock
        add Control = Control_L
      '';
    };
    ".notify-osd" = {
      text = ''
        text-title-size = 150%
        text-body-size = 150%
        bubble-width = 480px
        bubble-close-on-click = 1
      '';
    };
    ".config/bc".source = "${dotfiles}/bc";
    ".zsh-custom/themes/cantsin.zsh-theme".source = "${dotfiles}/zsh-custom/themes/cantsin.zsh-theme";
    ".backgrounds".source = "${dotfiles}/backgrounds";
    ".emacs.d" = {
      source = "${dotfiles}/emacs.d";
      recursive = true;
    };
    ".local/share/fonts" = {
      source = "${dotfiles}/fonts";
      recursive = true;
    };
  };

  # fix java applications
  home.sessionVariables._JAVA_AWT_WM_NONREPARENTING = "1";

  imports = [
    ./background.nix
    ./experimental.nix
  ] ++ (
    if builtins.pathExists ./secrets/default.nix then [./secrets/default.nix] else []
  );
}
