{ config, pkgs, ... }:

let
  dotfiles = "/home/james/.dotfiles";
in
{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  nixpkgs.overlays = [(self: super: {
    st = super.st.override {
      patches = builtins.map super.fetchurl [{
        url = "https://st.suckless.org/patches/bold-is-not-bright/st-bold-is-not-bright-20190127-3be4cf1.diff";
        sha256 = "1cpap2jz80n90izhq5fdv2cvg29hj6bhhvjxk40zkskwmjn6k49j";
      }
      {
        url = "http://st.suckless.org/patches/anysize/st-anysize-0.8.1.diff";
        sha256 = "03z5vvajfbkpxvvk394799l94nbd8xk57ijq17hpmq1g1p2xn641";
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
    firefox
    htop
    ispell
    nix-prefetch-scripts
    nix-zsh-completions
    nmap
    pass
    pstree
    ripgrep
    (st.override { conf = builtins.readFile ./st-config.h; })
    tig
    tmux
    xlibs.xmodmap
    zip

    # applets
    networkmanagerapplet
    qsyncthingtray
  ];

  programs.git = {
    package = pkgs.gitAndTools.gitFull;
    enable = true;
    userName = "James Tranovich";
    userEmail = "jtranovich@gmail.com";
    extraConfig.core = { editor = "emacs"; };
  };

  programs.zsh.enable = true;
  programs.zsh.sessionVariables = {
    BC_ENV_ARGS = "$HOME/.config/bc";
    TERM = "xterm-256color";
    ZSH_CUSTOM = "$HOME/.zsh-custom";
    ZSH_THEME = "cantsin";
    ZSH_TMUX_AUTOSTART = true;
    ZSH_TMUX_AUTOCONNECT = false;
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
    plugins = [ "git" "docker" "tmux" ];
  };

  programs.emacs = {
    enable = true;
    extraPackages = import ./emacs.nix { inherit pkgs; };
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

  xsession = {
    enable = true;
    windowManager.i3 = import ./i3.nix;
    initExtra = ''
      xmodmap ~/.Xmodmap
    '';
  };

  programs.jq.enable = true;
  services.emacs.enable = true;
  services.flameshot.enable = true;
  services.syncthing.enable = true;
  services.syncthing.tray = true;
  services.network-manager-applet.enable = true;
  services.random-background = {
    enable = true;
    imageDirectory = "%h/.backgrounds";
  };

  services.xscreensaver.enable = true;

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
    ".config/i3status/config".source = "${dotfiles}/i3status/config";
    ".config/bc".source = "${dotfiles}/bc";
    ".zsh-custom/themes/cantsin.zsh-theme".source = "${dotfiles}/zsh-custom/themes/cantsin.zsh-theme";
    ".backgrounds".source = "${dotfiles}/backgrounds";
    ".emacs.d" = {
      source = "${dotfiles}/emacs.d";
      recursive = true;
    };
    ".local/share/fonts" = {
      source = "${dotfiles}/triplicate";
      recursive = true;
    };
  };
}
