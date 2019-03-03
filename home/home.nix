{ config, pkgs, ... }:

let
  dotfiles = "/home/james/.dotfiles";
in
{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

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
    networkmanagerapplet
    nix-prefetch-scripts
    nmap
    pass
    pstree
    qsyncthingtray
    ripgrep
    roxterm
    tig
    tmux
    xlibs.xmodmap
    zip
  ];

  programs.git = {
    package = pkgs.gitAndTools.gitFull;
    enable = true;
    userName = "James Tranovich";
    userEmail = "jtranovich@gmail.com";
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
      set-window-option -g window-status-fg brightblue
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

  xsession.windowManager.i3 = import ./i3.nix;

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

    ".config/roxterm.sourceforge.net/Profiles/Default" = {
      text = ''
        [roxterm profile]
        hide_menubar=1
        show_add_tab_btn=0
        always_show_tabs=0
        colour_scheme=Tango
        font=Liberation Mono 16
        scrollbar_pos=0
        scroll_on_output=0
        scroll_on_keystroke=1
      '';
    };

    ".config/i3status/config".source = "${dotfiles}/i3status/config";
    ".config/bc".source = "${dotfiles}/bc";

    ".zsh-custom/themes/cantsin.zsh-theme".source = "${dotfiles}/zsh-custom/themes/cantsin.zsh-theme";

    ".emacs.d" = {
      source = "${dotfiles}/emacs.d";
      recursive = true;
    };

    ".backgrounds".source = "${dotfiles}/backgrounds";
  };
}
