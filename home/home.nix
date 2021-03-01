{ config, pkgs, ... }:

let
  settings = import ./settings.nix;
  useRemacs = false;
  dotfiles = "/home/james/.dotfiles";
in {
  programs.home-manager.enable = true;

  nixpkgs.overlays = [ (import ./overlays/st.nix) ];

  home.packages = with pkgs;
    [
      arandr
      bc
      bind
      brightnessctl
      direnv
      evince
      fd
      feh
      file
      gcc
      gitAndTools.hub
      htop
      ispell
      nmap
      pstree
      ripgrep
      sd
      st
      tig
      tmux
      xlibs.xmodmap
      zeal
      zip
      zsh-powerlevel10k

      # convenience
      nix-prefetch-scripts
      nix-zsh-completions
      any-nix-shell

      # applets
      networkmanagerapplet

      # notification system
      notify-osd-customizable
    ] ++ (if useRemacs then [ (import ./remacs/build.nix { }) ] else [ ]);

  programs.git = {
    package = pkgs.gitAndTools.gitFull;
    enable = true;
    userName = "James Tranovich";
    userEmail = "jtranovich@gmail.com";
    aliases = {
      co = "checkout";
      do = "!git fetch origin && git rebase origin/master";
    };
    extraConfig = {
      init = { defaultBranch = "main"; };
      core = { editor = "emacs"; };
      pull = { rebase = true; };
      rebase = { autostash = true; };
      sendemail = {
        confirm = "auto";
        smtpserver = "/home/james/.nix-profile/bin/msmtp";
      };
      github = { user = "cantsin"; };
    };
  };

  programs.zsh.enable = true;
  programs.zsh.sessionVariables = {
    BC_ENV_ARGS = "$HOME/.config/bc";
    ZSH_CUSTOM = "$HOME/.zsh-custom";
    ZSH_THEME = "powerlevel10k/powerlevel10k";
    ZSH_TMUX_AUTOSTART = "true";
    ZSH_TMUX_AUTOCONNECT = "false";
  };
  programs.zsh.initExtra = ''
    any-nix-shell zsh --info-right | source /dev/stdin
    command_not_found_handler() { command-not-found "$1" }
    nixify() {
      if [ ! -e ./.envrc ]; then
        echo 'eval "$(lorri direnv)"' > .envrc
        direnv allow
      fi
      if [ ! -e shell.nix ]; then
        cat > shell.nix <<'EOF'
    with import <nixpkgs> {};
    stdenv.mkDerivation {
      name = "env";
      buildInputs = [
        bashInteractive
      ];
    }
    EOF
        emacs shell.nix
      fi
    }
    eval "$(direnv hook zsh)"
    bindkey -r "^[l"
    bindkey "^[h" backward-kill-word
    source ~/.p10k.zsh
  '';
  # programs.zsh.shellAliases = { "emacs" = "emacsclient -c"; };
  programs.zsh.oh-my-zsh = {
    plugins = [ "tmux " ];
    enable = true;
  };

  programs.emacs = {
    enable = !useRemacs;
    extraPackages = import ./emacs-packages.nix { inherit pkgs; };
  };

  programs.tmux = {
    enable = true;
    extraConfig = ''
      set -g default-terminal 'screen-256color'
      set-option -g status-style fg=yellow,bg=colour237,default
      set-option -g pane-border-style fg=colour237
      set-option -g pane-active-border-style fg=brightgreen
      set-option -g message-style fg=white,bg=colour237
      set-option -g display-panes-active-colour blue
      set-option -g display-panes-colour brightred
      set-window-option -g mode-keys emacs
      set-window-option -g window-status-style fg=white,bg=default,dim
      set-window-option -g window-status-current-style fg=blue,bg=default
      set-window-option -g window-status-bell-style fg=colour237,bg=red
      set -g prefix C-o
      unbind-key -n C-a
      set -g mouse on
      set -g status-left '''
      set -g status-right '''
    '';
  };

  programs.jq.enable = true;
  programs.browserpass.enable = true;

  services.emacs.enable = !useRemacs;
  services.gpg-agent = {
    enable = true;
    extraConfig = ''
      allow-emacs-pinentry
      allow-loopback-pinentry
    '';
  };
  services.lorri.enable = true;
  services.network-manager-applet.enable = true;

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
    ".zsh-custom/themes/powerlevel10k" = {
      source = "${dotfiles}/home/powerlevel10k";
      recursive = true;
    };
    ".p10k.zsh".source = "${dotfiles}/home/p10k.zsh";
    ".config/bc".source = "${dotfiles}/bc";
    ".backgrounds".source = "${dotfiles}/backgrounds";
    ".config/emacs" = {
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

  # editor
  home.sessionVariables.ALTERNATE_EDITOR = "";
  home.sessionVariables.EDITOR = "emacsclient -t";
  home.sessionVariables.VISUAL = "emacsclient -c -a emacs";

  imports =
    (if settings.i3 then [ ./i3.nix ./background.nix ] else [ ./sway.nix ])
    ++ [ ./experimental.nix ./vale.nix ]
    ++ (if builtins.pathExists ./secrets/default.nix then
      [ ./secrets/default.nix ]
    else
      [ ]);
}
