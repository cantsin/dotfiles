{ config, pkgs, ... }:

let
  dotfiles = "/home/james/.dotfiles";
in
{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home.packages = with pkgs; [
    htop
    fortune
    pstree
    bc
    tmux
    ispell
    byobu
  ];

  programs.git = {
    package = pkgs.gitAndTools.gitFull;
    enable = true;
    userName = "James Tranovich";
    userEmail = "jtranovich@gmail.com";
  };

  programs.zsh.enable = true;
  programs.zsh.initExtra = ''
  '';

  programs.zsh.oh-my-zsh = {
    enable = true;
    plugins = [ "git" "docker" ];
    theme = "agnoster";
  };

  programs.emacs = {
    enable = true;
    extraPackages = import ./emacs.nix { inherit pkgs; };
  };

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

   ".byobu/keybindings.tmux" = {
      text = ''
        set -g prefix C-o
        unbind-key -n C-a
        set -g status-right '#(byobu-status tmux_right)'
        set -g mouse on
        set -g mouse-utf8 on
      '';
    };

   ".byobu/status" = {
      text = ''
        screen_upper_left="color"
        screen_upper_right="color"
        screen_lower_left="color"
        screen_lower_right="color"
        tmux_left="logo"
        tmux_right=""
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

     ".config/i3/config".source = "${dotfiles}/i3config";
     ".config/i3status/config".source = "${dotfiles}/i3statusconfig";
   };
}
