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
    byobu
  ];

  programs.git = {
    package = pkgs.gitAndTools.gitFull;
    enable = true;
    userName = "James Tranovich";
    userEmail = "jtranovich@gmail.com";
  };

  programs.zsh.enable = true;
  programs.zsh.oh-my-zsh = {
    enable = true;
    plugins = [ "git" "docker" ];
    theme = "agnoster";
  };

  programs.emacs = {
    enable = true;
    extraPackages = epkgs: (with epkgs.melpaPackages; [
      alchemist
      auto-yasnippet
      autodisass-llvm-bitcode
      cargo
      cask
      caskxy
      cider
      company
      company-emacs-eclim
      company-ghc
      deft
      diminish
      dockerfile-mode
      easy-kill
      easy-kill-extras
      eclim
      elm-mode
      ember-mode
      ember-yasnippets
      epl
      erc-image
      eshell-prompt-extras
      esup
      esxml
      expand-region
      f
      flycheck
      flycheck-cask
      flycheck-color-mode-line
      flycheck-haskell
      flycheck-rust
      flymake-lua
      format-sql
      fsharp-mode
      gitconfig
      gitignore-mode
      glsl-mode
      go-mode
      handlebars-mode
      haskell-mode
      helm
      helm-company
      helm-dash
      helm-git-grep
      helm-gtags
      helm-hoogle
      helm-idris
      helm-make
      helm-projectile
      helm-spaces
      helm-swoop
      ht
      htmlize
      hydra
      idris-mode
      jedi
      jinja2-mode
      js2-mode
      json-mode
      key-chord
      kv
      ledger-mode
      /* lua-mode */ /* cannot build dependency? */
      magit
      markdown-mode
      markdown-mode-plus
      mc-extras
      /* merlin */ /* broken? */
      moe-theme
      multiple-cursors
      nix-mode
      nodejs-repl
      ocodo-svg-modelines
      /* org */ /* not found? */
      org-bullets
      org-journal
      package-build
      pallet
      pandoc-mode
      paradox
      paredit
      pdf-tools
      pkg-info
      popup
      powerline
      pretty-mode
      prodigy
      projectile
      puppet-mode
      purescript-mode
      reason-mode
      restclient
      rust-mode
      s
      scala-mode
      scss-mode
      session
      smart-compile
      smartparens
      solarized-theme
      stripe-buffer
      swiper-helm
      tagedit
      terraform-mode
      tide
      toml-mode
      tuareg
      typescript-mode
      typo
      use-package
      virtualenvwrapper
      wc-mode
      web-mode
      wgrep
      whitespace-cleanup-mode
      writegood-mode
      yaml-mode
      yasnippet
    ]);
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
