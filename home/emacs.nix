/*
This is a nix expression to build Emacs and some Emacs packages I like
from source on any distribution where Nix is installed. This will install
all the dependencies from the nixpkgs repository and build the binary files
without interfering with the host distribution.

To build the project, type the following from the current directory:

$ nix-build emacs.nix

To run the newly compiled executable:

$ ./result/bin/emacs
*/
{ pkgs ? import <nixpkgs> {} }:

let
  myEmacs = pkgs.emacs;
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
in
  emacsWithPackages (epkgs: (with epkgs.melpaPackages; [
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
  ]))
