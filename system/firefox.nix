{ config, pkgs, ... }:
let
  bitwarden = pkgs.fetchFirefoxAddon {
    name = "bitwarden";
    url =
      "https://addons.mozilla.org/firefox/downloads/file/3688243/bitwarden_free_password_manager-1.47.1-an+fx.xpi";
    sha256 = "+DwVSW4ZrnlVgbj7zlIYPFzjHnKdMUY/t2vtiD3+rYQ=";
  };
  darkreader = pkgs.fetchFirefoxAddon {
    name = "darkreader";
    url =
      "https://addons.mozilla.org/firefox/downloads/file/3684946/dark_reader-4.9.26-an+fx.xpi";
    sha256 = "XyokSVJPWrBcLoVo0meMayV5XofOd+vJRI4T6BhOPF8=";
  };
  customFirefox = pkgs.firefox.override {
    nixExtensions = [ bitwarden darkreader ];
    extraPolicies = {
      Certificates = {
        ImportEnterpriseRoots = true;
        Install =
          [ "bitwarden.pem" "freenas.pem" "git.pem" "media.pem" "rss.pem" ];
      };
      DownloadDirectory = "\${home}";
      DefaultDownloadDirectory = "\${home}";
      DisableFirefoxAccounts = true;
      DisableFirefoxStudies = true;
      DisableFormHistory = true;
      DisablePocket = true;
      DisableTelemetry = true;
      Homepage.StartPage = "previous-session";
      FirefoxHome = {
        Pocket = false;
        Snippets = false;
      };
      PasswordManagerEnabled = false;
      PictureInPicture.Enabled = false; # not a11y friendly
      SearchSuggestEnabled = false;
    };
    extraPrefs = ''
      // Show more ssl cert infos
      lockPref("security.identityblock.show_extended_validation", true);
      // Enable dark mode
      lockPref("extensions.activeThemeID", "firefox-compact-dark@mozilla.org");
      lockPref("browser.in-content.dark-mode", true);
      lockPref("devtools.theme", "dark");
    '';
  };
in {
  environment.variables = { BROWSER = [ "firefox" ]; };
  environment.systemPackages = with pkgs; [ customFirefox ];
}
