{ config, pkgs, ... }:
let
  bitwarden = pkgs.fetchFirefoxAddon {
    name = "bitwarden";
    url =
      "https://addons.mozilla.org/firefox/downloads/file/3688243/bitwarden_free_password_manager-1.47.1-an+fx.xpi";
    sha256 = "+DwVSW4ZrnlVgbj7zlIYPFzjHnKdMUY/t2vtiD3+rYQ=";
  };
  adblock = pkgs.fetchFirefoxAddon {
    name = "adblock-plus";
    url =
      "https://addons.mozilla.org/firefox/downloads/file/3694628/adblock_plus-3.10.1-an+fx.xpi";
    sha256 = "hWAKYODe820K9OcwK5zHOcRkafvbh5wtokJiwOp62YI=";
  };
  customFirefox = pkgs.firefox.override {
    nixExtensions = [ bitwarden adblock ];
    extraPolicies = {
      Certificates = {
        ImportEnterpriseRoots = true;
        Install = [ "freenas.pem" "media.pem" "home.pem" ];
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
