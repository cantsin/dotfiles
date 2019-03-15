{ pkgs, ... }:
let
  sysconfig = (import <nixpkgs/nixos> {}).config;
  imageDirectory = "%h/.backgrounds/${sysconfig.networking.hostName}";
in
{
  systemd.user.services.set-background = {
    Unit = {
      Description = "Set desktop background using feh";
      After = [ "graphical-session-pre.target" ];
      PartOf = [ "graphical-session.target" ];
    };
    Service = {
      Type = "oneshot";
      ExecStart = "${pkgs.feh}/bin/feh --randomize --bg-tile ${imageDirectory}";
    };
    Install = {
      WantedBy = [ "graphical-session.target" ];
    };
  };
}
