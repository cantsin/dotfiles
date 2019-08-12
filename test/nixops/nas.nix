let
  domain = "somedomain.somewhere";

in
{
  network.description = "Network Attached Storage";
  nas = { config, pkgs, ... }:

  let
    selfSignedCert = pkgs.runCommand "self-signed-certificate" {} ''
      mkdir -p $out
      ${pkgs.openssl}/bin/openssl req -x509 -out $out/localhost.crt -keyout $out/localhost.key -newkey rsa:2048 -days 730 -nodes -sha256 -subj '/CN=*.${domain}' -extensions EXT -config <(printf "[dn]\nCN=*.${domain}\n[req]\ndistinguished_name = dn\n[EXT]\nsubjectAltName=DNS:${domain}\nkeyUsage=digitalSignature\nextendedKeyUsage=serverAuth")
    '';

in
  {
    services.nginx = {
      enable = true;
      recommendedGzipSettings = true;
      recommendedOptimisation = true;
      recommendedProxySettings = true;
      recommendedTlsSettings = true;
    };

    services.tt-rss = {
      enable = true;
      email.fromAddress = "james@tranovi.ch";
      selfUrlPath = "https://rss.${domain}";
      singleUserMode = true;
      root = "/var/lib/tt-rss";
      pool = "tt-rss";
    };
    services.nginx.virtualHosts."rss.${domain}" = {
      forceSSL = true;
      sslCertificate = "${selfSignedCert}/localhost.crt";
      sslCertificateKey = "${selfSignedCert}/localhost.key";
      root = "/var/lib/tt-rss";

      locations."/" = {
        index = "index.php";
      };

      locations."~ \.php$" = {
        extraConfig = ''
          fastcgi_split_path_info ^(.+\.php)(/.+)$;
          fastcgi_pass unix:${config.services.phpfpm.pools.tt-rss.listen};
          fastcgi_index index.php;
        '';
      };
    };

    networking.firewall.allowedTCPPorts = [ 80 443 ];
  };
}
