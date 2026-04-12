{ self }:
{ config, lib, pkgs, ... }:

let
  cfg = config.services.todo-sync-server;
  inherit (lib) mkEnableOption mkOption mkIf mkMerge types literalExpression;
in
{
  options.services.todo-sync-server = {
    enable = mkEnableOption "todo-sync-server";

    package = mkOption {
      type = types.package;
      default = self.packages.${pkgs.system}.todo-sync-server;
      defaultText = literalExpression "self.packages.\${pkgs.system}.todo-sync-server";
      description = "The todo-sync-server package to use.";
    };

    port = mkOption {
      type = types.port;
      default = 8080;
      description = "Port for the sync server to listen on.";
    };

    dataDir = mkOption {
      type = types.path;
      default = "/var/lib/todo-sync-server";
      description = "Directory where the SQLite database is stored.";
    };

    openFirewall = mkOption {
      type = types.bool;
      default = false;
      description = "Whether to open the firewall for the server port.";
    };

    nginx = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to configure an nginx reverse proxy.";
      };

      domain = mkOption {
        type = types.str;
        default = "";
        description = "FQDN for the nginx virtualHost and ACME certificate.";
      };

      acme = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to use Let's Encrypt for TLS.";
      };
    };
  };

  config = mkIf cfg.enable (mkMerge [
    # -- Core: systemd service + admin wrapper ---------------------------------
    {
      systemd.services.todo-sync-server = {
        description = "Todo Sync Server";
        after = [ "network.target" ];
        wantedBy = [ "multi-user.target" ];

        serviceConfig = {
          ExecStart = "${cfg.package}/bin/todo-sync-server serve --port ${toString cfg.port} --database ${cfg.dataDir}/todo-sync.db";
          Restart = "on-failure";
          RestartSec = 5;

          DynamicUser = true;
          StateDirectory = "todo-sync-server";

          # Hardening
          ProtectSystem = "strict";
          ProtectHome = true;
          NoNewPrivileges = true;
          PrivateTmp = true;
          PrivateDevices = true;
          RestrictAddressFamilies = [ "AF_INET" "AF_INET6" "AF_UNIX" ];
          MemoryDenyWriteExecute = true;
          SystemCallFilter = [ "@system-service" "~@privileged" "~@resources" ];
          CapabilityBoundingSet = if cfg.port < 1024 then [ "CAP_NET_BIND_SERVICE" ] else [ "" ];
          AmbientCapabilities = mkIf (cfg.port < 1024) [ "CAP_NET_BIND_SERVICE" ];
        };
      };

      environment.systemPackages =
        let
          serverUrlFlag =
            if cfg.nginx.enable && cfg.nginx.domain != ""
            then " --server-url https://${cfg.nginx.domain}"
            else "";

          todoctl = pkgs.writeShellScriptBin "todoctl" ''
            set -euo pipefail
            cmd="''${1:-}"
            shift || true
            case "$cmd" in
              generate-invite-code)
                exec ${cfg.package}/bin/todo-sync-server generate-invite-code \
                  --database ${cfg.dataDir}/todo-sync.db${serverUrlFlag} "$@"
                ;;
              list-invite-codes)
                exec ${cfg.package}/bin/todo-sync-server list-invite-codes \
                  --database ${cfg.dataDir}/todo-sync.db "$@"
                ;;
              *)
                echo "Usage: todoctl {generate-invite-code|list-invite-codes} [OPTIONS]"
                exit 1
                ;;
            esac
          '';
        in
        [ todoctl ];
    }

    # -- Firewall --------------------------------------------------------------
    (mkIf cfg.openFirewall {
      networking.firewall.allowedTCPPorts = [ cfg.port ];
    })

    # -- nginx reverse proxy ---------------------------------------------------
    (mkIf cfg.nginx.enable {
      assertions = [
        {
          assertion = cfg.nginx.domain != "";
          message = "services.todo-sync-server.nginx.domain must be set when nginx is enabled.";
        }
        {
          assertion = !cfg.nginx.acme || config.security.acme.acceptTerms;
          message = "You must set security.acme.acceptTerms = true to use ACME with todo-sync-server.";
        }
      ];

      services.nginx = {
        enable = true;
        recommendedProxySettings = true;
        recommendedTlsSettings = true;
        recommendedOptimisation = true;
        recommendedGzipSettings = true;

        virtualHosts.${cfg.nginx.domain} = {
          forceSSL = cfg.nginx.acme;
          enableACME = cfg.nginx.acme;
          http2 = true;

          locations."/" = {
            proxyPass = "http://127.0.0.1:${toString cfg.port}";
            extraConfig = ''
              client_max_body_size 1m;
              add_header Strict-Transport-Security "max-age=63072000; includeSubDomains; preload" always;
            '';
          };
        };
      };
    })
  ]);
}
