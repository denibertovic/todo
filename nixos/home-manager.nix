{ self }:
{ config, lib, pkgs, ... }:

let
  progCfg = config.programs.todo;
  svcCfg = config.services.todo-sync;
  inherit (lib) mkEnableOption mkOption mkIf types literalExpression;
in
{
  options.programs.todo = {
    enable = mkEnableOption "todo CLI";

    package = mkOption {
      type = types.package;
      default = self.packages.${pkgs.system}.todo;
      defaultText = literalExpression "self.packages.\${pkgs.system}.todo";
      description = "The todo package to use.";
    };
  };

  options.services.todo-sync = {
    enable = mkEnableOption "todo-sync daemon";

    configFile = mkOption {
      type = types.nullOr types.path;
      default = null;
      description = ''
        Path to the todo config file. When null, the CLI default
        (~/.todo.yaml) is used.
      '';
    };
  };

  config = lib.mkMerge [
    (mkIf progCfg.enable {
      home.packages = [ progCfg.package ];
    })

    (mkIf svcCfg.enable {
      assertions = [
        {
          assertion = progCfg.enable;
          message = "services.todo-sync requires programs.todo.enable = true";
        }
      ];

      systemd.user.services.todo-sync = {
        Unit = {
          Description = "Todo sync daemon";
          After = [ "network-online.target" ];
        };

        Service = {
          ExecStart =
            if svcCfg.configFile != null
            then "${progCfg.package}/bin/todo --config ${svcCfg.configFile} sync daemon"
            else "${progCfg.package}/bin/todo sync daemon";
          Restart = "on-failure";
          RestartSec = 10;
        };

        Install = {
          WantedBy = [ "default.target" ];
        };
      };
    })
  ];
}
