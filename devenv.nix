{
  lib,
  pkgs,
  ...
}:

{
  packages = with pkgs; [
    nixd
  ];

  languages = {
    rust.enable = true;
    rust.channel = "stable";
  };

  process.manager.implementation = "overmind";

  enterShell = lib.mkForce "";
}
