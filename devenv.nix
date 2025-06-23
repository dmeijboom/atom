{
  lib,
  pkgs,
  ...
}:

{
  packages = with pkgs; [
    nixd
    hyperfine
    samply
  ];

  languages = {
    javascript.enable = true;
    javascript.npm.enable = true;
    rust.enable = true;
    rust.channel = "stable";
  };

  process.manager.implementation = "overmind";

  enterShell = lib.mkForce "";
}
