flake-inputs:
{
  description     = "Jedi Knight";
  name            = "anakin";
  group           = "users";
  extraGroups     = [ "users" "jedi" "wheel" "docker" ];
  password        = "j3d1sSuck";
  home            = "/home/anakin";
  createHome      = true;
  useDefaultShell = true;
  profiles = [ "git" ];
}
