flakeInputs:
{
  bash = import ./bash flakeInputs;
  git =  import ./git flakeInputs;
  emacs = import ./emacs flakeInputs;
  docker = import ./docker flakeInputs;
  nix-utils = import ./nix-utils flakeInputs;
}
