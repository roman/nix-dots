flakeInputs:
{
  bash = import ./bash flakeInputs;
  git =  import ./git flakeInputs;
  emacs = import ./emacs flakeInputs;
  nix-utils = import ./nix-utils flakeInputs;
  ui = import ./ui flakeInputs;
}
