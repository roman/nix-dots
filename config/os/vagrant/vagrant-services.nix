# This file is overwritten by vagrant. See
# https://github.com/mitchellh/vagrant/blob/master/templates/guests/nixos/network.erb
{

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable DBus
  services.dbus.enable = true;

  # Replace ntpd by timesyncd
  services.timesyncd.enable = true;

  security.sudo.extraConfig =
    ''
      Defaults:root,%wheel env_keep+=LOCALE_ARCHIVE
      Defaults:root,%wheel env_keep+=NIX_PATH
      Defaults:root,%wheel env_keep+=TERMINFO_DIRS
      Defaults env_keep+=SSH_AUTH_SOCK
      Defaults lecture = never
      root   ALL=(ALL) SETENV: ALL
      %wheel ALL=(ALL) NOPASSWD: ALL, SETENV: ALL
    '';

  virtualisation.virtualbox.guest.enable = true;
}
