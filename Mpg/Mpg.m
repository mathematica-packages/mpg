BeginPackage["Mpg`"];

MpgInstall::usage = "Installs a package from the Mpg catalog."
MpgUninstall::usage = "Uninstalls an Mpg package."
MpgInstalled::usage = "Lists Mpg packages that are currently installed."
MpgAvailable::usage = "Lists Mpg packages that are available to install."
MpgUpdate::usage = "Updates installed packages."

Begin["`Private`"];

Unprotect[MpgInstall,MpgUninstall,MpgInstalled,MpgAvailable,MpgUpdate]

Protect[MpgInstall,MpgUninstall,MpgInstalled,MpgAvailable,MpgUpdate]

End[]

EndPackage[]
