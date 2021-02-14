param([string] $messagingDataVersion = "", [string] $versionNumber = "")

. ./ClmFunctions.ps1
InstallContGenService -messagingDataVersion $messagingDataVersion -versionNumber $versionNumber
