param([string] $messagingDataVersion = "", [string] $versionNumber = "")

. ./ClmFunctions.ps1
InstallMessagingService -messagingDataVersion $messagingDataVersion -versionNumber $versionNumber
