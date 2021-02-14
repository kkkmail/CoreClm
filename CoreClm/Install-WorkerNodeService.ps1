param([string] $messagingDataVersion = "", [string] $versionNumber = "")

. ./ClmFunctions.ps1
InstallWorkerNodeService -messagingDataVersion $messagingDataVersion -versionNumber $versionNumber
