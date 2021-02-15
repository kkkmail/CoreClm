param([string] $messagingDataVersion = "")

. ./ClmFunctions.ps1
UninstallContGenService -messagingDataVersion $messagingDataVersion
