param([string] $messagingDataVersion = "")

. ./ClmFunctions.ps1
UninstallMessagingService -messagingDataVersion $messagingDataVersion
