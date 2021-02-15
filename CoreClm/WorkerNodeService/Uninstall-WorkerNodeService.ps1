param([string] $messagingDataVersion = "")

. ./ClmFunctions.ps1
UninstallWorkerNodeService -messagingDataVersion $messagingDataVersion
