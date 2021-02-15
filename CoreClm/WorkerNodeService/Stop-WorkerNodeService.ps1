param([string] $messagingDataVersion = "")

. ./ClmFunctions.ps1
StopWorkerNodeService -messagingDataVersion $messagingDataVersion
