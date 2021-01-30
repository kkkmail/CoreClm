cls

echo "Terminating known orphanged VB / Rider processes..."

$msb = Get-Process -Name "MSBuild" -ea silentlycontinue
if ($msb) { Stop-Process -InputObject $msb -force }
Get-Process | Where-Object {$_.HasExited}

$vbcsc = Get-Process -Name "VBCSCompiler" -ea silentlycontinue
if ($vbcsc) { Stop-Process -InputObject $vbcsc -force }
Get-Process | Where-Object {$_.HasExited}

$w3wp = Get-Process -Name "w3wp" -ea silentlycontinue
if ($w3wp) { Stop-Process -InputObject $w3wp -force }
Get-Process | Where-Object {$_.HasExited}

if ((!$msb) -and (!$vbcsc) -and (!$w3wp)) { echo "No known orphanded processes found!" }
                                          

echo "Deleting all bin and obj content..."

Remove-Item -path .\Analytics\bin -recurse -force -ea silentlycontinue
Remove-Item -path .\Clm\bin -recurse -force -ea silentlycontinue
Remove-Item -path .\ClmDefaults\bin -recurse -force -ea silentlycontinue
Remove-Item -path .\ClmGenerator\bin -recurse -force -ea silentlycontinue
Remove-Item -path .\ClmImpure\bin -recurse -force -ea silentlycontinue
Remove-Item -path .\ClmSys\bin -recurse -force -ea silentlycontinue
Remove-Item -path .\ClmTests\bin -recurse -force -ea silentlycontinue
Remove-Item -path .\ContGen\bin -recurse -force -ea silentlycontinue
Remove-Item -path .\ContGenAdm\bin -recurse -force -ea silentlycontinue
Remove-Item -path .\ContGenService\bin -recurse -force -ea silentlycontinue
Remove-Item -path .\ContGenServiceInfo\bin -recurse -force -ea silentlycontinue
Remove-Item -path .\DbData\bin -recurse -force -ea silentlycontinue
Remove-Item -path .\Messaging\bin -recurse -force -ea silentlycontinue
Remove-Item -path .\MessagingAdm\bin -recurse -force -ea silentlycontinue
Remove-Item -path .\MessagingService\bin -recurse -force -ea silentlycontinue
Remove-Item -path .\MessagingServiceInfo\bin -recurse -force -ea silentlycontinue
Remove-Item -path .\Model\bin -recurse -force -ea silentlycontinue
Remove-Item -path .\NoSql\bin -recurse -force -ea silentlycontinue
Remove-Item -path .\OdeSolver\bin -recurse -force -ea silentlycontinue
Remove-Item -path .\ServiceProxy\bin -recurse -force -ea silentlycontinue
Remove-Item -path .\SolverRunner\bin -recurse -force -ea silentlycontinue
Remove-Item -path .\WorkerNodeAdm\bin -recurse -force -ea silentlycontinue
Remove-Item -path .\WorkerNodeService\bin -recurse -force -ea silentlycontinue
Remove-Item -path .\WorkerNodeServiceInfo\bin -recurse -force -ea silentlycontinue

Remove-Item -path .\Analytics\obj -recurse -force -ea silentlycontinue
Remove-Item -path .\Clm\obj -recurse -force -ea silentlycontinue
Remove-Item -path .\ClmDefaults\obj -recurse -force -ea silentlycontinue
Remove-Item -path .\ClmGenerator\obj -recurse -force -ea silentlycontinue
Remove-Item -path .\ClmImpure\obj -recurse -force -ea silentlycontinue
Remove-Item -path .\ClmSys\obj -recurse -force -ea silentlycontinue
Remove-Item -path .\ClmTests\obj -recurse -force -ea silentlycontinue
Remove-Item -path .\ContGen\obj -recurse -force -ea silentlycontinue
Remove-Item -path .\ContGenAdm\obj -recurse -force -ea silentlycontinue
Remove-Item -path .\ContGenService\obj -recurse -force -ea silentlycontinue
Remove-Item -path .\ContGenServiceInfo\obj -recurse -force -ea silentlycontinue
Remove-Item -path .\DbData\obj -recurse -force -ea silentlycontinue
Remove-Item -path .\Messaging\obj -recurse -force -ea silentlycontinue
Remove-Item -path .\MessagingAdm\obj -recurse -force -ea silentlycontinue
Remove-Item -path .\MessagingService\obj -recurse -force -ea silentlycontinue
Remove-Item -path .\MessagingServiceInfo\obj -recurse -force -ea silentlycontinue
Remove-Item -path .\Model\obj -recurse -force -ea silentlycontinue
Remove-Item -path .\NoSql\obj -recurse -force -ea silentlycontinue
Remove-Item -path .\OdeSolver\obj -recurse -force -ea silentlycontinue
Remove-Item -path .\ServiceProxy\obj -recurse -force -ea silentlycontinue
Remove-Item -path .\SolverRunner\obj -recurse -force -ea silentlycontinue
Remove-Item -path .\WorkerNodeAdm\obj -recurse -force -ea silentlycontinue
Remove-Item -path .\WorkerNodeService\obj -recurse -force -ea silentlycontinue
Remove-Item -path .\WorkerNodeServiceInfo\obj -recurse -force -ea silentlycontinue


echo "Deleting all garbage from user Temp folder..."
Remove-Item -path $env:userprofile\AppData\Local\Temp -recurse -force -ea silentlycontinue
