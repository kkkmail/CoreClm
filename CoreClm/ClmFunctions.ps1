. ./VersionInfo.ps1

[string] $global:messagingServiceName = "MessagingService"
[string] $global:workerNodeServiceName = "WorkerNodeService"
[string] $global:contGenServiceName = "ContGenService"
[string] $global:solverRunnerName = "SolverRunner"

function PrintToConsole($s)
{
    Write-Information -MessageData $s -InformationAction Continue
}

function CleanAll()
{
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
    Remove-Item -path ..\FSharp.Data.SqlClient\src\SqlClient.DesignTime\bin -recurse -force -ea silentlycontinue
    Remove-Item -path ..\FSharp.Data.SqlClient\src\SqlClient\bin -recurse -force -ea silentlycontinue
    Remove-Item -path ..\FSharp.Data.SqlClient\bin -recurse -force -ea silentlycontinue

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
    Remove-Item -path ..\FSharp.Data.SqlClient\src\SqlClient.DesignTime\obj -recurse -force -ea silentlycontinue
    Remove-Item -path ..\FSharp.Data.SqlClient\src\SqlClient\obj -recurse -force -ea silentlycontinue

    echo "Deleting all garbage from user Temp folder..."
    Remove-Item -path $env:userprofile\AppData\Local\Temp -recurse -force -ea silentlycontinue
}

# https://stackoverflow.com/questions/35064964/powershell-script-to-check-if-service-is-started-if-not-then-start-it
function TryStopService([string] $serviceName)
{
    Write-Host "Attempting to stop service: $serviceName..."
    $service = Get-Service -Name $serviceName -ErrorAction SilentlyContinue

    if($service)
    {
        if ($service.Status -ne 'Running')
        {
            Write-Host "    Service: $serviceName is not running."
        }
        else
        {
            Stop-Service -name $serviceName
            Write-Host "    Stopped service: $serviceName."
        }
    }
    else
    {
        Write-Host "    Service: $serviceName is not found."
    }
}

function UninstallService([string] $serviceName)
{
    Write-Host "Attempting to uninstall service: $serviceName..."
    if (Get-Service $serviceName -ErrorAction SilentlyContinue)
    {
        Remove-Service -Name $serviceName
        Write-Host "    Uninstalled service: $serviceName."
    }
    else
    {
        Write-Host "    Service: $serviceName is not found."
    }
}

function StartSertice([string] $serviceName)
{
    Write-Host "Attempting to start service: $serviceName..."
    $service = Get-Service -Name $serviceName -ErrorAction SilentlyContinue

    if($service)
    {
        if ($service.Status -eq 'Running')
        {
            Write-Host "    Service: $serviceName is already running."
            return
        }
    }

    # Trying to start new service.
    Write-Host "    Trying to start new service: $serviceName."
    Start-Service -Name $serviceName

    #Check that service has started.
    Write-Host "    Waiting 5 seconds to give service time to start..."
    Start-Sleep -s 5
    $testService = Get-Service -Name $serviceName

    if ($testService.Status -ne "Running")
    {
        [string] $errMessage = "    Failed to start service: $serviceName"
        Write-Host $errMessage
        Throw $errMessage
    }
    else
    {
        Write-Host "    Started service: $serviceName."
    }
}

# https://stackoverflow.com/questions/14708825/how-to-create-a-windows-service-in-powershell-for-network-service-account
function ReinstallService ([string] $serviceName, [string] $binaryPath, [string] $description = "", [string] $login = "NT AUTHORITY\LOCAL SERVICE", [string] $password = "", [string] $startUpType = "Automatic")
{
    Write-Host "Attempting to reinstall service: $serviceName..."

    #Check Parameters
    if ((Test-Path $binaryPath)-eq $false)
    {
        Write-Host "    BinaryPath to service was not found: $binaryPath."
        Write-Host "    Service was NOT installed."
        return
    }

    if (("Automatic", "Manual", "Disabled") -notcontains $startUpType)
    {
        Write-Host "    Value for startUpType parameter should be (Automatic or Manual or Disabled) but it was $startUpType."
        Write-Host "    Service was NOT installed."
        return
    }

    TryStopService -serviceName $serviceName
    UninstallService -serviceName $serviceName

    # if password is empty, create a dummy one to allow having credentias for system accounts:
    #     NT AUTHORITY\LOCAL SERVICE
    #     NT AUTHORITY\NETWORK SERVICE
    if ($password -eq "")
    {
        $secpassword = (new-object System.Security.SecureString)
    }
    else
    {
        $secpassword = ConvertTo-SecureString $password -AsPlainText -Force
    }

    $mycreds = New-Object System.Management.Automation.PSCredential ($login, $secpassword)

    # Creating Windows Service using all provided parameters.
    Write-Host "Installing service: $serviceName with user name: '$login'..."
    New-Service -name $serviceName -binaryPathName $binaryPath -Description $description -displayName $serviceName -startupType $startUpType -credential $mycreds
    Write-Host "    Installed service: $serviceName."

    # Trying to start new service.
    StartSertice -serviceName $serviceName
}

function GetValueOrDefault([string] $value, [string] $messagingDataVersion, [string] $defaultValue)
{
    if ($value -eq "")
    {
        $value = $defaultValue
    }

    return $value
}

function GetServiceName ([string] $serviceName, [string] $messagingDataVersion = "")
{
    $messagingDataVersion = GetValueOrDefault -value $messagingDataVersion -defaultValue $global:messagingDataVersion
    return "$serviceName-$messagingDataVersion"
}

function GetBinaryPathName ([string] $serviceName)
{
    [string] $folderName = Get-Location
    return "$folderName\$serviceName.exe"
}

function GetDescription([string] $serviceName, [string] $messagingDataVersion, [string] $versionNumber)
{
    $messagingDataVersion = GetValueOrDefault -value $messagingDataVersion -defaultValue $global:messagingDataVersion
    [string] $description = "$serviceName, version $versionNumber.$messagingDataVersion"
    return $description
}

function InstallSvc([string] $serviceName, [string] $messagingDataVersion = "",  [string] $versionNumber = "")
{
    $versionNumber = GetValueOrDefault -value $versionNumber -defaultValue $global:versionNumber
    [string] $windowsServiceName = GetServiceName -serviceName $serviceName -messagingDataVersion $messagingDataVersion
    [string] $binaryPath = GetBinaryPathName -serviceName $serviceName
    [string] $description = GetDescription -serviceName $serviceName -versionNumber $versionNumber -messagingDataVersion $messagingDataVersion
    ReinstallService -serviceName $windowsServiceName -binaryPath $binaryPath -description $description
}

function UninstallSvc([string] $serviceName, [string] $messagingDataVersion = "")
{
    [string] $windowsServiceName = GetServiceName -serviceName $serviceName -messagingDataVersion $messagingDataVersion
    TryStopService -serviceName $windowsServiceName
    UninstallService -serviceName $windowsServiceName
}

function StartSvc([string] $serviceName, [string] $messagingDataVersion = "")
{
    [string] $windowsServiceName = GetServiceName -serviceName $serviceName -messagingDataVersion $messagingDataVersion
    StartSertice -serviceName $windowsServiceName
}

function StopSvc([string] $serviceName, [string] $messagingDataVersion = "")
{
    [string] $windowsServiceName = GetServiceName -serviceName $serviceName -messagingDataVersion $messagingDataVersion
    TryStopService -serviceName $windowsServiceName
}

#===================================================================================================

function InstallMessagingService([string] $messagingDataVersion = "",  [string] $versionNumber = "")
{
    InstallSvc -serviceName $global:messagingServiceName -messagingDataVersion $messagingDataVersion -versionNumber $versionNumber
}

function UninstallMessagingService([string] $messagingDataVersion = "")
{
    UninstallSvc -serviceName $global:messagingServiceName -messagingDataVersion $messagingDataVersion
}

function StartMessagingService([string] $messagingDataVersion = "")
{
    StartSvc -serviceName $global:messagingServiceName -messagingDataVersion $messagingDataVersion
}

function StopMessagingService([string] $messagingDataVersion = "")
{
    StopSvc -serviceName $global:messagingServiceName -messagingDataVersion $messagingDataVersion
}

#===================================================================================================

function InstallWorkerNodeService([string] $messagingDataVersion = "",  [string] $versionNumber = "")
{
    InstallSvc -serviceName $global:workerNodeServiceName -messagingDataVersion $messagingDataVersion -versionNumber $versionNumber
}

function UninstallWorkerNodeService([string] $messagingDataVersion = "")
{
    UninstallSvc -serviceName $global:workerNodeServiceName -messagingDataVersion $messagingDataVersion
}

function StartWorkerNodeService([string] $messagingDataVersion = "")
{
    StartSvc -serviceName $global:workerNodeServiceName -messagingDataVersion $messagingDataVersion
}

function StopWorkerNodeService([string] $messagingDataVersion = "")
{
    StopSvc -serviceName $global:workerNodeServiceName -messagingDataVersion $messagingDataVersion
}

#===================================================================================================

function InstallContGenService([string] $messagingDataVersion = "",  [string] $versionNumber = "")
{
    InstallSvc -serviceName $global:contGenServiceName -messagingDataVersion $messagingDataVersion -versionNumber $versionNumber
}

function UninstallContGenService([string] $messagingDataVersion = "")
{
    UninstallSvc -serviceName $global:contGenServiceName -messagingDataVersion $messagingDataVersion
}

function StartContGenService([string] $messagingDataVersion = "")
{
    StartSvc -serviceName $global:contGenServiceName -messagingDataVersion $messagingDataVersion
}

function StopContGenService([string] $messagingDataVersion = "")
{
    StopSvc -serviceName $global:contGenServiceName -messagingDataVersion $messagingDataVersion
}

#===================================================================================================

function StopSolvers()
{
    PrintToConsole "Terminating ALL $global:solverRunnerName processes..."

    $slv = Get-Process -Name "$global:solverRunnerName" -ea silentlycontinue
    if ($slv)
    {
        Stop-Process -InputObject $slv -force
    }
    Get-Process | Where-Object { $_.HasExited }

    if (!$slv)
    {
        PrintToConsole "No $global:solverRunnerName processes found!"
    }
}
