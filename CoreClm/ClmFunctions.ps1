. ./VersionInfo.ps1

[string] $global:messagingServiceName = "MessagingService"
[string] $global:workerNodeServiceName = "WorkerNodeService"
[string] $global:contGenServiceName = "ContGenService"

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
}

# https://stackoverflow.com/questions/35064964/powershell-script-to-check-if-service-is-started-if-not-then-start-it
function TryStopService([string] $serviceName)
{
    $service = Get-Service -Name $serviceName -ErrorAction SilentlyContinue

    if($service)
    {
        if ($service.Status -ne 'Running')
        {
            Write-Host "Service: $serviceName is not running."
        }
        else
        {
            Write-Host "Attempting to stop service: $serviceName..."
            Stop-Service -name $serviceName
        }
    }
    else
    {
        Write-Host "Service: $serviceName is not found."
    }
}

function UninstallService([string] $serviceName)
{
    if (Get-Service $serviceName -ErrorAction SilentlyContinue)
    {
        # using WMI to remove Windows service because PowerShell does not have CmdLet for this
        $serviceToRemove = Get-WmiObject -Class Win32_Service -Filter "name='$serviceName'"

        $serviceToRemove.delete()
        Write-Host "Service removed: $serviceName"
    }
    else
    {
        Write-Host "Service: $serviceName does not exist!"
    }
}

function StartSertice([string] $serviceName)
{
    # Trying to start new service.
    Write-Host "Trying to start new service: $serviceName"
    $serviceToStart = Get-WmiObject -Class Win32_Service -Filter "name='$serviceName'"
    $serviceToStart.startservice()
    Write-Host "Service started: $serviceName"

    #Check that service has started.
    Write-Host "Waiting 5 seconds to give service time to start..."
    Start-Sleep -s 5
    $testService = Get-Service -Name $serviceName

    if ($testService.Status -ne "Running")
    {
        [string] $errMessage = "Failed to start service: $serviceName"
        Write-Host $errMessage
        Throw $errMessage
    }
    else
    {
        Write-Host "Service: $serviceName started successfully."
    }
}

# https://stackoverflow.com/questions/14708825/how-to-create-a-windows-service-in-powershell-for-network-service-account
function ReinstallService ([string] $serviceName, [string] $binaryPath, [string] $description = "", [string] $login = "NT AUTHORITY\NETWORK SERVICE", [string] $password = "", [string] $startUpType = "Automatic")
{
    Write-Host "Trying to create service: $serviceName"

    #Check Parameters
    if ((Test-Path $binaryPath)-eq $false)
    {
        Write-Host "BinaryPath to service was not found: $binaryPath"
        Write-Host "Service was NOT installed."
        return
    }

    if (("Automatic", "Manual", "Disabled") -notcontains $startUpType)
    {
        Write-Host "Value for startUpType parameter should be (Automatic or Manual or Disabled) and it was $startUpType"
        Write-Host "Service was NOT installed."
        return
    }

    # Verify if the service already exists, and if yes remove it first.
    if (Get-Service $serviceName -ErrorAction SilentlyContinue)
    {
        TryStopService -serviceName $serviceName

        # using WMI to remove Windows service because PowerShell does not have CmdLet for this.
        $serviceToRemove = Get-WmiObject -Class Win32_Service -Filter "name='$serviceName'"

        $serviceToRemove.delete()
        Write-Host "Service removed: $serviceName"
    }

    # if password is empty, create a dummy one to allow having credentias for system accounts:
    #NT AUTHORITY\LOCAL SERVICE
    #NT AUTHORITY\NETWORK SERVICE
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
    Write-Host "Installing service: $serviceName"
    New-Service -name $serviceName -binaryPathName $binaryPath -Description $description -displayName $serviceName -startupType $startUpType -credential $mycreds

    Write-Host "Installation completed: $serviceName"

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
    [string] $description = "$serviceName, version $versionNumber.$messagingDataVersion"
}

function Install([string] $serviceName, [string] $messagingDataVersion = "",  [string] $versionNumber = "")
{
    $versionNumber = GetValueOrDefault -value $versionNumber -defaultValue $global:versionNumber
    [string] $windowsServiceName = GetServiceName -serviceName $serviceName -messagingDataVersion $messagingDataVersion
    [string] $binaryPath = GetBinaryPathName -serviceName $serviceName
    [string] $description = GetDescription -serviceName $serviceName -versionNumber $versionNumber -messagingDataVersion $messagingDataVersion
    Reinstall-Service -serviceName $serviceName -binaryPath $binaryPath -description $description
}

function Uninstall([string] $serviceName, [string] $messagingDataVersion = "")
{
    [string] $windowsServiceName = GetServiceName -serviceName $serviceName -messagingDataVersion $messagingDataVersion
    TryStopService -serviceName $serviceName
    UninstallService -serviceName $serviceName
}

function Start([string] $serviceName, [string] $messagingDataVersion = "")
{
    [string] $windowsServiceName = GetServiceName -serviceName $serviceName -messagingDataVersion $messagingDataVersion
}

function Stop([string] $serviceName, [string] $messagingDataVersion = "")
{
    [string] $windowsServiceName = GetServiceName -serviceName $serviceName -messagingDataVersion $messagingDataVersion
    TryStopService -serviceName $serviceName
}

#===================================================================================================

function InstallMessagingService([string] $messagingDataVersion = "",  [string] $versionNumber = "")
{
    Install -serviceName $global:messagingServiceName -messagingDataVersion $messagingDataVersion -versionNumber $versionNumber
}

function UninstallMessagingService([string] $messagingDataVersion = "")
{
    Uninstall -serviceName $global:messagingServiceName -messagingDataVersion $messagingDataVersion
}

function StartMessagingService([string] $messagingDataVersion = "")
{
    Start -serviceName $global:messagingServiceName -messagingDataVersion $messagingDataVersion
}

function StopMessagingService([string] $messagingDataVersion = "")
{
    Stop -serviceName $global:messagingServiceName -messagingDataVersion $messagingDataVersion
}

#===================================================================================================

function InstallWorkerNodeService([string] $messagingDataVersion = "",  [string] $versionNumber = "")
{
    Install -serviceName $global:workerNodeServiceName -messagingDataVersion $messagingDataVersion -versionNumber $versionNumber
}

function UninstallWorkerNodeService([string] $messagingDataVersion = "")
{
    Uninstall -serviceName $global:workerNodeServiceName -messagingDataVersion $messagingDataVersion
}

function StartWorkerNodeService([string] $messagingDataVersion = "")
{
    Start -serviceName $global:workerNodeServiceName -messagingDataVersion $messagingDataVersion
}

function StopWorkerNodeService([string] $messagingDataVersion = "")
{
    Stop -serviceName $global:workerNodeServiceName -messagingDataVersion $messagingDataVersion
}

#===================================================================================================

function InstallContGenService([string] $messagingDataVersion = "",  [string] $versionNumber = "")
{
    Install -serviceName $global:contGenServiceName -messagingDataVersion $messagingDataVersion -versionNumber $versionNumber
}

function UninstallContGenService([string] $messagingDataVersion = "")
{
    Uninstall -serviceName $global:contGenServiceName -messagingDataVersion $messagingDataVersion
}

function StartContGenService([string] $messagingDataVersion = "")
{
    Start -serviceName $global:contGenServiceName -messagingDataVersion $messagingDataVersion
}

function StopContGenService([string] $messagingDataVersion = "")
{
    Stop -serviceName $global:contGenServiceName -messagingDataVersion $messagingDataVersion
}

#===================================================================================================
