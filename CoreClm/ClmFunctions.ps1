. ./VersionInfo.ps1

[string] $global:messagingServiceName = "MessagingService"
[string] $global:workerNodeServiceName = "WorkerNodeService"
[string] $global:contGenServiceName = "ContGenService"

function Clean-All()
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
function TryStop-Service([string] $serviceName)
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

function Uninstall-Service([string] $serviceName)
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

# https://stackoverflow.com/questions/14708825/how-to-create-a-windows-service-in-powershell-for-network-service-account
function Reinstall-Service ([string] $serviceName, [string] $binaryPath, [string] $description = "", [string] $login = "NT AUTHORITY\NETWORK SERVICE", [string] $password = "", [string] $startUpType = "Automatic")
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
        TryStop-Service -serviceName $serviceName

        # using WMI to remove Windows service because PowerShell does not have CmdLet for this
        $serviceToRemove = Get-WmiObject -Class Win32_Service -Filter "name='$serviceName'"

        $serviceToRemove.delete()
        Write-Host "Service removed: $serviceName"
    }

    # if password is empty, create a dummy one to allow have credentias for system accounts:
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

    # Creating Windows Service using all provided parameters
    Write-Host "Installing service: $serviceName"
    New-Service -name $serviceName -binaryPathName $binaryPath -Description $description -displayName $serviceName -startupType $startUpType -credential $mycreds

    Write-Host "Installation completed: $serviceName"

    # Trying to start new service
    Write-Host "Trying to start new service: $serviceName"
    $serviceToStart = Get-WmiObject -Class Win32_Service -Filter "name='$serviceName'"
    $serviceToStart.startservice()
    Write-Host "Service started: $serviceName"

    #SmokeTest
    Write-Host "Waiting 5 seconds to give time service to start..."
    Start-Sleep -s 5
    $SmokeTestService = Get-Service -Name $serviceName
    if ($SmokeTestService.Status -ne "Running")
    {
        Write-Host "Smoke test: FAILED. (SERVICE FAILED TO START)"
        Throw "Smoke test: FAILED. (SERVICE FAILED TO START)"
    }
    else
    {
        Write-Host "Smoke test: OK."
    }
}


function Get-ServiceName ([string] $serviceName, [string] $messagingDataVersion)
{
    return "$serviceName-$messagingDataVersion"
}

function Install-MessagingService([string] $messagingDataVersion = "",  [string] $versionNumber = "")
{
    if ($messagingDataVersion -eq "")
    {
        $messagingDataVersion = $global:messagingDataVersion
    }

    if ($versionNumber -eq "")
    {
        $versionNumber = $global:versionNumber
    }

    [string] $serviceName = Get-ServiceName -serviceName $global:messagingServiceName -messagingDataVersion $messagingDataVersion
    [string] $binaryPath = "$global:messagingServiceName.exe"
    [string] $description = "$global:messagingServiceName, version $versionNumber.$messagingDataVersion"

    Reinstall-Service -serviceName $serviceName -binaryPath $binaryPath -description $description
}
