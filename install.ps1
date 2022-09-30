$testchoco = powershell choco -v
if (-not($testchoco)) {
  Write-Output "Seems Chocolatey is not installed, installing now ..."
  Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; iex ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))
} else {
  Write-Output "Chocolatey Version $testchoco is already installed."
}

cinst msys2 --params "/InstallDir:C:\calm-msys64"

setx /M PATH "%PATH%;C:\calm-msys64\usr\bin"
refreshenv

# Run for the first time
C:\calm-msys64\msys2_shell.cmd -defterm -here -no-start -mingw64 -c 'which pacman'

$testsbcl = powershell sbcl --version
if (-not($testsbcl)) {
  Write-Output "Seems SBCL is not installed, installing now ..."
  cinst sbcl
} else {
  Write-Output "SBCL Version $testsbcl is already installed."
}

refreshenv

Write-Output "Installing CALM ..."

C:\calm-msys64\msys2_shell.cmd -defterm -here -no-start -mingw64 -c './install.sh'
