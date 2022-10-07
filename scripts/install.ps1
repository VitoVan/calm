Write-Output "CALM (Windows) Installer v0.0.1"

$testchoco = powershell choco -v
if (-not($testchoco)) {
  Write-Output "Seems Chocolatey is not installed, installing now ..."
  Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; iex ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))
} else {
  Write-Output "Chocolatey Version $testchoco is already installed."
}

cinst msys2 --params "/InstallDir:C:\msys64"

setx /M PATH "%PATH%;C:\msys64\usr\bin"
refreshenv

# Test pacman
C:\msys64\msys2_shell.cmd -defterm -here -no-start -mingw64 -c 'which pacman'

$testsbcl = powershell sbcl --version
if (-not($testsbcl)) {
  Write-Output "Seems SBCL is not installed, installing now ..."
  cinst sbcl
} else {
  Write-Output "SBCL Version $testsbcl is already installed."
}

refreshenv

Write-Output "Installing CALM ..."

if ((Test-Path ".\scripts\install.sh") -eq $true) {
  Write-Output "Using existing file install.sh to proceed installation."
  C:\msys64\msys2_shell.cmd -defterm -here -no-start -mingw64 -c './scripts/install.sh'
}
else {
  Write-Output "Downloading CALM Installer ..."
  (New-Object System.Net.WebClient).DownloadFile('https://raw.githubusercontent.com/VitoVan/calm/main/scripts/install.sh', 'install-calm.sh')
  C:\msys64\msys2_shell.cmd -defterm -here -no-start -mingw64 -c './install-calm.sh'
}
