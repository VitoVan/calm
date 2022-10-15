# this script is supposed to be executed on Windows via PowerShell
# before running, you may need to set your Execution Policies:
#
# Set-ExecutionPolicy Bypass -Scope Process -Force
#
# ref: https:/go.microsoft.com/fwlink/?LinkID=135170
#

# https://stackoverflow.com/questions/8343767/how-to-get-the-current-directory-of-the-cmdlet-being-executed
$SOURCE = (Get-Variable MyInvocation).Value.MyCommand.Path
$DIR = Split-Path $SOURCE

cd "$DIR"

$Env:PATH += ";$DIR\cdk\lib\calm"

$env:SBCL_HOME = "$DIR\cdk\lib\sbcl"

$CORE = "$env:SBCL_HOME\sbcl.core"

if (-not(Test-Path ".\launcher.exe")) {
       .\sbcl.ps1 --load launcher.lisp
}

& .\launcher.exe $Args
