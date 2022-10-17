# this script is supposed to be executed on Windows via PowerShell
# before running, you may need to set your Execution Policies:
#
# Set-ExecutionPolicy Bypass -Scope Process -Force
#
# ref: https:/go.microsoft.com/fwlink/?LinkID=135170
#

# https://stackoverflow.com/questions/8343767/how-to-get-the-current-directory-of-the-cmdlet-being-executed
$SOURCE = (Get-Variable MyInvocation).Value.MyCommand.Path
$Env:CALM_DIR = Split-Path $SOURCE

cd "$Env:CALM_DIR"

$Env:PATH += ";$Env:CALM_DIR\cdk\lib\calm"

$Env:SBCL_BIN = "$Env:CALM_DIR\cdk\lib\sbcl"
$Env:SBCL_HOME = "$Env:CALM_DIR\cdk\lib\sbcl"
$Env:SBCL_CORE = "$env:SBCL_HOME\sbcl.core"
$Env:SBCL_USERINIT = "$env:CALM_DIR\.sbclrc"

if (-not(Test-Path ".\launcher.exe")) {
   & "$SBCL_BIN" --core "$SBCL_CORE" --userinit "$SBCL_USERINIT" --load launcher.lisp
}

& .\launcher.exe $Args
