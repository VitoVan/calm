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

if (-not(Test-Path ".\cdk\bin\sbcl.exe")) {
  Write-Output "CDK not ready, please build with:"
  Write-Output "sbcl --load scripts/build-cdk.lisp"
  Exit 42
}

$Env:PATH += ";$DIR\cdk\lib\calm"

$env:SBCL_HOME = "$DIR\cdk\lib\sbcl"

$CORE = "$env:SBCL_HOME\sbcl.core"

& .\cdk\bin\sbcl.exe --core $CORE --userinit .sbclrc $Args
