echo "build launcher ..."
cl /Fe:calmNoConsole src\calm.c /link /SUBSYSTEM:WINDOWS /ENTRY:mainCRTStartup
cl /Fe:calm src\calm.c /link /SUBSYSTEM:CONSOLE
echo "executing build.sh in MSYS2 ..."
C:\msys64\msys2_shell.cmd -defterm -here -no-start -mingw64 -c 'bash ./build/build.sh'
