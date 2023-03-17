;NSIS Modern User Interface
;Basic Example Script
;Written by Joost Verburg

;--------------------------------
;Include Modern UI

  !include "MUI2.nsh"

;--------------------------------
;General

  ;Name and file
  Name "_APP_NAME_"
  OutFile "_APP_NAME_-Installer.exe"
  Unicode True

  ;Default installation folder
  InstallDir "$LOCALAPPDATA\_APP_NAME_"

  InstallColors /windows

  ;Request application privileges for Windows Vista
  RequestExecutionLevel user

;--------------------------------
;Interface Settings
  !define MUI_ICON "calm-installer-assets\app-installer.ico"
  !define MUI_UNICON "calm-installer-assets\app-uninstaller.ico"

  !define MUI_ABORTWARNING

  !define MUI_HEADERIMAGE
  !define MUI_HEADERIMAGE_BITMAP calm-installer-assets\installer-header.bmp
  !define MUI_WELCOMEFINISHPAGE_BITMAP calm-installer-assets\installer-page.bmp

;--------------------------------
;Pages

  !insertmacro MUI_PAGE_DIRECTORY
  !insertmacro MUI_PAGE_INSTFILES

  !define MUI_FINISHPAGE_RUN
  !define MUI_FINISHPAGE_RUN_FUNCTION "LaunchLink"
  Function LaunchLink
    ExecShell "" "$DESKTOP\_APP_NAME_.lnk"
  FunctionEnd

  !insertmacro MUI_PAGE_FINISH

  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES

;--------------------------------
;Languages

  !insertmacro MUI_LANGUAGE "English"

;--------------------------------
;Installer Sections

Section "Installer Section"

  SetOutPath "$INSTDIR"

  File /r calm-app-root

  WriteUninstaller "$INSTDIR\Uninstall.exe"

  ; Create application shortcut (first in calm-app-root to have the correct "start in" target)
  SetOutPath "$INSTDIR\calm-app-root\"
  CreateShortCut "$INSTDIR\calm-app-root\_APP_NAME_.lnk" "$INSTDIR\calm-app-root\calm.exe"
  SetOutPath "$INSTDIR\"
  CreateShortCut "$INSTDIR\uninstall-_APP_NAME_.lnk" "$INSTDIR\Uninstall.exe"

  ; Start menu entries
  SetOutPath "$SMPROGRAMS\_APP_NAME_\"
  CopyFiles "$INSTDIR\calm-app-root\_APP_NAME_.lnk" "$SMPROGRAMS\_APP_NAME_\"
  CopyFiles "$INSTDIR\calm-app-root\_APP_NAME_.lnk" "$DESKTOP\"
  CopyFiles "$INSTDIR\uninstall-_APP_NAME_.lnk" "$SMPROGRAMS\_APP_NAME_\"

  Delete "$INSTDIR\calm-app-root\_APP_NAME_.lnk"
  Delete "$INSTDIR\uninstall-_APP_NAME_.lnk"

  WriteRegStr HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\_APP_NAME_" "DisplayName" "_APP_NAME_"
  WriteRegStr HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\_APP_NAME_" "UninstallString" "$INSTDIR\Uninstall.exe"
  WriteRegStr HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\_APP_NAME_" "DisplayIcon" "$INSTDIR\calm-app-root\calm.exe"

SectionEnd

;--------------------------------
;Uninstaller Section

Section "Uninstall"

  Delete "$SMPROGRAMS\_APP_NAME_\*.*"
  RMDir "$SMPROGRAMS\_APP_NAME_\"
  Delete "$DESKTOP\_APP_NAME_.lnk"

  Delete "$INSTDIR\Uninstall.exe"
  RMDir /r "$INSTDIR\calm-app-root"
  RMDir "$INSTDIR"

  DeleteRegKey HKCU "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\_APP_NAME_"

SectionEnd