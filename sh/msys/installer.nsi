;NSIS Modern User Interface
;Basic Example Script
;Written by Joost Verburg

;--------------------------------
;Include Modern UI

  !include "MUI2.nsh"

;--------------------------------
;General

  ;Name and file
  Name "__APP_NAME__"
  OutFile "__APP_NAME__-Installer.exe"
  Unicode True

  ;Default installation folder
  InstallDir "$LOCALAPPDATA\__APP_NAME__"

  InstallColors /windows

  ;Request application privileges for Windows Vista
  RequestExecutionLevel user

;--------------------------------
;Interface Settings
  !define MUI_ICON "calm-install-root-assets\app-installer.ico"
  !define MUI_UNICON "calm-install-root-assets\app-uninstaller.ico"

  !define MUI_ABORTWARNING

  !define MUI_HEADERIMAGE
  !define MUI_HEADERIMAGE_BITMAP calm-install-root-assets\installer-header.bmp
  !define MUI_WELCOMEFINISHPAGE_BITMAP calm-install-root-assets\installer-page.bmp

;--------------------------------
;Pages

  !insertmacro MUI_PAGE_DIRECTORY
  !insertmacro MUI_PAGE_INSTFILES

  !define MUI_FINISHPAGE_RUN
  !define MUI_FINISHPAGE_RUN_FUNCTION "LaunchLink"
  Function LaunchLink
    ExecShell "" "$DESKTOP\__APP_NAME__.lnk"
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

  File /r calm-install-root

  WriteUninstaller "$INSTDIR\Uninstall.exe"

  ; Create application shortcut (first in calm-install-root to have the correct "start in" target)
  SetOutPath "$INSTDIR\calm-install-root\"
  CreateShortCut "$INSTDIR\calm-install-root\__APP_NAME__.lnk" "$INSTDIR\calm-install-root\calm.exe"
  SetOutPath "$INSTDIR\"
  CreateShortCut "$INSTDIR\uninstall-__APP_NAME__.lnk" "$INSTDIR\Uninstall.exe"

  ; Start menu entries
  SetOutPath "$SMPROGRAMS\__APP_NAME__\"
  CopyFiles "$INSTDIR\calm-install-root\__APP_NAME__.lnk" "$SMPROGRAMS\__APP_NAME__\"
  CopyFiles "$INSTDIR\calm-install-root\__APP_NAME__.lnk" "$DESKTOP\"
  CopyFiles "$INSTDIR\uninstall-__APP_NAME__.lnk" "$SMPROGRAMS\__APP_NAME__\"

  Delete "$INSTDIR\calm-install-root\__APP_NAME__.lnk"
  Delete "$INSTDIR\uninstall-__APP_NAME__.lnk"

  WriteRegStr HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\__APP_NAME__" "DisplayName" "__APP_NAME__"
  WriteRegStr HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\__APP_NAME__" "UninstallString" "$INSTDIR\Uninstall.exe"
  WriteRegStr HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\__APP_NAME__" "DisplayIcon" "$INSTDIR\calm-install-root\calm.exe"

SectionEnd

;--------------------------------
;Uninstaller Section

Section "Uninstall"

  Delete "$SMPROGRAMS\__APP_NAME__\*.*"
  RMDir "$SMPROGRAMS\__APP_NAME__\"
  Delete "$DESKTOP\__APP_NAME__.lnk"

  Delete "$INSTDIR\Uninstall.exe"
  RMDir /r "$INSTDIR\calm-install-root"
  RMDir "$INSTDIR"

  DeleteRegKey HKCU "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\__APP_NAME__"

SectionEnd