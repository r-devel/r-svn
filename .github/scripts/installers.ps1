### MikTex Mirror
$MIKTEX_MIRROR = "https://github.com/r-windows/bundles/releases/download/miktex-23.10/basic-miktex-23.10-x64.exe"

function CheckExitCode($msg) {
  if ($LastExitCode -ne 0) {
    Throw $msg
  }
}

function bash($command) {
    Write-Host $command -NoNewline
    cmd /c start /wait C:\rtools40\usr\bin\sh.exe --login -c $command
    Write-Host " - OK" -ForegroundColor Green
}

Function InstallMiktex {
  $miktexinstall = "--unattended --auto-install=yes --shared"
  Write-Host "Downloading " + $MIKTEX_MIRROR
  & "C:\Program Files\Git\mingw64\bin\curl.exe" --retry 5 -sSL -o ../basic-miktex-x64.exe -L $MIKTEX_MIRROR

  Write-Host "Installing MiKTeX: " + $miktexinstall
  Start-Process -FilePath ..\basic-miktex-x64.exe -ArgumentList $miktexinstall -NoNewWindow -Wait

  Write-Host "Setting PATH variable for current process"
  $env:PATH = 'C:\Program Files\MiKTeX\miktex\bin\x64;' + $env:PATH

  # Enable auto-install (just in case)
  initexmf --admin --enable-installer
  initexmf --admin --set-config-value "[MPM]AutoInstall=1"

  try {
    Write-Host "Installing CTAN packages"
    mpm --admin --set-repository=https://ctan.math.illinois.edu/systems/win32/miktex/tm/packages/
    miktex --admin packages update-package-database --repository https://ctan.math.illinois.edu/systems/win32/miktex/tm/packages/
    miktex --admin packages update --repository https://ctan.math.illinois.edu/systems/win32/miktex/tm/packages/
    miktex --admin packages install inconsolata --repository=https://ctan.math.illinois.edu/systems/win32/miktex/tm/packages/
  } catch {
    "Problem installing MiKTeX"
  }

  # Log
  Get-Content -Path C:\ProgramData\MiKTeX\miktex\log\mpmcli_admin.log
  Write-Host "MiKTeX installation: Done" -ForegroundColor Green
}

Function SetTimezone {
	tzutil /g
	tzutil /s "GMT Standard Time"
	tzutil /g
}
