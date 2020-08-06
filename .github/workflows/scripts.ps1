### Global variables
$RTOOLS_ARCH = "x86_64"
$RTOOLS_ZIP = "rtools40-${RTOOLS_ARCH}.7z"
$RTOOLS_EXE = "rtools40-${RTOOLS_ARCH}.exe"

### Use for bootstrapping installation
# $RTOOLS_MIRROR = "https://dl.bintray.com/rtools/installer/"
$RTOOLS_MIRROR = "https://cloud.r-project.org/bin/windows/Rtools/"
# $RTOOLS_MIRROR = "https://ftp.opencpu.org/archive/rtools/4.0/"

### InnoSetup Mirror
$INNO_MIRROR = "http://www.jrsoftware.org/download.php/is.exe?site=2"
# $INNO_MIRROR = "https://github.com/jrsoftware/issrc/releases/download/is-5_6_1/innosetup-5.6.1-unicode.exe"
# $INNO_MIRROR = "https://mlaan2.home.xs4all.nl/ispack/innosetup-5.6.1-unicode.exe"
# $INNO_MIRROR = "http://files.jrsoftware.org/is/5/innosetup-5.6.1-unicode.exe"

### MikTex Mirror
$MIKTEX_MIRROR = "https://miktex.org/download/win/basic-miktex-x64.exe"
#$MIKTEX_MIRROR = "https://cloud.r-project.org/bin/windows/Rtools/basic-miktex-2.9.7152-x64.exe"
#$MIKTEX_MIRROR = "https://cloud.r-project.org/bin/windows/Rtools/basic-miktex-2.9.7386-x64.exe"

function CheckExitCode($msg) {
  if ($LastExitCode -ne 0) {
    Throw $msg
  }
}

# Unzip and Initiate Rtools dump
Function InstallRtoolsZip {
	Write-Host "Installing ${RTOOLS_ZIP}..." -ForegroundColor Cyan
	$tmp = "$($env:USERPROFILE)\${RTOOLS_ZIP}"
	(New-Object Net.WebClient).DownloadFile($RTOOLS_MIRROR + $RTOOLS_ZIP, $tmp)
	7z x $tmp -y -oC:\ | Out-Null
	CheckExitCode "Failed to extract ${RTOOLS_ZIP}"
	C:\rtools40\usr\bin\bash.exe --login -c exit 2>$null
	Write-Host "Installation of ${RTOOLS_ZIP} done!" -ForegroundColor Green
}

# Don't use installer when: (1) architecture doesn't match host (2) Dir C:/rtools40 already exists
Function InstallRtoolsExe {
	Write-Host "Installing ${RTOOLS_EXE}..." -ForegroundColor Cyan
	$tmp = "$($env:USERPROFILE)\${RTOOLS_EXE}"	
	(New-Object Net.WebClient).DownloadFile($RTOOLS_MIRROR + $RTOOLS_EXE, $tmp)
	Start-Process -FilePath $tmp -ArgumentList /VERYSILENT -NoNewWindow -Wait
	Write-Host "Installation of ${RTOOLS_EXE} done!" -ForegroundColor Green
}

function bash($command) {
    Write-Host $command -NoNewline
    cmd /c start /wait C:\rtools40\usr\bin\sh.exe --login -c $command
    Write-Host " - OK" -ForegroundColor Green
}

function InstallRtools {
	InstallRtoolsZip
	bash 'pacman -Sy --noconfirm pacman pacman-mirrors'
	bash 'pacman -Syyu --noconfirm --ask 20'
}

Function InstallInno {
  Write-Host "Downloading InnoSetup from: " + $INNO_MIRROR
  & "C:\Program Files\Git\mingw64\bin\curl.exe" --retry 5 -s -o ../innosetup.exe -L $INNO_MIRROR
  CheckExitCode "Failed to download $INNO_MIRROR"

  Write-Host "Installig InnoSetup..."
  Start-Process -FilePath ..\innosetup.exe -ArgumentList "/ALLUSERS /SILENT" -NoNewWindow -Wait
  CheckExitCode "Failed to install InnoSetup"

  Write-Host "InnoSetup installation: Done" -ForegroundColor Green
  Get-ItemProperty "C:\Program Files (x86)\Inno Setup 6\ISCC.exe"
}

function InnoBuild($iss){
	Write-Host "Creating installer..." -NoNewline
	& "C:\Program Files (x86)\Inno Setup 5\iscc.exe" "${env:RTOOLS_NAME}.iss" | Out-File output.log
	Write-Host "OK!" -ForegroundColor Green
}

function SignFiles($files) {
  & $env:SignTool sign /f $env:KeyFile /p "$env:CertPassword" /tr http://sha256timestamp.ws.symantec.com/sha256/timestamp /td sha256 /fd sha256 $files
  CheckExitCode "Failed to sign files."
}

Function InstallMiktex {
  $miktexinstall = "--unattended --auto-install=yes --shared --package-set=basic"

  # Hack around the random mirror redirect because many mirrors are broken
  $effective_url = & "C:\Program Files\Git\mingw64\bin\curl.exe" -LIs -o NUL -w "%{url_effective}" $MIKTEX_MIRROR
  $miktex_file = [io.path]::GetFileName($effective_url)
  $MIKTEX_MIRROR = "https://ctan.math.illinois.edu/systems/win32/miktex/setup/windows-x64/" + $miktex_file

  Write-Host "Downloading " + $MIKTEX_MIRROR
  & "C:\Program Files\Git\mingw64\bin\curl.exe" --retry 5 -sSL -o ../basic-miktex-x64.exe -L $MIKTEX_MIRROR

  Write-Host "Installing MiKTeX: " + $miktexinstall
  Start-Process -FilePath ..\basic-miktex-x64.exe -ArgumentList $miktexinstall -NoNewWindow -Wait

  Write-Host "Setting PATH variable for current process"
  $env:PATH = 'C:\Program Files\MiKTeX\miktex\bin\x64;' + $env:PATH

  # Enable auto-install (just in case)
  initexmf --admin --enable-installer
  initexmf --admin --set-config-value "[MPM]AutoInstall=1"

  Write-Host "Installing CTAN packages"
  mpm --admin --set-repository=https://ctan.math.illinois.edu/systems/win32/miktex/tm/packages/
  mpm --admin --verbose --update-db
  mpm --admin --verbose --update
  mpm --admin --install=inconsolata
  #mpm --admin --install=fancyvrb  
  #mpm --admin --install=epsf
  #mpm --admin --install=preprint

  # See https://tex.stackexchange.com/a/129523/12890
  # $conffile = "C:\Program Files\MiKTeX\miktex\config\updmap.cfg"
  # Write-Host "Adding zi4.map"
  # initexmf --admin --update-fndb
  # Add-Content $conffile "`nMap zi4.map`n"
  # initexmf --admin --mkmaps

  # First time running 'pdflatex' always fails with some inite
  # Write-Host "Trying pdflatex..."
  # pdflatex.exe --version
  Write-Host "MiKTeX installation: Done" -ForegroundColor Green
}

########### OLD CODE ###########

Function InstallMSYS32 {
	Write-Host "Installing MSYS2 32-bit..." -ForegroundColor Cyan

	# download installer
	$zipPath = "$($env:USERPROFILE)\msys2-i686-latest.tar.xz"
	$tarPath = "$($env:USERPROFILE)\msys2-i686-latest.tar"
	Write-Host "Downloading MSYS installation package..."
	(New-Object Net.WebClient).DownloadFile('http://repo.msys2.org/distrib/msys2-i686-latest.tar.xz', $zipPath)

	Write-Host "Untaring installation package..."
	7z x $zipPath -y -o"$env:USERPROFILE" | Out-Null

	Write-Host "Unzipping installation package..."
	7z x $tarPath -y -oC:\ | Out-Null
	del $zipPath
	del $tarPath
}

function rtools_bootstrap {
	# AppVeyor only has msys64 preinstalled
	if($env:MSYS_VERSION -eq 'msys32') {
		InstallMSYS32
		bash 'pacman -Sy --noconfirm pacman pacman-mirrors'

		# May upgrade runtime, need to exit afterwards
		bash 'pacman -Syyuu --noconfirm --ask 20'
	}
	bash 'pacman --version'
	bash 'pacman --noconfirm -Rcsu mingw-w64-x86_64-toolchain mingw-w64-i686-toolchain'
	bash 'repman add rtools "https://dl.bintray.com/rtools/${MSYSTEM_CARCH}"'
	bash 'pacman --noconfirm --sync rtools/pacman-mirrors rtools/pacman rtools/tar'
	bash 'mv /etc/pacman.conf /etc/pacman.conf.old'
	bash 'mv /etc/pacman.conf.pacnew /etc/pacman.conf'
	bash 'pacman --noconfirm -Scc'
	bash 'pacman --noconfirm --ask 20 -Syyu'
}


Function SetTimezone {
	tzutil /g
	tzutil /s "GMT Standard Time"
	tzutil /g
}
