call "C:\Program Files (x86)\Embarcadero\Studio\15.0\bin\rsvars.bat"

msbuild .\Examples.groupproj /t:Build /p:config=Release

pause
