..\lazarus64\lazbuild.exe particles.lpi --bm=opt
..\lazarus64\fpc\3.2.2\bin\x86_64-win64\delp.exe -r .
upx.exe --best --ultra-brute -o particles_upx.exe particles.exe
move /Y particles_upx.exe particles.exe