del main.exe
REM ghc -prof -fprof-auto -rtsopts main.hs -O2 -funbox-strict-fields -threaded
ghc -prof -fprof-auto -rtsopts main.hs -O2 -funbox-strict-fields
REM main.exe +RTS -p -N1 -s
main.exe +RTS -p
