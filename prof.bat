ghc -prof -fprof-auto -rtsopts main.hs -O2 -funbox-strict-fields -threaded
REM ghc -prof -fprof-auto -rtsopts main.hs -O2 -funbox-strict-fields
main.exe +RTS -p -N4 -s
REM main.exe +RTS -p
