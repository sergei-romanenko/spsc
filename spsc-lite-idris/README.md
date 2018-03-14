# A Small Positive Supercompiler in Idris

To run tests, execute
```
idris --testpkg spsc_lite_idris.ipkg
```

To build the executable `spsc-lite-idris`, execute
```
idris --build spsc_lite_idris.ipkg
```

To run a sample task `taskName`, go to the subdirectory `tasks` and
execute
```
spsc-lite-idris taskName
```

The SPSC Lite will read the file `taskName.task` and write the supercompiled
task into the file `taskName.res`. The process tree (= the graph of
configurations) will be written into the file `taskName.res`.
