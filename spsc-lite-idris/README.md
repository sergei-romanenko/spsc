# A Small Positive Supercompiler in Idris (Lite)

To run tests, execute
```
idris --testpkg spsc_lite_idris.ipkg
```

To build the executable `spsc_lite_idris`, execute
```
idris --build spsc_lite_idris.ipkg
```

To run a sample task `name`, go to the subdirectory `tasks` and
execute
```
spsc_lite_idris name
```

The SPSC Lite will read the file `name.task` and write the supercompiled
task into the file `name.res`. The process tree (= the graph of
configurations) will be written into the file `name.tree`.
