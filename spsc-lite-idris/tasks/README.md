# Tasks for SPSC Lite in Idris

First, build the SPSC Lite executable: go to the directory
`spsc-lite-idris` and run
```
idris --build spsc_lite_idris.ipkg
```

The tasks for the SPSC Lite are in files with the extension `.task`.

Go to this directory and, for each task `taskName`, run
```
../spsc-lite-idris taskName
```
Then SPSC Lite will read the file `taskName.task` and produce the files
`taskName.tree` (containig the process tree) and `taskName.res`
(containing the residual task).
