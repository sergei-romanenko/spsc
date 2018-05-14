# Tasks for the SPSC Lite in Idris

First, build the SPSC Lite executable: go to the directory
`spsc-lite-idris` and run
```
idris --build spsc_lite_idris.ipkg
```

The tasks for the SPSC Lite are in files with the extension `.task`.

Go to this directory and, for each task `name`, run
```
../spsc_lite_idris name
```
Then SPSC Lite will read the file `name.task` and produce the files
`name.tree` (containig the process tree) and `name.res`
(containing the residual task).
