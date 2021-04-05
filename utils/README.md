This directory contains two utilities:

- One reads in a grib 2 file of land mask and removes the small lakes.
See ./sorc/remove_lakes.
- One reads in grib 2 files of land mask and terrain and ensures
all water bodies have a constant elevation and that all land points
are at least 1 meter higher than the water. See ./sorc/fix_coasts.

The utilities currently build on WCOSS-Dell and Hera. To build, invoke
the build script with no arguments:
```
./build.sh
```

The build script loads the required libraries via the machine-specific
build module (in ./modulefiles). Executables will be placed under ./exec.

Sample run scripts for each program (WCOSS-Dell) are under ./scripts:
- run_no.lakes.wcoss_dell.sh (runs lake removal program).
- run_coast.wcoss_dell.sh (runs coastal fix program).

Like the build script, the run scripts load the required libraries via 
the machine-specific build module (in ./modulefiles).
