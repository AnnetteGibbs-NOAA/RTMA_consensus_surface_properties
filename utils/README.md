This directory contains four utilities:

- One reads in a grib 2 file of land mask and removes the small lakes.
See ./sorc/remove_lakes.
- One reads in grib 2 files of land mask and terrain and ensures
all water bodies have a constant elevation and that all land points
are at least 1 meter higher than the water. See ./sorc/fix_coasts.
- One reads in a grib 2 file with a scan mode of '80' and converts
it to a scan model of '64'. See ./sorc/fix_scan_mode.
- One reads in a grib 2 file of land mask and converts the Raytheon
category convention to the standard 0-water and 1-land. See ./sorc/fix_mask_cats.


The utilities currently build on WCOSS-Dell and Hera. To build, invoke
the build script with no arguments:
```
./build.sh
```

The build script loads the required libraries via the machine-specific
build module (in ./modulefiles). Executables will be placed under ./exec.

Sample run scripts for some programs (WCOSS-Dell) are under ./scripts:
- run_no.lakes.wcoss_dell.sh (runs lake removal program).
- run_coast.wcoss_dell.sh (runs coastal fix program).

Like the build script, the run scripts load the required libraries via 
the machine-specific build module (in ./modulefiles).
