#!/bin/bash

#---------------------------------------------------------------------------------
# BUILD UTILITY PROGRAMS.  Invoke this script with no arguments.
#---------------------------------------------------------------------------------

mac=$(hostname -f)

if [ ! -d ./exec ] ;then
  mkdir ./exec
fi

rm -fr ./build
mkdir ./build && cd ./build

case $mac in

#---------------------------------------------------------------------------------
# BUILD ON WCOSS DELL.
#---------------------------------------------------------------------------------

v????.ncep.noaa.gov | m????.ncep.noaa.gov)

  module purge
  module use ../modulefiles
  module load build.wcoss_dell_p3.intel
  module list ;;

#---------------------------------------------------------------------------------
# BUILD ON Hera
#---------------------------------------------------------------------------------

h????)

  module purge
  module use ../modulefiles
  module load build.hera.intel
  module list ;;

*)

  echo "MACHINE OPTION NOT FOUND. EXIT."
  exit  ;;

esac

cmake .. -DCMAKE_INSTALL_PREFIX=../

make -j 1 VERBOSE=1

make install

exit
