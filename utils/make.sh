#!/bin/bash

mac=$(hostname -f)

case $mac in

#---------------------------------------------------------------------------------
# BUILD UTILITY PROGRAMS ON WCOSS DELL.
#---------------------------------------------------------------------------------

v????.ncep.noaa.gov | m????.ncep.noaa.gov)

  module purge
  module use ./modulefiles
  module load build.wcoss_dell_p3.intel
  module list

  if [ ! -d ./exec ] ;then
    mkdir ./exec
  fi

  rm -fr ./build
  mkdir ./build && cd ./build

  cmake .. -DCMAKE_INSTALL_PREFIX=../

  make -j 1 VERBOSE=1

  make install

  rc=$? ;;

*)

  echo "MACHINE OPTION NOT FOUND. EXIT."
  exit  ;;

esac

exit
