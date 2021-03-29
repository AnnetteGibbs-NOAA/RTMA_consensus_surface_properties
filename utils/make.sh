#!/bin/bash

mac=$(hostname -f)

case $mac in

#---------------------------------------------------------------------------------
# BUILD PROGRAM ON WCOSS DELL.
#---------------------------------------------------------------------------------

v????.ncep.noaa.gov | m????.ncep.noaa.gov)

  module purge

  module load cmake/3.16.2

  module use /usrx/local/nceplibs/dev/hpc-stack/libs/hpc-stack/modulefiles/stack

  module load hpc/1.1.0
  module load hpc-ips/18.0.1.163

  module load jasper/2.0.22
  module load zlib/1.2.11
  module load png/1.6.35
  
  module load g2/3.4.1
  module load bacio/2.4.1
  module load w3nco/2.4.1
  module list

  mkdir ./exec

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
