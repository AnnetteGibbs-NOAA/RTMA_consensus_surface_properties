#!/bin/bash

#-----------------------------------------------------
# Invoke this script as follows:
# cat run.lsf | bsub
#-----------------------------------------------------

#BSUB -oo coasts.log
#BSUB -eo coasts.log
#BSUB -q debug
#BSUB -R affinity[core(1)]
#BSUB -J rmlakes
#BSUB -P GFS-DEV
#BSUB -W 0:02

set -x

module purge
module use ../modulefiles
module load build.wcoss_dell_p3.intel
module list

run_dir=$PWD/..

WORKDIR=/gpfs/dell1/stmp/$LOGNAME/coasts
rm -fr $WORKDIR
mkdir -p $WORKDIR
cd $WORKDIR

ln -fs /gpfs/dell2/emc/modeling/noscrub/Annette.Gibbs/RTMA_consensus_surface_properties/conus_landwater_v2p5_fine.gb2  ./fort.50
ln -fs /gpfs/dell2/emc/modeling/noscrub/Annette.Gibbs/RTMA_consensus_surface_properties/nam_smarttopoconus2p5.grb2  ./fort.51

${run_dir}/exec/fix_coasts_conus.exe

exit 0
