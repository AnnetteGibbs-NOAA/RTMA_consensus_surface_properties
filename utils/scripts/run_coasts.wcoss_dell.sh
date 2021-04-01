#!/bin/bash

#-----------------------------------------------------
# Run the coastal fix code on WCOSS-Dell.
#
# Invoke this script as follows:
# cat $script | bsub
#-----------------------------------------------------

#BSUB -oo coasts.log
#BSUB -eo coasts.log
#BSUB -q debug
#BSUB -R affinity[core(1)]
#BSUB -J fix_coast
#BSUB -P GFS-DEV
#BSUB -W 0:02

set -x

module purge
module use ../modulefiles
module load build.wcoss_dell_p3.intel
module list

run_dir=$PWD/..

#-----------------------------------------------------
# Set your working directory.  This is where the
# output orography file will reside.
#-----------------------------------------------------

WORKDIR=/gpfs/dell1/stmp/$LOGNAME/coasts
rm -fr $WORKDIR
mkdir -p $WORKDIR
cd $WORKDIR

#-----------------------------------------------------
# The mask file is fort.50.  The orography file is
# fort.51.
#-----------------------------------------------------

#ln -fs /gpfs/dell1/stmp/George.Gayno/fix.mask.cats/alaska_landwater_urma.gb2  ./fort.50
#ln -fs /gpfs/dell1/stmp/George.Gayno/fix.mask.cats/alaska_terrain.v2p0.gb2  ./fort.51
ln -fs /gpfs/dell2/emc/modeling/noscrub/Annette.Gibbs/RTMA_consensus_surface_properties/conus_landwater_v2p5_fine.gb2  ./fort.50
ln -fs /gpfs/dell2/emc/modeling/noscrub/Annette.Gibbs/RTMA_consensus_surface_properties/nam_smarttopoconus2p5.grb2  ./fort.51

${run_dir}/exec/fix_coasts.exe

exit 0
