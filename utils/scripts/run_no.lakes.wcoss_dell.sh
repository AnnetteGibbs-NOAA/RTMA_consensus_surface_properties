#!/bin/bash

#------------------------------------------------------------------
# Run the lake removal code on WCOSS-Dell.
#------------------------------------------------------------------

#BSUB -W 0:02
#BSUB -o nolakes.log
#BSUB -e nolakes.log
#BSUB -J no.lakes
#BSUB -q debug
#BSUB -R "affinity[core(1)]"
#BSUB -P GFS-DEV

set -x

module purge
module use ../modulefiles
module load build.wcoss_dell_p3.intel
module list

run_dir=$PWD

#------------------------------------------------------------------
# Set your working directory, where the 'no lake' file will
# reside.
#------------------------------------------------------------------

WORKDIR=/gpfs/dell1/stmp/$LOGNAME/no.lakes
rm -fr $WORKDIR
mkdir -p $WORKDIR
cd $WORKDIR

#------------------------------------------------------------------
# Link to the input file with all lakes.
#------------------------------------------------------------------

#ln -fs /gpfs/dell1/nco/ops/nwprod/smartinit.v4.3.0/fix/mask/nam_smartmaskak3.grb2  ./fort.50
#ln -fs /gpfs/dell1/nco/ops/nwprod/smartinit.v4.3.0/fix/mask/nam_smartmaskhi.grb2  ./fort.50
#ln -fs /gpfs/dell1/nco/ops/nwprod/smartinit.v4.3.0/fix/mask/nam_smartmaskpr1p25.grb2  ./fort.50
#ln -fs /gpfs/dell1/nco/ops/nwprod/smartinit.v4.3.0/fix/mask/nam_smartmaskpr2p5.grb2  ./fort.50
ln -fs /gpfs/dell2/emc/modeling/noscrub/Annette.Gibbs/RTMA_consensus_surface_properties/conus_landwater_v2p5_fine.gb2  ./fort.50

$run_dir/../exec/nolakes_conus.exe

exit 0
