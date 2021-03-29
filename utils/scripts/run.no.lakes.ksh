#!/bin/ksh --login

#PBS -l procs=1
#PBS -l vmem=2G
#PBS -l walltime=0:03:00
#PBS -A fv3-cpu
#PBS -q debug
#PBS -N rtma
#PBS -o log
#PBS -e log

set -x

rundir=$PBS_O_WORKDIR

WORKDIR=/scratch3/NCEPDEV/stmp1/George.Gayno/nolakes
rm -fr $WORKDIR
mkdir -p $WORKDIR
cd $WORKDIR

ln -fs /scratch4/NCEPDEV/da/noscrub/George.Gayno/rtma_v2.6/fix.scan.mode/co2p5o_lw_finelakes.gb2  ./fort.50

${rundir}/nolakes.exe

exit 0
