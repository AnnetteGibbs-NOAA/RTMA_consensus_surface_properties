#!/bin/bash

#-----------------------------------------------------
# Invoke this script as follows:
# cat run.lsf | bsub
#-----------------------------------------------------

#BSUB -oo log
#BSUB -eo log
#BSUB -q dev_shared
#BSUB -R rusage[mem=1000]
#BSUB -R affinity[core(1)]
#BSUB -J rmlakes
#BSUB -P RTMA-T2O
#BSUB -W 0:05

set -x

mkdir -p /ptmpp1/George.Gayno/waterfalls

rundir=${LS_SUBCWD}

cd /ptmpp1/George.Gayno/waterfalls

${rundir}/fix.exe

exit 0
