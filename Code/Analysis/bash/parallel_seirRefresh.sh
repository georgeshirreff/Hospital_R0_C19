#!/bin/bash

#SBATCH --job-name=bProf_E1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH -o errors/bProf_E1.out%a.txt
#SBATCH -e errors/bProf_E1.err%a.txt
#SBATCH -q normal
#SBATCH --time=05:00:00
#SBATCH --array=101-999

module load R/4.0.2 || exit 1
 
Rscript --vanilla ./bashCall_seirRefresh_tinit_profile.R $SLURM_ARRAY_TASK_ID || exit 2

exit 0

