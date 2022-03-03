#!/bin/bash

#SBATCH --job-name=bpt_abs_E1_w
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH -o errors/bpt_abs_E1_w.out%a.txt
#SBATCH -e errors/bpt_abs_E1_w.err%a.txt
#SBATCH -q normal
#SBATCH --time=05:00:00
#SBATCH --array=1101-12999

module load R/4.0.2 || exit 1
 
Rscript --vanilla ./callBash_seirRefresh_ALLwardLetter_betaprofile_tinit.R $SLURM_ARRAY_TASK_ID || exit 2

exit 0

