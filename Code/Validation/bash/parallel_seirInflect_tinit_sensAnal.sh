#!/bin/bash

#SBATCH --job-name=b12tsensAnal
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH -o errors/b12tsensAnal.out%a.txt
#SBATCH -e errors/b12tsensAnal.err%a.txt
#SBATCH -q normal
#SBATCH --time=05:00:00
#SBATCH --array=1-30

module load R/4.0.2 || exit 1
 
Rscript --vanilla ./bashCall_seirInflect_tinit_beta2_sensAnalysis.R $SLURM_ARRAY_TASK_ID || exit 2

exit 0

