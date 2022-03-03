#!/bin/bash

#SBATCH --job-name=val3tinf23
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH -o errors/val3tinf23.out%a.txt
#SBATCH -e errors/val3tinf23.err%a.txt
#SBATCH -q normal
#SBATCH --time=05:00:00
#SBATCH --array=1-7530
 
module load R/4.0.2 || exit 1
 
Rscript --vanilla ./bashCall_seirInflect_valid3param.R $SLURM_ARRAY_TASK_ID || exit 2

exit 0

