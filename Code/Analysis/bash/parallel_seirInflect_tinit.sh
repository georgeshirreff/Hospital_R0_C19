#!/bin/bash

#SBATCH --job-name=b2_E1_tinf23
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH -o errors/b2_E1_tinf23.out%a.txt
#SBATCH -e errors/b2_E1_tinf23.err%a.txt
#SBATCH -q normal
#SBATCH --time=05:00:00
#SBATCH --array=101-999

module load R/4.0.2 || exit 1
 
Rscript --vanilla ./seirInflect_tinit_beta2.R $SLURM_ARRAY_TASK_ID || exit 2

exit 0

