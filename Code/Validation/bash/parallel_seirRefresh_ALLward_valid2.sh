#!/bin/bash

#SBATCH --job-name=bt_val2_w
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH -o errors/bt_val2_w.out%a.txt
#SBATCH -e errors/bt_val2_w.err%a.txt
#SBATCH -q normal
#SBATCH --time=05:00:00
#SBATCH --array=1-1510
module load R/4.0.2 || exit 1
 
Rscript --vanilla ./bashCall_seirRefresh_ALLward_valid2param.R $SLURM_ARRAY_TASK_ID || exit 2

exit 0

