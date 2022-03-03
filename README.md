# Hospital_R0_C19
 Analysis of hospital data during a Covid-19 outbreak to estimate viral transmission rate and basic reproduction number


#### Model code for running locally ####

Analysis/seirInflect_source.R - two-beta model, creates a pomp object containing parameter values and input data
Analysis/seirRefresh_source.R - one-beta model, creates a pomp object containing parameter values and input data
Analysis/seirRefresh_source_ward.R - one-beta model which encodes a function called call_pomp_ward, which takes the ward code as an argument e.g. "A_0", "B_3" and returns a pomp object as above

#### Principle analysis files and bash file example ####

In each case, the bash file is designed to be called by an HPC cluster using SLURM commands. Each call of the bash script creates many jobs each with a different SLURM_ARRAY_TASK_ID, which is read by the relevant Rscript and used to control starting parameter values or which ward is analysed, using the commandArgs function. 

Analysis/bash/parallel_seirInflect_tinit.sh
Analysis/bashCall_seirInflect_tinit_beta2.R - script which searches for values of beta1, beta2 and tinit

Analysis/bash/parallel_seirInflect_tinit_b1p.sh
Analysis/bashCall_seirInflect_tinit_beta1profile.R - script which creates a profile of beta1 and searches for beta2 and tinit

Analysis/bash/parallel_seirRefresh.sh
Analysis/bashCall_seirRefresh_tinit_profile.R - script which creates a profile of beta and searches for tinit (one phase model)

Analysis/bash/parallel_seirRefresh_ALLward_tinit.sh
Analysis/callBash_seirRefresh_ALLwardLetter_betaprofile_tinit.R - script which creates a profile of beta and searches for tinit (one phase model) for each ward

#### Validation analysis files ####

Validation/bash/parallel_seirRefresh_valid2.sh
Validation/bashCall_seirRefresh_valid2param.R - test estimation bias in the one-phase model

Validation/bash/parallel_validate3param_tinit_seirInflect.sh
Validation/bashCall_seirInflect_valid3param.R - test estimation bias in the two-phase model

Validation/bash/parallel_seirRefresh_ALLward_valid2.sh
Validation/bashCall_seirRefresh_ALLward_valid2param.R - test estimation bias in the one-phase model at ward level

Validation/bash/parallel_seirInflect_tinit_sensAnal.sh
Validation/bashCall_seirInflect_tinit_beta2_sensAnalysis.R - sensitivity analysis of two-phase model to perturbations in parameter values

#### Visualisation ####

file_combiner.R - each of the HPC analyses above produces a single file for each job, so the result of the analysis is many small files. This script combines them, reading them from "TempResults" and writing to "Results" folder

figures.R - this script can reproduce all the graphics and tables in the paper (excluding ppt files, Fig1 and FigS1 and Tables 1 and Table S1, which were created manually), calling on both the raw data or other inputs ("Data" folder) or the results of analyses ("Results" folder)


#### Data ####

posneg_alltests.csv - principle data file for the whole hospital, consisting of number of positive and negative tests per day, as well as admissions and discharges
posneg_alltests_<X>_<N>.csv - data file for ward <X> and floor <N>

# Additional data files
posneg_fpt.csv - as above but including only the first positive for a given patient
pos_tests.csv - data file of tests following first positive, for estimating typical duration of time in which PCR will appear positive
SAR_numer.csv - total numbers of individual patients positive in each ward, for calculating the Secondary Attack Rate
relativeDateKey.csv - reference file between actual dates and dates relative to the first positive test (which is day 1)

# Analysis control files
sensAnalysis_scenarios.csv - scenarios for sensitivity analysis
BiasTest_2param.csv - parameter values used for validation of the one-phase model
BiasTest_3param.csv - parameter values used for validation of the two-phase model

#### Results ####

The uses of these files can be examined with reference to their use in creation of graphics (figures.R file)

The title is informative:
-> seirRefresh - refers to the one-phase model
-> seirInflect - refers to the two-phase model
-> any mention of ward in the title means ward-level analysis, otherwise it is analysed at the whole hospital level
-> if a date is mentioned, this is the fixed value of t_init in the analysis