library(tidyverse)

input_folder = "TempResults/"
output_folder = "Results/"

# experiment_name = "seirRefresh_posneg betaProfile tinit Einit1 seedFix"
# 
# experiment_name = "seirInflect_posneg Inflect beta1profile Einit1 tinflect23"
# experiment_name = "seirInflect_posneg Inflect beta2 Einit1 tinflect23"
# 
# experiment_name = "seirRefresh_posneg ALLwardLetter Refresh betaprofile Einit1"
# experiment_name = "seirRefresh_posneg ALLwardLetter Refresh Einit1 ABStrans"

experiment_name = "seirInflect_posneg Inflect beta2 Einit1 tinflect23 smallExample"


files = list.files(path = input_folder, pattern = paste0(experiment_name, "_.*.csv")
                   , full.names = T)

for(i in 1:length(files)){
  if(i %% 100 == 0) {print(c(experiment_name, i));gc()}
  if(i == 1){
    res_read = read_csv(files[i])
    cols = spec(res_read)
  } else {
    res_read = rbind(res_read, read_csv(files[i], col_types = cols))
  }
}

res = res_read %>% unique

filename = file.path(output_folder, paste0(experiment_name, ".csv" ))
res %>% write_csv(filename)
