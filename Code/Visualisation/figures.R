library(tidyverse)
library(readxl)
library(magrittr)
library(lubridate)
library(gridExtra)
library(pomp)
library(EpiEstim)
library(incidence)

figures_folder = "Figures/"
tables_folder = "Tables/"


###################
####  Figure 2 ####
###################

posneg <- read_csv(paste0("Data/posneg_alltests.csv")) %>% 
  mutate(Date = as.numeric(Date), tests = neg + pos) %>% 
  filter(Date >= as.numeric(as.Date("2020-03-01"))
         , Date <= as.numeric(as.Date("2020-06-22")))

posnegWard = NULL
for(building in c("A", "B", "C")){
  for(floor in 0:3){
    this_file = paste0("Data/posneg_alltests_", building, "_", floor, ".csv")
    if(file.exists(this_file)){
      posnegWard_piece = read_csv(this_file) %>% 
        mutate(Building = building, Floor = floor)
      
    } else {
      posnegWard_piece = posnegWard_piece %>% 
        mutate(neg = 0, pos = 0, Patients = 0, adm = 0, dd = 0) %>% 
        mutate(Building = building, Floor = floor)
      
    }
    
    if(is.null(posnegWard)){
      posnegWard = posnegWard_piece
    } else {
      posnegWard = rbind(posnegWard
                         , posnegWard_piece)
    }
    
    
    
  }
}

posnegWard %<>% mutate(wardCode = paste0(Building, Floor)
                       , Floor = factor(Floor, levels = 3:0))

# Figure 2A #

TestsWhole_relative <-
  posneg %>% 
  transmute(Date = as.Date(Date, origin = "1970-01-01"), Positives = pos, Negatives = neg) %>% 
  filter(Date >= as.numeric(as.Date("2020-03-01"))
         , Date <= as.numeric(as.Date("2020-05-01"))) %>% 
  pivot_longer(cols = c("Positives", "Negatives")) %>% 
  # ggplot(aes(x = Date, y = value, fill = name)) + geom_bar(stat = "identity", position = "dodge") +
  # theme_bw() + labs(x = "", y = "Number per day", fill = "")
  mutate(WeekStart = floor_date(Date, unit = "week", week_start = 7)) %>% group_by(WeekStart, name) %>% summarise(value = sum(value)) %>%
  mutate(relWeekStart = difftime(WeekStart, "2020-03-11", units = "days") %>% as.numeric %>% round) %>% 
  
  mutate(facet_strip = "Whole hospital") %>% 
  ggplot(aes(x = relWeekStart, y = value, fill = name)) + geom_bar(stat = "identity", position = "stack") +
  theme_bw() + labs(x = "relative date (week start)", y = "Tests per week", fill = "Result") + 
  scale_fill_manual(values = c(Negatives = "blue", Positives = "red")) + 
  theme(legend.position = c(0.8, 0.8), legend.background = element_blank()
        , text = element_text(size = 20)
        , axis.text.x = element_text(size = 20, angle = 90, hjust = 0, vjust = 0.5)
        , plot.margin = margin(r = 1, unit = "cm")) + 
  facet_wrap(.~facet_strip)

# Figure 2B #

TestsEveryWard_relative <-
  posnegWard %>% 
  transmute(Date = as.Date(Date, origin = "1970-01-01"), wardCode, Building, Floor, Positives = pos, Negatives = neg) %>% 
  filter(Date >= as.numeric(as.Date("2020-03-01"))
         , Date <= as.numeric(as.Date("2020-05-01"))) %>% 
  pivot_longer(cols = c("Positives", "Negatives")) %>% 
  # ggplot(aes(x = Date, y = value, fill = name)) + geom_bar(stat = "identity", position = "dodge") +
  # theme_bw() + labs(x = "", y = "Number per day", fill = "")
  mutate(WeekStart = floor_date(Date, unit = "week", week_start = 7)) %>% 
  mutate(relWeekStart = difftime(WeekStart, "2020-03-11", units = "days") %>% as.numeric %>% round) %>% 
  
  group_by(relWeekStart, wardCode, Building, Floor, name) %>% 
  summarise(value = sum(value)) %>%
  mutate(fig_label = ifelse(wardCode == "A3", "b", "")) %>% 
  ggplot(aes(x = relWeekStart, y = value, fill = name)) + 
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(Floor~Building) + 
  # geom_text(aes(x = -11, y = 30, label = fig_label), size = 20) +
  theme_bw() + 
  labs(x = "relative date (week start)", y = "Tests per week", fill = "Result") + 
  scale_fill_manual(values = c(Negatives = "blue", Positives = "red")) + 
  # theme(legend.position = c(0.8, 0.8), legend.background = element_blank()) + 
  theme(text = element_text(size = 20)
        , axis.text.x = element_text(size = 20, angle = 90, hjust = 0, vjust = 0.5)
        , plot.margin = margin(r = 1, unit = "cm")
        , legend.position = "none")


# Figure 2C #

SAR_numer <- read_csv("Data/SAR_numer.csv")

SAR_denom <- posnegWard %>% 
  group_by(wardCode) %>% 
  summarise(max_Patients = max(Patients)
            , total_Patients = Patients[1] + sum(adm)) %>% 
  rbind(posneg %>% 
          summarise(max_Patients = max(Patients)
                    , total_Patients = Patients[1] + sum(adm)) %>% 
          mutate(wardCode = "Whole\nhospital")
  )

SARplot <- SAR_numer %>% 
  left_join(SAR_denom) %>% 
  filter(total_Patients > 0) %>% 
  mutate(SAR = patients_positive/total_Patients) %>% 
  arrange(SAR) %>% mutate(wardCode = factor(wardCode, levels = wardCode) %>% fct_relevel("Whole\nhospital", after = Inf)) %>% 
  ggplot(aes(x = wardCode, y = SAR)) + geom_bar(stat = "identity") + 
  labs(y = "Secondary Attack Rate", x = "Ward") + theme_bw() + 
  scale_y_continuous(labels = scales::percent) + 
  theme(text = element_text(size = 20))


data_fig_scale = 4
ggsave(
  TestsWhole_relative + theme(strip.background = element_blank(),
                     strip.text.x = element_blank())
  , filename = paste0(figures_folder, "Fig2A", ".jpg")
  , height = 5, width = 5, units = "cm", scale = data_fig_scale, device = "jpeg", dpi = 600
)

ggsave(
  TestsEveryWard_relative
  , filename = paste0(figures_folder, "Fig2B", ".jpg")
  , height = 5, width = 5, units = "cm", scale = data_fig_scale, device = "jpeg", dpi = 600
)
ggsave(
  SARplot
  , filename = paste0(figures_folder, "Fig2C", ".jpg")
  , height = 5, width = 5, units = "cm", scale = data_fig_scale, device = "jpeg", dpi = 600
)


#####################
####  Figure S 2 ####
#####################

posneg %>% 
  transmute(Date = as.Date(Date, origin = "1970-01-01"), Admissions = adm, Discharges = dd, Tests = tests) %>% 
  filter(Date >= as.numeric(as.Date("2020-03-01"))
         , Date <= as.numeric(as.Date("2020-05-02"))) %>% 
  pivot_longer(cols = c("Admissions", "Discharges", "Tests")) %>% 
  # ggplot(aes(x = Date, y = value, fill = name)) + geom_bar(stat = "identity", position = "dodge") +
  # theme_bw() + labs(x = "", y = "Number per day", fill = "")
  mutate(WeekStart = floor_date(Date, unit = "week", week_start = 7)) %>% 
  mutate(relWeekStart = difftime(WeekStart, "2020-03-11", units = "days") %>% as.numeric %>% round) %>% 
  
  group_by(relWeekStart, name) %>% 
  summarise(value = sum(value)) %>%
  ggplot(aes(x = relWeekStart, y = value, fill = name)) + geom_bar(stat = "identity", position = "dodge") +
  theme_bw() + labs(x = "relative date (week start)", y = "Number per week", fill = "Event") + 
  theme(legend.position = c(0.2, 0.8), legend.background = element_blank()
        # , text = element_text(size = 15)
        , plot.margin = margin(r = 1, unit = "cm"))



ggsave(
  # SARplot
  # , filename = paste0("~/tars/output/Figs/", "Fig2C", ".tif")
  filename = paste0(figures_folder, "SFig2", ".jpg")
  , height = 7, width = 10, units = "cm", scale = 1, device = "jpeg", dpi = 600
)


#####################
####  Figure S 3 ####
#####################


pos_tests <- read_csv("Data/pos_tests.csv")

pos_tests %>% 
  filter(DatePrelevement > firstPCRPos + 7) %>% 
  mutate(TimeInRp = TimeSincefirstPCRPos - 7) -> pos_tests_Rp



#### MLE ####
dur_Rp = 5
sens_Rp = 0.5
spec_R = 0.999

lls = c()
for(dur_Rp in 1:76){
  pos_tests_Rp %>% 
    select(TimeInRp, cov) %>% 
    arrange(TimeInRp) %>% 
    mutate(prob_pos = ifelse(TimeInRp <= dur_Rp, sens_Rp, 1 - spec_R)
           , prob = ifelse(cov == 1, prob_pos, 1 - prob_pos)
           , loglik = log(prob)) %>% 
    pull(loglik) %>% sum -> ll
  
  lls = c(lls, ll) 
}


Rp_plots = list()

Rp_plots[[1]] = pos_tests %>% 
  select(TimeSincefirstPCRPos, COVID) %>% 
  ggplot(aes(x = TimeSincefirstPCRPos, fill = COVID)) + geom_histogram(position = "dodge") + 
  scale_fill_manual(values = c(pos = "red", neg = "blue")) + 
  labs(x = "Days since first positive PCR", y = "Tests", fill = "Test result") + 
  theme_bw() + 
  coord_cartesian(ylim = c(-2, NA)) +
  geom_segment(x = 0, xend = 7, y = -0.5, yend = -0.5, arrow = arrow(ends = "last", length = unit(3, "mm"))) + 
  geom_text(x = 3.5, y = -1, label = "dur_I") + 
  geom_segment(x = 8, xend = 7+25, y = -0.5, yend = -0.5, arrow = arrow(ends = "last", length = unit(3, "mm"))) + 
  geom_text(x = 15, y = -1, label = "dur_Rp")

Rp_plots[[2]] = tibble(dur_Rp = 1:76, logLik = lls) %>% 
  ggplot(aes(x = dur_Rp, y = logLik)) + geom_line() + 
  geom_point(x = 23, y = lls[23], colour = "red") +
  geom_vline(xintercept = 23, colour = "red", linetype = "dashed") +
  theme_bw()


ggsave(Rp_plots[[1]]
       , filename = paste0(figures_folder, "SFig3A.jpg"), units = "cm", width = 15, height = 10, device = "jpeg", dpi = 600)

ggsave(Rp_plots[[2]]
       , filename = paste0(figures_folder, "SFig3B.jpg"), units = "cm", width = 15, height = 10, device = "jpeg", dpi = 600)




#################################
####  Simulation Figures 3+4 ####
#################################

# Whole hospital figures (3A, 3B, 4A) #

posneg <- read_csv(paste0("Data/posneg_alltests.csv")) %>% 
  mutate(Date = as.numeric(Date), tests = neg + pos) %>% 
  filter(Date >= as.numeric(as.Date("2020-02-01"))
                            , Date <= as.numeric(as.Date("2020-04-30")))

SAR_numer_threshold = 3


source("Code/Analysis/seirRefresh_source.R")
source("Code/Analysis/seirInflect_source.R")

# one beta version (2 parameters including t_init) #

experiment_name = "seirRefresh_posneg betaProfile tinit Einit1 seedFix"

this_title = ""



pars = 2
ci_interval <- 0.5*qchisq(df=pars,p=0.95)

par_est <-  read_csv(paste0("Results/", experiment_name, ".csv" ))



beta_cutoff = 5.1
maxloglik <- max(par_est %>% filter(beta<beta_cutoff) %>% pull(loglik),na.rm=TRUE)
ci.cutoff <- maxloglik-0.5*qchisq(df=pars,p=0.95)


best_params = par_est %>% 
  filter(beta < beta_cutoff) %>%
  arrange(-loglik) %>% 
  select(-loglik, -loglik.se) %>% 
  {.[1, ]} %>% unlist

bestbeta = round(best_params["beta"], digits = 2)

R0 = with(best_params %>% as.list
          , beta*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/delta)))
R0

# range of params

par_est %>% 
  filter(beta < beta_cutoff) %>% 
  filter(loglik > ci.cutoff) %>% 
  arrange(beta) -> approved_params

approved_params %>% pull(beta) %>% range

R0s <- approved_params %>% apply(1, function(x) with(x %>% as.list, beta*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/delta))))
R0s %>% range


obs_df1 <- seirRefresh %>% simulate(params = seirRefresh@params %>% replace(c("beta", "E_init", "t_init")
                                                                            , best_params[c("beta", "E_init", "t_init")])
                                    , seed = 1, nsim = 1, include.data = T, format = "data.frame") %>% 
  filter(.id == "data") %>% 
  mutate(rep = 0
         , seed_index = as.numeric(NA)
         , id = 0
         , beta = as.numeric(NA)
         , t_init = as.numeric(NA)
  )


# simulate from single set of most likely parameter values

NSIM = 1000

new_sims1 <- seirRefresh %>% simulate(params = seirRefresh@params %>% replace(c("beta", "E_init", "t_init")
                                                                              , best_params[c("beta", "E_init", "t_init")])
                                      , seed = 1, nsim = NSIM, include.data = F, format = "data.frame") %>% 
  mutate(rep = as.numeric(.id)
         , seed_index = 0
         , id = seed_index*NSIM*10 + rep
         , beta = best_params["beta"]
         , t_init = best_params["t_init"]
         , E_init = best_params["E_init"])


# simulate from range of likely parameter values

NSETS = 1000
these_paramsets = sample(nrow(approved_params), size = NSETS, replace = F)

nrow(approved_params)
NSIM = 1
i = 1
#i = 281
for(i in 1:NSETS){
  print(i)
  # this_approved_sims <- simulate(seirRefresh, params = approved_params[i, ] %>% unlist
  #                                , seed = i, nsim = NSIM, include.data = F, format = "data.frame")
  
  this_approved_sims_pompobj <- simulate(seirRefresh, params = approved_params[these_paramsets[i], ] %>% unlist
                                         , seed = i, nsim = NSIM, include.data = F)
  
  ll <- this_approved_sims_pompobj %>% {dmeasure(.
                                                 , y = obs(seirRefresh)
                                                 , x = states(.)
                                                 , params = .@params
                                                 , times = seirRefresh@times
                                                 , log = T)} %>% sum
  
  this_approved_sims <- this_approved_sims_pompobj %>% as.data.frame %>% transmute(Date
                                                                                   , rep = i #as.numeric(.id)
                                                                                   , pos
                                                                                   , SAR_numer
                                                                                   , Ninfected
  )
  
  this_approved_sims_meta <- data.frame(rep = i #as.numeric(.id)
                                        , seed_index = i
                                        , beta = approved_params$beta[i]
                                        , t_init = approved_params$t_init[i]
                                        , E_init = approved_params$E_init[i]
                                        , ll = ll)
  
  if(i == 1){
    these_approved_sims1 = this_approved_sims
    these_approved_sims_meta = this_approved_sims_meta
  } else {
    these_approved_sims1 = rbind(these_approved_sims1, this_approved_sims)
    these_approved_sims_meta = rbind(these_approved_sims_meta, this_approved_sims_meta)
  }
  
}

these_approved_sims1 %<>% left_join(these_approved_sims_meta) %>% 
  mutate(seed_index = rep)

best_id = these_approved_sims1 %>% filter(ll == max(ll)) %>% pull(rep) %>% unique %>% {.[1]}


# two beta version (3 parameters including t_init)#

pars = 3
ci_interval <- 0.5*qchisq(df=pars,p=0.95)


res = NULL
for(Eini in c(1)){
  for(analysis in c("beta2", "beta1profile")){
    exp_name = paste0("seirInflect_posneg Inflect ", analysis, " Einit", Eini, " tinflect23")
    
    res_piece = read_delim(paste0("Results/", exp_name, ".csv" ), delim = ";") %>% mutate(analysis = analysis)
    if(is.null(res)){
      res = res_piece
    } else {
      res = rbind(res, res_piece)
    }
    
  }
}


res <- res %>% 
  mutate(R0_before = beta1*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/(delta*kappa3)))) %>% 
  mutate(R0_after = beta2*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/(delta*kappa3)))) %>% 
  mutate_at(c("t_inflect", "t_init", "start_tinit"), function(x) as.Date(x, origin = "1970-01-01")) %>% 
  group_by(E_init) %>% 
  mutate(ci_boundary = max(loglik) - ci_interval
         , ci =  ifelse(loglik > max(loglik) - ci_interval, "in_ci", "out_ci")) %>% 
  ungroup




best_params = res %>% 
  select(seirInflect@params %>% names, starts_with("loglik")) %>% 
  arrange(-loglik) %>% 
  select(-loglik, -loglik.se) %>% 
  {.[1, ]} %>% unlist

approved_params <- res %>% 
  filter(ci == "in_ci") %>% 
  select(-loglik, -loglik.se) %>% 
  arrange(beta1)





obs_df2 <- seirInflect %>% simulate(params = seirInflect@params %>% replace(c("beta1", "beta2", "t_init", "E_init", "t_inflect")
                                                                            , best_params[c("beta1", "beta2", "t_init", "E_init", "t_inflect")])
                                    , seed = 1, nsim = 1, include.data = T, format = "data.frame") %>% 
  filter(.id == "data") %>% 
  mutate(rep = 0
         , seed_index = as.numeric(NA)
         , id = 0
         , beta1 = as.numeric(NA)
         , beta2 = as.numeric(NA)
         , t_init = as.numeric(NA)
         , E_init = as.numeric(NA)
         , t_inflect = as.numeric(NA)
  )


# simulate from single set of most likely parameter values

NSIM = 1000

new_sims2 <- seirInflect %>% simulate(params = seirInflect@params %>% replace(c("beta1", "beta2", "t_init", "E_init", "t_inflect")
                                                                              , best_params[c("beta1", "beta2", "t_init", "E_init", "t_inflect")])
                                      , seed = 1, nsim = NSIM, include.data = F, format = "data.frame") %>% 
  mutate(rep = as.numeric(.id)
         , seed_index = 0
         , id = seed_index*NSIM*10 + rep
         , beta1 = best_params["beta1"]
         , beta2 = best_params["beta2"]
         , t_init = best_params["t_init"]
         , E_init = best_params["E_init"]
         , t_inflect = best_params["t_inflect"]
  )

# simulate from range of likely parameter values

NSIM = 1
NSETS = 1000
these_paramsets = sample(nrow(approved_params), size = NSETS, replace = F)
i = 1
#i = 281
for(i in 1:NSETS){
  print(i)
  # this_approved_sims <- simulate(seirInflect, params = approved_params[i, ] %>% unlist
  #                                , seed = i, nsim = NSIM, include.data = F, format = "data.frame")
  
  this_approved_sims_pompobj <- simulate(seirInflect, params = approved_params[these_paramsets[i], names(seirInflect@params)] %>% unlist
                                         , seed = i, nsim = NSIM, include.data = F)
  
  ll <- this_approved_sims_pompobj %>% {dmeasure(.
                                                 , y = obs(seirInflect)
                                                 , x = states(.)
                                                 , params = .@params
                                                 , times = seirInflect@times
                                                 , log = T)} %>% sum
  
  this_approved_sims <- this_approved_sims_pompobj %>% as.data.frame %>% transmute(Date
                                                                                   , rep = i #as.numeric(.id)
                                                                                   , pos
                                                                                   , SAR_numer
                                                                                   # , DailyInc
                                                                                   , Ninfected
                                                                                   # , presymptomatic = (E + ET) * seirInflect@params["psi"] + Es + EsT
                                                                                   # , asymptomatic = (E + ET) * (1 - seirInflect@params["psi"]) + Ea + EaT + Ia + IaT
                                                                                   # , symptomatic = Is + IsT
                                                                                   # , presymptomatic_tested = (EToday) * seirInflect@params["psi"] + EsToday
                                                                                   # , asymptomatic_tested = (EToday) * (1 - seirInflect@params["psi"]) + EaToday + IaToday
                                                                                   # , symptomatic_tested = IsToday
                                                                                   # 
                                                                                   # , cumulative_untested = Undetected
                                                                                   
                                                                                   # , Ea, Es, EaT, EsT
                                                                                   # , Ia, Is, IaT, IsT
                                                                                   # , N
                                                                                   
                                                                                   # , pAsymptomatic_undetected = (Ea + Ia + EaT*(1-seirInflect@params["Zea"]) + IaT*(1-seirInflect@params["Zia"]))/N
                                                                                   # , pSymptomatic_undetected = (Es + Is + EsT*(1-seirInflect@params["Zes"]) + IsT*(1-seirInflect@params["Zis"]))/N
                                                                                   # , pAsymptomatic_detected = (EaT*seirInflect@params["Zea"] + IaT*seirInflect@params["Zia"])/N
                                                                                   # , pSymptomatic_detected = (EsT*seirInflect@params["Zes"] + IsT*seirInflect@params["Zis"])/N
                                                                                   
                                                                                   , pAsymptomatic_undetected = (Ea + Ia + EaT*(1-seirInflect@params["Zea"]) + IaT*(1-seirInflect@params["Zia"]))
                                                                                   , pSymptomatic_undetected = (Es + Is + EsT*(1-seirInflect@params["Zes"]) + IsT*(1-seirInflect@params["Zis"]))
                                                                                   , pAsymptomatic_detected = (EaT*seirInflect@params["Zea"] + IaT*seirInflect@params["Zia"])
                                                                                   , pSymptomatic_detected = (EsT*seirInflect@params["Zes"] + IsT*seirInflect@params["Zis"])
                                                                                   
                                                                                   
                                                                                   
                                                                                   # , presymptomatic_caught = EToday*seirInflect@params["psi"]*seirInflect@params["Ze"] + 
                                                                                   #                           EsToday*seirInflect@params["Zes"]
                                                                                   # , symptomatic_caught = IsToday*seirInflect@params["Zis"]
                                                                                   # , asymptomatic_caught = EToday*(1-seirInflect@params["psi"])*seirInflect@params["Ze"] + 
                                                                                   #                         EaToday*seirInflect@params["Zea"] + 
                                                                                   #                         IaToday*seirInflect@params["Zia"]
                                                                                   # 
                                                                                   # , symptomatic_missed = sympt_untested_rec + 
                                                                                   #                        E_untested_dis*seirInflect@params["psi"] + 
                                                                                   #                        sympt_untested_dis + 
                                                                                   #                        EToday*seirInflect@params["psi"]*(1-seirInflect@params["Ze"]) + 
                                                                                   #                        EsToday*(1-seirInflect@params["Zes"]) + 
                                                                                   #                        IsToday*(1-seirInflect@params["Zis"])
                                                                                   # , asymptomatic_missed = asympt_untested_rec + 
                                                                                   #                         E_untested_dis*(1-seirInflect@params["psi"]) + 
                                                                                   #                         asympt_untested_dis + 
                                                                                   #                         EToday*(1-seirInflect@params["psi"])*(1-seirInflect@params["Ze"]) + 
                                                                                   #                         EaToday*(1-seirInflect@params["Zea"]) + 
                                                                                   #                         IaToday*(1-seirInflect@params["Zia"])
                                                                                   # 
                                                                                   # , infected_retest = retest
                                                                                   
                                                                                   
                                                                                   
  )
  
  this_approved_sims_meta <- data.frame(rep = i #as.numeric(.id)
                                        , seed_index = these_paramsets[i]
                                        , beta1 = approved_params$beta1[these_paramsets[i]]
                                        , beta2 = approved_params$beta2[these_paramsets[i]]
                                        , t_init = approved_params$t_init[these_paramsets[i]]
                                        , E_init = approved_params$E_init[these_paramsets[i]]
                                        , t_inflect = approved_params$t_inflect[these_paramsets[i]]
                                        , ll = ll)
  
  if(i == 1){
    these_approved_sims2 = this_approved_sims
    these_approved_sims_meta = this_approved_sims_meta
  } else {
    these_approved_sims2 = rbind(these_approved_sims2, this_approved_sims)
    these_approved_sims_meta = rbind(these_approved_sims_meta, this_approved_sims_meta)
  }
  
}

these_approved_sims2 %<>% left_join(these_approved_sims_meta) %>%
  mutate(seed_index = rep)

best_id = these_approved_sims2 %>% filter(ll == max(ll)) %>% pull(rep) %>% unique %>% {.[1]}





#### plot these together

onephase = obs_df1 %>% 
  transmute(Date, Data = pos) %>% 
  filter(Date >= as.Date("2020-03-01")) %>% 
  left_join(
    these_approved_sims1 %>%
      # new_sims %>% 
      group_by(seed_index, rep) %>% 
      mutate(not_extinct = any(SAR_numer >= SAR_numer_threshold)) %>% 
      filter(not_extinct) %>% 
      ungroup %>% 
      group_by(Date) %>% 
      summarise(lowCI = quantile(pos, 0.025)
                , highCI = quantile(pos, 0.975)
                , median = quantile(pos, 0.5)
                , max = quantile(pos, 1)
      )) %>% 
  mutate(diff = Data - median
         , abs_diff = abs(diff)
         , rat = median/Data
         , lowCI_transposed = lowCI - Data
         , highCI_transposed = highCI - Data
         , median_transposed = median - Data
  )

twophase = obs_df2 %>% 
  transmute(Date, Data = pos) %>% 
  filter(Date >= as.Date("2020-03-01")) %>% 
  left_join(
    these_approved_sims2 %>%
      # new_sims %>% 
      group_by(seed_index, rep) %>% 
      mutate(not_extinct = any(SAR_numer >= SAR_numer_threshold)) %>% 
      filter(not_extinct) %>% 
      ungroup %>% 
      group_by(Date) %>% 
      summarise(lowCI = quantile(pos, 0.025)
                , highCI = quantile(pos, 0.975)
                , median = quantile(pos, 0.5)
                , max = quantile(pos, 1)
      )) %>% 
  mutate(diff = Data - median
         , abs_diff = abs(diff)
         , rat = median/Data
         , lowCI_transposed = lowCI - Data
         , highCI_transposed = highCI - Data
         , median_transposed = median - Data
  )

sim_prev_plot3_relative <- obs_df2 %>%
  transmute(Date, Data = pos) %>%
  filter(Date >= as.Date("2020-03-01")) %>%
  left_join(
    these_approved_sims2 %>%
      # new_sims %>%
      group_by(seed_index, rep) %>% mutate(not_extinct = any(SAR_numer >= SAR_numer_threshold)) %>% filter(not_extinct) %>% ungroup %>%
      group_by(Date) %>%
      summarise(across(contains("detected"), median))
  ) %>% 
  # pivot_longer(-c("Date", "Data"), names_sep = "_", values_to = "Patients", names_to = c("Disease", "Detected")) %>% 
  pivot_longer(contains("detected"), values_to = "Prevalence", names_to = c("Fate")) %>% 
  mutate(Fate = gsub("^p", "prev. ", Fate)) %>% 
  mutate(Fate = gsub("_", "\n", Fate)) %>% 
  # mutate(Fate = gsub("_", "\n", Fate)) %>% 
  mutate(Fate = factor(Fate, levels = c("prev. Asymptomatic\nundetected"
                                        , "prev. Symptomatic\nundetected"
                                        , "prev. Asymptomatic\ndetected"
                                        , "prev. Symptomatic\ndetected"))) %>% 
  # ))) %>% 
  mutate(relDate = Date - as.numeric(as.Date("2020-03-11"))) %>% 
  # mutate(lab = "Whole hospital") %>% 
  # transmute(Date, relDate, as.Date(Date, origin = "1970-01-01")) %>% print(n = 100)
  # ggplot(aes(x = as.Date(Date, origin = "1970-01-01"), y = Prevalence, fill = Fate, alpha = Fate)) + geom_area() + 
  ggplot(aes(x = relDate, y = Prevalence, fill = Fate, alpha = Fate)) + geom_area() + 
  # facet_grid(.~lab) + 
  scale_fill_manual(values = c(`prev. Asymptomatic\nundetected` = "green"
                               , `prev. Symptomatic\nundetected` = "blue"
                               , `prev. Asymptomatic\ndetected` = "green"
                               , `prev. Symptomatic\ndetected` = "blue")) + 
  scale_alpha_manual(values = c(`prev. Asymptomatic\nundetected` = 0.3
                                , `prev. Symptomatic\nundetected` = 0.3
                                , `prev. Asymptomatic\ndetected` = 1
                                , `prev. Symptomatic\ndetected` = 1)) + 
  labs(x = "relative date", y = "Prevalent cases", fill = "", alpha = "") +
  theme_bw() + theme(text = element_text(size = 20)
                     , legend.text = element_text(size = 10)
                     , legend.position = c(0.8, 0.9)
                     , legend.background = element_rect(fill=NA)
                     # size=0.5, linetype="solid"
                     # , colour = "black"
  ) 

sim_prev_plot3_relative



# basic_sims <- rbind(onephase %>% mutate(model = "One phase", fig_label = "a")
#                     , twophase %>% mutate(model = "Two phase", fig_label = "b")) %>%
#   ggplot(aes(x = as.Date(Date, origin = "1970-01-01"))) + 
#   
#   # geom_bar(aes(y = Data, fill = "Data"), stat = "identity") + 
#   geom_ribbon(aes(ymin = lowCI, ymax = highCI), alpha = 0.6, fill = "grey") +
#   geom_line(aes(y = highCI, colour = "CI", size = "CI", linetype = "CI")) +
#   geom_line(aes(y = lowCI, colour = "CI", size = "CI", linetype = "CI")) +
#   # geom_line(aes(y = max_logLik, colour = "bestFit", size = "bestFit", linetype = "bestFit")) +
#   geom_line(aes(y = median, colour = "median", size = "median", linetype = "median")) +
#   geom_point(aes(y = Data, colour = "Data", size = "Data", linetype = "Data")) + 
#   scale_colour_manual(values = c(  Data = "red"   , bestFit = "blue", median = "black" , mode = "blue", CI = "grey")) +
#   scale_linetype_manual(values = c(Data = "blank", bestFit = "dashed", median = "dashed", mode = "dotted", CI = "solid")) +
#   scale_size_manual(values = c(    Data = 2       , bestFit = 1, median = 1, mode = 1       , CI = 1)) +
#   # guides(fill = "Data") +
#   labs(x = "", y = "Positive tests", linetype = "Simulations", size = "Simulations", colour = "Simulations") + 
#   facet_wrap(.~model) + 
#   geom_text(aes(x = as.Date("2020-03-01"), y = 32, label = fig_label), size = 20) + 
#   theme_bw() + theme(text = element_text(size = 20)
#                      , legend.position = c(0.1, 0.80)
#                      , legend.background = element_rect(fill="white",
#                                                         size=0.5, linetype="solid"
#                                                         # , colour = "black"
#                      )
#                      , strip.background = element_blank()
#                      , strip.text.x = element_blank()
#   ) + 
#   coord_cartesian(ylim = c(0, 32))
# 
# basic_sims_relative <- rbind(twophase %>% mutate(model = "Two phase", fig_label = "b")
#                              , onephase %>% mutate(model = "One phase", fig_label = "a")) %>%
#   mutate(relDate = Date - as.numeric(as.Date("2020-03-11"))) %>% 
#   # ggplot(aes(x = as.Date(Date, origin = "1970-01-01"))) + 
#   ggplot(aes(x = relDate)) +
#   # geom_bar(aes(y = Data, fill = "Data"), stat = "identity") + 
#   geom_ribbon(aes(ymin = lowCI, ymax = highCI), alpha = 0.6, fill = "grey") +
#   geom_line(aes(y = highCI, colour = "CI", size = "CI", linetype = "CI")) +
#   geom_line(aes(y = lowCI, colour = "CI", size = "CI", linetype = "CI")) +
#   # geom_line(aes(y = max_logLik, colour = "bestFit", size = "bestFit", linetype = "bestFit")) +
#   geom_line(aes(y = median, colour = "median", size = "median", linetype = "median")) +
#   geom_point(aes(y = Data, colour = "Data", size = "Data", linetype = "Data")) + 
#   # scale_colour_manual(values = c(  Data = "red"   , bestFit = "blue", median = "black" , mode = "blue", CI = "grey")) +
#   # scale_linetype_manual(values = c(Data = "blank", bestFit = "dashed", median = "dashed", mode = "dotted", CI = "solid")) +
#   # scale_size_manual(values = c(    Data = 2       , bestFit = 1, median = 1, mode = 1       , CI = 1)) +
#   scale_colour_manual(values = c(  Data = "red"   , median = "black" , CI = "grey")) +
#   scale_linetype_manual(values = c(Data = "blank", median = "dashed", CI = "solid")) +
#   scale_size_manual(values = c(    Data = 2       , median = 1, CI = 1)) +
#   # guides(fill = "Data") +
#   labs(x = "relative date", y = "Positive tests", linetype = "Simulations", size = "Simulations", colour = "Simulations") + 
#   facet_wrap(.~model) + 
#   # geom_text(aes(x = as.Date("2020-03-01"), y = 32, label = fig_label), size = 20) + 
#   theme_bw() + theme(text = element_text(size = 20)
#                      , legend.position = c(0.1, 0.80)
#                      , legend.background = element_rect(fill="white",
#                                                         size=0.5, linetype="solid"
#                                                         # , colour = "black"
#                      )
#                      # , strip.background = element_blank()
#                      # , strip.text.x = element_blank()
#   ) + 
#   coord_cartesian(ylim = c(0, 32))

one_phase_sims <- rbind(onephase %>% mutate(model = "One phase", fig_label = "a")) %>%
  mutate(relDate = Date - as.numeric(as.Date("2020-03-11"))) %>% 
  # ggplot(aes(x = as.Date(Date, origin = "1970-01-01"))) + 
  ggplot(aes(x = relDate)) +
  # geom_bar(aes(y = Data, fill = "Data"), stat = "identity") + 
  geom_ribbon(aes(ymin = lowCI, ymax = highCI), alpha = 0.6, fill = "grey") +
  geom_line(aes(y = highCI, colour = "CI", size = "CI", linetype = "CI")) +
  geom_line(aes(y = lowCI, colour = "CI", size = "CI", linetype = "CI")) +
  # geom_line(aes(y = max_logLik, colour = "bestFit", size = "bestFit", linetype = "bestFit")) +
  geom_line(aes(y = median, colour = "median", size = "median", linetype = "median")) +
  geom_point(aes(y = Data, colour = "Data", size = "Data", linetype = "Data")) + 
  # scale_colour_manual(values = c(  Data = "red"   , bestFit = "blue", median = "black" , mode = "blue", CI = "grey")) +
  # scale_linetype_manual(values = c(Data = "blank", bestFit = "dashed", median = "dashed", mode = "dotted", CI = "solid")) +
  # scale_size_manual(values = c(    Data = 2       , bestFit = 1, median = 1, mode = 1       , CI = 1)) +
  scale_colour_manual(values = c(  Data = "red"   , median = "black" , CI = "grey")) +
  scale_linetype_manual(values = c(Data = "blank", median = "solid", CI = "solid")) +
  scale_size_manual(values = c(    Data = 2       , median = 1, CI = 1)) +
  # guides(fill = "Data") +
  labs(x = "relative date", y = "Positive tests", linetype = "Simulations", size = "Simulations", colour = "Simulations") + 
  # facet_wrap(.~model) + 
  # geom_text(aes(x = as.Date("2020-03-01"), y = 32, label = fig_label), size = 20) + 
  theme_bw() + theme(text = element_text(size = 20)
                     , legend.position = c(0.1, 0.80)
                     , legend.background = element_rect(fill="white",
                                                        size=0.5, linetype="solid"
                                                        # , colour = "black"
                     )
                     # , strip.background = element_blank()
                     # , strip.text.x = element_blank()
  ) + 
  coord_cartesian(ylim = c(0, 32))

two_phase_sims <- rbind(twophase %>% mutate(model = "One phase", fig_label = "a")) %>%
  mutate(relDate = Date - as.numeric(as.Date("2020-03-11"))) %>% 
  # ggplot(aes(x = as.Date(Date, origin = "1970-01-01"))) + 
  ggplot(aes(x = relDate)) +
  # geom_bar(aes(y = Data, fill = "Data"), stat = "identity") + 
  geom_ribbon(aes(ymin = lowCI, ymax = highCI), alpha = 0.6, fill = "grey") +
  geom_line(aes(y = highCI, colour = "CI", size = "CI", linetype = "CI")) +
  geom_line(aes(y = lowCI, colour = "CI", size = "CI", linetype = "CI")) +
  # geom_line(aes(y = max_logLik, colour = "bestFit", size = "bestFit", linetype = "bestFit")) +
  geom_line(aes(y = median, colour = "median", size = "median", linetype = "median")) +
  geom_point(aes(y = Data, colour = "Data", size = "Data", linetype = "Data")) + 
  # scale_colour_manual(values = c(  Data = "red"   , bestFit = "blue", median = "black" , mode = "blue", CI = "grey")) +
  # scale_linetype_manual(values = c(Data = "blank", bestFit = "dashed", median = "dashed", mode = "dotted", CI = "solid")) +
  # scale_size_manual(values = c(    Data = 2       , bestFit = 1, median = 1, mode = 1       , CI = 1)) +
  scale_colour_manual(values = c(  Data = "red"   , median = "black" , CI = "grey")) +
  scale_linetype_manual(values = c(Data = "blank", median = "solid", CI = "solid")) +
  scale_size_manual(values = c(    Data = 2       , median = 1, CI = 1)) +
  # guides(fill = "Data") +
  labs(x = "relative date", y = "Positive tests", linetype = "Simulations", size = "Simulations", colour = "Simulations") + 
  # facet_wrap(.~model) + 
  # geom_text(aes(x = as.Date("2020-03-01"), y = 32, label = fig_label), size = 20) + 
  theme_bw() + theme(text = element_text(size = 20)
                     , legend.position = c(0.1, 0.80)
                     , legend.background = element_rect(fill="white",
                                                        size=0.5, linetype="solid"
                                                        # , colour = "black"
                     )
                     # , strip.background = element_blank()
                     # , strip.text.x = element_blank()
  ) + 
  coord_cartesian(ylim = c(0, 32))



one_phase_sims
two_phase_sims

fig_scale = 2

ggsave(one_phase_sims + theme(legend.position = c(0.2, 0.8))
       , filename = paste0(figures_folder, "Fig3A", ".jpg")
       # , units = "cm", width = 40, height = 20
       , device = "jpeg"
       , height = 7/7, width = 7/7, units = "cm", scale = fig_scale*7, dpi = 600
)
ggsave(two_phase_sims + theme(legend.position = "none")
       , filename = paste0(figures_folder, "Fig3B", ".jpg")
       # , units = "cm", width = 40, height = 20
       , device = "jpeg"
       , height = 7, width = 7, units = "cm", scale = fig_scale, dpi = 600
)

ggsave(sim_prev_plot3_relative
       , filename = paste0(figures_folder, "Fig4A",".jpg")
       # , units = "cm", width = 40, height = 20
       , device = "jpeg"
       , height = 7, width = 7, units = "cm", scale = fig_scale, dpi = 600
)



# Ward-level figures (3C-3F, 4B-4E) #

pomp_sim_distribution <- function(pompModel, param_df, NSIM = 10, SAR_numer_threshold = 3, calculate_logLik = F){
  
  # extract the original observations as a data frame
  obs_df <- pompModel %>% simulate(params = pompModel@params
                                   , seed = 1, nsim = 1, include.data = T, format = "data.frame") %>% 
    filter(.id == "data")
  
  ### make the simulations
  
  if(nrow(param_df) == 1){
    # simulate single
    
    sims <- pompModel %>% simulate(params = param_df %>% unlist
                                   , seed = 1, nsim = NSIM, include.data = F, format = "data.frame")
    
    if(calculate_logLik){
      stop("Can't calculate logLik for a single parameter set yet")
    }
    
    
  } else {
    # simulate range
    
    sampled_rows = sample(nrow(param_df), size = NSIM, replace = NSIM > nrow(param_df))
    sampled_param_df = param_df[sampled_rows, ]
    
    for(i in 1:NSIM){
      sims_piece <- simulate(pompModel, params = sampled_param_df[i, ] %>% unlist
                             , seed = i, nsim = 1, include.data = F, format = "data.frame") %>% 
        mutate(.id = i)
      
      if(calculate_logLik){
        sims_obj <- simulate(pompModel, params = sampled_param_df[i, ] %>% unlist
                             , seed = i, nsim = 1, include.data = F)
        
        
        sims_piece %<>% mutate(logLik = dmeasure(pompModel
                                                 , x = states(sims_obj)
                                                 , y = obs(pompModel)
                                                 , params = sims_obj@params
                                                 , times = pompModel@times
                                                 , log = T) %>% as.vector
        )
        
      }
      
      if(i == 1){
        sims = sims_piece
      } else {
        sims = rbind(sims, sims_piece)
      }
    }
    
    
    
    
    
  }
  
  if(calculate_logLik){
    sims_filtered <- sims %>% 
      group_by(.id) %>% 
      mutate(not_extinct = any(SAR_numer >= SAR_numer_threshold)) %>% 
      filter(not_extinct) %>% 
      ungroup %>% 
      select(-not_extinct)
    
    max_logLik_id <- sims_filtered %>% 
      group_by(.id) %>% 
      summarise(logLik = sum(logLik)) %>% 
      filter(logLik == max(logLik)) %>% 
      pull(.id)
    
    sims_filtered %>% 
      group_by(Date) %>% 
      summarise(lowCI = quantile(pos, 0.025)
                , highCI = quantile(pos, 0.975)
                , median = quantile(pos, 0.5)
                , max = quantile(pos, 1)) %>% 
      left_join(obs_df %>% transmute(Date, Data = pos)) %>% 
      left_join(sims_filtered %>% 
                  filter(.id == max_logLik_id) %>% 
                  transmute(Date, max_logLik = pos))
  } else {
    sims %>% 
      group_by(.id) %>% 
      mutate(not_extinct = any(SAR_numer >= SAR_numer_threshold)) %>% 
      filter(not_extinct) %>% 
      ungroup %>% 
      select(-not_extinct) %>% 
      
      mutate(pAsymptomatic_undetected = (Ea + Ia + EaT*(1-pompModel@params["Zea"]) + IaT*(1-pompModel@params["Zia"]))
             , pSymptomatic_undetected = (Es + Is + EsT*(1-pompModel@params["Zes"]) + IsT*(1-pompModel@params["Zis"]))
             , pAsymptomatic_detected = (EaT*pompModel@params["Zea"] + IaT*pompModel@params["Zia"])
             , pSymptomatic_detected = (EsT*pompModel@params["Zes"] + IsT*pompModel@params["Zis"])
      ) %>% 
      
      group_by(Date) %>% 
      summarise(lowCI = quantile(pos, 0.025)
                , highCI = quantile(pos, 0.975)
                , median = quantile(pos, 0.5)
                , max = quantile(pos, 1)
                , pAsymptomatic_undetected = median(pAsymptomatic_undetected)
                , pSymptomatic_undetected = median(pAsymptomatic_undetected)
                , pAsymptomatic_detected = median(pAsymptomatic_detected)
                , pSymptomatic_detected = median(pAsymptomatic_detected)
                # , pAsymptomatic_undetected = mean(pAsymptomatic_undetected)
                # , pSymptomatic_undetected = mean(pAsymptomatic_undetected)
                # , pAsymptomatic_detected = mean(pAsymptomatic_detected)
                # , pSymptomatic_detected = mean(pAsymptomatic_detected)
                , Ninfected_median = median(Ninfected)
                , Ninfected_mean = mean(Ninfected)
                
      ) %>% 
      left_join(obs_df %>% transmute(Date, Data = pos))
    
  }
  
  
  
}

pomp_sim_plot_relative <- function(res_ward, pompModel_source, NSIM = 10, SAR_numer_threshold = 3, detection_plot = F, calculate_logLik = F){
  
  sim_distribution_df = NULL
  wardLetter = "A"
  floor = 2
  for(wardLetter in c("A", "B", "C")){
    # print(wardLetter)
    
    for(floor in 0:3){
      this_wardCode = paste0(wardLetter, floor)
      # print(floor)
      
      
      if(this_wardCode %in% res_ward$wardCode %>% unique){
        print(this_wardCode)
        
        this_ward = paste0(wardLetter, "_", floor)
        
        source(pompModel_source)
        pompModel = call_pomp_ward(this_ward)
        
        
        sim_distribution_df_piece = pomp_sim_distribution(pompModel = pompModel
                                                          , param_df = res_ward %>% 
                                                            filter(ci == "in_ci") %>% 
                                                            filter(wardCode == this_wardCode) %>% 
                                                            # {.[which(.$loglik == max(.$loglik, na.rm = T))[1], ]} %>% 
                                                            select(names(pompModel@params))
                                                          , NSIM = NSIM
                                                          , SAR_numer_threshold = SAR_numer_threshold
                                                          , calculate_logLik = calculate_logLik) %>% 
          mutate(wardCode = this_wardCode)
        
        # print("done distribution")
        
        if(is.null(sim_distribution_df)){
          sim_distribution_df = sim_distribution_df_piece
        } else {
          sim_distribution_df = rbind(sim_distribution_df, sim_distribution_df_piece)
        }
        
        # print("binded")
      }
    }
  }
  
  print(c("calculate_loglik", calculate_logLik))
  
  if(detection_plot){
    
    sim_plot = sim_distribution_df %>% 
      filter(Date > as.Date("2020-03-01")) %>% 
      pivot_longer(contains("detected"), values_to = "Prevalence", names_to = c("Fate")) %>% 
      mutate(wardCode_label = paste("ward", wardCode)) %>% 
      # mutate(Fate = gsub("_", "\n", Fate)) %>% 
      mutate(Fate = factor(Fate, levels = c("pAsymptomatic_undetected"
                                            , "pSymptomatic_undetected"
                                            , "pAsymptomatic_detected"
                                            , "pSymptomatic_detected"
      ))) %>% 
      mutate(relDate = Date - as.numeric(as.Date("2020-03-11"))) %>% 
      ggplot(aes(x = relDate, y = Prevalence, fill = Fate, alpha = Fate)) + geom_area() + 
      # ggplot(aes(x = as.Date(Date, origin = "1970-01-01"), y = Prevalence, fill = Fate, alpha = Fate)) + geom_area() + 
      scale_fill_manual(values = c(pAsymptomatic_undetected = "green"
                                   , pSymptomatic_undetected = "blue"
                                   , pAsymptomatic_detected = "green"
                                   , pSymptomatic_detected = "blue")) +
      scale_alpha_manual(values = c(pAsymptomatic_undetected = 0.3
                                    , pSymptomatic_undetected = 0.3
                                    , pAsymptomatic_detected = 1
                                    , pSymptomatic_detected = 1)) + 
      labs(x = "relative date", y = "Prevalent cases", fill = "", alpha = "") +
      theme_bw() + theme(text = element_text(size = 20)
                         , legend.text = element_text(size = 10)
                         # , legend.position = c(0.2, 0.80)
                         , legend.background = element_rect(fill=NA)
                         # size=0.5, linetype="solid"
                         # , colour = "black"
      ) + 
      # scale_y_continuous(labels = function(x) scales::percent(x = x, accuracy = 1)) + 
      # facet_wrap(.~wardCode, scales = "free_y")
      facet_wrap(.~wardCode_label, scales = "free_y")
    
  }
  else if(!calculate_logLik){
    sim_plot = sim_distribution_df %>% 
      filter(Date > as.Date("2020-03-01")) %>% 
      mutate(relDate = Date - as.numeric(as.Date("2020-03-11"))) %>% 
      mutate(wardCode_label = paste("ward", wardCode)) %>% 
      
      ggplot(aes(x = relDate)) + 
      # ggplot(aes(x = as.Date(Date, origin = "1970-01-01"))) + 
      # geom_bar(aes(y = Data, fill = "Data"), stat = "identity") + 
      geom_ribbon(aes(ymin = lowCI, ymax = highCI), alpha = 0.6, fill = "grey") +
      geom_line(aes(y = highCI, colour = "CI", size = "CI", linetype = "CI")) +
      geom_line(aes(y = lowCI, colour = "CI", size = "CI", linetype = "CI")) +
      geom_line(aes(y = median, colour = "median", size = "median", linetype = "median")) +
      geom_point(aes(y = Data, colour = "Data", size = "Data", linetype = "Data")) + 
      scale_colour_manual(values = c(  Data = "red"   , median = "black" , mode = "blue", CI = "grey")) +
      scale_linetype_manual(values = c(Data = "blank", median = "solid", mode = "dotted", CI = "solid")) +
      scale_size_manual(values = c(    Data = 2       , median = 1, mode = 1       , CI = 1)) +
      labs(x = "relative date", y = "Positive tests", linetype = "Simulations", size = "Simulations", colour = "Simulations") + 
      theme_bw() + theme(text = element_text(size = 20)
                         # , legend.position = c(0.8, 0.2)
                         , legend.background = element_rect(fill="white",
                                                            size=0.5, linetype="solid")
                         , axis.text.x = element_text(angle = 90)
                         
      ) + 
      # facet_wrap(.~wardCode, scales = "free_y")
      facet_wrap(.~wardCode_label, scales = "free_y")
    
  } else {
    sim_plot = sim_distribution_df %>% 
      filter(Date > as.Date("2020-03-01")) %>% 
      mutate(relDate = Date - as.numeric(as.Date("2020-03-11"))) %>% 
      mutate(wardCode_label = paste("ward", wardCode)) %>% 
      
      ggplot(aes(x = relDate)) + 
      # ggplot(aes(x = as.Date(Date, origin = "1970-01-01"))) + 
      # geom_bar(aes(y = Data, fill = "Data"), stat = "identity") + 
      geom_ribbon(aes(ymin = lowCI, ymax = highCI), alpha = 0.6, fill = "grey") +
      geom_line(aes(y = highCI, colour = "CI", size = "CI", linetype = "CI")) +
      geom_line(aes(y = lowCI, colour = "CI", size = "CI", linetype = "CI")) +
      geom_line(aes(y = max_logLik, colour = "bestFit", size = "bestFit", linetype = "bestFit")) +
      # geom_line(aes(y = median, colour = "median", size = "median", linetype = "median")) +
      geom_point(aes(y = Data, colour = "Data", size = "Data", linetype = "Data")) + 
      scale_colour_manual(values = c(  Data = "red"   , bestFit = "blue", median = "black" , mode = "blue", CI = "grey")) +
      scale_linetype_manual(values = c(Data = "blank", bestFit = "solid", median = "solid", mode = "dotted", CI = "solid")) +
      scale_size_manual(values = c(    Data = 2       , bestFit = 1, median = 1, mode = 1       , CI = 1)) +
      labs(x = "relative date", y = "Positive tests", linetype = "Simulations", size = "Simulations", colour = "Simulations") + 
      theme_bw() + theme(text = element_text(size = 20)
                         # , legend.position = c(0.8, 0.2)
                         , legend.background = element_rect(fill="white",
                                                            size=0.5, linetype="solid")
                         , axis.text.x = element_text(angle = 90)
                         
      ) + 
      # facet_wrap(.~wardCode, scales = "free_y")   
      facet_wrap(.~wardCode_label, scales = "free_y")
    
  }
  
  
  sim_plot
}

ward_exp_name = "seirRefresh_posneg ALLwardLetter Refresh Einit1 ABStrans"
ward_exp_name_prof = "seirRefresh_posneg ALLwardLetter Refresh betaprofile Einit1"


pars = 2
ci_interval <- 0.5*qchisq(df=pars,p=0.95)

res_ward_refresh <- rbind(read_delim(paste0("Results/", ward_exp_name, ".csv" ), delim = ";") %>% mutate(analysis = "beta2")
                          , read_delim(paste0("Results/", ward_exp_name_prof, ".csv" ), delim = ";") %>% mutate(analysis = "beta1profile"))%>% 
  mutate(R0 = beta*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/(delta*kappa3)))) %>% 
  mutate_at(c("t_init", "start_tinit"), function(x) as.Date(x, origin = "1970-01-01")) %>% 
  group_by(wardCode) %>% 
  mutate(ci_boundary = max(loglik) - ci_interval
         , ci =  ifelse(loglik > max(loglik) - ci_interval, "in_ci", "out_ci")) %>% 
  ungroup %>% 
  filter(wardCode %in% c("A2", "C0", "C2", "C3"))



ward_sim_plot3_relative_list = list()
ward_sim_prev_plot3_relative_list = list()
for(wC in res_ward_refresh$wardCode %>% unique){
  #takes a few minutes to run
  ward_sim_plot3_relative_list[[wC]] <- pomp_sim_plot_relative(res_ward = res_ward_refresh %>% filter(wC == wardCode)
                                                               , pompModel_source = "Code/Analysis/seirRefresh_source_ward.R"
                                                               , NSIM = 1000
                                                               , SAR_numer_threshold = 3
                                                               , detection_plot = F
                                                               , calculate_logLik = F)
  
  ward_sim_prev_plot3_relative_list[[wC]] <- pomp_sim_plot_relative(res_ward = res_ward_refresh %>% filter(wC == wardCode)
                                                                    , pompModel_source = "Code/Analysis/seirRefresh_source_ward.R"
                                                                    , NSIM = 1000
                                                                    , SAR_numer_threshold = 3
                                                                    , detection_plot = T
                                                                    , calculate_logLik = F)

}

ggsave(ward_sim_plot3_relative_list[["A2"]] + theme(legend.position = "none")
       , filename = paste0(figures_folder, "Fig3C.jpg")
       , device = "jpeg"
       , height = 7, width = 7, units = "cm", scale = fig_scale, dpi = 600
)
ggsave(ward_sim_plot3_relative_list[["C0"]] + theme(legend.position = "none")
       , filename = paste0(figures_folder, "Fig3D.jpg")
       , device = "jpeg"
       , height = 7, width = 7, units = "cm", scale = fig_scale, dpi = 600
)
ggsave(ward_sim_plot3_relative_list[["C2"]] + theme(legend.position = "none")
       , filename = paste0(figures_folder, "Fig3E.jpg")
       , device = "jpeg"
       , height = 7, width = 7, units = "cm", scale = fig_scale, dpi = 600
)
ggsave(ward_sim_plot3_relative_list[["C3"]] + theme(legend.position = "none")
       , filename = paste0(figures_folder, "Fig3F.jpg")
       , device = "jpeg"
       , height = 7, width = 7, units = "cm", scale = fig_scale, dpi = 600
)

ggsave(ward_sim_prev_plot3_relative_list[["A2"]] + theme(legend.position = "none")
       , filename = paste0(figures_folder, "Fig4B.jpg")
       , device = "jpeg"
       , height = 7, width = 7, units = "cm", scale = fig_scale, dpi = 600
)
ggsave(ward_sim_prev_plot3_relative_list[["C0"]] + theme(legend.position = "none")
       , filename = paste0(figures_folder, "Fig4C.jpg")
       , device = "jpeg"
       , height = 7, width = 7, units = "cm", scale = fig_scale, dpi = 600
)
ggsave(ward_sim_prev_plot3_relative_list[["C2"]] + theme(legend.position = "none")
       , filename = paste0(figures_folder, "Fig4D.jpg")
       , device = "jpeg"
       , height = 7, width = 7, units = "cm", scale = fig_scale, dpi = 600
)
ggsave(ward_sim_prev_plot3_relative_list[["C3"]] + theme(legend.position = "none")
       , filename = paste0(figures_folder, "Fig4E.jpg")
       , device = "jpeg"
       , height = 7, width = 7, units = "cm", scale = fig_scale, dpi = 600
)



###################
####  Table S2 ####
###################

pars = 2
ci_interval <- 0.5*qchisq(df=pars,p=0.95)

### Einit ###


t_res <- rbind(
  read_csv(paste0("Results/", "seirRefresh_posneg betaProfile Einit tinitMar5 seedFix", ".csv" ))
  , read_csv(paste0("Results/", "seirRefresh_posneg betaProfile Einit tinitFeb27 seedFix", ".csv" ))
  , read_csv(paste0("Results/", "seirRefresh_posneg betaProfile Einit tinitFeb20 seedFix", ".csv" ))
) %>% 
  # mutate(t_init = as.Date(t_init, origin = "1970-01-01")) %>% 
  mutate(across(contains("t_"), function(x) as.numeric(x - as.numeric(as.Date("2020-03-11"))))) %>% 
  mutate(R0 = beta*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/delta)))


t_tab <- t_res %>% 
  group_by(t_init) %>% 
  mutate(ci_boundary = max(loglik) - ci_interval
         , ci =  ifelse(loglik > max(loglik) - ci_interval, "in_ci", "out_ci")) %>% 
  filter(ci == "in_ci") %>%
  summarise(max_loglik = max(loglik)
            , bestbeta = beta[loglik == max(loglik)]
            , minbeta = min(beta)
            , maxbeta = max(beta)
            , bestR0 = R0[loglik == max(loglik)]
            , minR0 = min(R0)
            , maxR0 = max(R0)
            , bestEinit = E_init[loglik == max(loglik)]
            , minEinit = min(E_init)
            , maxEinit = max(E_init)
  ) %>% 
  mutate(AIC = 2*pars - 2*max_loglik) %>% 
  ungroup %>% 
  # mutate(across(contains("t_in"), function(x) gsub("^0", " ", format(x, "%d %b")))) %>% 
  
  mutate_if(is.numeric, function(x) format(round(x, 2), nsmall = 2)) %>%
  # mutate_at(c("bestR0", "minR0", "maxR0", "bestEinit", "minEinit", "maxEinit"), function(x) format(round(x, 1), nsmall = 1)) %>%
  transmute(t_init
            , beta = paste0(bestbeta, "\n(", minbeta, "-", maxbeta, ")")
            # , beta_factor = paste0(bestbeta_factor, " (", minbeta_factor, "-", maxbeta_factor, ")")
            
            , R0 = paste0(bestR0, "\n(", minR0, "-", maxR0, ")")
            
            , E_init = paste0(bestEinit, "\n(", minEinit, "-", maxEinit, ")")
            , AIC
            
  ) 

t_tab %>% write_csv(paste0(tables_folder, "TableS2", "_bottomhalf.csv"))


### tinit ###


E_res <- rbind(
  read_csv(paste0("Results/", "seirRefresh_posneg betaProfile tinit Einit1 seedFix", ".csv" ))
  , read_csv(paste0("Results/", "seirRefresh_posneg betaProfile tinit Einit3 seedFix", ".csv" ))
  , read_csv(paste0("Results/", "seirRefresh_posneg betaProfile tinit Einit10 seedFix", ".csv" ))
) %>% 
  # mutate(t_init = as.Date(t_init, origin = "1970-01-01")) %>% 
  mutate(across(contains("t_"), function(x) as.numeric(x - as.numeric(as.Date("2020-03-11"))))) %>% 
  mutate(R0 = beta*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/delta)))


E_tab <- E_res %>% 
  group_by(E_init) %>% 
  mutate(ci_boundary = max(loglik) - ci_interval
         , ci =  ifelse(loglik > max(loglik) - ci_interval, "in_ci", "out_ci")) %>% 
  filter(ci == "in_ci") %>%
  summarise(max_loglik = max(loglik)
            , bestbeta = beta[loglik == max(loglik)]
            , minbeta = min(beta)
            , maxbeta = max(beta)
            , bestR0 = R0[loglik == max(loglik)]
            , minR0 = min(R0)
            , maxR0 = max(R0)
            , besttinit = t_init[loglik == max(loglik)]
            , mintinit = min(t_init)
            , maxtinit = max(t_init)
            # , betaw = sum(beta*loglik)/sum(loglik)
            # , R0w = sum(R0*loglik)/sum(loglik)
  ) %>% 
  mutate(AIC = 2*pars - 2*max_loglik) %>% 
  mutate_if(is.numeric, function(x) format(round(x, 2), nsmall = 2)) %>%
  mutate(across(contains("t_in"), function(x) round(as.numeric(x - as.numeric(as.Date("2020-03-11")))))) %>%
  # mutate_at(c("bestR0", "minR0", "maxR0", "bestEinit", "minEinit", "maxEinit"), function(x) format(round(x, 1), nsmall = 1)) %>%
  transmute(E_init
            , beta = paste0(bestbeta, "\n(", minbeta, "-", maxbeta, ")")
            # , beta_factor = paste0(bestbeta_factor, " (", minbeta_factor, "-", maxbeta_factor, ")")
            
            , R0 = paste0(bestR0, "\n(", minR0, "-", maxR0, ")")
            
            , t_init = paste0(besttinit, "\n(", mintinit, "-", maxtinit, ")")
            , AIC
            
  ) %>% 
  mutate(E_init = as.numeric(E_init %>% trimws))

E_tab %>% write_csv(paste0(tables_folder, "TableS2", "_tophalf.csv"))

#########################
####  Table 2 and S3 ####
#########################


exp_name = "seirRefresh_posneg betaProfile tinit Einit1 seedFix"

res = read_delim(paste0("Results/", exp_name, ".csv" ))

final_date = as.Date("2020-04-30")

pars = 2
ci_interval <- 0.5*qchisq(df=pars,p=0.95)

res <- res %>% 
  mutate(R0 = beta*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/(delta*kappa3)))) %>% 
  mutate_at(c("t_init", "start_tinit"), function(x) as.Date(x, origin = "1970-01-01")) %>% 
  ungroup


rtable <- res %>%  
  mutate(ci_boundary = max(loglik) - ci_interval
         , ci =  ifelse(loglik > max(loglik) - ci_interval, "in_ci", "out_ci")
         , AIC = 2*pars - 2*max(loglik)) %>% 
  filter(ci == "in_ci") %>%
  group_by(E_init) %>% 
  summarise(max_loglik = max(loglik)
            , bestbeta = beta[loglik == max(loglik)]
            , minbeta = min(beta)
            , maxbeta = max(beta)
            , bestt_init = t_init[loglik == max(loglik)]
            , mint_init = min(t_init)
            , maxt_init = max(t_init)
            , bestR0 = R0[which(loglik == max(loglik))[1]]
            , minR0 = min(R0)
            , maxR0 = max(R0)
            , AIC = mean(AIC)
  ) %>% 
  ungroup %>% 
  # mutate(across(contains("t_in"), function(x) gsub("^0", " ", format(x, "%d %b")))) %>% 
  
  mutate_if(is.numeric, function(x) format(round(x, 2), nsmall = 2)) %>%
  mutate(across(contains("t_in"), function(x) round(as.numeric(x - as.numeric(as.Date("2020-03-11")))))) %>%
  # mutate_at(c("bestR0", "minR0", "maxR0", "bestEinit", "minEinit", "maxEinit"), function(x) format(round(x, 1), nsmall = 1)) %>%
  transmute(E_init
            , beta = paste0(bestbeta, "\n(", minbeta, "-", maxbeta, ")")
            # , beta_factor = paste0(bestbeta_factor, " (", minbeta_factor, "-", maxbeta_factor, ")")
            
            , R0 = paste0(bestR0, "\n(", minR0, "-", maxR0, ")")
            
            , t_init = paste0(bestt_init, "\n(", mint_init, "-", maxt_init, ")")
            , AIC
            
  ) %>% 
  mutate(E_init = as.numeric(E_init %>% trimws))


rtable %>% write_csv(paste0(tables_folder, "Table2", "_onephase.csv"))


################ fixed Einit ##########

res = NULL
for(Eini in c(1, 3, 10)){
  for(analysis in c("beta2", "beta1profile")){
    # exp_name = paste0("posneg Inflect ", analysis, " Einit", Eini)
    exp_name = paste0("seirInflect_posneg Inflect ", analysis, " Einit", Eini, " tinflect23")
    print(exp_name)
    res_piece = read_delim(paste0("Results/", exp_name, ".csv" ), delim = ";") %>% mutate(analysis = analysis)
    # res_piece = read_csv(paste0("~/tars/output/TOY/cat/seirInflect_", exp_name, ".csv" )) %>% mutate(analysis = analysis)
    if(is.null(res)){
      res = res_piece
    } else {
      res = rbind(res, res_piece)
    }
    
  }
}

final_date = as.Date("2020-04-30")

pars = 3
ci_interval <- 0.5*qchisq(df=pars,p=0.95)

res <- res %>% 
  mutate(R0_before = beta1*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/(delta*kappa3)))) %>% 
  mutate(R0_after = beta2*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/(delta*kappa3)))) %>% 
  mutate_at(c("t_inflect", "t_init", "start_tinit"), function(x) as.Date(x, origin = "1970-01-01")) %>% 
  mutate(time_phase1 = max(0, t_inflect - t_init)
         , time_phase2 = max(0, final_date - t_inflect)
         , R0 = (R0_before*time_phase1 + R0_after*time_phase2)/(time_phase1 + time_phase2)) %>%
  mutate(risk_ratio = beta2/beta1) %>% 
  group_by(E_init) %>% 
  mutate(ci_boundary = max(loglik) - ci_interval
         , ci =  ifelse(loglik > max(loglik) - ci_interval, "in_ci", "out_ci")) %>% 
  ungroup


itable <- res %>%  
  group_by(E_init) %>% 
  mutate(ci_boundary = max(loglik) - ci_interval
         , ci =  ifelse(loglik > max(loglik) - ci_interval, "in_ci", "out_ci")
         , AIC = 2*pars - 2*max(loglik)) %>% 
  filter(ci == "in_ci") %>%
  summarise(max_loglik = max(loglik)
            , bestbeta1 = beta1[loglik == max(loglik)]
            , minbeta1 = min(beta1)
            , maxbeta1 = max(beta1)
            , bestbeta2 = beta2[loglik == max(loglik)]
            , minbeta2 = min(beta2)
            , maxbeta2 = max(beta2)
            , bestt_init = t_init[loglik == max(loglik)]
            , mint_init = min(t_init)
            , maxt_init = max(t_init)
            , bestR0before = R0_before[which(loglik == max(loglik))[1]]
            , minR0before = min(R0_before)
            , maxR0before = max(R0_before)
            , bestR0after = R0_after[which(loglik == max(loglik))[1]]
            , minR0after = min(R0_after)
            , maxR0after = max(R0_after)
            , bestR0 = R0[which(loglik == max(loglik))[1]]
            , minR0 = min(R0)
            , maxR0 = max(R0)
            , bestrisk_ratio = risk_ratio[which(loglik == max(loglik))[1]]
            , minrisk_ratio = min(risk_ratio)
            , maxrisk_ratio = max(risk_ratio)
            
            , AIC = mean(AIC)
  ) %>% 
  ungroup %>% 
  # mutate(across(contains("t_in"), function(x) gsub("^0", " ", format(x, "%d %b")))) %>% 
  
  mutate_if(is.numeric, function(x) format(round(x, 2), nsmall = 2)) %>%
  mutate(across(contains("t_in"), function(x) round(as.numeric(x - as.numeric(as.Date("2020-03-11")))))) %>% 
  # mutate_at(c("bestR0", "minR0", "maxR0", "bestEinit", "minEinit", "maxEinit"), function(x) format(round(x, 1), nsmall = 1)) %>%
  transmute(E_init
            , beta1 = paste0(bestbeta1, "\n(", minbeta1, "-", maxbeta1, ")")
            # , beta_factor = paste0(bestbeta_factor, " (", minbeta_factor, "-", maxbeta_factor, ")")
            , beta2 = paste0(bestbeta2, "\n(", minbeta2, "-", maxbeta2, ")")
            
            , R0_before = paste0(bestR0before, "\n(", minR0before, "-", maxR0before, ")")
            , R0_after = paste0(bestR0after, "\n(", minR0after, "-", maxR0after, ")")
            
            , R0 = paste0(bestR0, "\n(", minR0, "-", maxR0, ")")
            , risk_ratio = paste0(bestrisk_ratio, "\n(", minrisk_ratio, "-", maxrisk_ratio, ")")
            
            , t_init = paste0(bestt_init, "\n(", mint_init, "-", maxt_init, ")")
            , AIC
            
  ) %>% 
  mutate(E_init = as.numeric(E_init %>% trimws))

itable %>% 
  filter(E_init == 1) %>% 
  write_csv(paste0(tables_folder, "Table2", "_twophase.csv"))


itable %>% 
  write_csv(paste0(tables_folder, "TableS3", "_Einit.csv"))


################ fixed tinit ##########


res = NULL
for(tini in c("Feb20", "Feb27", "Mar5")){
  # for(tinf in c("Mar12", "Mar17", "Mar19")){
    for(analysis in c("beta2", "beta1profile")){
      exp_name = paste("seirInflect_posneg Inflect", analysis, tini, "tinflect23")
      
      res_piece = read_delim(paste0("Results/", exp_name, ".csv" ), delim = ";") %>% 
        mutate(analysis = analysis)
      
      if(is.null(res)){
        res = res_piece
      } else {
        res = rbind(res, res_piece)
      }
    }
}


pars = 3
ci_interval <- 0.5*qchisq(df=pars,p=0.95)

res <- res %>% 
  mutate(R0_before = beta1*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/(delta*kappa3)))) %>% 
  mutate(R0_after = beta2*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/(delta*kappa3)))) %>% 
  mutate_at(c("t_inflect", "t_init", "start_tinit"), function(x) as.Date(x, origin = "1970-01-01")) %>% 
  group_by(t_init, t_inflect) %>% 
  mutate(ci_boundary = max(loglik) - ci_interval
         , ci =  ifelse(loglik > max(loglik) - ci_interval, "in_ci", "out_ci")) %>% 
  ungroup %>% 
  mutate(across(contains("t_in"), function(x) round(as.numeric(x - as.numeric(as.Date("2020-03-11"))))))



itable <- res %>%  
  group_by(t_init, t_inflect) %>% 
  mutate(ci_boundary = max(loglik) - ci_interval
         , ci =  ifelse(loglik > max(loglik) - ci_interval, "in_ci", "out_ci")
         , AIC = 2*pars - 2*max(loglik)) %>% 
  filter(ci == "in_ci") %>%
  summarise(max_loglik = max(loglik)
            , bestbeta1 = beta1[loglik == max(loglik)]
            , minbeta1 = min(beta1)
            , maxbeta1 = max(beta1)
            , bestbeta2 = beta2[loglik == max(loglik)]
            , minbeta2 = min(beta2)
            , maxbeta2 = max(beta2)
            , bestEinit = E_init[loglik == max(loglik)]
            , minEinit = min(E_init)
            , maxEinit = max(E_init)
            , bestR0before = R0_before[which(loglik == max(loglik))[1]]
            , minR0before = min(R0_before)
            , maxR0before = max(R0_before)
            , bestR0after = R0_after[which(loglik == max(loglik))[1]]
            , minR0after = min(R0_after)
            , maxR0after = max(R0_after)
            , AIC = mean(AIC)
  ) %>% 
  ungroup %>% 
  # mutate(across(contains("t_in"), function(x) gsub("^0", " ", format(x, "%d %b")))) %>% 
  mutate_if(is.numeric, function(x) format(round(x, 2), nsmall = 2)) %>%
  # mutate_at(c("bestR0", "minR0", "maxR0", "bestEinit", "minEinit", "maxEinit"), function(x) format(round(x, 1), nsmall = 1)) %>%
  transmute(t_init
            , t_inflect
            , beta1 = paste0(bestbeta1, "\n(", minbeta1, "-", maxbeta1, ")")
            # , beta_factor = paste0(bestbeta_factor, " (", minbeta_factor, "-", maxbeta_factor, ")")
            , beta2 = paste0(bestbeta2, "\n(", minbeta2, "-", maxbeta2, ")")
            
            , R0_before = paste0(bestR0before, "\n(", minR0before, "-", maxR0before, ")")
            , R0_after = paste0(bestR0after, "\n(", minR0after, "-", maxR0after, ")")
            
            , E_init = ifelse(minEinit == maxEinit, minEinit, paste0(bestEinit, "\n(", minEinit, "-", maxEinit, ")"))
            , AIC
            
  ) %>% 
  mutate(E_init = gsub("^([ ]?[0-9]+)[.]0$", "\\1", E_init)) %>% 
  mutate(E_init = gsub("\\( ", "\\(", E_init))

itable %>% 
  write_csv(paste0(tables_folder, "TableS3", "_tinit.csv"))

##################
####  Table 3 ####
##################


ward_exp_name = "seirRefresh_posneg ALLwardLetter Refresh Einit1 ABStrans"
ward_exp_name_prof = "seirRefresh_posneg ALLwardLetter Refresh betaprofile Einit1"


pars = 2
ci_interval <- 0.5*qchisq(df=pars,p=0.95)

res_ward_refresh <- rbind(read_delim(paste0("Results/", ward_exp_name, ".csv" ), delim = ";") %>% mutate(analysis = "beta2")
                          , read_delim(paste0("Results/", ward_exp_name_prof, ".csv" ), delim = ";") %>% mutate(analysis = "beta1profile"))%>% 
  mutate(R0 = beta*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/(delta*kappa3)))) %>% 
  mutate_at(c("t_init", "start_tinit"), function(x) as.Date(x, origin = "1970-01-01")) %>% 
  group_by(wardCode) %>% 
  mutate(ci_boundary = max(loglik) - ci_interval
         , ci =  ifelse(loglik > max(loglik) - ci_interval, "in_ci", "out_ci")) %>% 
  ungroup %>% 
  filter(wardCode %in% c("A2", "C0", "C2", "C3"))

refresh_wtable <- res_ward_refresh %>% 
  group_by(wardCode, E_init) %>% 
  mutate(ci_boundary = max(loglik) - ci_interval
         , ci =  ifelse(loglik > max(loglik) - ci_interval, "in_ci", "out_ci")
         , AIC = 2*pars - 2*max(loglik)) %>% 
  filter(ci == "in_ci") %>%
  summarise(max_loglik = max(loglik)
            , bestbeta = beta[loglik == max(loglik)]
            , minbeta = min(beta)
            , maxbeta = max(beta)
            , bestt_init = t_init[loglik == max(loglik)]
            , mint_init = min(t_init)
            , maxt_init = max(t_init)
            , bestR0 = R0[which(loglik == max(loglik))[1]]
            , minR0 = min(R0)
            , maxR0 = max(R0)
            , AIC = mean(AIC)
  ) %>% 
  ungroup %>% 
  # mutate(across(contains("t_in"), function(x) gsub("^0", " ", format(x, "%d %b")))) %>% 
  mutate_if(is.numeric, function(x) format(round(x, 2), nsmall = 2)) %>%
  mutate(across(contains("t_in"), function(x) round(as.numeric(x - as.numeric(as.Date("2020-03-11")))))) %>% 
  # mutate_at(c("bestR0", "minR0", "maxR0", "bestEinit", "minEinit", "maxEinit"), function(x) format(round(x, 1), nsmall = 1)) %>%
  transmute(wardCode
            , E_init
            , beta = paste0(bestbeta, "\n(", minbeta, "-", maxbeta, ")")
            # , beta_factor = paste0(bestbeta_factor, " (", minbeta_factor, "-", maxbeta_factor, ")")
            , R0 = paste0(bestR0, "\n(", minR0, "-", maxR0, ")")
            
            , t_init = paste0(bestt_init, "\n(", mint_init, "-", maxt_init, ")")
            , AIC
            
  ) %>% 
  mutate(E_init = as.numeric(E_init %>% trimws))

refresh_wtable %>% write_csv(paste0(tables_folder, "Table3.csv"))

####################
####  Table S 4 ####
####################

res = NULL
for(exp_name in c("seirInflect_posneg Inflect beta2 Einit1 tinflect12"
                  , "seirInflect_posneg Inflect beta2 Einit1 tinflect17"
                  , "seirInflect_posneg Inflect beta2 Einit1"
                  , "seirInflect_posneg Inflect beta2 Einit1 tinflect21"
                  , "seirInflect_posneg Inflect beta2 Einit1 tinflect23"
                  , "seirInflect_posneg Inflect beta2 Einit1 tinflect25"
                  , "seirInflect_posneg Inflect beta2 Einit1 tinflect27"
                  , "seirInflect_posneg Inflect beta1profile Einit1 tinflect12"
                  , "seirInflect_posneg Inflect beta1profile Einit1 tinflect17"
                  , "seirInflect_posneg Inflect beta1profile Einit1"
                  , "seirInflect_posneg Inflect beta1profile Einit1 tinflect21"
                  , "seirInflect_posneg Inflect beta1profile Einit1 tinflect23"
                  , "seirInflect_posneg Inflect beta1profile Einit1 tinflect25"
                  , "seirInflect_posneg Inflect beta1profile Einit1 tinflect27")){
  res_piece = read_delim(paste0("Results/", exp_name, ".csv" ), delim = ";")
  
  
  if(is.null(res)){
    res = res_piece
  } else {
    res = rbind(res, res_piece)
  }
  
}



final_date = as.Date("2020-04-30")

pars = 3
ci_interval <- 0.5*qchisq(df=pars,p=0.95)

res <- res %>% 
  mutate(R0_before = beta1*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/(delta*kappa3)))) %>% 
  mutate(R0_after = beta2*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/(delta*kappa3)))) %>% 
  mutate_at(c("t_inflect", "t_init", "start_tinit"), function(x) as.Date(x, origin = "1970-01-01")) %>% 
  mutate(time_phase1 = max(0, t_inflect - t_init)
         , time_phase2 = max(0, final_date - t_inflect)
         , R0 = (R0_before*time_phase1 + R0_after*time_phase2)/(time_phase1 + time_phase2)) %>%
  mutate(risk_ratio = beta2/beta1) %>% 
  group_by(E_init) %>% 
  mutate(ci_boundary = max(loglik) - ci_interval
         , ci =  ifelse(loglik > max(loglik) - ci_interval, "in_ci", "out_ci")) %>% 
  ungroup


tinf_table <- res %>%  
  group_by(t_inflect) %>% 
  mutate(ci_boundary = max(loglik) - ci_interval
         , ci =  ifelse(loglik > max(loglik) - ci_interval, "in_ci", "out_ci")
         , AIC = 2*pars - 2*max(loglik)) %>% 
  filter(ci == "in_ci") %>%
  summarise(max_loglik = max(loglik)
            , bestbeta1 = beta1[loglik == max(loglik)]
            , minbeta1 = min(beta1)
            , maxbeta1 = max(beta1)
            , bestbeta2 = beta2[loglik == max(loglik)]
            , minbeta2 = min(beta2)
            , maxbeta2 = max(beta2)
            , bestt_init = t_init[loglik == max(loglik)]
            , mint_init = min(t_init)
            , maxt_init = max(t_init)
            , bestR0before = R0_before[which(loglik == max(loglik))[1]]
            , minR0before = min(R0_before)
            , maxR0before = max(R0_before)
            , bestR0after = R0_after[which(loglik == max(loglik))[1]]
            , minR0after = min(R0_after)
            , maxR0after = max(R0_after)
            , bestR0 = R0[which(loglik == max(loglik))[1]]
            , minR0 = min(R0)
            , maxR0 = max(R0)
            , bestrisk_ratio = risk_ratio[which(loglik == max(loglik))[1]]
            , minrisk_ratio = min(risk_ratio)
            , maxrisk_ratio = max(risk_ratio)
            
            , AIC = mean(AIC)
  ) %>% 
  ungroup %>% 
  mutate(across(contains("t_in"), function(x) gsub("^0", " ", format(x, "%d %b")))) %>% 
  mutate_if(is.numeric, function(x) format(round(x, 2), nsmall = 2)) %>%
  # mutate_at(c("bestR0", "minR0", "maxR0", "bestEinit", "minEinit", "maxEinit"), function(x) format(round(x, 1), nsmall = 1)) %>%
  transmute(t_inflect
            , beta1 = paste0(bestbeta1, "\n(", minbeta1, "-", maxbeta1, ")")
            # , beta_factor = paste0(bestbeta_factor, " (", minbeta_factor, "-", maxbeta_factor, ")")
            , beta2 = paste0(bestbeta2, "\n(", minbeta2, "-", maxbeta2, ")")
            
            , R0_before = paste0(bestR0before, "\n(", minR0before, "-", maxR0before, ")")
            , R0_after = paste0(bestR0after, "\n(", minR0after, "-", maxR0after, ")")
            
            , R0 = paste0(bestR0, "\n(", minR0, "-", maxR0, ")")
            , risk_ratio = paste0(bestrisk_ratio, "\n(", minrisk_ratio, "-", maxrisk_ratio, ")")
            
            , t_init = paste0(bestt_init, "\n(", mint_init, "-", maxt_init, ")")
            , AIC
            
  )

tinf_table %>% write_csv(paste0(tables_folder, "TableS4", ".csv"))


####################
####  Table S 5 ####
####################

final_date = as.numeric(as.Date("2020-04-30"))

exp_name = "seirInflect_posneg ALLward Inflect beta2 Einit1 ABStrans tinflect23"
exp_name_prof = "seirInflect_posneg ALLward Inflect beta1profile Einit1 ABStrans tinflect23"

pars = 3
ci_interval <- 0.5*qchisq(df=pars,p=0.95)

res_ward_inflect <- rbind(read_delim(paste0("Results/", exp_name, ".csv" ), delim = ";") %>% mutate(analysis = "beta2")
                          , read_delim(paste0("Results/", exp_name_prof, ".csv" ), delim = ";") %>% mutate(analysis = "beta1profile")
)%>%
  mutate(R0_before = beta1*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/(delta*kappa3)))) %>%
  mutate(R0_after = beta2*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/(delta*kappa3)))) %>%
  mutate(time_phase1 = max(0, t_inflect - t_init)
         , time_phase2 = max(0, final_date - t_inflect)
         , R0 = (R0_before*time_phase1 + R0_after*time_phase2)/(time_phase1 + time_phase2)) %>%
  # mutate(R0 = (R0_before*(t_inflect - t_init) + R0_after*(final_date - t_inflect))/(final_date - t_init)) %>%
  mutate(risk_ratio = beta2/beta1) %>% 
  mutate_at(c("t_inflect", "t_init", "start_tinit"), function(x) as.Date(x, origin = "1970-01-01")) %>%
  group_by(wardCode) %>%
  mutate(ci_boundary = max(loglik) - ci_interval
         , toplot = ifelse(loglik > ci_boundary - ci_interval, "Y", "N")
         , ci =  ifelse(loglik > max(loglik) - ci_interval, "in_ci", "out_ci")) %>%
  ungroup


inflect_wtable <- res_ward_inflect %>%  
  filter(wardCode %in% c("A2", "C0", "C2", "C3")) %>% 
  group_by(wardCode, E_init) %>% 
  mutate(ci_boundary = max(loglik) - ci_interval
         , ci =  ifelse(loglik > max(loglik) - ci_interval, "in_ci", "out_ci")
         , AIC = 2*pars - 2*max(loglik)) %>% 
  filter(ci == "in_ci") %>%
  summarise(max_loglik = max(loglik)
            , bestbeta1 = beta1[loglik == max(loglik)]
            , minbeta1 = min(beta1)
            , maxbeta1 = max(beta1)
            , bestbeta2 = beta2[loglik == max(loglik)]
            , minbeta2 = min(beta2)
            , maxbeta2 = max(beta2)
            , bestt_init = t_init[loglik == max(loglik)]
            , mint_init = min(t_init)
            , maxt_init = max(t_init)
            , bestt_inflect = t_inflect[loglik == max(loglik)]
            , mint_inflect = min(t_inflect)
            , maxt_inflect = max(t_inflect)
            , bestR0before = R0_before[which(loglik == max(loglik))[1]]
            , minR0before = min(R0_before)
            , maxR0before = max(R0_before)
            , bestR0after = R0_after[which(loglik == max(loglik))[1]]
            , minR0after = min(R0_after)
            , maxR0after = max(R0_after)
            , bestR0 = R0[which(loglik == max(loglik))[1]]
            , minR0 = min(R0)
            , maxR0 = max(R0)
            , bestrisk_ratio = risk_ratio[which(loglik == max(loglik))[1]]
            , minrisk_ratio = min(risk_ratio)
            , maxrisk_ratio = max(risk_ratio)
            
            , AIC = mean(AIC)
  ) %>% 
  ungroup %>% 
  # mutate(across(contains("t_in"), function(x) gsub("^0", " ", format(x, "%d %b")))) %>% 
  mutate_if(is.numeric, function(x) format(round(x, 2), nsmall = 2)) %>%
  mutate(across(contains("t_in"), function(x) round(as.numeric(x - as.numeric(as.Date("2020-03-11")))))) %>% 
  
  # mutate_at(c("bestR0", "minR0", "maxR0", "bestEinit", "minEinit", "maxEinit"), function(x) format(round(x, 1), nsmall = 1)) %>%
  transmute(wardCode
            , E_init
            , beta1 = paste0(bestbeta1, "\n(", minbeta1, "-", maxbeta1, ")")
            # , beta_factor = paste0(bestbeta_factor, " (", minbeta_factor, "-", maxbeta_factor, ")")
            , beta2 = paste0(bestbeta2, "\n(", minbeta2, "-", maxbeta2, ")")
            
            , R0_before = paste0(bestR0before, "\n(", minR0before, "-", maxR0before, ")")
            , R0_after = paste0(bestR0after, "\n(", minR0after, "-", maxR0after, ")")
            
            , R0 = paste0(bestR0, "\n(", minR0, "-", maxR0, ")")
            , risk_ratio = paste0(bestrisk_ratio, "\n(", minrisk_ratio, "-", maxrisk_ratio, ")")
            
            
            , t_inflect = paste0(bestt_inflect, "\n(", mint_inflect, "-", maxt_inflect, ")")
            , t_init = paste0(bestt_init, "\n(", mint_init, "-", maxt_init, ")")
            , AIC
            
  ) %>% 
  mutate(E_init = as.numeric(E_init %>% trimws))

inflect_wtable %>% write_csv(paste0(tables_folder, "TableS5.csv"))

#####################
####  Figure S 4 ####
#####################


quantile.date.or <- function(vec, prob){
  if(class(vec) == "Date"){
    num_vec = as.numeric(vec)
    num_vec %>% quantile(prob = prob) %>% as.Date(origin = "1970-01-01")
  } else {
    vec %>% quantile(prob = prob)
  }
}


experiment_name = "BiasTest_seirRefresh_posneg Refresh valid2param tinit extinction_threshold0"


res <- read_delim(paste0("Results/", experiment_name, ".csv"), delim = ";") %>% 
  # mutate(across(contains("t_"), function(x) as.Date(x, origin = "1970-01-01")))
  mutate(across(contains("t_"), function(x) as.numeric(x - as.numeric(as.Date("2020-03-11")))))

Sys.setlocale(category = "LC_TIME", locale = "English")
upper_limits <- list(beta = c(NA, 2)
                     , E_init = c(0.1, 100)
                     # , t_init = as.Date(c("2020-02-01", "2020-05-01"))
                     # , t_inflect = as.Date(c("2020-02-01", "2020-05-01"))
                     , t_init = c(-39, 51)
                     , t_inflect = c(-39, 51)
                     
)

pls <- list()
e = "beta"
true_labels = c(beta = expression(true~beta)
                , t_init = expression(true~t[init]~relative))
est_labels = c(beta = expression(estimate~beta)
               , t_init = expression(estimate~t[init]~relative))

e = 't_init'
for(e in c("beta"
           # , "beta2"
           # , "beta_factor"
           , "t_init"
           # , "E_init", "t_inflect"
)){
  
  pls[[e]] <-  res %>% 
    rename(`true:VAR` = paste0("true:", e)) %>% 
    # pull(`true:VAR`)
    group_by(`true:VAR`) %>% 
    summarise_(`mean:VAR` = paste0("mean(", e, ")")
               , `median:VAR` = paste0("median(", e, ")")
               , `lowci:VAR` = paste0("quantile.date.or(", e, ", prob = 0.025)")
               , `highci:VAR` = paste0("quantile.date.or(", e, ", prob = 0.975)")) %>% 
    ggplot(aes(x = `true:VAR`)) +
    geom_point(aes(y = `median:VAR`), size = 1, alpha = 0.2) +
    # geom_point(aes(y = `mean:VAR`, colour = "Mean"), size = 1) +
    geom_abline(slope = 1, intercept = 0) + 
    # geom_ribbon(aes(x = `true:VAR`, ymin = `lowci:VAR`, ymax = `highci:VAR`), colour = "grey", alpha = 0.2) +
    labs(title = "", colour = "", x = true_labels[e], y = est_labels[e])  + 
    coord_cartesian(ylim = upper_limits[[e]]) +
    guides(color = F) + 
    theme_bw() #+ 
  #theme(text = element_text(size = 20), legend.position = c(0.2, 0.8)
  # , legend.background = element_rect(fill="white", linetype="solid"))
  
}

# ggpubr::ggarrange(plotlist = pls, ncol = 2)

ggsave(pls[["beta"]]
       , filename = paste0(figures_folder, "SFig4A.jpg"), units = "cm", width = 7, height = 7, device = "jpeg", dpi = 600)

ggsave(pls[["t_init"]]
       , filename = paste0(figures_folder, "SFig4B.jpg"), units = "cm", width = 7, height = 7, device = "jpeg", dpi = 600)


#####################
####  Figure S 5 ####
#####################

experiment_name = "BiasTest_seirInflect_posneg Inflect valid3param tinit extinction_threshold0 tinflect23"

res <- read_delim(paste0("Results/", experiment_name, ".csv"), delim = ";") %>% 
  # mutate(across(contains("t_"), function(x) as.Date(x, origin = "1970-01-01")))
  mutate(across(contains("t_"), function(x) as.numeric(x - as.numeric(as.Date("2020-03-11")))))


upper_limits <- list(beta1 = c(NA, 5), beta2 = c(NA, 5), beta_factor = c(NA, 5), E_init = c(0.1, 100)
                     # , t_init = as.Date(c("2020-02-01", "2020-05-01"))
                     # , t_inflect = as.Date(c("2020-02-01", "2020-05-01"))
                     , t_init = c(-39, 51)
                     , t_inflect = c(-39, 51)
)

pls <- list()
e = "beta2"
true_labels = c(beta1 = expression(true~beta[1])
                , beta2 = expression(true~beta[2])
                , E_init = expression(true~E[init])
                , t_init = expression(true~t[init]~relative)
                , t_inflecgt = expression(true~t[inflect]~relative)
)
est_labels = c(beta1 = expression(estimate~beta[1])
               , beta2 = expression(estimate~beta[2])
               , E_init = expression(estimate~E[init])
               , t_init = expression(estimate~t[init]~relative)
               , t_inflect = expression(estimate~t[inflect]~relative)
)

e = 't_init'
for(e in c("beta1", "beta2"
           # , "beta_factor"
           , "t_init"
           # , "E_init", "t_inflect"
)){
  
  pls[[e]] <-  res %>% 
    rename(`true:VAR` = paste0("true:", e)) %>% 
    group_by(`true:VAR`) %>% 
    summarise_(`mean:VAR` = paste0("mean(", e, ")")
               , `median:VAR` = paste0("median(", e, ")")
               , `lowci:VAR` = paste0("quantile.date.or(", e, ", prob = 0.025)")
               , `highci:VAR` = paste0("quantile.date.or(", e, ", prob = 0.975)")) %>% 
    ggplot(aes(x = `true:VAR`)) +
    geom_point(aes(y = `median:VAR`), size = 1, alpha = 0.2) +
    # geom_point(aes(y = `mean:VAR`, colour = "Mean"), size = 1) +
    geom_abline(slope = 1, intercept = 0) + 
    # geom_ribbon(aes(x = `true:VAR`, ymin = `lowci:VAR`, ymax = `highci:VAR`), colour = "grey", alpha = 0.2) +
    labs(title = "", colour = "", x = true_labels[e], y = est_labels[e])  + 
    coord_cartesian(ylim = upper_limits[[e]]) +
    guides(color = F) + 
    theme_bw() #+ 
  # theme(text = element_text(size = 20), legend.position = c(0.2, 0.8)
  #       , legend.background = element_rect(fill="white", linetype="solid"))
  
}


# pls[["E_init"]] = pls[["E_init"]] + scale_x_log10() + scale_y_log10() + coord_cartesian(ylim = c(1, 100))
ggpubr::ggarrange(plotlist = pls, ncol = 3)

# ggsave(paste0("~/tars/output/Figs/", "validation_", experiment_name, ".png"), units = "cm", width = 30, height = 12)


ggsave(pls[["beta1"]]
       , filename = paste0(figures_folder, "SFig5A.jpg"), units = "cm", width = 7, height = 7, device = "jpeg", dpi = 600)
ggsave(pls[["beta2"]]
       , filename = paste0(figures_folder, "SFig5B.jpg"), units = "cm", width = 7, height = 7, device = "jpeg", dpi = 600)
ggsave(pls[["t_init"]]
       , filename = paste0(figures_folder, "SFig5C.jpg"), units = "cm", width = 7, height = 7, device = "jpeg", dpi = 600)


#######################
####  Figure S 6+7 ####
#######################


Sys.setlocale("LC_TIME", "English")

experiment_name = "BiasTest_seirRefresh_posnegWard Refresh valid2param tinit extinction_threshold0"

res <- read_delim(paste0("Results/", experiment_name, ".csv" ), delim = ";") %>% 
  mutate(across(contains("t_"), function(x) as.numeric(x - as.numeric(as.Date("2020-03-11")))))


res %>% 
  mutate(wardLetter = substring(wardCode, 1, 1)
         , Floor = substring(wardCode, 2, 2) %>% factor(levels = 3:0)) %>%  
  group_by(wardLetter, Floor, `true:beta`) %>% 
  mutate(beta_med = median(beta)
         , beta_lo = quantile(beta, 0.025)
         , beta_hi = quantile(beta, 0.975)) %>% 
  group_by(wardLetter, Floor) %>% 
  mutate(dev = beta/`true:beta`) %>% 
  
  # mutate(median_dev = median(dev[`true:beta`])) %>%
  mutate(median_dev = median(dev)) %>%
  
  mutate(dev_label = paste("Median ratio =", round(median_dev, digits = 2))) %>% 
  ggplot(aes(x = `true:beta`, label = dev_label)) + 
  # geom_point(aes(y = beta), alpha = 0.1, colour = "blue") +
  geom_ribbon(aes(ymin = beta_lo, ymax = beta_hi), colour = "lightgrey", alpha = 0.3) + 
  geom_line(aes(y = beta_med), linetype = "dashed") + 
  facet_grid(Floor~wardLetter) + 
  geom_abline(slope = 1, intercept = 0, linetype = "solid") + 
  geom_text(x = 0.2, y = 5.5, size = 3.5) + 
  labs(x = expression(true~beta), y = expression(estimated~beta)) + 
  coord_cartesian(ylim = c(0, 6)) +
  theme_bw() + 
  theme(text = element_text(size = 10))


ggsave(paste0(figures_folder, "SFig6.jpeg"), units = "cm", width = 15, height = 15, device = "jpeg", dpi = 300)

res %>% 
  mutate(wardLetter = substring(wardCode, 1, 1)
         , Floor = substring(wardCode, 2, 2) %>% factor(levels = 3:0)) %>%  
  group_by(wardLetter, Floor, `true:t_init`) %>% 
  mutate(t_init_med = median(t_init)
         , t_init_lo = quantile.date.or(t_init, 0.025)
         , t_init_hi = quantile.date.or(t_init, 0.975)) %>% 
  # mutate(across(contains("t_init"), function(x) as.Date(x, origin = "1970-01-01"))) %>% 
  ggplot(aes(x = `true:t_init`)) + 
  # geom_point(aes(y = t_init), alpha = 0.1, colour = "blue") +
  geom_ribbon(aes(ymin = t_init_lo, ymax = t_init_hi), colour = "lightgrey", alpha = 0.3) + 
  geom_line(aes(y = t_init_med), linetype = "dashed") + 
  facet_grid(Floor~wardLetter) + 
  geom_abline(slope = 1, intercept = 0, linetype = "solid") + 
  labs(x = expression(true~t[init]~relative), y = expression(estimated~t[init]~relative))+
  # coord_cartesian(ylim = as.Date(c("2020-02-01", "2020-03-20"))) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)
        , text = element_text(size = 10))

ggsave(paste0(figures_folder, "SFig7.jpeg"), units = "cm", width = 15, height = 15, device = "jpeg", dpi = 300)


#####################
####  Figure S 8 ####
#####################


pars = 3
ci_interval <- 0.5*qchisq(df=pars,p=0.95)


### sensitivity analysis

experiment_name = "sensAnal_seirInflect_posneg Inflect beta2"

sA_raw <- read_csv(paste0("Results/", experiment_name, ".csv" )) %>% 
  mutate_at(c("t_inflect", "t_init"), function(x) as.Date(x, origin = "1970-01-01")) %>% 
  # mutate(R0 = beta*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/delta)))
  mutate(R0_before = beta1*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/(delta*kappa3)))
         , R0_after = beta2*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/(delta*kappa3))))

sa_scen <- read_csv("Data/sensAnalysis_scenarios.csv") %>% 
  mutate(t_inflect = as.Date(t_inflect))


sA <- sA_raw %>% 
  left_join(sa_scen %>% select(-beta1, -beta2, -t_init)) %>% 
  mutate(across(contains(c("t_in")), function(x) round(as.numeric(x - as.numeric(as.Date("2020-03-11")))))) %>% 
  mutate(across(contains(c("tinit")), function(x) round(as.numeric(x - as.numeric(as.Date("2020-03-11"))))))


# deal with some which are missing
sA$scenario_number[sA$Ze == 0.6 & sA$Zrp == 0.6] = 19
sA$scenario_name[sA$Ze == 0.6 & sA$Zrp == 0.6] = "level_Z"

now_missing = which(is.na(sA$scenario_number))
# sA[rep(now_missing, each = 5) + rep(-2:2, times = 3), ] %>% View
sA$scenario_number[now_missing] = sA$scenario_number[now_missing - 1]
sA$scenario_name[now_missing] = sA$scenario_name[now_missing - 1]

sA$scenario_number %>% table(useNA = "always")


baseline_values <- sa_scen %>% 
  filter(scenario_name == "baseline") %>% 
  dplyr::select(-scenario_name, -scenario_number) %>% 
  mutate(across(contains(c("t_inflect")), function(x) round(as.numeric(x - as.numeric(as.Date("2020-03-11")))))) %>% 
  
  # mutate(t_inflect = as.numeric(t_inflect)) %>% 
  t %>% 
  {tibble(var = rownames(.), baseline = as.numeric(.[, 1]))} %>% 
  mutate(var = case_when(var == "E_init" ~ "Einit"
                         , var == "t_inflect" ~ "tinflect"
                         , var == "Zis" ~ "Z"
                         , var == "kappa2" ~ "kappa23"
                         , T ~ var)) %>% 
  mutate(baseline = ifelse(var %in% c("alpha", "gamma", "delta"), signif(baseline, 2), baseline))

baseline_values %>% print(n = 1000)


var_values <-
  sa_scen %>% 
  mutate(across(contains(c("t_inflect")), function(x) round(as.numeric(x - as.numeric(as.Date("2020-03-11")))))) %>% 
  mutate(var = gsub(".*_", "", scenario_name)) %>% 
  rename(Einit = E_init, tinflect = t_inflect) %>% 
  {mutate(., var_val = apply(., 1, function(x) case_when(x["var"] == "baseline" ~ as.numeric(NA)
                                                         , x["var"] == "kappa23" ~ as.numeric(x["kappa2"])
                                                         , x["var"] == "Z" ~ as.numeric(x["Zis"])
                                                         , x["var"] == "alpha" ~ signif(as.numeric(x["alpha"]), 2)
                                                         , x["var"] == "gamma" ~ signif(as.numeric(x["gamma"]), 2)
                                                         , x["var"] == "delta" ~ signif(as.numeric(x["delta"]), 2)
                                                         # , x["var"] == "tinflect" ~ "DATE"
                                                         , T ~ as.numeric(x[as.character(x["var"])]))))} %>% 
  left_join(baseline_values) %>% 
  mutate(from = baseline
         , to = var_val) %>%
  # mutate(from = ifelse(var == "tinflect", as.Date(baseline, origin = "1970-01-01") %>% format("%d%b") %>% {gsub("^0", "", .)}, baseline)
  #        , to = ifelse(var == "tinflect", as.Date(var_val, origin = "1970-01-01") %>% format("%d%b") %>% {gsub("^0", "", .)}, var_val)) %>%
  transmute(scenario = scenario_name, var, var_val, baseline
            , scenario_label =  ifelse(scenario == "baseline", "baseline", paste0(scenario, " (", from, "->", to, ")"))
            
            # , scenario_label =  ifelse(scenario == "baseline", "baseline", paste0(scenario, " (", ifelse(var == "tinflect", as.Date(baseline, origin = "1970-01-01") %>% format("%d%b") %>% {gsub("^0", "", .)}, baseline), "->", ifelse(var == "tinflect", as.Date(var_val, origin = "1970-01-01") %>% as.character, var_val), ")"))
  ) %>% 
  mutate(scenario_label = case_when(scenario == "low_Z" ~ "low_Zx (all -> low)"
                                    , scenario == "high_Z" ~ "high_Zx (all -> high)"
                                    , scenario == "level_Z" ~ "level_Zx (all -> 0.6)"
                                    , T ~ scenario_label))


var_values %>% print(n = 100)


var_colours <- c(RColorBrewer::brewer.pal(11, "Paired"), RColorBrewer::brewer.pal(3, "Dark2")) %>% {c("#000000", .)}
names(var_colours) = var_values$var %>% unique


e_vec = c("beta1", "beta2", "R0before", "R0after", "t_init")

pl_list = list()

for(e in e_vec){
  
  
  pl_list[[e]] <- sA %>% 
    rename(scenario = scenario_name) %>% 
    left_join(var_values) %>% 
    mutate(scenario = factor(scenario, levels = var_values$scenario)
           , scenario_label = factor(scenario_label, levels = var_values$scenario_label)) %>% 
    arrange(var, scenario_number) %>% 
    group_by(scenario, var) %>% 
    mutate(ci_boundary = max(loglik) - ci_interval
           , ci =  ifelse(loglik > max(loglik) - ci_interval, "in_ci", "out_ci")) %>% 
    group_by(scenario, scenario_label, var) %>% 
    filter(ci == "in_ci") %>% 
    summarise(bestbeta1 = beta1[loglik == max(loglik)]
              , minbeta1 = min(beta1)
              , maxbeta1 = max(beta1)
              , bestbeta2 = beta2[loglik == max(loglik)]
              , minbeta2 = min(beta2)
              , maxbeta2 = max(beta2)
              , bestt_init = t_init[loglik == max(loglik)]
              , mint_init = min(t_init)
              , maxt_init = max(t_init)
              , bestR0before = R0_before[which(loglik == max(loglik))[1]]
              , minR0before = min(R0_before)
              , maxR0before = max(R0_before)
              , bestR0after = R0_after[which(loglik == max(loglik))[1]]
              , minR0after = min(R0_after)
              , maxR0after = max(R0_after)
    ) %>% 
    
    ggplot(aes_string(y = "scenario_label", x = paste0("best", e), colour = "var")) +
    geom_errorbar(aes_string(xmin=paste0("min",e), xmax=paste0("max", e)), width=.3, size = 1) +
    labs(x = e, colour = "", y = "") + #lims(x = c(0, 1)) +
    # coord_cartesian(xlim=c(NA, NA)) +
  geom_point(size = 2) +
    scale_colour_manual(values = var_colours) + 
    theme_bw() + 
    guides(colour = F)+
    theme(panel.grid.major.y = element_blank()
          , axis.text.y = eval(parse(text = ifelse(e == "beta1", "element_text()", "element_blank()")))
          , text = element_text(size=10)) 
  

}

ggsave(plot = arrangeGrob(grobs = pl_list
                          , nrow = 1, widths = list(20, 12, 12, 12, 12))
       , filename = paste0(figures_folder, "SFig8.jpeg"), units = "cm", width = 30, height = 15, device = "jpeg", dpi = 300)

#####################
####  Figure S 9 ####
#####################

relative_date_key <- read_csv("Data/relativeDateKey.csv") %>%
  transmute(dates = as.Date(Date, format = "%d/%m/%Y"), relDate = relativeDate)

posneg_fpt = read_csv("Data/posneg_fpt.csv") %>%
  transmute(dates = Date, I = pos) %>%
  filter(dates >= as.Date("2020-03-01")
         , dates <= as.Date("2020-04-30")) %>% 
  left_join(relative_date_key %>% select(dates, relDate)) %>% 
  transmute(dates = relDate, I)


ndates <- nrow(posneg_fpt)
t_start <- seq(6, ndates-6) # starting at 2 as conditional on the past observations
t_end <- t_start + 6


this_config <- make_config(list(mean_si = 5.8, #He et al. serial interval
                                std_si = 1/1.96, #He et al. standard deviation based on 95% CI
                                # mcmc_control = mcmc_control,
                                seed = 2,
                                n1 = 50, 
                                n2 = 50, 
                                t_start = t_start,
                                t_end = t_end
))


ee_out <- EpiEstim::estimate_R(incid = posneg_fpt, 
                               method="parametric_si",
                               config = this_config)

Sys.setlocale("LC_TIME", "English")

plot(ee_out, "R") + 
  theme_bw() + 
  labs(title = "", y = expression(R[t]), x = "relative date") + 
  coord_cartesian(xlim = c(0, 50)) + 
  geom_segment(x = 12, xend = 12, y = 17, yend = 10, col = "red", arrow = arrow(ends = "last", length = unit(0.1, "inches"))) + 
  geom_text(x = 12, y = 20, col = "red", label = expression(t[inflect])) + 
  theme(axis.title.y = element_text(angle = 0, hjust = 0, vjust = 0.5)) + 
  guides(color = "none", fill = "none")

ggsave(paste0(figures_folder, "SFig9", ".jpg")
       , height = 7, width = 10, units = "cm", scale = 1, device = "jpeg", dpi = 600)


