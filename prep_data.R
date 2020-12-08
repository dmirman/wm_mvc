library(tidyverse)

#####################
## read and prep data from Walshe et al. (2019)
#####################
raw_dat <- haven::read_sav("N118_RomerDrivingSurvey_externalshare_11.08.19.sav")
wm_crash_n118 <- raw_dat %>% 
  mutate(ID = paste0("P", 1:118)) %>% # make a participant ID variable
  # fix some variable names for consistency
  rename(Ob2back_5 = Ob2b5, Ob2back_6 = OTBtotcor6, NumCrashes = Q10) %>%
  # calculate driver survey measures
  mutate(Reckless = rowMeans(cbind(Q14, Q15, Q16, Q17, 
                                   Q18, Q19, Q23, Q24), 
                             na.rm = TRUE),
         Driving_Errors = rowMeans(cbind(Q21, Q20), na.rm = TRUE),
         Risk_Behav = rowMeans(cbind(scale(i45n+i46n+i49n+
                                             i50n+i51n+i52n), 
                               scale(SS_total), 
                               scale(q5an + fight12mo_yn), 
                               scale(AUDcritfinal), 
                               scale(mjcrtot_c)), 
                             na.rm = TRUE),
         Gender = factor(gender, levels = 1:2, labels=c("Male", "Female"))) %>%
  # select main variables for analysis
  select(ID, Gender, NumCrashes, 
         starts_with("DS"), starts_with("CBT"), 
         starts_with("Ob2back"), starts_with("SWM"),
         Reckless, Driving_Errors, Risk_Behav) %>%
  # pivot into long form and fix variable names
  pivot_longer(cols = 4:26, names_to = "var", values_to = "Score") %>%
  separate(var, sep = "_", 
           into = c("Test", "Wave"), convert=TRUE) %>%
  # create categorical crash/no-crash groups, plus non-driver group
  mutate(Group = factor(replace_na(ifelse(NumCrashes==1, "Never_Crashed", "Crashed"), "Non_Driver"))) %>% 
  # standardise WM using full set of N=118
  group_by(Test) %>% # common baseline std score
  mutate(StdScoreCB = scale(Score)) %>% 
  ungroup() %>% group_by(Wave, Test) %>%
  mutate(StdScore = scale(Score)) # within-wave std score
  
summary(wm_crash_n118)  

# check that sample sizes match the paper
length(unique(wm_crash_n118$ID)) # should be 118
wm_crash_n118 %>% group_by(Group) %>%
  summarize(N = length(unique(ID)))
# should be crashed = 25, never crashed = 59, non-driver = 34

#####################
# calculate composite scores
#####################
# invert SWM score b/c it is errors
dat_comp <- filter(wm_crash_n118, Test != "DS") %>%
  mutate(StdScore = replace(StdScore, Test == "SWM", -1*StdScore),
         StdScoreCB = replace(StdScoreCB, Test == "SWM", -1*StdScoreCB)) %>%
  group_by(ID, Gender, Group, Wave) %>%
  summarize(StdScoreM = mean(StdScore, na.rm=TRUE),
            StdScoreM_CB = mean(StdScoreCB, na.rm=TRUE),
            Reckless = unique(Reckless), 
            Driving_Errors = unique(Driving_Errors), 
            Risk_Behav = unique(Risk_Behav))

# a time adjustment: make Wave 1 = Time 0, and 
#   double the gap b/w wave 5 and 6
dat_comp$Time <- replace(dat_comp$Wave, dat_comp$Wave < 6, 
                         dat_comp$Wave[dat_comp$Wave < 6] - 1)
summary(dat_comp)

# use cfa to calculate composite scores
source("get_cfa_scores.R")
cfa_scores <- get_cfa_scores(wm_crash_n118)
cfa_comp <- pivot_longer(cfa_scores, cols = 2:7, values_to = "StdScore_CFA") %>% 
  mutate(Wave = as.numeric(str_remove(name, "WM"))) %>% 
  rename(ID = 1) %>% 
  select(-name)
# merge with other comp data
dat_comp_cfa <- merge(dat_comp, cfa_comp)
summary(dat_comp_cfa)

save(raw_dat, wm_crash_n118, dat_comp, dat_comp_cfa, file="WM_crash_N118_2020-11.RData")

pivot_longer(dat_comp_cfa, cols=c(StdScoreM, StdScoreM_CB, StdScore_CFA),
             names_to = "Std_Type") %>%
  mutate(Std_Type = factor(Std_Type, labels = c("CFA", "Within Wave", "Common Baseline"))) %>% 
  ggplot(aes(Time, value, color=Group)) + 
  facet_wrap(~ Std_Type) +
  stat_summary(fun.data = mean_se, geom="pointrange") + 
  stat_smooth(method = "lm", se=FALSE) +
  theme_bw() + labs(y="WM Composite Score")

#####################
# make wide-format data for LGC analyses
#####################
scores_wide <- pivot_wider(wm_crash_n118, 
                           id_cols = c(ID, Gender, Group, Reckless, Driving_Errors, Risk_Behav, Wave), 
                           names_from = Test:Wave, values_from = Score)
comp_ww_wide <- pivot_wider(dat_comp_cfa, 
                            id_cols = c(ID, Gender, Group, Reckless, Driving_Errors, Risk_Behav, Wave), 
                            names_from = Wave, values_from = StdScoreM)
comp_cb_wide <- pivot_wider(dat_comp_cfa, 
                            id_cols = c(ID, Gender, Group, Reckless, Driving_Errors, Risk_Behav, Wave), 
                            names_from = Wave, values_from = StdScoreM_CB)
comp_cfa_wide <- pivot_wider(dat_comp_cfa, 
                             id_cols = c(ID, Gender, Group, Reckless, Driving_Errors, Risk_Behav, Wave), 
                             names_from = Wave, values_from = StdScore_CFA)
save(scores_wide, comp_ww_wide, comp_cb_wide, comp_cfa_wide, file="WM_crash_N118_2020-11_wide.RData")
write_csv(scores_wide, "scores_wide.csv")
write_csv(comp_ww_wide, "comp_ww_wide.csv")
write_csv(comp_cb_wide, "comp_cb_wide.csv")
write_csv(comp_cfa_wide, "comp_cfa_wide.csv")

#####################
# CFA loadings for supplementary table
#####################
cfa_model <- get_cfa_scores(wm_crash_n118, "model")
summary(cfa_model)
parameterEstimates(cfa_model, standardized=TRUE)
parameterEstimates(cfa_model, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Wave'=lhs, Indicator=rhs, B=est, SE=se, Z=z, 'p-value'=pvalue, Beta=std.all) %>% 
  write_csv(., "CFA_Loadings.csv")
