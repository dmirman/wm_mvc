get_cfa_scores <- function(dat = wm_crash_n118, var_out = "scores"){
  require(tidyverse)
  require(lavaan)
  # put data into wide format
  dat_wide <- pivot_wider(dat, id_cols = c(ID, Wave), 
                          names_from = Test:Wave, values_from = Score)
  
  # specify model
  long_model <- 'WM1 =~ lds*DS_1 + CBT_1 + SWM_1 
  WM2 =~ lds*DS_2 + CBT_2 + SWM_2 + Ob2back_2
  WM3 =~ lds*DS_3 + CBT_3 + SWM_3 + Ob2back_3
  WM4 =~ lds*DS_4 + CBT_4 + SWM_4 + Ob2back_4
  WM5 =~ lds*DS_5 + CBT_5 + SWM_5 + Ob2back_5
  WM6 =~ lds*DS_6 + CBT_6 + SWM_6 + Ob2back_6
  SWM_1 ~~ SWM_2
  SWM_2 ~~ SWM_3
  SWM_3 ~~ SWM_4
  SWM_4 ~~ SWM_5
  SWM_5 ~~ SWM_6
  CBT_1 ~~ CBT_2
  CBT_2 ~~ CBT_3
  CBT_3 ~~ CBT_4
  CBT_4 ~~ CBT_5
  CBT_5 ~~ CBT_6
  DS_1 ~~ DS_2
  DS_2 ~~ DS_3
  DS_3 ~~ DS_4
  DS_4 ~~ DS_5
  DS_5 ~~ DS_6
  Ob2back_2 ~~ Ob2back_3
  Ob2back_3 ~~ Ob2back_4
  Ob2back_4 ~~ Ob2back_5
  Ob2back_5 ~~ Ob2back_6
  DS_1 ~ ids*1
  CBT_1 ~ icbt*1
  SWM_1 ~ ismw*1
  DS_2 ~ ids*1
  CBT_2 ~ icbt*1
  SWM_2 ~ ismw*1
  Ob2back_2 ~ iob2*1
  DS_3 ~ ids*1
  CBT_3 ~ icbt*1
  SWM_3 ~ ismw*1
  Ob2back_3 ~ iob2*1
  DS_4 ~ ids*1
  CBT_4 ~ icbt*1
  SWM_4 ~ ismw*1
  Ob2back_4 ~ iob2*1
  DS_5 ~ ids*1
  CBT_5 ~ icbt*1
  SWM_5 ~ ismw*1
  Ob2back_5 ~ iob2*1
  DS_6 ~ ids*1
  CBT_6 ~ icbt*1
  SWM_6 ~ ismw*1
  Ob2back_6 ~ iob2*1
  WM1 ~ 1 
  WM2 ~ 1
  WM3 ~ 1
  WM4 ~ 1
  WM5 ~ 1
  WM6 ~ 0*1
  WM6 ~~ 1*WM6
  WM1 ~~ WM1
  WM2 ~~ WM2
  WM3 ~~ WM3
  WM4 ~~ WM4
  WM5 ~~ WM5
  '
  # fit model
  long_model.fit <- cfa(long_model, data = dat_wide, missing='FIML', meanstructure=TRUE)
  #summary(long_model.fit)
  if(var_out == "scores"){
    scores <- lavPredict(long_model.fit, newdata=dat_wide)
    # attach participant ID and return result
    result <- cbind(dat_wide$ID, as.data.frame(scores))
    return(result)
  } else {return(long_model.fit)}
}