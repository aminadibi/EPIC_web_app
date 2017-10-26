

init_input<-function(session)
{
  input<-list()
  input_aux<-list()
  
  input$global_parameters<-list(
    time_horizon=20,
    discount_cost=0,
    discount_qaly=0
  )
  input_aux$global_parameters$label<-"Global Parameters"
  
  input_aux$global_parameters$time_horizon$label<-"Time Horizon"
  
  input_aux$global_parameters$discount_cost$label="Annual discounting for costs"
  input_aux$global_parameters$discount_cost$control_type="sliderInput"
  input_aux$global_parameters$discount_cost$control_parms="min=0,max=1,step=0.01"
  input_aux$global_parameters$discount_cost$inline=T
  
  input_aux$global_parameters$discount_qaly$label="Annual discounting for QALYs"
  input_aux$global_parameters$discount_qaly$control_type="sliderInput"
  input_aux$global_parameters$discount_qaly$control_parms="min=0,max=1,step=0.01"

  
  
   
  input_aux$agent$label<-"Demographic"
  
  input$agent$p_female<-0.5
  input_aux$agent$p_female$label="Proportion female"
  input_aux$agent$p_female$control_type="sliderInput"
  input_aux$agent$p_female$control_parms="min=0,max=1,step=0.01"
  
  
  input$agent$age_at_baseline_mu<-40;
  input_aux$agent$age_at_baseline_mu$label="Mean age at baseline"
  input$agent$age_at_baseline_sd<-5;
  input_aux$agent$age_at_baseline_sd$label="SD of age at baseline"
  
    
  
    
  
  #smoking;
  input_aux$smoking$label<-"Smoking"
  
  temp<-matrix(0,111,2)
  colnames(temp)<-c('male','female')
  
  temp[20:35,1]<-15/1000
  temp[20:35,2]<-10.6/1000
  input$smoking$inc_by_age_sex<-temp
  input_aux$smoking$inc_by_age_sex$label<-"Sex- and age-specific incidence of smoking"
  input_aux$smoking$inc_by_age_sex$control_parms<-"cols=20, rows=20"
  
  
  temp<-temp*0
  temp[20:35,1]<-55/1000
  temp[36:50,1]<-41/1000
  temp[50:111,1]<-10/1000
  temp[20:35,2]<-55/1000
  temp[36:50,2]<-41/1000
  temp[50:111,2]<-10/1000
  input$smoking$ces_by_age_sex<-temp
  input_aux$smoking$ces_by_age_sex$label<-"Sex- and age-specific incidence of smoking cessation"
  input_aux$smoking$ces_by_age_sex$control_parms<-"cols=20, rows=20"
  
  temp<-temp*0
  temp[20:35,1]<-376/1000
  temp[36:50,1]<-376/1000
  temp[50:60,1]<-376/1000
  temp[20:35,2]<-376/1000
  temp[36:50,2]<-376/1000
  temp[50:60,2]<-376/1000
  input$smoking$rel_by_age_sex<-temp
  input_aux$smoking$rel_by_age_sex$label<-"Sex- and age-specific incidence of smoking relapse"
  input_aux$smoking$rel_by_age_sex$control_parms<-"cols=20, rows=20"
  
 
  
  
  #Lung function;
  input_aux$lung_function$label<-"Lung Function"
  
  input$lung_function$pred_fev1_betas_by_sex<-rbind(c(intercept=-0.7453,age=-0.04106,age2=0.004477,height2=0.00014098),c(-0.871,0.06537,0,0.00011496))
  input_aux$lung_function$pred_fev1_betas_by_sex$label<-"Regression coefficients for predicted FEV1, by sex"
  input_aux$lung_function$pred_fev1_betas_by_sex$control_parms<-"cols=50, rows=5"
    
  input$lung_function$fev1_0_betas<-t(as.matrix(c(intercept=3,sex=0,age=0)))
  input_aux$lung_function$fev1_0_betas$label<-"Regression coefficients for FEV1 calculation at baseline"
  input$lung_function$fev1_0_res_sd<-0;
  input_aux$lung_function$fev1_0_res_sd$label<-"Residual error (standard deviation) of baseline FEV1 predictor"
  
  input$lung_function$dfev1_betas<-t(as.matrix(c(intercept=-0.05,sex=0,age0=0,fev1_0=0,smoking=-0.02,time=-0.002)))
  input_aux$lung_function$dfev1_betas$label="Regression coefficients for FEV1 slope"
  
  input$lung_function$dfev1_re_sds<-t(as.matrix(c(intercept=0.01,time=0)))
  input_aux$lung_function$dfev1_re_sds$label<-"SDs of random-effect terms"
  input$lung_function$dfev1_re_rho<--0.5
  input_aux$lung_function$dfev1_re_rho$label<-"Correlation coefficient of random efeft terms"
  
  #Exacerbation;
  input$exacerbation$exac_betas=t(as.matrix(c(intercept=0.1,sex=0.2,age=0.0,fev1=0.01)))
  input_aux$exacerbation$exac_betas$label<-"Cefficients of logarithm of the rate of exacerbation"
  
  input$exacerbation$exac_re_var=0.01
  input_aux$exacerbation$exac_re_var$label<-"Variance of random-effect intercept"
  
  input$exacerbation$p_moderate_severe<-t(as.matrix(c(moderate=0.4,severe=0.2)))
  input_aux$exacerbation$p_moderate_severe$label<-"Probabity of moderate-severe (reference is mild exacerbation)"
  
  input$exacerbation$exac_end_rate<-t(as.matrix(c(mild=365/5,moderate=365/10,severe=365/15)))
  input_aux$exacerbation$exac_end_rate$label<-"Annual rate of exacerbation ending (365/average length)"
  
  input$exacerbation$p_death<-t(as.matrix(c(mild=0.1, moderate=0.2, severe=0.3)))
  input_aux$exacerbation$p_death$label<-"Probability of death due to exacrbation"
  
  #Costs and utilities
  input$costs$bg_cost_by_stage=t(as.matrix(c(I=1000,II=2000,III=3000,IV=4000)))
  input$costs$exac_dcost=t(as.matrix(c(mild=1500,moderate=2500,severe=3500)))
  
  input$utilities$bg_util_by_stage=t(as.matrix(c(I=0.7,II=0.6,III=0.5,IV=0.4)))
  input$utilities$exac_dutil=t(as.matrix(c(mild=-0.1,moderate=-0.2,severe=-0.3)))
  
  
  input$bg_death<-list(
    p_by_sex=cbind(
      male=c(0.00522,0.00030,0.00022,0.00017,0.00013,0.00011,0.00010,0.00009,0.00008,0.00008,0.00009,0.00010,0.00012,0.00015,0.00020,0.00028,0.00039,0.00051,0.00059,0.00066,0.00071,0.00075,0.00076,0.00076,0.00074,0.00071,0.00070,0.00069,0.00070,0.00071,0.00074,0.00078,0.00082,0.00086,0.00091,0.00096,0.00102,0.00108,0.00115,0.00123,0.00132,0.00142,0.00153,0.00165,0.00179,0.00194,0.00211,0.00229,0.00251,0.00275,0.00301,0.00331,0.00364,0.00401,0.00441,0.00484,0.00533,0.00586,0.00645,0.00709,0.00780,0.00859,0.00945,0.01040,0.01145,0.01260,0.01387,0.01528,0.01682,0.01852,0.02040,0.02247,0.02475,0.02726,0.03004,0.03310,0.03647,0.04019,0.04430,0.04883,0.05383,0.05935,0.06543,0.07215,0.07957,0.08776,0.09680,0.10678,0.11780,0.12997,0.14341,0.15794,0.17326,0.18931,0.20604,0.21839,0.23536,0.25290,0.27092,0.28933,0.30802,0.32687,0.34576,0.36457,0.38319,0.40149,0.41937,0.43673,0.45350,0.46960,1.00000)
      ,
      female=c(0.00449,0.00021,0.00016,0.00013,0.00010,0.00009,0.00008,0.00007,0.00007,0.00007,0.00008,0.00008,0.00009,0.00011,0.00014,0.00018,0.00022,0.00026,0.00028,0.00029,0.00030,0.00030,0.00031,0.00031,0.00030,0.00030,0.00030,0.00031,0.00032,0.00034,0.00037,0.00040,0.00043,0.00047,0.00051,0.00056,0.00060,0.00066,0.00071,0.00077,0.00084,0.00092,0.00100,0.00109,0.00118,0.00129,0.00140,0.00153,0.00166,0.00181,0.00197,0.00215,0.00235,0.00257,0.00280,0.00307,0.00336,0.00368,0.00403,0.00442,0.00485,0.00533,0.00586,0.00645,0.00710,0.00782,0.00862,0.00951,0.01051,0.01161,0.01284,0.01420,0.01573,0.01743,0.01934,0.02146,0.02384,0.02649,0.02947,0.03280,0.03654,0.04074,0.04545,0.05074,0.05669,0.06338,0.07091,0.07940,0.08897,0.09977,0.11196,0.12542,0.13991,0.15541,0.17190,0.18849,0.20653,0.22549,0.24526,0.26571,0.28671,0.30810,0.32970,0.35132,0.37280,0.39395,0.41461,0.43462,0.45386,0.47222,1.00000)
    )
  )
  input_aux$bg_death$label<-"Background mortality"
  input_aux$bg_death$p_by_sex$label<-"Annual mortality rate by sex (male,female)"
  input_aux$bg_death$p_by_sex$control_parms<-"cols=30, rows=20"
  
  input$bg_death$MORT_COEFF<-1
  input_aux$bg_death$MORT_COEFF$label="Mortality multiplier (default=1)"
  input_aux$bg_death$MORT_COEFF$control_type="sliderInput"
  input_aux$bg_death$MORT_COEFF$control_parms="min=0,max=1,step=0.01"
  
  
  #Proportion of death by COPD that should be REMOVED from background mortality (http://vizhub.healthdata.org/cod/)
  temp<-cbind(
    #   0 for <35         35-39           40-44           45-49          50-54      55-59           60-64        65-69          70-74       75-79       80+
    male=c(rep(0,35),rep(0.2/100,10),rep(0.4/100,5),rep(0.6/100,5),rep(1.4/100,5),rep(2.1/100,5),rep(3/100,5),rep(4.5/100,5),rep(6/100,5),rep(7/100,5),rep(8/100,5))
    ,
    female=c(rep(0,35),rep(0.2/100,10),rep(0.4/100,5),rep(0.6/100,5),rep(1.4/100,5),rep(2.1/100,5),rep(3/100,5),rep(4.5/100,5),rep(6/100,5),rep(7/100,5),rep(8/100,5))
  )
  #Carry the last observation forward up to age 111;
  input$bg_death$PROP_COPD_DEATH_BY_SEX_AGE<-rbind(temp,matrix(rep(temp[dim(temp)[1],],111-dim(temp)[1]),ncol=2))
  input_aux$bg_death$PROP_COPD_DEATH_BY_SEX_AGE$label<-"Ratio of cause-specific COPD mortality to overall mortality"
  input_aux$bg_death$PROP_COPD_DEATH_BY_SEX_AGE$control_parms<-"cols=30, rows=20"  
  
  session$model_input<-input
  session$model_input_aux<-input_aux
}



init_input(.GlobalEnv)  #creates one instance of default input 'model_input' in the global environment. Sessions do not have to re-initialize this and just copy it (see server())





#processes interface nput (.GlobalEnv$input and makes it ready for model input (C++))
process_input<-function(ls)
{
  ls$bg_death$p_by_sex<-ls$bg_death$p_by_sex*(1-ls$bg_death$PROP_COPD_DEATH_BY_SEX_AGE)*ls$bg_death$MORT_COEFF
  ls$bg_death$MORT_COEFF<-NULL
  ls$bg_death$PROP_COPD_DEATH_BY_SEX_AGE<-NULL
  
  return(ls)
}

