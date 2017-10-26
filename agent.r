agent_template<-list(
  id=0,
  alive=1,
  
  local_time=0,
  tte=0,  #merely to facilitate reporting...;
    
  sex=DEF_SEX_MAN,
  dob=0, #on the calendar time scale; represents the cohort effect;
  
  age_at_creation=40,
  
  height=170,
  
  fixed_LPT=0,  #last local time the fixed event occured (necessary for cost and qaly)
    
  text_field=''
)




create_agent<-function(id)
{
  agent<-agent_template
  
  agent$id<-id
  agent$alive<-1
  
  agent$local_time<-0
  
  agent$sex<-ifelse(runif(1)>input$agent$p_female,DEF_SEX_MAN,DEF_SEX_WOMAN)
  agent$dob<-calendar_time
  
  temp<-rnorm(1,input$agent$age_at_baseline_mu,input$agent$age_at_baseline_sd)
  agent$age_at_creation<-temp
   
  return(agent)
}








