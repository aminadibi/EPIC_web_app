#goes through ls elements one byu one and applies them to Cset_input_var. There should be one-one matching!
set_Cmodel_inputs<-function(ls)
{
   nms<-get_list_elements(ls)
  for(i in 1:length(nms))
  {
    val<-eval(parse(text=paste("ls$",nms[i])))
    
    #important: CPP is column major order but R is row major; all matrices should be tranposed before vectorization;
    if(is.matrix(val))  val<-as.vector(t(val))
    
    res<-Cset_input_var(nms[i],val)
    if(res!=0) return(res);
  }
  
  return(0);
}








run_shared<-function(n_agents=.GlobalEnv$n_agents)
{
  res<-set_Cmodel_inputs(process_input(model_input))
  if(res!=0) return(res)
  
  withProgress(
  {
    res<-Cinit_session();
    if(res==0)
    {
      #step 1: run for n_recorded_agents
      if(n_recorded_agents>0)
      {
        Cset_settings_var("record_mode",RECORD_MODE_EVENT)
        res<-Cmodel(n_recorded_agents)
        setProgress(0.33)
        load_data()
        setProgress(0.67)
      }
      
      #step 2: run for n_recorded_agents
      Cset_settings_var("record_mode",RECORD_MODE_NONE)
      if(res==0)  res<-Cmodel(max(n_agents-n_recorded_agents,0))
      setProgress(1)
    }
    if(res==0)
      {set_session_status(ENV_STATUS_RESULT_EVENT); out<-paste("Model ran successfully");} #We put the status to results_events because we have loaded the first batch;
    else
      {set_session_status(ENV_STATUS_RUN_ERROR); out<-paste("ERROR - error code:",res);}
  }
  ,message="Running")
  
  outputs<<-Cget_outputs()
  outputs_ex<<-Cget_outputs_ex()
  
  return(0);
}









run_exclusive<-function(n_agents=.GlobalEnv$n_agents)
{
  Cinit_session();
  
  res<-set_Cmodel_inputs(process_input(model_input))
  
  runtime_time<<-round(system.time(
    {
      withProgress(message="Running...",res<-Cmodel(n_agents),value=0)
    }
    , gcFirst = TRUE),2)
  
  
  if(res<0) return(res)
  
  message("I am done!!!")
  
  set_session_status(ENV_STATUS_RUN)
  
  return(0);
}