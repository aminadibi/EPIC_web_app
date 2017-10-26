#Application-level settings (applicable to all session)


app_type<-APP_TYPE_SHARED
ui_type<-UI_TYPE_DUMB
user<-"Mohsen"


app_status<-0

run_env_settings<-list(
  record_mode=RECORD_MODE_NONE,
  agent_creation_mode=AGENT_CREATION_MODE_ONE,
  update_continuous_outcomes_mode=0,
  runif_buffer_size=1000000,
  rnorm_buffer_size=1000000,
  rexp_buffer_size=1000000,
  agent_stack_size=1000000,
  event_stack_size=1000000
  )


n_agents<-100000
n_recorded_agents<-1000 #for shared apps, the number whose events to be recorded.




apply_run_env_settings<-function(settings)
{
  res<-0;
  ls<-Cget_settings()
  for(i in 1:length(ls))
  {
    nm<-names(ls)[i]
    if(!is.null(settings[nm]))
    {
      res<-Cset_settings_var(nm,settings[[nm]])
      if(res!=0) return(res)
    }
  }
  
  return(res)
}


load_model<-function()
{
  message("Loading the model");  
  Rcpp::sourceCpp(paste(master_path,"/C/model.cpp",sep=""))
  apply_run_env_settings(run_env_settings)
  if(Callocate_resources()<0) stopApp(returnValue = -1)
}




tryCatch(
{
  message("Try catch!")
  if(exists("Cget_version")) 
  {
    Cget_version()
    message("Model is already loaded")
  }
  else
  {
    message("Compiling the model as model functions are not around!")
    load_model()
  }
},warning=function(w)
{
  message("Compiling model after warning")
  load_model()
},error=function(e)
{
  message("loading model after error")
  load_model()
})





