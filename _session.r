init_session<-function()
{
  out<-0;
  
  if(app_status==-1)
  {
    output$global_message<-renderText("Exclusive access by another user!")
    return(-1)
  }
    
  if(app_type==APP_TYPE_EXCLUSIVE)
    app_status<<--1
  else
    app_status<<-app_status+1
  
  server_env<-parent.env(environment())
  
  if(exists("user",envir=.GlobalEnv)) server_env$user<-.GlobalEnv$user else server_env$user<-NULL
  
  server_env$updateInputAction<-0
  server_env$loadResultsAction<-0
  server_env$runAction<-0
  server_env$model_input<-.GlobalEnv$model_input
  server_env$model_input_aux<-.GlobalEnv$model_input_aux
  server_env$session_status<-NULL
  
  server_env$all_data<-NULL
  server_env$all_ids<-NULL
  
  server_env$outputs<-NULL
  server_env$outputs_ex<-NULL
  
  return(0)
}



terminate_session<-function()
{
  app_status<<-app_status-1
  message("Bye bye")
  if(app_status==0) {message("Not cleaning department!");} #Cdeallocate_resources()}
}



#Listof session status is in basic.r

set_session_status<-function(value)
{
  session_status<<-value;
}


get_session_status<-function()
{
  return(session_status);
}
