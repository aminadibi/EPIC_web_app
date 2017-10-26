#NOTE: set user to DEFAULT and all log-in shit will disappear
template_regular_user_settings<-list(max_agents=100000,max_events=1000000,max_loaded_events=10000,ui_type=UI_TYPE_DUMB)
template_power_user_settings<-list(max_agents=1000000,max_events=100000000,max_loaded_events=100000,ui_type=UI_TYPE_SMART)

users<-data.frame(rbind(
  c(user_name="Mohsen",password="Nadoon123",template_power_user_settings),
  c("Hessam","Nadoon123",template_regular_user_settings),
  c("Managol","Nadoon123",template_regular_user_settings),
  c("Stirling","C2E2",template_regular_user_settings)
  ))


authenticate<-function(user_name,password)
{
    id<-which(users[,1]==user_name)
    if(users[id,2]==password) return(0) else return(-1)
}
