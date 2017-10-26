RECORD_MODE_EVENT<-1;   #Records all events for all agents;
RECORD_MODE_AGENT<-2;   #Records the agent as is at time of kill;
RECORD_MODE_NONE<-3;  #Records nothing and only leaves the global_outcomes for you!;


RECORD_MODE<-RECORD_MODE_EVENT;
stack_vars<-names(agent_template) #c('id','sex','age_at_creation')




stack_size<-10000
stack<-rep(c(agent_template[stack_vars],event='                    '),each=stack_size)
dim(stack)<-c(stack_size,1+length(agent_template[stack_vars]))
colnames(stack)<-c(names(agent_template[stack_vars]),'event')
stack_counter<-0;
stack_csv<-paste(getwd(),"/output/events.csv",sep="")

init_stack<-function()
{
  stack_counter<<-0
  write.table(stack[0,],file=stack_csv,sep=";",col.names=TRUE)
}


push_event<-function(event_name,agent_after_event)
{
  if(stack_counter>=stack_size)
  {
    save_stack()
  }
  stack_counter<<-stack_counter+1
  stack[stack_counter,]<<-c(as.vector(agent_after_event[stack_vars]),event_name)
}



save_stack<-function()
{
  write.table(stack[1:stack_counter,],file=stack_csv,append=T,sep=";",col.names=FALSE, row.names=FALSE)
  message('stack saved')
  stack_counter<<-0
}
