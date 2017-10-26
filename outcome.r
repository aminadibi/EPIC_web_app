
global_outcomes<-list(
  agents=c(agents=0),
  cost=c(cost=0),
  qaly=c(qaly=0),
  exacerbation=c(exacerbation=0),
  
  pack_years=c("Pack years"=0),
  
  death=c("death"=0),
  agent_time=c("Total agent time"=0)
)



collect_outcomes<-function(agent)
{
  global_outcomes$agents<<-global_outcomes$agents+1
  if(agent$alive==0)
    global_outcomes$death<<-global_outcomes$death+1
  
  global_outcomes$agent_time<<-global_outcomes$agent_time+agent$local_time
  
  global_outcomes$pack_years<<-global_outcomes$pack_years+agent$pack_years
  
  global_outcomes$cost<<-global_outcomes$cost+agent$cumul_cost
  global_outcomes$qaly<<-global_outcomes$qaly+agent$cumul_qaly
  global_outcomes$exacerbation<<-global_outcomes$exacerbation+agent$cumul_exacerbation
}