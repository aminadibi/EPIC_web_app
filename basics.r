master_path=getwd()
  #"C:/Users/Mohsen/main/Projects/project.COPDES/code/R/models/simple"

APP_TYPE_EXCLUSIVE<-0
APP_TYPE_SHARED<-1

UI_TYPE_DUMB<-0
UI_TYPE_SMART<-1


RECORD_MODE_NONE<-0;  #Records nothing and only leaves the global_outcomes for you!;
RECORD_MODE_AGENT<-1;   #Records the agent as is at time of kill;
RECORD_MODE_EVENT<-2;   #Records all events for all agents;


AGENT_CREATION_MODE_ONE<-0
AGENT_CREATION_MODE_ALL<-1
AGENT_CREATION_MODE_PRE<-2


DEF_SEX_MAN<-0
DEF_SEX_WOMAN<-1
NEVER_SMOKER<-0
EX_SMOKER<-1
CURRENT_SMOKER<-2


event_start=0
event_fixed=1
event_smoking_change=2
event_exacerbation=3
event_exacerbation_end=4
event_exacerbation_death=5
event_bg_death=6
event_end=7



#Shiny environment status vars;
ENV_STATUS_INPUT=1;   #input updated, model not run yet (also start up value);
ENV_STATUS_INPUT_ERROR=-1;
ENV_STATUS_RUN=2;
ENV_STATUS_RUN_ERROR=-2;
ENV_STATUS_RESULT_NONE=3;#results loaded but setting is no event rcording;
ENV_STATUS_RESULT_AGENT=4;#results loaded but setting is agent rcording;
ENV_STATUS_RESULT_EVENT=5;#results loaded but setting is agent rcording;
ENV_STATUS_RESULT_ERROR=-2;









plot_message<-function(message)
{  
  plot(1, type="n", axes=F, xlab=message, ylab="")
  title(message)
}




get_list_elements<-function(ls,running_name="")
{
  out<-NULL
  if(length(ls)>0)
  {
    for (i in 1:length(ls))
    {
      if(typeof(ls[[i]])=="list")
      {
        out<-c(out,paste(names(ls)[i],"$",get_list_elements(ls[[i]]),sep=""))
      }
      else
      {
        out<-c(out,names(ls)[i])
      }
    }
  }
  return(out)
}



get_list_values<-function(ls,running_name="")
{
  out<-NULL
  if(length(ls)>0)
  {
    for (i in 1:length(ls))
    {
      if(typeof(ls[[i]])=="list")
      {
        if(is.null(out))
          return(get_list_values(ls[[i]]))
        else
          return(list(out,get_list_values(ls[[i]])));
      }
      else
      {
        if(is.null(out))
          out<-list(ls[[i]])
        else
          out<-list(out,ls[[i]]);
      }
    }
  }
  return(out)
}





creat_list_from_elements<-function(elements,values)
{
  outL<-list()
  
  for(i in 1:length(elements))
  {
    eval(parse(text=paste("outL","$",elements[i],"<-",values[[i]],sep="")))
  }
  return(outL)
}




cvrho_to_covmat<-function(mu,cvrho)
{
  sd1<-(abs(mu[1])*cvrho[1])
  sd2<-(abs(mu[2])*cvrho[3])
  covariance<-cvrho[2]*sd1*sd2
  
  covmat<-c(sd1^2,covariance,covariance,sd2^2)
  
  return(matrix(covmat,c(2,2)))
}





readMatrixFromText<-function(txt,header=NULL)
{
  temp<-gsub(" ","", txt, fixed=TRUE)
  
  if(nchar(txt)==0) return(NULL);
  
  if(is.null(header))
  {
    if(is.numeric(substring(txt,1,1)))
      header<-FALSE
    else
      header<-TRUE;
  }
  
  
  con <- textConnection(txt)
  out<-read.table(con,header = header)
  close(con)
  
  #browser()
  
  if(length(out)>0) out<-as.matrix(out)
  
  return(out)
}



writeMatrixtoText<-function(mat)
{
  con <- textConnection("temp","w")
  write.table(mat,con)
  close(con)
  
  return(paste(temp,collapse="\r\n"))
}



