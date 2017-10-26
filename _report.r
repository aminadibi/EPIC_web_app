#This file is to be sourced within Shiny server environment


process_outputs<-function(ls)
{
  nDec<-2

  out<-data.frame(rbind(
    c("Number of agents (individuals)",ls$n_agent)
    ,c("Average time in the model",round(ls$cumul_time/ls$n_agents,nDec))
    ,c("% of agents died",round(100*ls$n_death/ls$n_agents,nDec))
    ,c("Mortality rate",round(ls$n_death/ls$cumul_time,nDec))
    ,c("Exacerbation rate",round(sum(ls$total_exac)/ls$cumul_time,nDec))
    ,cbind(paste("Exacerbation rate by type - ",c("mild","moderate","severe")),round((ls$total_exac)/ls$cumul_time,nDec))
    ,c("Average exacerbation duration (days)",round(sum(ls$total_exac_time)/sum(ls$total_exac)*365,nDec))
    ,cbind(paste("Average exacerbation duration (days) by type - ",c("mild","moderate","severe")),round((ls$total_exac_time)/(ls$total_exac)*365,nDec))
    ,c("Pack-years per individual (at last event)",round(ls$total_pack_years/ls$n_agent,nDec))
    ,c("Average costs /PY",round(ls$total_cost/ls$cumul_time,nDec))
    ,c("Average costs per individual",round(ls$total_cost/ls$n_agent,nDec))
    ,c("Average QALYs /PY",round(ls$total_qaly/ls$cumul_time,nDec))
    ,c("Average QALYs per individual",round(ls$total_qaly/ls$n_agent,nDec))
  ),stringsAsFactors=FALSE)
  
  colnames(out)<-c("Parameter","Value")
  
  return(out)
}





load_data<-function()
{
  n_events<-Cget_n_events();
  
  all_data<<-as.data.frame(NULL)
  ll_ids<-NULL
  
  if(n_events>0)
  {
    ls<-Cget_event(0)
    data_row<-c(do.call("cbind",ls))
    all_data<<-matrix(0,nrow=n_events,ncol=length(data_row))
    all_data[1,]<<-data_row;
    colnames(all_data)<<-names(ls)
  }
  
  if(n_events>1)
  {
    for(i in 1:(n_events-1)) all_data[i+1,]<<-c(do.call("cbind",Cget_event(i)))
  }
  
  if(n_events>0)
  {
    all_ids<<-unique(all_data[,"id"])
    all_data<<-as.data.frame(all_data) 
  }
  
  return(0)
}




extract_data<-function(id=NULL,fixed=FALSE,data=NULL)
{
  if(is.null(data)) data<-all_data
    
  if(!(is.null(id)||is.na(id)))
  {
    out<-data[which(data[,'id']==id),]
    if(fixed==TRUE)
      out<-out[which(out[,'event']==event_fixed | out[,'event']==event_start),]
  }
  else 
  {
    out<-data[which(data[,'id']>0),]
    if(fixed==TRUE)
      out<-out[which(out[,'event']==event_fixed | out[,'event']==event_start),]
  }
  return(out)
}










get_agent_events<-function(id)
{
  if(app_type==APP_TYPE_EXCLUSIVE)
  {
    x<-get_agent_events(id)
    data<-data.frame(matrix(unlist(x),nrow=length(x),byrow=T))
    names(data)<-names(x[[1]])
  }
  else
    return(all_data[which(all_data[,'id']==id),])
}







generic_line_plot<-function(param,id=NULL,fixed=TRUE,xlim=NULL,ylim=NULL,xlab='time',data=NULL)
{
  plot.new()
  if(!(is.null(id)||is.na(id)||is.nan(id)))
  {
    data<-extract_data(id=id,fixed=fixed,data=data)
    plot(as.vector(data[,'local_time']+data[,'age_at_creation']),as.vector(data[,param]),type='l',xlab='time',ylab=param)
  }
  else
  {
    cs<-colors()
    data<-extract_data(id=NULL,fixed=fixed,data=data)
    ids<-unique(data[,'id'])
    
    if(is.null(xlim)) xlim<-c(min(data[,'local_time']),max(data[,'local_time']))
    if(is.null(ylim)) ylim<-c(min(data[,param]),max(data[,param]))
    
    counter<-1
    for(i in ids)
    {
      thisData<-data[which(data[,'id']==i),]
      if(dim(thisData)[1]>0)
      {
        if(counter==1)
          plot(as.vector(thisData[,'local_time']),as.vector(thisData[,param]),type='l',xlab=xlab,ylab=param,xlim=xlim,ylim=ylim,col=cs[counter])
        else
          lines(as.vector(thisData[,'local_time']),as.vector(thisData[,param]),col=cs[counter])
        counter<-counter+1
      }
    }
  }
}




generic_show_list<-function(ls)
{
  strOut<-""
  #browser()
  if(length(ls)==0) return(strOut)
  
  for(i in 1:length(ls))
  {
    #message(paste("processing",names(ls)[i]))
            
    if(typeof(ls[[i]])=="list")
      strOut<-paste(strOut,generic_show_list(ls[[i]]))
    else
      strOut<-paste(strOut,names(ls)[i],renderTable(as.matrix(ls[[i]]))(),"</BR>");
  }
  
  return(strOut)
}




generic_step_plot<-function(param,id=NULL,fixed=TRUE)
{
  plot.new()
  if(!(is.null(id)||is.na(id)||is.nan(id)))
  {
    data<-extract_data(id=id,fixed=fixed)
    x<-c(data[1,'age_at_creation'],as.vector(data[,'local_time']+data[,'age_at_creation']))
    y<-c(0,as.vector(data[,param]))
    plot(stepfun(x[-1],y),xlab='age',ylab=param)
  }
  else
  {
    cs<-colors()
    data<-extract_data(id=NULL,fixed=fixed)
    
    xlim<-c(min(data[,'local_time']+data[,'age_at_creation']),max(data[,'local_time']+data[,'age_at_creation']))
    ylim<-c(min(data[,param]),max(data[,param]))
    
    counter<-1
    for(i in all_ids)
    {
      thisData<-data[which(data[,'id']==i),]
      if(dim(thisData)[1]>0)
      {
        x<-c(thisData[1,'age_at_creation'],as.vector(thisData[,'local_time']+thisData[,'age_at_creation']))
        y<-c(0,as.vector(thisData[,param]))
        plot(stepfun(x[-1],y),xlab='age',ylab=param,xlim=xlim,ylim=ylim,col=cs[counter])
        counter<-counter+1
      }
      par(new=T)
    }
    par(new=F)
  }
}




plot_lung_function<-function(id=NULL,fixed=TRUE,type="MEAN_FEV1_V_TIME")
{
  if(get_session_status()<ENV_STATUS_RESULT_EVENT) 
  {
    plot_message("Please load results for event-level recording to display this plot"); 
    return();
  }
  
  if(is.null(id) || is.na(id) || is.nan(id)) #If no id is provided, then it is requesting average across all ids for this ordinal variable here means reporting percentages;
  {
    id<-NULL
  }
    
  else
    all_data<-extract_data(id=id,fixed=F);
  
  if(type=="FEV1_V_TIME")
  {
    browser()
    ylim<-c(0,max(all_data[,'fev1']))    
    generic_line_plot(param='fev1',id=id,fixed=fixed,ylim=ylim)
  }
  if(type=="MEAN_FEV1_V_TIME")
  {
    temp<-extract_data(id=NULL,fixed=TRUE)[,c('age_at_creation','local_time','fev1','event')]
    xlab<-"time"
    ylim<-c(0,max(all_data[,'fev1']))
    x<-aggregate(temp[,'fev1'],by=list(local_time=temp[,'local_time']),FUN=mean)
    plot(x,type='l',xlim=c(0,max(x[,'local_time'])),xlab="time",ylab="FEV1",ylim=ylim)
  }
  if(type=="FEV1_V_AGE")
  {
    all_data[,'local_time']<-all_data[,'local_time']+all_data[,'age_at_creation']
    ylim<-c(0,max(all_data[,'fev1']))    
    generic_line_plot(param='fev1',id=id,fixed=fixed,data=all_data,ylim=ylim,xlab='age')
  }
  if(type=="MEAN_FEV1_V_AGE")
  {
    temp<-extract_data(id=NULL,fixed=TRUE)[,c('age_at_creation','local_time','fev1','event')]
    temp[,'local_time']<-temp[,'local_time']+round(temp[,'age_at_creation'])
    ylim<-c(0,max(all_data[,'fev1']))
    x<-aggregate(temp[,'fev1'],by=list(local_time=temp[,'local_time']),FUN=mean)
    plot(x,type='l',xlim=c(min(x[,'local_time']),max(x[,'local_time'])),xlab="age",ylab="FEV1",ylim=ylim)
  } 
}




plot_exacerbation<-function(id=NULL,fixed=TRUE,type="MEAN_EXAC_V_TIME")
{
  if(get_session_status()<ENV_STATUS_RESULT_AGENT) 
  {
    plot_message("Please load results with agent-level or event-level recording to display this plot"); 
    return();
  }
  
  if(is.null(id) || is.na(id) || is.nan(id)) #If no id is provided, then it is requesting average across all ids for this ordinal variable here means reporting percentages;
    id<-NULL
  
  if(type=="P_BY_TYPE")
  {
    slices<-colSums(all_data[which(all_data[,'event']==event_end),c('cumul_exac0','cumul_exac1','cumul_exac2')])
    labels<-c("Mild","Moderate","Severe")
    pie(slices, labels, main="Proportion of time with each smoking status",col=c("green","blue","red"))
  }
  
  
  if(type=="EXAC_V_TIME")
  {
    if(is.null(id)) {plot_message("No id was provided"); return(-1);}
    
    all_data<-extract_data(id=id,fixed=F);
    
    x_max<-max(all_data[,'local_time'])
    y_max<-max(c(all_data[,'cumul_exac0'],all_data[,'cumul_exac1'],all_data[,'cumul_exac2']))
    
    plot(stepfun(all_data[-1,'local_time'],all_data[,'cumul_exac0']),xlim=c(0,x_max),ylim=c(0,y_max),xlab="local time",ylab="Cumulative number of exacerbation",col="green")
    par(new=T)
    plot(stepfun(all_data[-1,'local_time'],all_data[,'cumul_exac1']),xlim=c(0,x_max),ylim=c(0,y_max),xlab="local time",ylab="Cumulative number of exacerbation",col="blue")
    par(new=T)
    plot(stepfun(all_data[-1,'local_time'],all_data[,'cumul_exac2']),xlim=c(0,x_max),ylim=c(0,y_max),xlab="local time",ylab="Cumulative number of exacerbation",col="red")
    par(new=F)
  }
  
  if(type=="MEAN_EXAC_V_TIME")
  {
    x_max<-max(all_data[,'local_time'])
    y_max<-max(c(all_data[,'cumul_exac0'],all_data[,'cumul_exac1'],all_data[,'cumul_exac2']))
    
    temp<-all_data[which(all_data[,"event"]==event_fixed),]
    
    x<-aggregate(temp[,'cumul_exac0'],by=list(local_time=temp[,'local_time']),FUN=mean)
    plot(x,type='l',xlim=c(0,x_max),ylim=c(0,y_max),col="green")
    x<-aggregate(temp[,'cumul_exac1'],by=list(local_time=temp[,'local_time']),FUN=mean)
    lines(x,type='l',xlim=c(0,x_max),ylim=c(0,y_max),col="blue")
    x<-aggregate(temp[,'cumul_exac2'],by=list(local_time=temp[,'local_time']),FUN=mean)
    lines(x,type='l',xlim=c(0,x_max),ylim=c(0,y_max),col="red")
  }
}





plot_cost<-function(id=NULL,fixed=TRUE,type="MEAN_V_TIME")
{
  if(is.null(id) || is.na(id) || is.nan(id)) #If no id is provided, then it is requesting average across all ids for this ordinal variable here means reporting percentages;
    id<-NULL
  
  if(type=="ID_V_TIME")
  {
    if(is.null(id)) {plot_message("No id was provided"); return(-1);}
    
    all_data<-extract_data(id=id,fixed=F);
    
    x_max<-max(all_data[,'local_time'])
    y_max<-max(all_data[,'cumul_cost'])
    
    plot(all_data[,'local_time'],all_data[,'cumul_cost'],xlim=c(0,x_max),ylim=c(0,y_max),xlab="local time",ylab="Cumulative costs",type='l')
  }
  
  if(type=="MEAN_V_TIME")
  {
    x_max<-max(all_data[,'local_time'])
    y_max<-max(all_data[,'cumul_cost'])
    
    temp<-all_data[which(all_data[,"event"]==event_fixed),]
    
    x<-aggregate(temp[,'cumul_cost'],by=list(local_time=temp[,'local_time']),FUN=mean)
    plot(x,type='l',xlim=c(0,x_max),ylim=c(0,y_max))
  }
}




plot_qaly<-function(id=NULL,fixed=TRUE,type="MEAN_V_TIME")
{
  if(is.null(id) || is.na(id) || is.nan(id)) #If no id is provided, then it is requesting average across all ids for this ordinal variable here means reporting percentages;
    id<-NULL
  
  if(type=="ID_V_TIME")
  {
    if(is.null(id)) {plot(1, type="n", axes=F, xlab="No id was provided", ylab=""); return(-1);}
    
    all_data<-extract_data(id=id,fixed=F);
    
    x_max<-max(all_data[,'local_time'])
    y_max<-max(all_data[,'cumul_qaly'])
    
    plot(all_data[,'local_time'],all_data[,'cumul_qaly'],xlim=c(0,x_max),ylim=c(0,y_max),xlab="local time",ylab="Cumulative QALYs",type='l')
  }
  
  if(type=="MEAN_V_TIME")
  {
    x_max<-max(all_data[,'local_time'])
    y_max<-max(all_data[,'cumul_qaly'])
    
    temp<-all_data[which(all_data[,"event"]==event_fixed),]
    
    x<-aggregate(temp[,'cumul_qaly'],by=list(local_time=temp[,'local_time']),FUN=mean)
    plot(x,type='l',xlim=c(0,x_max),ylim=c(0,y_max))
  }
}





plot_smoking_status<-function(id=NULL,fixed=TRUE,type)
{
  if(is.null(id) || is.na(id) || is.nan(id)) #If no id is provided, then it is requesting average across all ids for this ordinal variable here means reporting percentages;
    id<-NULL
  else
    all_data<-extract_data(id=id,fixed=F);
  
  if(type=="SUM_TIME_BY_STATUS")
  {
    slices<-outputs_ex$cumul_time_by_smoking_status;
    labels<-c("Never smoker","current smoker","Ex smoker")
    pie(slices, labels, main="Proportion of time with each smoking status")
    
    #temp<-all_data[,c("pack_years","smoking_status","tte")]
    #smoking_status<-temp[,"smoking_status"]+(temp[,"pack_years"]>0)
    #temp<-aggregate(all_data[,"tte"],by=list(smoking_status=smoking_status),FUN=sum)
    #labels<-c("Never smoker","Ex smoker","current smoker")[1+temp[,1]]
    #slices<-temp[,2]
    #pie(slices, labels, main="Proportion of time with each smoking status")
  }
  
  
  if(type=="STATUS_V_CTIME")
  {
    data<-outputs_ex$n_smoking_status_ctime;
    barplot(t(data/rowSums(data)))
  }
  
  
  if(type=="STATUS_V_TIME")  
  {
    if(get_session_status()<ENV_STATUS_RESULT_EVENT) 
    {
      plot_message("Please load results for event-level recording to display this plot"); 
      return();
    }
    
    if(!is.null(id))
    {
      joints<-all_data[,'local_time']
      ttl<-"Smoking history versus time (for id)"
      if(type=="STATUS_V_AGE") 
      {
        joints<-joints+all_data[,'age_at_creation']
        ttl<-"Smoking history versus age (for id)"
      }
      levels<-c(0,all_data[,'smoking_status'])
      plot(stepfun(joints,levels))
      title(ttl)
    }
    else #History over time for aggregate invokes ploting the proportion of smoking status at each time unit (year)
    {
      temp<-extract_data(id=NULL,fixed=TRUE)[,c('age_at_creation','local_time','smoking_status','event')]
      xlab<-"local time"
      x<-aggregate(temp[,1]*0+1,by=list(smoking_status=temp[,'smoking_status'],local_time=temp[,'local_time']),FUN=sum)
      n<-aggregate(x[,'x'],by=list(local_time=x[,'local_time']),FUN=sum)
      plot(x=NULL,y=NULL,xlim=c(0,max(n[,'local_time'])),ylim=c(0,1),xlab=xlab,ylab="Proportion")
      clr<-c('blue','green','red')
      for(i in 0:2)
      {
        y<-rep(0,dim(n)[1])
        y[1+x[which(x[,'smoking_status']==i),'local_time']]<-x[which(x[,'smoking_status']==i),'x']
        y<-y/n[,'x']
        lines(y,col=clr[i+1])
      }
      title("Proportion of individuals across smoking status over time")
    }
  }
  
  if(type=="STATUS_V_AGE")  
  {
    if(get_session_status()<ENV_STATUS_RESULT_EVENT) 
    {
      plot_message("Please load results for event-level recording to display this plot"); 
      return();
    }
    
    if(!is.null(id))
    {
      joints<-all_data[,'local_time']+all_data[,'age_at_creation']
      ttl<-"Smoking history versus time (for id)"
      if(type=="STATUS_V_AGE") 
      {
        joints<-joints+all_data[,'age_at_creation']
        ttl<-"Smoking history versus age (for id)"
      }
      levels<-c(0,all_data[,'smoking_status'])
      plot(stepfun(joints,levels))
      title(ttl)
    }
    else #History over age for aggregate invokes ploting the proportion of smoking status at each time unit (year)
    {
      temp<-extract_data(id=NULL,fixed=TRUE)[,c('age_at_creation','local_time','smoking_status','event')]
      xlab<-"age"
      temp[,'local_time']<-temp[,'local_time']+round(temp[,'age_at_creation'])
      x<-aggregate(temp[,1]*0+1,by=list(smoking_status=temp[,'smoking_status'],local_time=temp[,'local_time']),FUN=sum)
      n<-aggregate(x[,'x'],by=list(local_time=x[,'local_time']),FUN=sum)
      min_age<-min(n[,'local_time'])
      max_age<-max(n[,'local_time'])
      plot(x=NULL,y=NULL,xlim=c(min_age,max_age),ylim=c(0,1),xlab=xlab,ylab="Proportion")
      clr<-c('blue','green','red')
      for(i in 0:2)
      {
        y<-rep(NA,max_age-min_age)
        y[1+n[,'local_time']-min_age]<-0
        y[1+x[which(x[,'smoking_status']==i),'local_time']-min_age]<-x[which(x[,'smoking_status']==i),'x']
        y[1+n[,'local_time']-min_age]<-y[1+n[,'local_time']-min_age]/n[,'x']
        lines((min_age:max_age),y,col=clr[i+1])
      }
      title("Proportion of individuals across smoking status over age")
    }
  }
}








plot_epidemiology<-function(id=NULL,type)
{
  if(type=="N_ALIVE_BY_CTIME")
  {
    data<-outputs_ex$n_alive;
    names(data)<-c(1:length(data))
    barplot(t(data))
    title("Proportion alive at each year");
  }
  
  if(type=="KAPLAN_MEIER")
  {
    if(get_session_status()<ENV_STATUS_RESULT_AGENT) 
    {
      plot_message("Please load results with agent-level or event-level recording to display this plot"); 
      return();
    }
    else
    {
      data<-all_data[which(all_data[,'event']==event_end),c('id','local_time','alive')]
      res<-survfit(Surv(local_time, 1-alive)~1,data=data)
      plot(res,xlab="Years from start",ylab="Survival prbability")
      title("Survival (Kaplan-Meier) curve");
    }
  }
}






plot_journey<-function(id)
{
  data<-get_agent_events(id)
    
  times<-NULL;
  
  th<-max(data[,'local_time'])
  
  for(i in 1:dim(data)[1])    
  {
    dr<-data[i,]
      
    if(dr["event"]==event_exacerbation)
    {
      st<-as.numeric(dr["local_time"])
      type<-as.numeric(dr["exac_status"])
    }
    if(dr["event"]==event_exacerbation_end)
    {
      et<-as.numeric(dr["local_time"])
      times<-rbind(times,c(st,et,type))
      st<-NULL
    }
    if(dr["event"]==event_end)
    {
      if(!is.null(st))
      {
        et<-as.numeric(dr["local_time"])
        times<-rbind(times,c(st,et,type))
        st<-NULL
      }
    }
  }
  
  
  
  plot(NULL,xlim=c(0,th),ylim=c(-0.1,1))
  
  
  for(i in 1:dim(times)[1])
    lines(times[i,1:2],c(1,1),col=c('green','blue','red')[times[i,3]],lwd=5)
  
  par(new=T)
  plot(data[,'local_time'],data[,'cumul_cost']/max(data[,'cumul_cost']),type='l',xlim=c(0,th),ylim=c(-0.1,1),col='orange',lwd=2)
  
  par(new=T)
  plot(data[,'local_time'],data[,'cumul_qaly']/max(data[,'cumul_qaly']),type='l',xlim=c(0,th),ylim=c(-0.1,1),col='green',lwd=2)
  
  
  return(data)
}




gold<-function(session)
{
  data<-extract_data(fixed=TRUE)
  ratio<-data[,'fev1']/data[,'fev1pr']
  gold<-ifelse(ratio>0.8,0,ifelse(ratio>0.5,1,ifelse(ratio>0.3,2,3)))
  
  ne<-rep(0,length(gold))
  nc<-ne
  
  cid<-0
  pree<-0
  prec<-0
  for(i in 1:dim(data)[1])
  {
    if(data[i,'id']==cid)
    {
      ne[i]<-data[i,'cumul_exacerbation']-pree
      pree<-data[i,'cumul_exacerbation']
      
      nc[i]<-data[i,'cumul_cost']-prec
      prec<-data[i,'cumul_cost']
    }
    else
    {
      ne[i]<-data[i,'cumul_exacerbation']
      pree<-0
      
      nc[i]<-data[i,'cumul_cost']
      prec<-0
      
      cid<-data[i,'id']
    }
  }
  
  n0<-length(which(gold==0))
  n1<-length(which(gold==1))
  n2<-length(which(gold==2))
  n3<-length(which(gold==3))
  
  ne0<-sum(ne[which(gold==0)])
  ne1<-sum(ne[which(gold==1)])
  ne2<-sum(ne[which(gold==2)])
  ne3<-sum(ne[which(gold==3)])
  
  nc0<-sum(nc[which(gold==0)])
  nc1<-sum(nc[which(gold==1)])
  nc2<-sum(nc[which(gold==2)])
  nc3<-sum(nc[which(gold==3)])
  
  return(list(n=c(n0,n1,n2,n3),ne=c(ne0,ne1,ne2,ne3),nc=c(nc0,nc1,nc2,nc3)))
}

