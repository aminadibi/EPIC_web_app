create_interface<-function(model_input,model_input_aux=NULL)
{
  iStr<<-"";
  message("Create_interface called")
  if(iStr=="")
  {
    iStr<<-iterate_input(model_input,model_input_aux,"input")
    iStr<<-paste(strsplit(iStr,",")[[1]][-1],collapse=",")
  }
  eval(parse(text=iStr))
}


clear_interface<-function()
{
  iStr<<-"";
}




iterate_input<-function(model_input,model_input_aux=NULL,name)
{  
  lStr<-""
  fStr<-""
  bTabset<-FALSE
  
  if(length(model_input)>0)
  {
    for (i in 1:length(model_input))
    {
      message(paste("processing "),names(model_input)[i])
      
      id<-paste(name,names(model_input)[i],sep="$")
      
      #browser();
      
      label<-NULL
      control_type<-NULL;
      control_parms=NULL;
      suppress<-NULL;
      inlint<-NULL
      if(!is.null(model_input_aux) )
      {
          label<-eval(parse(text=paste("model_input_aux",paste(strsplit(id[1],"\\$")[[1]][-1],collapse="$"),"label",sep="$")))
          control_type<-eval(parse(text=paste("model_input_aux",paste(strsplit(id[1],"\\$")[[1]][-1],collapse="$"),"control_type",sep="$")))
          control_parms<-eval(parse(text=paste("model_input_aux",paste(strsplit(id[1],"\\$")[[1]][-1],collapse="$"),"control_parms",sep="$")))
          suppress<-eval(parse(text=paste("model_input_aux",paste(strsplit(id[1],"\\$")[[1]][-1],collapse="$"),"suppress",sep="$")))
          inline<-eval(parse(text=paste("model_input_aux",paste(strsplit(id[1],"\\$")[[1]][-1],collapse="$"),"inline",sep="$")))
          #browser()
      }
      
      
      if(is.null(suppress))
      {
        
        if(is.null(label)) label<-names(model_input)[i];
        
        if(typeof(model_input[[i]])=="list")
        {
          if(bTabset==FALSE)
          {
            lStr<-paste(lStr,",tabsetPanel(id='",name,"'",sep="")
            bTabset<-TRUE
          }
          lStr<-paste(lStr,",tabPanel(title='",label,"'",sep="")
          lStr<-paste(lStr,iterate_input(model_input[[i]],model_input_aux[names(model_input)[i]],id),sep="")
          lStr<-paste(lStr,")",sep="")
        }
        else
        {
          if(is.null(control_type))
          {
            str<-switch(class(model_input[[i]]),
                        array=paste(",HTML('",label,"<br/><textarea id=\"",id,"\" rows=\"3\" cols=\"40\">",writeMatrixtoText(model_input[[i]]),"</textarea>')",sep=""),
                        matrix=paste(",HTML('",label,"<br/><textarea id=\"",id,"\"",control_parms,">",writeMatrixtoText(model_input[[i]]),"</textarea>')",sep=""),
                        #vector=paste(",HTML('",label,"<<br/>textarea id=\"",id,"\" rows=\"3\" cols=\"40\">",paste(model_input[[i]],collapse=","),"</textarea>')"),
                        paste(",textInput('",id,"', label='",label,"',value='",paste(model_input[[i]],collapse=","),"')",sep="")
            )
          }
          else
          {
            str<-switch(control_type,
                        sliderInput=paste(",sliderInput('",id,"',label='",label,"',value=",paste(model_input[[i]],collapse=","),ifelse(is.null(control_parms),"",paste(",",control_parms)),")",sep=""),
                        "ERROR!"
            )
          }
          
          #Add the help button
          temp<-paste(str,",HTML(\"<A target='reference' href='reference.htm#",gsub("\\$","__",id),"'><img src='q.jpg'/></a><hr/>\")",sep="")
          
          fStr<-paste(fStr,temp,sep="")
        }        
      }
    }
  }
  
  if(bTabset==TRUE) lStr<-paste(lStr,")",sep="")
  return(paste(fStr,lStr))
}



#Reads interface values and puts them into model_input variable of the env environment (pass by reference, yay!.
#Thism ight sound strange but the reference is the model_input otherwise the user can modify HTML code and causes server model_input to be modified in structure
read_shiny_input<-function(shiny_input,env)
{  
  message("read_shiny_input called")
  out<-0;
  
  elements<-get_list_elements(env$model_input)
  
  for(element in elements)
  {
    temp<-switch(class(eval(parse(text=paste("env$model_input$",element,sep="")))),
                 array=eval(parse(text=paste(paste("env$model_input$",element,sep=""),"<-readMatrixFromText(shiny_input[[\"input$",element,"\"]])",sep=""))),
                 matrix=eval(parse(text=paste(paste("env$model_input$",element,sep=""),"<-readMatrixFromText(shiny_input[[\"input$",element,"\"]])",sep=""))),
                 #vector=eval(parse(text=paste(paste("env$model_input$",element,sep=""),"<-readMatrixFromText(shiny_input[[\"input$",element,"\"]])",sep=""))),
                 eval(parse(text=paste(paste("env$model_input$",element,sep=""),"<-as.numeric(shiny_input[[\"input$",element,"\"]])",sep="")))
    )
    
    if(is.null(temp) || length(temp)==0 || is.nan(temp) || is.na(temp)) {out<--1;}
  }
  message("read_shiny_input quitted")
  return(out)
}









