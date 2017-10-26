
library(shiny)
library("mvtnorm")
library("survival")


source("basics.r")
source("interface.r")
source("input.r")
source("outcome.r")
source("authenticate.r")
source("application.r")







# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output, session)
{ 
  source("_session.r",local=TRUE)
  source("_report.r",local=TRUE)
  source("_caller.r",local=TRUE)
  
  if(init_session()!=0) return()
  
  session$onSessionEnded(function(){terminate_session()})
 
  set_session_status(ENV_STATUS_INPUT);
  
  
  reactive(
  {
    input$updateInputAction
    message("dooshvari")
    ls<-parseQueryString(session$clientData$url_search)
    if(!is.null(ls$get_status))
    {
      app_status
      "Chacherim"
    } 
  })
  
  
  
  output$global_message<-renderText(
  {
    out<-"";
    
    #This invokes running the model after loading
    if(is.null(input$updateInputAction))   res<-run_shared();  
    
    
    input$updateInputAction
    input$runAction
    input$loadResultsAction
    
    
    
    if(!is.null(input$updateInputAction) && input$updateInputAction>updateInputAction) 
    { 
      updateInputAction<<-input$updateInputAction; 
      
      isolate({out<-update_input_action()});
      if(app_type==APP_TYPE_SHARED)
      {
        res<-run_shared();
        if(res==0) out<-"Model ran successfully" else out<-paste("OOPS! Error, code ",res)
      }
      else
      {
      }
    }
    
    if(!is.null(input$runAction) && input$runAction>runAction) 
    {
      runAction<<-input$runAction; 
      isolate({
        res<-run_exclusive(input$nSim)
        if(res==0)
        {set_session_status(ENV_STATUS_RUN); out<-paste("Model ran successfully; time=",runtime_time[1]);}
        else
        {set_session_status(ENV_STATUS_RUN_ERROR); out<-paste("ERROR - error code:",res);}
      })
    }
    
    
    if(!is.null(input$loadResultsAction) && input$loadResultsAction>loadResultsAction) 
    {
      loadResultsAction<<-input$loadResultsAction; 
      if(get_session_status()>=ENV_STATUS_RUN)
      {
        res<-load_data();
        
        if(res==0)   {set_session_status(ENV_STATUS_RESULT_NONE);out<-"Results loaded (record mode: none)"; }
        if(res==1)   {set_session_status(ENV_STATUS_RESULT_AGENT);out<-"Results loaded (record mode: agent)";}
        if(res==2)   {set_session_status(ENV_STATUS_RESULT_EVENT);out<-"Results loaded (record mode: event)"}
      }
      else
        out<-"The model has not run yet. Run the model first";   
    }  
    
    out
  })


  
  inputMessage<-"";
  
  output$allUI<-renderUI(
  {
    if(!is.null(input$sign_action) && input$sign_action==1) 
    {
      if(authenticate(input$user_name,input$password)==0)
      {
        clear_interface()
        init_session()
        user<<-input$user_name; 
        set_session_status(ENV_STATUS_INPUT);
      }
      else
      {set_session_status(-10)}
    }
    
   # browser()
    if(!exists("user") || is.null(user))
    {
      list(textOutput("user"),textInput("user_name","End your user name"),passwordInput("password","Enter your password"),actionButton("sign_action","Sign In"))
      #return()
    }
    
    else
    {    
      if(ui_type==UI_TYPE_SMART)
      {
        message("You are smart!")
        mainPanel(
          tabsetPanel(type = "tabs"
           ,draw_input_tab()
           ,draw_run_tab()
           ,draw_output_tab()
           ,tabPanel(title="Log & Debug"
              ,textInput("inputCheckVar","input variable"),actionButton("inputCheckAction","evaluate"),uiOutput("inputCheckResults"),uiOutput("show_all_inputs")
            )
          )
        )
      }
    
      else if(ui_type==UI_TYPE_DUMB)
      {
        list(sidebarPanel(draw_input_tab()),
        mainPanel(
          tabsetPanel(type = "tabs"
              ,draw_output_tab()
          )
        ))  
      }
    }
  })

  
  
  
 
 
  update_input_action<-function()
  {
    out<-""
    res<-read_shiny_input(input,parent.env(environment()))  #The second argument is the environment of model_input as we pass by reference;
    if(res==0)
    {
      out<-"Inputs updated successfully"
      set_session_status(ENV_STATUS_INPUT)
    }
    else
    {
      out<-"Error updating inputs"
      set_session_status(ENV_STATUS_INPUT_ERROR)
    }
    
    return(out)
  }
     
  
  
  
  output$mainUI<-renderUI({input$load_input_name; create_interface(model_input,model_input_aux)})
  
  
    
  output$results<-renderUI(
  {
    input$updateInputAction;
    if((!is.null(input$runAction) && input$runAction>0) || (ui_type==UI_TYPE_DUMB)) 
      if(get_session_status()>=ENV_STATUS_RUN)
        HTML(renderTable(as.data.frame(process_outputs(outputs)))())
      else
        HTML("Main results will appear here once the model is run");
  })
  
  
  output$plot_smoking_status<-renderPlot(
  {
    input$updateInputAction;
    input$runAction;
    input$loadResultsAction;
    plot_smoking_status(id=input$agentOfInterest,type=input$smoking_status_plot_type)
  })
  
  output$plot_lung_function<-renderPlot(
  {
    input$updateInputAction;
    input$runAction;
    input$loadResultsAction;
    plot_lung_function(id=input$agentOfInterest,type=input$lung_function_plot_type)
  })
  
  
  output$plot_cost<-renderPlot({input$updateInputAction; input$loadResultsAction; plot_cost(id=input$agentOfInterest,type=input$cost_plot_type)})
  
  
  output$plot_qaly<-renderPlot({input$updateInputAction; input$loadResultsAction; plot_qaly(id=input$agentOfInterest,type=input$qaly_plot_type)})
  
  
  output$plot_exacerbation<-renderPlot(
  {
    input$updateInputAction;
    input$runAction;
    input$loadResultsAction; 
    plot_exacerbation(id=input$agentOfInterest,type=input$exacerbation_plot_type)
  })
  
  
  output$plot_epidemiology<-renderPlot(
  {
    input$updateInputAction;
    input$runAction; 
    input$loadResultsAction; 
    plot_epidemiology(id=input$agentOfInterest,type=input$epidemiology_plot_type);
  })
  
  
  output$ui_journey<-renderUI(
  {
    input$updateInputAction;
    input$runAction; 
    input$loadResultsAction;
    list(plotOutput("ui_journey_plot"),tableOutput("ui_journey_data"))
  })
  output$ui_journey_plot<-renderPlot(plot_journey(id=input$agentOfInterest));
  output$ui_journey_data<-renderTable(
  {
    temp<-get_agent_events(id=input$agentOfInterest);
    temp2<-matrix(unlist(temp),ncol=length(temp[[1]]),byrow=T);
    colnames(temp2)<-names(temp[[1]]);
    temp2
  })
  

  #This part is for checking input values;
  output$inputCheckResults<-renderUI({input$inputCheckAction; textOutput("inputCheckResults1")})
  output$inputCheckResults1<-renderText({input$inputCheckAction; isolate(if(input$inputCheckAction>0) {temp<-eval(parse(text=paste("input$",input$inputCheckVar,sep="")),envir=session); paste(input$inputCheckVar,"=",ifelse(is.null(input$inputCheckVar),"",temp))})})
  output$show_all_inputs<-renderUI({input$updateInputAction; HTML(generic_show_list(model_input))})
  
  
  
  
  
  
  
  draw_input_tab<-function()
  {
    return(tabPanel(title="Input"
                    ,textOutput("inputMessage")
                    ,actionButton("updateInputAction",label=ifelse(ui_type==UI_TYPE_SMART,"Update Inputs","Run"))
                    ,textOutput("updateInputMessage")
                    ,uiOutput("mainUI")
                    
                    ,div(style="background-color:#EEEEEE"
                         ,checkboxInput("input_options", "Show input saving options", FALSE)
                         ,conditionalPanel(condition = "input.input_options>0"
                                           ,selectInput("load_input_name","",width="100%", choices=c("Default input"="DEFAULT",as.list(list.files(path=paste(master_path,"/saved_inputs",sep=""),pattern="\\.inp"))))
                                           ,textOutput("loadInputMessage")
                                           ,textInput("save_input_name","Save current input as")
                                           ,actionButton("saveInputAction",label="Save Inputs")
                                           ,textOutput("saveInputMessage")
                         )
                    ) 
    )
    )
  }
  
  
  
  
  draw_run_tab<-function()
  {
    return(tabPanel(title="Run",textOutput("runMessage")
                    ,actionButton("runAction",label="Run")
                    ,numericInput("nSim","Simulation size",1000),a(target="reference",href="reference.htm#run__nSim",img(src="q.jpg")),hr()
                    ,radioButtons("agent_creation_mode", "Agent Creation Mode", c("One at a time, disposed" = 0,"One at a time, kept alive" = 1, "Pre-existing" = 3),selected=0),a(target="reference",href="reference.htm#run__agent_creation_mode",img(src="q.jpg")),hr()
                    ,radioButtons("record_mode", "Record Mode", c("none" = 0,"agent" = 1,"event" = 2),selected=2),a(target="reference",href="reference.htm#run__record_mode",img(src="q.jpg")),hr()
                    ,radioButtons("extra_outouts", "Extra Outputs", c("no" = 0,"yes" = 1),selected=0),a(target="reference",href="reference.htm#run__extra_outputs",img(src="q.jpg")),hr()
                    ,radioButtons("update_continuous_outcomes_mode", "Update Continuous Outcomes", c("when required" = 0,"any opportunity" = 1),selected=0),a(target="reference",href="reference.htm#run__update_continuous_outcomes_mode",img(src="q.jpg")),hr()
                    ,uiOutput("runtime_stats")))
  }
  
  
  
  draw_output_tab<-function()
  {
    return(tabPanel(title="Output"
                    ,conditionalPanel(ifelse(ui_type==UI_TYPE_SMART,"1","0"),actionButton("loadResultsAction","Load Results")), textOutput("loadResultsMessage"), numericInput("agentOfInterest","for id (empty will generate results for all)","1")
                    ,tabsetPanel(type = "tabs"
                                 ,tabPanel(title="Main results",uiOutput("results"))
                                 ,tabPanel(title="Epidemiology",
                                           selectInput("epidemiology_plot_type", "type:",c("Proportion alive by calendar time"="N_ALIVE_BY_CTIME","Kaplan-Meier"="KAPLAN_MEIER")),
                                           plotOutput("plot_epidemiology")
                                 ),
                                 tabPanel(title="Smoking",
                                          selectInput("smoking_status_plot_type", "type:",c("Time by smoking status"="SUM_TIME_BY_STATUS","Smoking status by calendar time"="STATUS_V_CTIME","Smoking status by age"="STATUS_V_AGE")),
                                          plotOutput("plot_smoking_status")
                                 ),
                                 tabPanel(title="Lung function",
                                          selectInput("lung_function_plot_type", "type:",c("FEV1 v time"="FEV1_V_TIME","Average FEV1 v time"="MEAN_FEV1_V_TIME","FEV1 v age"="FEV1_V_AGE","Average FEV1 v age"="MEAN_FEV1_V_AGE"),choice="MEAN_FEV1_V_TIME"),
                                          plotOutput("plot_lung_function")
                                 ),
                                 tabPanel(title="Exacerbations",
                                          selectInput("exacerbation_plot_type", "type:",c("Proportion by type"="P_BY_TYPE","Count v time"="EXAC_V_TIME","Population-average v time"="MEAN_EXAC_V_TIME")),
                                          plotOutput("plot_exacerbation")
                                 ),
                                 tabPanel(title="Costs",
                                          selectInput("cost_plot_type", "type:",c("Individualv time"="ID_V_TIME","Average v time"="MEAN_V_TIME"),choice="MEAN_V_TIME"),
                                          plotOutput("plot_cost"))
                                 ,tabPanel(title="Qaly",
                                           selectInput("qaly_plot_type", "type:",c("Individualv time"="ID_V_TIME","Average v time"="MEAN_V_TIME"),choice="MEAN_V_TIME"),
                                           plotOutput("plot_qaly"))
                                 ,tabPanel(title="Journey",
                                           uiOutput("ui_journey"))
                                 ,tabPanel(title="Download", downloadLink('downloadData', "Download results as CSV"))
                                 ,tabPanel(title="Log")
                    )
    )
    )
  }
  
  
  
  
})







