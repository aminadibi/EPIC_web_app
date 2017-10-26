library(shiny)
library(shinythemes)

source("basics.r")




draw_ui<-function()
{
  if(ui_type==UI_TYPE_SMART)
  {
    return(
      fluidPage(theme = shinytheme("united"),
        
        
        #headerPanel("Evaluation Platform in COPD (EPIC)"),
        
        mainPanel(
          textOutput("app_status_message"),textOutput("global_message"),uiOutput("sign_in")                      
          ,conditionalPanel("document.getElementById('user').innerHTML.length>0"
          ,tabsetPanel("main"
                       
                       ,draw_input_tab()
                       
                       ,draw_run_tab()
                       
                       ,draw_output_tab()
                       
                       ,tabPanel(title="Log & Debug",
                                 textInput("inputCheckVar","input variable"),actionButton("inputCheckAction","evaluate"),uiOutput("inputCheckResults"),
                                 uiOutput("show_all_inputs")
                       
                       )
              )
            )
        )
      )
    )
  }
  else
  {
    return(
      fluidPage(
        #headerPanel("Evaluation Platform in COPD (EPIC)"),
        sidebarPanel(draw_input_tab())
        ,mainPanel(
          textOutput("app_status_message"),textOutput("global_message")
           ,tabsetPanel("main"
                       ,draw_output_tab(),draw_run_tab()
                       )
            )
          )
      )
  }
}



fluidPage(
  #headerPanel("Evaluation Platform in COPD (EPIC)"),
  textOutput("global_message")
  ,uiOutput("allUI")  
  #draw_ui()
)


