library(shiny)
library(ggplot2)
library(glue)
library(dplyr) 
library(ggwordcloud)


drug_by_state = read.csv("./Data/Number Of Drug Addicts By State, Malaysia.csv",header = TRUE)
drug_by_gender = read.csv("./Data/Number Of Drug Addicts By Sex, Malaysia.csv",header = TRUE)
drug_by_age = read.csv("./Data/Number Of Drug Addicts By Age Group, Malaysia.csv",header = TRUE)
drug_by_type = read.csv("./Data/Number Of Addicts By Type Of Drugs, Malaysia.csv",header = TRUE)
drug_feedback = read.csv("./Data/drugsComTest_raw.csv",header = TRUE)


ui <- fluidPage(
  titlePanel(title = h2("Exploring Shiny With Drug Data from Malaysia Gov", align="center")),
  h5("This is just to explore Shiny's functionality.  The data was gathered from the Government of Malaysia."),
  h5("The files are not merged together and is run based on each files. "),
  
  sliderInput(inputId ="one", label = "Select A Year",value = min(drug_by_state$Year)+4, min = min(drug_by_state$Year),max = max(drug_by_state$Year),width = '100%' ,animate =TRUE),
  animationOptions(interval = 1000, loop = TRUE),
  
  
  fluidRow(
          wellPanel(h3("Among The Reason Why Some Are Addicted to Some Kind of Substance"),
          plotOutput("wordycloud"),),
    
    
    column(6,
           wellPanel(
             h3("Number of Drug Addicts by States"),
             plotOutput("barone"),
             h5(glue("This chart shows that trend of drug users from  {min(drug_by_state$Year)} until {max(drug_by_state$Year)}"))
             )
           
           ),
    
    column(6, 
           wellPanel(
             h3("Number of Drug Addicts by Gender"),
             plotOutput("bartwo"),
             h5(glue("This chart shows that trend of drug users by gender from  {min(drug_by_state$Year)} until {max(drug_by_state$Year)}")),
           )
    ),
    column(6, 
           wellPanel(
             h3("Number of Drug Addicts by Age Group"),
             plotOutput("barthree"),
             h5(glue("This chart shows that trend of drug users by age group from  {min(drug_by_state$Year)} until {max(drug_by_state$Year)}")),
             )
    ),
    column(6, 
           wellPanel(
             h3("Type of drug that is being Abused across the years"),
             plotOutput("barfour"),
             h5(glue("This chart shows that trend of drug users by gender from  {min(drug_by_state$Year)} until {max(drug_by_state$Year)}")),
            )
    )
  ))

  
server <- function(input,output){
    output$barone <- renderPlot({
      ggplot(data=drug_by_state %>% filter(Year == input$one), aes(x=State, y = Value, fill=State))+ 
        geom_bar(stat = "identity")+ geom_text(aes(label = Value), vjust = -0.2) +  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
      
      
    })
    output$bartwo <- renderPlot({
      ggplot(data=drug_by_gender %>% filter(Year == input$one), aes(x=Sex, y = Value, fill=Sex))+ 
        geom_bar(stat = "identity",color="white")+ geom_text(aes(label = Value), vjust = -0.2) + coord_polar("y", start=0)
      
      
    })
    output$barthree <- renderPlot({
      ggplot(data=drug_by_age %>% filter(Year == input$one), aes(x=Age.group, y = Value, fill=Age.group))+ geom_bar(stat = "identity")+
        theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
      
      
    })
    output$barfour <- renderPlot({
      ggplot(data=drug_by_type %>% filter(Year == input$one), aes(x=drug_type, y = Value, fill=drug_type))+ geom_bar(stat = "identity", fill=alpha("skyblue", 0.7))+ylim(-100,120)+coord_polar(start = 0)+ geom_text(aes(label = Value), vjust = -0.2)
      
      
      
    })
    output$wordycloud <-  renderPlot({
      ggplot(data= drug_feedback %>% filter(year == input$one), aes(label = cause_of_addiction)) +geom_text_wordcloud(eccentricity  = 1)+scale_size_area(max_size = 24) +
        theme_minimal() +   scale_color_gradient(low = "darkred", high = "red")
      
    })
  }
  
  shinyApp(ui = ui,server=server)