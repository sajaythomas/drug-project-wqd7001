library(shiny)
library(ggplot2)
library(glue)
library(dplyr) 
library(ggwordcloud)
library(readxl)
library(leaflet)
library(geojsonio)
library(RColorBrewer)
library(viridis)
library(maps)
library(shinydashboard)
library(rintrojs)
library(shinyWidgets)
library(DT)



#icon = shiny::icon(verify_fa=FALSE)
drug_by_gender = read.csv("./Data/Number Of Drug Addicts By Sex, Malaysia.csv",header = TRUE)
drug_by_state = read.csv("./Data/Number Of Drug Addicts By State, Malaysia.csv",header = TRUE)
drug_by_gender = read.csv("./Data/Number Of Drug Addicts By Sex, Malaysia.csv",header = TRUE)
drug_by_age = read.csv("./Data/Number Of Drug Addicts By Age Group, Malaysia.csv",header = TRUE)
drug_by_type = read.csv("./Data/Number Of Addicts By Type Of Drugs, Malaysia.csv",header = TRUE)
master <- read_excel("Data/Master List.xlsx")
drug_by_reason = read_excel("./Data/jumlah-penagih-mengikut-sebab-mula-mengguna-dadah-2014-2018.xlsx")
master$Coordnates.Latitude <-  as.numeric(master$Coordnates.Latitude)
master$Coordnates.Longitude <-  as.numeric(master$Coordnates.Longitude)
master$Opiate <-  as.numeric(master$Opiate)
subset = drug_by_reason[3:9]

library(shinythemes)
mybins <- seq(0, 7000, by=100)
mypalette <- colorBin( palette="YlOrBr", domain=master$Value, na.color="transparent", bins=mybins)

mytext <- paste(
  "State: ", master$State, "<br/>", 
  "year: ", master$Year, "<br/>", 
  "Total Addicts: ", master$Value, sep="") %>%
  lapply(htmltools::HTML)


# sidebar <- dashboardSidebar(
#   sidebarMenu(
#     menuItem("Main", tabName = "Main", icon =icon("dashboard")),
#     menuItem("Trend", tabName = "Trend", icon =icon("dashboard")),
#     menuItem("Source", tabName = "Source", icon =icon("dashboard"))
#   )
# )
# header <- dashboardHeader(
#   title = "My Dashboard",
#   dropdownMenu(type = "notification",notificationItem(text = "Source"),notificationItem(text = "Github Page")))
  

ui <- fluidPage(
  dashboardPage(
    dashboardHeader(title="My Dashboard", dropdownMenu(type = "notification",notificationItem(text = "Source"), notificationItem(text = "Github Page"))),
    dashboardSidebar(  
      sidebarMenu(
        menuItem("Main", tabName = "Main", icon =icon("dashboard")),
        menuItem("Trend", tabName = "Trend", icon =icon("line-chart")),
        menuItem("Documentation", tabName = "Documentation", icon =icon("book")),
        menuItem("Source", icon =icon("file-code-o"), href = "https://github.com/sajaythomas/drug-project-wqd7001", newtab = FALSE))
      ),
    dashboardBody(
      tabItems(
        # First tab content
        tabItem(tabName = "Main",
        titlePanel(title = h2("Drug Related Cases In Malaysia", align="center")),
        column(12,box( 
          fluidRow(
            valueBoxOutput("opi",width = 1),
            valueBoxOutput("her",width = 1),
            valueBoxOutput("Ben",width = 1),
            valueBoxOutput("mor",width = 1),
            valueBoxOutput("opiu",width = 1),
            valueBoxOutput("Met",width = 1),
            valueBoxOutput("Tra",width = 1),
            valueBoxOutput("Cod",width = 1),
            valueBoxOutput("Mar",width = 1),
            valueBoxOutput("Amp",width = 1),
            valueBoxOutput("Psy",width = 1),
            valueBoxOutput("Oth",width = 1)
          ),
          fluidRow(
          ),width = 20,         
)
        ),

          fluidRow(
            wellPanel(
            column(12,box(leafletOutput("mymap",width = "100%", height = 800),width = 9),
                   box(
                     h4("Filters"),width=3,  sliderInput(inputId ="one", label = "Select A Year",value = min(master$Year)+4, min = min(master$Year),max = max(master$Year),width = '100%'),
                     selectInput(inputId = "stateselect", label = "Choose a state", choices = c("All",unique(drug_by_state$State)), multiple = FALSE)
                     , height = "250px"),),
          )
            
          ),
        
        
      fluidRow(
        column(4, 
               box(wellPanel(
                 h3("Number of Drug Addicts by Age Group"),
                 plotOutput("plot"),
                 h5(glue("This chart shows that trend of drug users by age group from  {min(drug_by_state$Year)} until {max(drug_by_state$Year)}")),
               ),width = 25)
        ),
        column(4, 
               box(wellPanel(
                 h3("Number of Drug Addicts by Gender"),
                 plotOutput("bartwo"),
                 h5(glue("This chart shows that trend of drug users by gender from  {min(drug_by_gender$Year)} until {max(drug_by_gender$Year)}")),
               ),width = 25)
        ),
        column(4, 
               box(wellPanel(
                 h3(glue("Number of Drug Addicts Died in {min(drug_by_gender$Year)}")),
                 plotOutput("death"),
                 h5(glue("This chart shows that trend of drug users by gender from  {min(drug_by_gender$Year)} until {max(drug_by_gender$Year)}")),
               ),width = 25)
        )
        
        
      )
    ),
    # Second tab content
    tabItem(tabName = "Trend",
            introBox(
              pickerInput("stateselect2",
                          "Choose a State", 
                          choices = unique(drug_by_state$State), 
                          options = list(`actions-box` = TRUE),
                          selected = drug_by_state[2, 1],
                          multiple = T),
              width = 2),
            fluidRow(
              wellPanel(h3("Trend of Drug Addicts By States"),
                        plotOutput("linetrend")),
              ),
              introBox(
                pickerInput("reasonselect",
                            "Choose a Reason", 
                            choices = colnames(subset), 
                            options = list(`actions-box` = TRUE),
                            selected = drug_by_state[2, 1]),
                width = 2),
            fluidRow(
              wellPanel(h3("Trend of Reason Using Drugs By States"),
                        plotOutput("linereason")),
            ),
    ),
    # Third tab content
    tabItem(tabName = "Documentation",
            fluidPage(
              HTML("
               <h1>Documentation</h1>
                <p>This is the documentation of this drug exploratory analysis. Each functionality is explained for all the pages. Kindly read this to gain a better understanding of the application.</p>
                <br/>
                <div style='background-color:white; padding: 5px 10px; border-radius: 5px; margin-bottom: 50px;'>
                  <h4><b>Main Page</b></h4>
                  <p>The main page consists of an interactive map that shows the density of the drug addicts in the country over the years. It can be filtered using years and state.
                  In addition to that, the main page contains charts that visualises the number of drug addicts by age, gender, and the number of addicts that died.
                  The top of the page displays the count of the most commonly used drug. Each of the elements in the main page can be filtered by the year and state.</p>
                  <br/>
                  <h4><b>Trend</b></h4>
                  <p>This tab contains the trends of the drug addicts over the years. The first chart displays the trend of addicts and it can be filtered by each state. Multiple states can be selected to
                  compare the trend among the respective states. The second chart displays the reason for using the drugs. There is a filter to choose the type of reason and it will output the trend for 
                  the state that was selected in the state filter</p>
                     <br/>
                  <h4><b>Source</b></h4>
                  <p>Clicking this tab will redirect you to the Github page where the code for the application can be found.</p>
                  <br/>
                </div>
               ")
            )
    ),
    #Fourth tab content
    tabItem(tabName = "Source",

    )
    
  ),

    )
  )
  )

server <- function(input, output,session) {
output$mymap <- renderLeaflet({
  my_year=input$one
  xxxx <-  filter(master, master$Year==my_year)
  leaflet(xxxx) %>% 
    addTiles() %>% 
    fitBounds(~min(Coordnates.Longitude), ~min(Coordnates.Latitude), ~max(Coordnates.Longitude), ~max(Coordnates.Latitude)) %>%
  addCircleMarkers(~Coordnates.Longitude, ~Coordnates.Latitude, 
                   fillColor = ~mypalette(Value),fillOpacity = 0.7, color="white", radius=~Value/250, stroke=FALSE,
                   label = mytext,labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto"))
  })




output$pieone <- renderPlot({
  ggplot(data=drug_by_gender %>% filter(Year == input$one), aes(x=Sex, y = Value, fill=Sex))+ 
    geom_bar(stat = "identity",color="white")+ geom_text(aes(label = Value), vjust = -0.2) + coord_polar("y", start=0)
})

output$death <- renderPlot({
  ggplot(data=master %>% filter(Year == input$one), aes(x=State, y = Death, fill=State))+ 
    geom_bar(stat = "identity",color="white")+ geom_text(aes(label = Death), vjust = -0.2) + 
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
})

output$opi <- renderValueBox({
  dgraf <- as.character(input$stateselect)
  my_year=input$one
  if (dgraf == "All"){xxxxx <-  filter(master, master$Year==my_year)}
  else {
  xxxxx <-  filter(master, master$Year==my_year & master$State %in% dgraf)
  }
  valueBox( 
    value = sum(xxxxx$Opiate),
    width = 1,
    subtitle = "Opiate")
})

output$her <- renderValueBox({
  dgraf <- as.character(input$stateselect)
  my_year=input$one
  if (dgraf == "All"){xxxxx <-  filter(master, master$Year==my_year)}
  else {
    xxxxx <-  filter(master, master$Year==my_year & master$State %in% dgraf)
  }
  valueBox( 
    value = sum(xxxxx$Heroin),
    width = 1,
    subtitle = "Heroin")
})

output$mor <- renderValueBox({
  dgraf <- as.character(input$stateselect)
  my_year=input$one
  if (dgraf == "All"){xxxxx <-  filter(master, master$Year==my_year)}
  else {
    xxxxx <-  filter(master, master$Year==my_year & master$State %in% dgraf)
  }
  valueBox( 
    value = sum(xxxxx$Morphine),
    width = 1,
    subtitle = "Morphine")
})

output$opiu <- renderValueBox({
  dgraf <- as.character(input$stateselect)
  my_year=input$one
  if (dgraf == "All"){xxxxx <-  filter(master, master$Year==my_year)}
  else {
    xxxxx <-  filter(master, master$Year==my_year & master$State %in% dgraf)
  }
  valueBox( 
    value = sum(xxxxx$Opium),
    width = 2,
    subtitle = "Opium")
})

output$Cod <- renderValueBox({dgraf <- as.character(input$stateselect)
my_year=input$one
if (dgraf == "All"){xxxxx <-  filter(master, master$Year==my_year)}
else {
  xxxxx <-  filter(master, master$Year==my_year & master$State %in% dgraf)
} 
valueBox(value = sum(xxxxx$Codeine),width = 1,subtitle = "Codeine")})

output$Mar <- renderValueBox({dgraf <- as.character(input$stateselect)
my_year=input$one
if (dgraf == "All"){xxxxx <-  filter(master, master$Year==my_year)}
else {
  xxxxx <-  filter(master, master$Year==my_year & master$State %in% dgraf)
} 
valueBox(value = sum(xxxxx$Marijuana),width = 1,subtitle = "Marijuana")})

output$Amp <- renderValueBox({dgraf <- as.character(input$stateselect)
my_year=input$one
if (dgraf == "All"){xxxxx <-  filter(master, master$Year==my_year)}
else {
  xxxxx <-  filter(master, master$Year==my_year & master$State %in% dgraf)
} 
valueBox(value = sum(xxxxx$Amphetamine-type.stimulants.(ATS)),width = 1,subtitle = "Amphetamine-type stimulants (ATS)")})

output$Ect <- renderValueBox({dgraf <- as.character(input$stateselect)
my_year=input$one
if (dgraf == "All"){xxxxx <-  filter(master, master$Year==my_year)}
else {
  xxxxx <-  filter(master, master$Year==my_year & master$State %in% dgraf)
}
valueBox(value = sum(xxxxx$Ectasy.Pill),width = 1,subtitle = "Ectasy Pill")})

output$Amp <- renderValueBox({dgraf <- as.character(input$stateselect)
my_year=input$one
if (dgraf == "All"){xxxxx <-  filter(master, master$Year==my_year)}
else {
  xxxxx <-  filter(master, master$Year==my_year & master$State %in% dgraf)
}
valueBox(value = sum(xxxxx$Amphetamine),width = 1,subtitle = "Amphetamine")})

output$Met <- renderValueBox({dgraf <- as.character(input$stateselect)
my_year=input$one
if (dgraf == "All"){xxxxx <-  filter(master, master$Year==my_year)}
else {
  xxxxx <-  filter(master, master$Year==my_year & master$State %in% dgraf)
} 
valueBox(value = sum(xxxxx$mymeta),width = 1,subtitle = "Metamphetamine")})

output$Psy <- renderValueBox({dgraf <- as.character(input$stateselect)
my_year=input$one
if (dgraf == "All"){xxxxx <-  filter(master, master$Year==my_year)}
else {
  xxxxx <-  filter(master, master$Year==my_year & master$State %in% dgraf)
} 
valueBox(value = sum(xxxxx$Psychotropic.Pill),width = 1,subtitle = "Psychotropic Pill")})



output$Ben <- renderValueBox({dgraf <- as.character(input$stateselect)
my_year=input$one
if (dgraf == "All"){xxxxx <-  filter(master, master$Year==my_year)}
else {
  xxxxx <-  filter(master, master$Year==my_year & master$State %in% dgraf)
}
valueBox(value = sum(xxxxx$Benzoylecgonine),width = 1,subtitle = "Benzoylecgonine")})

output$Tra <- renderValueBox({dgraf <- as.character(input$stateselect)
my_year=input$one
if (dgraf == "All"){xxxxx <-  filter(master, master$Year==my_year)}
else {
  xxxxx <-  filter(master, master$Year==my_year & master$State %in% dgraf)
} 
valueBox(value = sum(xxxxx$Tramadol),width = 1,subtitle = "Tramadol")})

output$Oth <- renderValueBox({dgraf <- as.character(input$stateselect)
my_year=input$one
if (dgraf == "All"){xxxxx <-  filter(master, master$Year==my_year)}
else {
  xxxxx <-  filter(master, master$Year==my_year & master$State %in% dgraf)
} 
valueBox(value = sum(xxxxx$Others),width = 1,subtitle = "Others")})


output$bartwo <- renderPlot({
  dgraf <- as.character(input$stateselect)  

  if (dgraf == "Johor")
    ggplot(data=drug_by_gender %>% filter(Year == input$one), aes(x=Sex, y = Johor, fill=Sex))+ geom_bar(stat = "identity",  width=0.2)+
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) + coord_polar("y", start=0) + geom_text(aes(label = Johor), vjust = -0.2)
  
  else if (dgraf == "Kedah")
    ggplot(data=drug_by_gender %>% filter(Year == input$one), aes(x=Sex, y = Kedah, fill=Sex))+ geom_bar(stat = "identity",  width=0.2)+
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5))+ coord_polar("y", start=0)+ geom_text(aes(label = Kedah), vjust = -0.2)
  
  else if (dgraf == "Kelantan")
    ggplot(data=drug_by_gender %>% filter(Year == input$one), aes(x=Sex, y = Kelantan, fill=Sex))+ geom_bar(stat = "identity",  width=0.2)+
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5))+ coord_polar("y", start=0)+ geom_text(aes(label = Kelantan), vjust = -0.2)
  
  else if (dgraf == "Melaka")
    ggplot(data=drug_by_gender %>% filter(Year == input$one), aes(x=Sex, y = Melaka, fill=Sex))+ geom_bar(stat = "identity",  width=0.2)+
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5))+ coord_polar("y", start=0)+ geom_text(aes(label = Melaka), vjust = -0.2)
  
  else if (dgraf == "Negeri Sembilan")
    ggplot(data=drug_by_gender %>% filter(Year == input$one), aes(x=Sex, y = Negeri.Sembilan, fill=Sex))+ geom_bar(stat = "identity",  width=0.2)+
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5))+ coord_polar("y", start=0)+ geom_text(aes(label = Negeri.Sembilan), vjust = -0.2)
  
  else if (dgraf == "Pahang")
    ggplot(data=drug_by_gender %>% filter(Year == input$one), aes(x=Sex, y = Pahang, fill=Sex))+ geom_bar(stat = "identity",  width=0.2)+
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5))+ coord_polar("y", start=0)+ geom_text(aes(label = Pahang), vjust = -0.2)
  
  else if (dgraf == "Perak")
    ggplot(data=drug_by_gender %>% filter(Year == input$one), aes(x=Sex, y = Perak, fill=Sex))+ geom_bar(stat = "identity",  width=0.2)+
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5))+ coord_polar("y", start=0)+ geom_text(aes(label = Perak), vjust = -0.2)
  
  else if (dgraf == "Perlis")
    ggplot(data=drug_by_gender %>% filter(Year == input$one), aes(x=Sex, y = Perlis, fill=Sex))+ geom_bar(stat = "identity",  width=0.2)+
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5))+ coord_polar("y", start=0)+ geom_text(aes(label = Perlis), vjust = -0.2)
  else if (dgraf == "Sabah")
    ggplot(data=drug_by_gender %>% filter(Year == input$one), aes(x=Sex, y = Sabah, fill=Sex))+ geom_bar(stat = "identity",  width=0.2)+
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5))+ coord_polar("y", start=0)+ geom_text(aes(label = Sabah), vjust = -0.2)
  
  else if (dgraf == "Sarawak")
    ggplot(data=drug_by_gender %>% filter(Year == input$one), aes(x=Sex, y = Sarawak, fill=Sex))+ geom_bar(stat = "identity",  width=0.2)+
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5))+ coord_polar("y", start=0)+ geom_text(aes(label = Sarawak), vjust = -0.2)
  
  else if (dgraf == "Terengganu")
    ggplot(data=drug_by_gender %>% filter(Year == input$one), aes(x=Sex, y = Terengganu, fill=Sex))+ geom_bar(stat = "identity",  width=0.2)+
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5))+ coord_polar("y", start=0)+ geom_text(aes(label = Terengganu), vjust = -0.2)
  
  else if (dgraf == "Kuala Lumpur")
    ggplot(data=drug_by_gender %>% filter(Year == input$one), aes(x=Sex, y = Kuala.Lumpur, fill=Sex))+ geom_bar(stat = "identity",  width=0.2)+
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5))+ coord_polar("y", start=0)+ geom_text(aes(label = Kuala.Lumpur), vjust = -0.2)
  
  else if (dgraf == "Labuan")
    ggplot(data=drug_by_gender %>% filter(Year == input$one), aes(x=Sex, y = Labuan, fill=Sex))+ geom_bar(stat = "identity",  width=0.2)+
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5))+ coord_polar("y", start=0)+ geom_text(aes(label = Labuan), vjust = -0.2)
  
  else if (dgraf == "Selangor")
    ggplot(data=drug_by_gender %>% filter(Year == input$one), aes(x=Sex, y = Selangor, fill=Sex))+ geom_bar(stat = "identity",  width=0.2)+
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5))+ coord_polar("y", start=0)+ geom_text(aes(label = Selangor), vjust = -0.2)
  
  else if (dgraf == "Putrajaya")
    ggplot(data=drug_by_gender %>% filter(Year == input$one), aes(x=Sex, y = Putrajaya, fill=Sex))+ geom_bar(stat = "identity",  width=0.2)+
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5))+ coord_polar("y", start=0)+ geom_text(aes(label = Putrajaya), vjust = -0.2)
  
  else if (dgraf == "All")
    ggplot(data=drug_by_gender %>% filter(Year == input$one), aes(x=Sex, y = Value, fill=Sex))+ geom_bar(stat = "identity",  width=0.2)+
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5))+ coord_polar("y", start=0)+ geom_text(aes(label = Value), vjust = -0.2)
  
  
})


output$plot <- renderPlot({
  
  dgraf <- as.character(input$stateselect)  
  if (dgraf == "Johor")
  ggplot(data=drug_by_age %>% filter(Year %in% input$one), aes(x=Age.group,  y =Johor ,fill=Age.group))+ geom_bar(stat = "identity", width=0.7)+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ coord_flip()+ geom_text(aes(label = Johor, vjust = -0.2))
  
  else if (dgraf == "Kedah")
  ggplot(data=drug_by_age %>% filter(Year %in% input$one), aes(x=Age.group,  y =Kedah ,fill=Age.group))+ geom_bar(stat = "identity",  width=0.7)+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ coord_flip()
  
  else if (dgraf == "Kelantan")
  ggplot(data=drug_by_age %>% filter(Year %in% input$one), aes(x=Age.group,  y =Kelantan ,fill=Age.group))+ geom_bar(stat = "identity",  width=0.7)+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ coord_flip()
  
  else if (dgraf == "Melaka")
  ggplot(data=drug_by_age %>% filter(Year %in% input$one), aes(x=Age.group,  y =Melaka ,fill=Age.group))+ geom_bar(stat = "identity",  width=0.7)+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ coord_flip()
  
  else if (dgraf == "Negeri Sembilan")
  ggplot(data=drug_by_age %>% filter(Year %in% input$one), aes(x=Age.group,  y =Negeri.Sembilan ,fill=Age.group))+ geom_bar(stat = "identity",  width=0.7)+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ coord_flip()

  else if (dgraf == "Pahang")
  ggplot(data=drug_by_age %>% filter(Year %in% input$one), aes(x=Age.group,  y =Pahang ,fill=Age.group))+ geom_bar(stat = "identity",  width=0.7)+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ coord_flip()
  
  else if (dgraf == "Perak")
    ggplot(data=drug_by_age %>% filter(Year %in% input$one), aes(x=Age.group,  y =Perak ,fill=Age.group))+ geom_bar(stat = "identity",  width=0.7)+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ coord_flip()
  
  else if (dgraf == "Selangor")
    ggplot(data=drug_by_age %>% filter(Year %in% input$one), aes(x=Age.group,  y =Selangor ,fill=Age.group))+ geom_bar(stat = "identity",  width=0.7)+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ coord_flip()
  
  else if (dgraf == "Perlis")
    ggplot(data=drug_by_age %>% filter(Year %in% input$one), aes(x=Age.group,  y = Perlis ,fill=Age.group))+ geom_bar(stat = "identity",  width=0.7)+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ coord_flip()

  else if (dgraf == "Sabah")
    ggplot(data=drug_by_age %>% filter(Year %in% input$one), aes(x=Age.group,  y =Sabah ,fill=Age.group))+ geom_bar(stat = "identity",  width=0.7)+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ coord_flip()
  
  else if (dgraf == "Sarawak")
    ggplot(data=drug_by_age %>% filter(Year %in% input$one), aes(x=Age.group,  y =Sarawak ,fill=Age.group))+ geom_bar(stat = "identity",  width=0.7)+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ coord_flip()
  
  else if (dgraf == "Pulau Pinang")
    ggplot(data=drug_by_age %>% filter(Year %in% input$one), aes(x=Age.group,  y =Pulau.Pinang ,fill=Age.group))+ geom_bar(stat = "identity",  width=0.7)+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ coord_flip()
  
  else if (dgraf == "Terengganu")
    ggplot(data=drug_by_age %>% filter(Year %in% input$one), aes(x=Age.group,  y =Terengganu ,fill=Age.group))+ geom_bar(stat = "identity",  width=0.7)+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ coord_flip()
  
  else if (dgraf == "Kuala Lumpur")
    ggplot(data=drug_by_age %>% filter(Year %in% input$one), aes(x=Age.group,  y =Kuala.Lumpur ,fill=Age.group))+ geom_bar(stat = "identity",  width=0.7)+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ coord_flip()
  
  else if (dgraf == "Labuan")
    ggplot(data=drug_by_age %>% filter(Year %in% input$one), aes(x=Age.group,  y =Labuan ,fill=Age.group))+ geom_bar(stat = "identity",  width=0.7)+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ coord_flip()
  
  else if (dgraf == "Putrajaya")
    ggplot(data=drug_by_age %>% filter(Year %in% input$one), aes(x=Age.group,  y =Putrajaya ,fill=Age.group))+ geom_bar(stat = "identity",  width=0.7)+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ coord_flip()
  
  else if (dgraf == "All")
    ggplot(data=drug_by_age %>% filter(Year %in% input$one), aes(x=Age.group,  y =Value ,fill=Age.group))+ geom_bar(stat = "identity",  width=0.7)+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ coord_flip()
  
  
  })

  output$linetrend <- renderPlot({
    ggplot(data=drug_by_state %>% filter(State %in% input$stateselect2), aes(x=Year, y=Value, fill=State)) +
      geom_line(aes(color=State))+
      geom_point(aes(color=State))+
      scale_x_continuous(n.breaks = 20)
  })
  
 
  output$linereason <- renderPlot({
    reason <- as.character(input$reasonselect)
    ggplot(data=drug_by_reason %>% filter(State %in% input$stateselect2), aes_string(x="Year", y=input$reasonselect, fill="State")) +
      geom_line(aes(color=State))+
      geom_point(aes(color=State))+
      scale_x_continuous(n.breaks = 20)
  })
  # output$linereason <- renderPlot({
  #   select_quo <- quo(input$reasonselect)
  #   
  #   drug_by_reason %>%
  #     mutate(user_input = !!select_quo) %>%
  #     ggplot(data=drug_by_reason %>% filter(State %in% input$stateselect), aes(fill=State,  y=user_input, x=Year)) + 
  #     geom_line(aes(color=State))+
  #     geom_point(aes(color=State))+
  #     scale_x_continuous(n.breaks = 20)
  # 
  # })
}
shinyApp(ui, server)