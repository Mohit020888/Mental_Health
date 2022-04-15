#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#importing raw survey data 
#D:\Sem3_DA\CPSC\Exam
# install.packages("plotly")
# install.packages("lazyeval")
# install.packages("ggmap")

install.packages("gridExtra")

library(shiny)
library(tidyverse)
library(DT)

library(plotly)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(leaflet.minicharts)
library(maps)
library(mapview)
library(htmltools)
# testing different map 
library(scatterpie)
#read.csv("mycsv.csv")
#C:\Users\mohit\OneDrive\Documents
survey_raw_df <- read.csv("C:/Users/mohit/OneDrive/Documents/survey.csv")
str(survey_raw_df)
View(survey_raw_df)


#Importing required columns which we will consider for our analysis and cleaning age column
survey_raw_df<- survey_raw_df %>% 
  select(Age, Gender, Country, state,family_history,work_interfere,self_employed,remote_work,tech_company,care_options,wellness_program,seek_help,anonymity,leave, treatment, benefits,supervisor,mental_health_interview, mental_health_consequence, phys_health_interview, mental_vs_physical, obs_consequence) %>% 
  filter(Age>=18 & Age<=65) 
  

male_str <- c("male","cis male","Male (CIS)", "m", "M", "Male-ish", "maile", "Mal", "Male", "Cis Male", "Make", "male ", "Man","msle", "Mail", "Malr","Cis Man", "Guy (-ish) ^_^")
trans_str <- c("male leaning androgynous","Female (trans)","Trans-female", "something kinda male?", "queer/she/they", "non-binary","Nah", "All", "Enby", "fluid", "Genderqueer", "Androgyne", "Agender", "male leaning androgynous", "guy (-ish) ^_^", "Trans woman", "Neuter", "female (trans)", "queer", "ostensibly male, unsure what that really means" )
female_str <- c("Female","woman", "Female (cis)", "f", "F", "female", "Woman",  "Femake", "Female","cis-female/femme", "Cis Female", "femail")

survey_raw_df <- survey_raw_df %>%
  filter(Gender != "a little about you",
         Gender != "p")
survey_raw_df$Gender <- sapply(as.vector(survey_raw_df$Gender), function(x) if(x %in% male_str) "Male" else x )
survey_raw_df$Gender <- sapply(as.vector(survey_raw_df$Gender), function(x) if(x %in% female_str) "Female" else x )
survey_raw_df$Gender <- sapply(as.vector(survey_raw_df$Gender), function(x) if(x %in% trans_str) "Trans" else x )
View(survey_raw_df)
summary(survey_raw_df)
str(survey_raw_df)

  
View(survey_raw_df)
summary(survey_raw_df)


#remove countries with less than 10 data points
clean_data <- survey_raw_df 

countries_df <- clean_data %>%
group_by(Country) %>%
summarize(n=n()) %>%
filter(n>=10) %>%
select(Country)

countries_df


clean_data <-
  clean_data %>%
  filter(Country %in% countries_df$Country)
clean_data
str(clean_data)

sum(is.na(clean_data$Country=="United States"))
summary(clean_data)

sapply(clean_data, function(x) sum(is.na (x)))


#CHECKING TO REMOVE REST OF THE NA VALUES, WE KEEP STATE NA VALUES AS ONLY USA HAS STATE DISTINCTION
unique(clean_data$self_employed) #DROP NA
unique(clean_data$work_interfere) #DROP NA


clean_data = clean_data[complete.cases(clean_data$self_employed),]
clean_data = clean_data[complete.cases(clean_data$work_interfere),]

#write.csv(clean_data,"D:/Sem3_DA/CPSC/Exam/survey_cleaned.csv")

library(ggmap)
library(tidyverse)

#C:\Users\mohit\OneDrive\Desktop
# api <- readLines("C:/Users/mohit/OneDrive/Desktop/google.api.txt") # Text file with the API key
# register_google(key = api)
data <- read.csv("C:/Users/mohit/OneDrive/Documents/survey_cleaned.csv")
# getOption("ggmap")
# 
# countries <- unique(data$Country)
# 
# countries_df <- data.frame(country = countries)
# countries_df %>% mutate_if(is.factor, as.character) -> countries_df
# locations_df <- mutate_geocode(countries_df, country, source = "google")
# 
# geocode(locations_df, source='google')
# geocode

# str(countries_df$country)

mapStates = map("world", fill = TRUE, plot = FALSE)
mapStates
#D:\Sem3_DA\shiny_code
geo_location <- read.csv("D:/Sem3_DA/shiny_code/countries.csv")
geo_location
mylist <- c(
  "Have sought treamtent for mental condition?" = "treatment",
  "Does your employer provide mental health benefits?" = "benefits",
  "Possible consequence if you discuss a mental health issue with employer?" = "mental_health_consequence",
  "Would you bring up a physical health issue with a potential employer in an interview?" = "phys_health_interview",
  "Do you feel that your employer takes mental health as seriously as physical health?" = "mental_vs_physical",
  "Have you heard of or observed negative consequence for coworkers with metnal health condition at workplace?" = "obs_consequence"
)

status <- list(
  "treatment" = "Have sought treamtent for mental condition?" ,
  "benefits" ="Do employers provide mental health benefits?",
  "mental_health_consequence" ="Possible consequence from disucssing mental health with employers?" ,
  "phys_health_interview" = "Would you bring up a physical health issue in an interview?" ,
  "mental_vs_physical" ="Does employer takes mental health as seriously as physical health?" ,
  "obs_consequence" = "Any negative consequence with mental health condition at workplace?"
)
lot_labeller <- function(variable,value){
  if (variable=='question') {
    return(str_wrap(status[[value]],width = 40))
  } 
  else
    #return(str_wrap(names(countryList)[value],width = 40))
    return("")
}

# extract country list choices
countries<-unique(sort(data$Country))
country_choices = data.frame(
  var = countries, # need to change it to the values inside the "Question" Column
  num = countries
)
# List of choices for selectInput
countryList <- as.list(country_choices$num)
# Name it
names(countryList) <- country_choices$var
print(countryList)


# extract state values for choices 
states <- unique(sort(data$state))
state_choices = data.frame(
  var = states,
  num = states
)
# List of choices for selectInput
stateList <- as.list(state_choices$num)
# Name it
names(stateList) <- state_choices$var

library(lazyeval)
library(plotly)
ui <- dashboardPage(skin = "blue",
                    #dashboardHeader(title="Mental Health Perception", titleWidth= 300 ),
                    dashboardHeader(title = "Mental Health Perception", titleWidth = 300,
                                    #dropdownMenu(icon=icon("info-circle")),
                                    tags$li(a(href = 'www.github.com', 
                                              
                                              title = "GitHub"),
                                            class = "dropdown")),
                    
                    dashboardSidebar( 
                      # Columns selector
                      uiOutput("columns"),
                      uiOutput("singleColumn"),
                      
                      # Location selector
                      uiOutput("country"),
                      
                      uiOutput("state"),
                      
                      
                      # age selector
                      sliderInput("ages",
                                  "Age Range:",
                                  min = 18,
                                  max = 65,
                                  value = c(20,60)),
                      
                      # Genders filters
                      checkboxGroupInput("genders", "Gender",
                                         choices = list("Male" = "Male",
                                                        "Female" = "Female",
                                                        "Trans" = "Trans"),
                                         selected = c("Male", "Female", "Trans")
                      ),
                      width=300),
                    dashboardBody( min_width=950,
                                   tabBox(id="tabs", type = "tabs", width = 12,
                                          tabPanel("Country Map", 
                                                   leafletOutput("map", height= "800px"), 
                                                   hr()),
                                          
                                          tabPanel("Compare Countries", tags$p("Select Survey Questions and Countries for comparison"), hr(),
                                                   plotlyOutput("plot", 
                                                                height= "790px", inline=FALSE )),
                                          tabPanel("Compare US States", tags$p("Select Survey Questions and US states for comparison"), hr(),
                                                   plotlyOutput("state_plot", 
                                                                height= "790px", inline=FALSE )),
                                          
                                          tabPanel("Data Explorer", 
                                                   tags$p("Select Survey Questions and Country for data exploring"),
                                                   br(),
                                                   dataTableOutput("table"))
                                          
                                   )
                    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Get the reactive for the data after filter   
  data_filtered <- reactive(data %>%
                              # filter here 
                              filter(Country %in% input$country | is.null(input$country )) %>% # filter country
                              # filter state
                              # keep country that is not United States OR Country is United state then filter State, Show all States if no State selected
                              filter(Country != "United States" | (Country == "United States" & (state %in% input$state | is.null(input$state))) ) %>%
                              # filter age
                              filter(between(Age, input$ages[1],input$ages[2])) %>%
                              # filter gender 
                              filter(Gender %in% input$genders)
  )
  # get the data from the filtered and only select the columns that user select
  data_selected <- reactive(
    data_filtered() %>%
      select(input$columns) # need to append the filters values such as gender and age
  )
  
  # map use different filter because we are not filtering countries
  map_data_filtered <- reactive(
    data %>%
      filter(between(Age, input$ages[1],input$ages[2])) %>%
      # filter gender 
      filter(Gender %in% input$genders) %>%
      select(Country, input$singleColumn)
  )
  
  # group the data for chart later 
  data_chart_input <- reactive(
    # data_selected() %>%
    data_filtered() %>%
      gather(key = "question", value = "answer", one_of(input$columns))
  )
  
  chart_input <- reactive(
    
    if(length(input$country) == 0)
    {
      data_chart_input() %>%
        ggplot(aes(x = answer, fill = Country)) +
        geom_bar() +
        facet_wrap( ~ question,ncol = 3,  scales="free", labeller = lot_labeller ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), 
              panel.spacing.x=unit(3.0, "lines"),
              panel.spacing.y=unit(1.0, "lines"),
              text = element_text(size=14) , 
              axis.title.x=element_blank(),
              axis.title.y = element_blank(),
              strip.background = element_blank()
              
        ) 
    }
    else
    {
      data_chart_input() %>%
        ggplot(aes(x = answer, fill = Country)) +
        geom_bar() +
        coord_cartesian(clip = "off")+
        facet_wrap( question ~ Country,
                    ncol = length(input$country),  
                    scales="free",  
                    strip.position = "top",
                    labeller = lot_labeller) +
        
        theme(
          panel.spacing.x=unit(3.0, "lines"),
          panel.spacing.y=unit(2.0, "lines"),
          text = element_text(size=12) , 
          axis.title.x=element_blank(),
          axis.title.y = element_blank(),
          strip.background = element_blank(),
          
        ) 
    }
    
  )
  
  # Datatable
  
  output$table <- renderDataTable(
    data_selected()
    #data_chart_input()
  )
  
  # Barchart
  output$plot <- renderPlotly({
    # print(head(data_chart_input()))
    if(length(input$columns) !=  0)
      ggplotly(chart_input()) 
  })
  
  # State compare plot 
  output$state_plot <- renderPlotly({
    
    if( length(input$columns) !=  0 )
    {
      ggplotly(
        
        if(length(input$state) == 0)
        {
          return()
        }
        else
        {
          data_chart_input() %>%
            filter(state %in% input$state) %>%
            ggplot(aes(x = answer, fill = state)) +
            geom_bar() +
            coord_cartesian(clip = "off")+
            facet_wrap( question ~ state,
                        ncol = length(input$state),  
                        scales="free",  
                        strip.position = "top",
                        labeller = lot_labeller) +
            
            theme(
              panel.spacing.x=unit(3.0, "lines"),
              panel.spacing.y=unit(2.0, "lines"),
              text = element_text(size=12) , 
              axis.title.x=element_blank(),
              axis.title.y = element_blank(),
              strip.background = element_blank(),
              
            ) 
        }
        
      ) 
    }
    
    
  })
  
  # Map
  output$map <- renderLeaflet({
    # BEWARE, need to check if the all the countries are in the table and has proper geolocation
    data <- map_data_filtered()
    
    if(nrow(data) == 0)
      return(  leaflet( options = leafletOptions(minZoom = 2, maxZoom = 5)) %>%
                 addProviderTiles("OpenStreetMap.Mapnik"))
    
    values <- unique(data[, input$singleColumn])
    
    spread_data <-data %>%
      group_by(Country, value = data[1:nrow(data), input$singleColumn]) %>%
      summarise(n = n())
    # complete(Country, fill = list(value = 0)) %>%
    
    
    spread_data <- spread_data %>%
      spread(value, n ) %>%
      inner_join(geo_location, by =c("Country" = "name")) %>%
      select(-c("X", "X.1","X.2","country"))
    
    cols <- colnames(spread_data)
    cols <- cols[!( cols%in% c("Country", "latitude", "longitude"))]
    print(cols)
    
    spread_data[is.na(spread_data)] <- 0
    
    spread_data$total <- rowSums(spread_data[,cols], na.rm=TRUE)
    
    print(head(spread_data))
    
    leaflet(data = spread_data, options = leafletOptions(minZoom = 2, maxZoom = 5)) %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>% 
      addLabelOnlyMarkers(lng = ~longitude, 
                          lat = ~latitude, 
                          label = ~Country,
                          labelOptions = labelOptions(noHide = TRUE, 
                                                      textOnly = FALSE, 
                                                      direction = "top", 
                                                      textsize = "12px",
                                                      offset = c(0,-30))) %>%
      addMinicharts(
        spread_data$longitude, spread_data$latitude,
        type = "pie",
        chartdata = spread_data[, cols], 
        transitionTime = 0, 
        width = 100* sqrt(spread_data$total) / sqrt(max(spread_data$total)), 
        #width = 50, height = 60,
        labelText = spread_data$Country, 
        #showLabels = TRUE,
        popup = popupArgs(showTitle = TRUE, showValues = TRUE),
        opacity = 0.8
      ) %>%
      #addCircleMarkers(label=~Country, opacity = 0.0, radius= 10.0001)%>%
      setView(-71.0382679, 42.3489054, zoom = 3)
    
  })
  
  observe({
    print(input$tabs)
  })
  
  # only show column question with multiple select if we are NOT on the map tab
  output$columns <- renderUI(
    if(input$tabs != "Country Map")
    {
      selectizeInput("columns", "Survey Questions",
                     choices= mylist,
                     multiple = TRUE, # was True 
                     #selected = c("Gender", "Country","Age"), # can remove later
                     options = list(maxItems = 3)
      )
    }
    else
      return()
    
  )
  
  # only show single column if on country map
  output$singleColumn <- renderUI(
    
    if(input$tabs == "Country Map" | input$tabs == "Second Map" )
    {
      selectizeInput("singleColumn", "Survey Question",
                     choices= mylist,
                     multiple = FALSE, 
      )
    }
    else
      return()
  )
  
  output$country <- renderUI(
    if(input$tabs == "Compare Countries" | input$tabs == "Data Explorer")
    {
      
      selectizeInput("country", "Country",
                     choices = countryList, multiple = TRUE,
                     options = list(maxItems = 2))
    }
    else
    {
      
      return()
    }
  )
  
  output$state <- renderUI(
    
    if(input$tabs == "Compare US States")
    {
      selectizeInput("state", "States",
                     choices = stateList, multiple = TRUE,
                     options = list(maxItems = 2))
    }
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)