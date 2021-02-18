library(shiny)
library(DT)
library(shinythemes)
library(tools)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(forecast)

#Directions:
#Create three (3) different kinds of plots/figures
#Use DT to create one (1) data table
#Include at least two (2) different types of inputs
#One (1) functioning downloadButton()
#Inputs must use reactivity in a logical manner with all outputs displayed to users

## 1. Bar plot for average price for selected commodity region wise
## 2. Bar plot for average price for selected commodity for selected city
## 3. Line plot for average price for selected commodity over years


#read file

states <- read.csv("india_key_commodities.csv")

#convert date and extract year
states$Date <- dmy(states$Date)
states$year <- year(states$Date)
states$month <- month(states$Date)
#states$month_year >- format_ISO8601(states$Date, precision = "ym")

states$month_year <- format(states$Date, "%m-%Y")

#summarise average price
states_avg_month <- states %>% 
    group_by(year, Centre, Commodity, Region) %>% 
    summarise(price= mean(Price.per.Kg))
#convert in dollars
#assume 1 USD  = 70 INR

#variable names
city_names <- unique(states$Centre)
region_names <- unique(states$Region)
commodity_names <- unique(states$Commodity)
col_names <- names(states_avg_month)
year <-  unique(states$year)

#convert to USD
states_avg_month$price_usd <- states_avg_month$price/70

states_avg_month <- na.omit(states_avg_month)

#regional avg
region_commodity <-  states_avg_month %>% 
    group_by(Commodity,Region ) %>% 
    summarise(price = mean(price))

#city avg
city_year <-  states_avg_month %>% 
    group_by(Centre,year ) %>% 
    summarise(price = mean(price))
#convert factor
city_year$year <- as.factor(city_year$year)


#data
# UI for application
ui <- fluidPage(
    
    #Theme 
    theme = shinytheme("united"),
    
    # Application title
    titlePanel("REGION AND YEAR WISE ESSENTIAL COMMODITIES PRICE VARIATION IN INDIA"),
    
    # Sidebar layout with a input and output definitions
    sidebarLayout(
        
        # Inputs: Select variables to plot 
        sidebarPanel(
            
            # Select first plot input - Select commodity
            selectInput(inputId = "commodity", 
                        label = "Select Commodity",
                        choices = commodity_names,
                        selected = commodity_names[1]),
            
            #Select second plot input -select city
            selectInput(inputId = "city",
                        label = "City",
                        choices = city_names,
                        selected = city_names[2]),
            
            #Show first data table - Top 10 costliest city
            checkboxInput(inputId = "costliest",
                          label = "DATATABLE: Region and average prices",
                          value = TRUE),
            
            #Show second data table - 10 cheapest city
            checkboxInput(inputId = "cheapest",
                          label = "DATATABLE: City and year average prices",
                          value = TRUE),
            
            # Enter text for plot title 
            textInput(inputId = "plot_title", 
                      label = "Plot title", 
                      placeholder = "Summary and year"),
            
            
            hr()
            
        ),
        
        # Output: -------------------------------------------------------
        mainPanel(
            
            # first barchart --------------------------------------------
            plotOutput(outputId = "barchart_region"),
            br(),        # separation
            
            # second barchart
            plotOutput(outputId = "barchart_city"),
            br(),        #separation
            
            # third violin plot
            plotOutput(outputId = "violinplot_com"),
            br(),        #separation
            
            # data table
            DT::dataTableOutput(outputId = "table_costliest"),
            
            #datatable 
            DT:: dataTableOutput(outputId = "table_cheapest"),
            
            #Download button 
            downloadButton(
                outputId = "downloadbutton",
                label = "Download",
                class = NULL,
                icon = shiny::icon("download"))
            
        )
    )
)


# server function required to create the barplot
server <- function(input, output, session) {
    
    # filter subset for commodity and region 
    commodity_subset <- reactive({
        req(input$commodity) # ensure availablity of value before proceeding
        region_commodity[region_commodity$Commodity == input$commodity,]
    })
    
    #fiter city and year
    city_subset <- reactive({
        req(input$city) # ensure availability of value before proceeding
        city_year[city_year$Centre == input$city,]
    })
    
    
    
    # toTitleCase 
    pretty_plot_title <- reactive({ toTitleCase(input$plot_title) })
    
    #plot for commodity
    output$barchart_region <- renderPlot({
        ggplot(commodity_subset(),aes_string(x= commodity_subset()$Region,y= commodity_subset()$price, fill =commodity_subset()$Region ), color = "green")+
            geom_bar(stat ="identity")+
            xlab("Regions in India")+
            ylab("Avg prices for essential commodities")+
            ggtitle(pretty_plot_title())
        
        
    })
    # plot for city
    output$barchart_city <- renderPlot({
        ggplot(city_subset(),
               aes_string(x= "year",
                          y= "price" ), color = "green")+
            geom_line(aes(group = 1))+
            xlab("City in India")+
            ylab("Yearly Avg prices for essential commodities")+
            ggtitle("Price trend for commodity")
        
    })
    
    # plot for city
    output$violinplot_com <- renderPlot({
        ggplot(states_avg_month)+
            geom_violin(aes_string(x= "Commodity",y= "price", fill = "Commodity" ))+
            xlab("Prices")+
            ylab("Essential commodities")+
            ggtitle("Violin Plot for Commodities across Country")+ theme(axis.text.x = element_text(angle = 60, hjust =1, vjust =1, element_text( size = 12)))
        
    })
    
    
    # DT1
    output$table_costliest <- DT::renderDataTable(
        if(input$costliest){
            DT::datatable(data = commodity_subset(), 
                          options = list(pageLength = 10), 
                          rownames = FALSE)
        })
    
    #DT2
    output$table_cheapest <- DT::renderDataTable(
        if(input$cheapest){
            DT::datatable(data = city_subset(),
                          options = list(pagelength=10),
                          rownames = FALSE)
        }
    )
    #Download data if checked 
    output$downloadbutton <- downloadHandler(
        filename = function(){
            paste('file1.csv')
        },
        content = function(file) {
            write.csv(city_subset, file)
        }
    )
}


# Run the application 
shinyApp(ui = ui, server = server)
