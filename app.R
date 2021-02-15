
# libraries to include
library(shiny)
library(shinydashboard)
library(reshape2)
library(leaflet)
library(ggplot2)
library(DT)
library(usmap)
# Prevent numbers from going to Scientific Notation
options(scipen = 999)

# Interested in rows: TYPE OF PRODUCER,ENERGY SOURCE equal to Total Electric Power Industry,Total
# [You] should convert the STATE, TYPE OF PRODUCER, and ENERGY SOURCE to categorical values
data <- read.csv(file = "annual_generation_state.csv", TRUE, sep = ",")
# Get header column names
# colnames(data)

industry_name = "Total Electric Power Industry"
# Reformat the header names
colnames(data)[colnames(data) == "TYPE.OF.PRODUCER"] <- "TYPE_OF_PRODUCER"
colnames(data)[colnames(data) == "ENERGY.SOURCE"] <- "ENERGY_SOURCE"
colnames(data)[colnames(data) == "GENERATION..Megawatthours."] <- "GENERATION"
# Reformat the GENERATION col from char to dbl
data$GENERATION <- as.numeric(gsub(",", "", data$GENERATION))
# Fix Mislabeled US-TOTAL (US-Total)
# Mislabeled US-TOTAL: 38888-38959, 40907-40980, 42963-43035, 45016-45087, 47087-47159, 49184-49256, 
# 51259-51368, 53416-53489, 

data <- subset(data, data$TYPE_OF_PRODUCER == "Total Electric Power Industry")

data$STATE <- toupper(data$STATE)
# factorize categories
data$STATE <- as.factor(data$STATE)
data$TYPE_OF_PRODUCER <- as.factor(data$TYPE_OF_PRODUCER)
data$ENERGY_SOURCE <- as.factor(data$ENERGY_SOURCE)
# Remove Missing Identified & negative data
data <- subset(data, data$STATE != "  ")
data <- subset(data, data$GENERATION > 0)
# Remove references to Other, Other Gases, Other Biomass, Pumped Storage
# to_drop <- c("Other", "Other Gases", "Other Biomass", "Pumped Storage")
data <- subset(data, data$ENERGY_SOURCE != "Other")
data <- subset(data, data$ENERGY_SOURCE != "Other Gases")
data <- subset(data, data$ENERGY_SOURCE != "Other Biomass")
data <- subset(data, data$ENERGY_SOURCE != "Pumped Storage")
# drop unused levels
data$ENERGY_SOURCE <- factor(data$ENERGY_SOURCE)
# rename levels
levels(data$ENERGY_SOURCE) <- c("Coal", "Geo", "Hydro", "Gas", "Nuclear", "Petrol", "Solar", "Total", "Wind", "Wood")
# view levels
# levels(data$ENERGY_SOURCE)

# Make lists for select menus to use
categories <- c("All", "Coal", "Geo", "Hydro", "Gas", "Nuclear", "Petrol", "Solar", "Wind", "Wood")
states <- setNames(state.abb, state.name)
states["US-TOTAL"] <- "US-TOTAL"
states["Washington DC"] <-  "DC"
states <- append(states, setNames("_All", "All"))
states <- sort(states)
years <- c(1990:2019)


# Drop Type of Producer Column
data$TYPE_OF_PRODUCER <- NULL

# levels(All$ENERGY_SOURCE)

# Convert data and percents from long to wide format
data_l_to_w <- dcast(data, STATE ~ YEAR + ENERGY_SOURCE, value.var = "GENERATION", drop = FALSE)
percents_l_to_w <- dcast(data, STATE ~ YEAR + ENERGY_SOURCE, value.var = "GENERATION", drop = FALSE)

#Remove first empty row of datasets
data_l_to_w <- data_l_to_w[-1,]
percents_l_to_w <- percents_l_to_w[-1,]

# set vars for following loop
total_index <- 9
denominator <- data_l_to_w[,total_index]

# Calculate the percents then save to percents_l_to_w
for (i in 2:ncol(data_l_to_w)) {
  if (i %% 11 == 0) {
    total_index <- total_index + 10
    denominator <- data_l_to_w[,total_index]
  }
  
  percents_l_to_w[,i] <- round((data_l_to_w[,i] / denominator), 2) * 100.0
}

# convert data and percents wide format to long format
data_w_to_L <- melt(data_l_to_w, id.vars=c("STATE"))
percents_w_to_L <- melt(percents_l_to_w, id.vars=c("STATE"))
# split the variable column into two columns (removes the factor/level)
data_w_to_L$variable <- data.frame(do.call("rbind", strsplit(as.character(data_w_to_L$variable), "_", fixed = TRUE)))
percents_w_to_L$variable <- data.frame(do.call("rbind", strsplit(as.character(percents_w_to_L$variable), "_", fixed = TRUE)))
# set the year column as an integer
data_w_to_L$variable$X1 <- as.integer(data_w_to_L$variable$X1)
percents_w_to_L$variable$X1 <- as.integer(percents_w_to_L$variable$X1)
# set the energy source column as a factor
data_w_to_L$variable$X2 <- as.factor(data_w_to_L$variable$X2)
percents_w_to_L$variable$X2 <- as.factor(percents_w_to_L$variable$X2)


#Create a custom color scale
myColors <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
              "#44AA99", "#999933", "#882255", "#661100")
names(myColors) <- levels(factor(c(levels(data_w_to_L$variable$X2), levels(data_w_to_L$variable$X2))))

# Build the body of the shiny app dashboard
body <- dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "p1", class='active',
            fluidRow(
              column(6,
                     fluidRow(
                       tabBox(
                         title = "Stacked Bar Charts/Line Graphs", height= "300px", width="auto", id="tab",
                         # The id lets us use input$tabset1 on the server to find the current tab
                         tabPanel("Amounts",
                                  fluidRow(
                                    plotOutput("plot1"),
                                    br(),
                                    plotOutput("plot3"),
                                  )
                         ),
                         tabPanel("Percents", 
                                  fluidRow(
                                    plotOutput("plot2"),
                                    br(),
                                    plotOutput("plot4"),
                                  )
                         )
                       )
                       
                     )
              ),
              column(6,
                     h2("Raw Total Electric Power Industry Data"),
                     DT::dataTableOutput("mytable"),
                     style="height:720px",
              )
            )
    ),
    
    # Second tab content
    tabItem(tabName = "p2",
            fluidRow(
              tabBox(
                title = "Region 1", height= "100%", width="auto", id="tab",
                # The id lets us use input$tabset1 on the server to find the current tab
                tabPanel("Amounts",
                         column(6, plotOutput("compR1_plot1")),
                         column(6, plotOutput("compR1_plot3"))
                         
                ),
                tabPanel("Percents",
                         column(6, plotOutput("compR1_plot2")),
                         column(6, plotOutput("compR1_plot4"))
                ),
                tabPanel("Table",
                         column(6, 
                                h5("Energy Production in Amounts"),
                                DT::dataTableOutput("compR1_dt1"),
                                style="height:60%"),
                         column(6, 
                                h5("Energy Production in Percents"),
                                DT::dataTableOutput("compR1_dt2"),
                                style="height:60%")
                         
                )
              ), style="height:40%;"
              
            ),
            br(),
            fluidRow(
              tabBox(
                title = "Region 2", height= "100%", width="auto", id="tab",
                # The id lets us use input$tabset1 on the server to find the current tab
                tabPanel("Amounts",
                         column(6,plotOutput("compR2_plot1")),
                         column(6, plotOutput("compR2_plot3"))
                ),
                tabPanel("Percents",
                         column(6, plotOutput("compR2_plot2")),
                         column(6, plotOutput("compR2_plot4"))
                ),
                tabPanel("Table",
                         column(6, 
                                h5("Energy Production in Amounts"),
                                DT::dataTableOutput("compR2_dt1"),
                                style="height:60%"),
                         column(6, 
                                h5("Energy Production in Percents"),
                                DT::dataTableOutput("compR2_dt2"),
                                style="height:60%")
                )
              )
            ), style="height:40%;"
    ),
    
    # Third tab content
    tabItem(tabName = "p3",
            column(6, 
                   fluidRow(plotOutput("geoR1_hm")),
                   fluidRow(plotOutput("geoR3_hm"))),
            column(6,
                   fluidRow(plotOutput("geoR2_hm")),
                   fluidRow(plotOutput("geoR4_hm")))
            
    ),
    
    # About tab content
    tabItem(tabName = "about",
            fluidRow(
              h1("About this Project"),
              hr(),
              p("The original data is from https://www.eia.gov/electricity/data/state/ but my Professor (Dr. Andy Johnson) made it into a CSV file."),
              p("The creator of this app is me (Matt Ziminski), I am currently taking CS 424 with with Dr. Andy Johnson."),
              p("This is 1 of 3 projects done for CS 424, and it was worked on from January 30, 2021 to February 14, 2021."),
            ), style="padding-left:2rem;"
    )
  )
)

ui = dashboardPage(
  dashboardHeader(title = "CS 424 - Project 1"),
  # Create the desired sidebar loyout
  dashboardSidebar(
    sidebarMenu(
      menuItem("Part 1", tabName = "p1"),
      menuItem("Part 1 options",
               selectInput("select", label = "Energy Source",
                           choices = categories,
                           selected = "All")),
      menuItem("Part 2", tabName = "p2"),
      menuItem("Part 2 options",
               h5("Region 1"),
               selectInput("select_compR1_categories", label = "Energy Source",
                           choices = categories,
                           selected = "All"),
               selectInput("select_compR1_states", label = "State",
                           choices = states,
                           selected = "IL"),
               selectInput("select_compR1_years", label = "Year",
                           choices = years,
                           selected = "All"),
               h5("Region 2"),
               selectInput("select_compR2_categories", label = "Energy Source",
                           choices = categories,
                           selected = "All"),
               selectInput("select_compR2_states", label = "State",
                           choices = states,
                           selected = "US-TOTAL"),
               selectInput("select_compR2_years", label = "Year",
                           choices = years,
                           selected = "All")),
      
      menuItem("Part 3", tabName = "p3"),
      menuItem("Part 3 options",
               h5("Region 1"),
               selectInput("select_geoR1_categories", label = "Energy Source",
                           choices = categories[-1],
                           selected = "Coal"),
               selectInput("select_geoR1_years", label = "Years",
                           choices = years,
                           selected = 1990),
               h5("Region 2"),
               selectInput("select_geoR2_categories", label = "Energy Source",
                           choices = categories[-1],
                           selected = "Coal"),
               selectInput("select_geoR2_years", label = "Years",
                           choices = years,
                           selected = 2000),
               h5("Region 3"),
               selectInput("select_geoR3_categories", label = "Energy Source",
                           choices = categories[-1],
                           selected = "Coal"),
               selectInput("select_geoR3_years", label = "Years",
                           choices = years,
                           selected = 2010),
               h5("Region 4"),
               selectInput("select_geoR4_categories", label = "Energy Source",
                           choices = categories[-1],
                           selected = "Coal"),
               selectInput("select_geoR4_years", label = "Years",
                           choices = years,
                           selected = 2019)), style="height: 90vh; overflow-y: auto;",
      menuItem("About", tabName = "about")
      
    )
  ),
  body
)

# Here lies the code that handles the interactivity of the shiny app
server = function(input, output) {
  
  # Render the 1st plot in part 1
  output$plot1 <- renderPlot({
    new_data <- 0
    # extract all energy sources or specified energy source by specified input
    if (input$select == "All") {
      new_data <- subset(data_w_to_L, data_w_to_L$variable$X2 != "Total")
    } else {
      new_data <- subset(data_w_to_L, data_w_to_L$variable$X2 == input$select)
    }
    
    # Create stacked bar chart showing the amount of each energy source per year from 1990 - 2019
    ggplot(data=new_data, aes(x=variable$X1, y=value, fill=variable$X2)) +
      geom_bar(position="stack", stat="identity") +
      scale_y_continuous(labels = function(y) paste0(y / 1e6, " Million")) +
      labs(x="Time", y= "Amount", title="Energy Production in Amounts", subtitle="Amount vs. Time") + 
      scale_fill_manual(values=myColors, 
                        name="Energy Source(s)")
  })
  # Render the 2nd plot in part 1
  output$plot2 <- renderPlot({
    new_data <- 0
    # extract all energy sources or specified energy source by specified input
    if (input$select == "All") {
      new_data <- subset(percents_w_to_L, percents_w_to_L$variable$X2 != "Total")
    } else {
      new_data <- subset(percents_w_to_L, percents_w_to_L$variable$X2 == input$select)
    }
    
    # Create stacked bar chart showing percent of the total production for each energy source per year from 1990 - 2019
    ggplot(data=new_data, aes(x=variable$X1, y=value, fill=factor(variable$X2))) +
      geom_bar(position="fill", stat="identity") +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(x="Time", y= "Percent", title="Percent of total Energy Production", subtitle="Percent vs. Time") +  
      scale_fill_manual(values=myColors, 
                        name="Energy Source(s)")
  })
  # Render the 3rd plot in part 1
  output$plot3 <- renderPlot({
    new_data <- 0
    # extract all energy sources or specified energy source by specified input
    if (input$select == "All") {
      new_data <- subset(data_w_to_L, data_w_to_L$variable$X2 != "Total")
    } else {
      new_data <- subset(data_w_to_L, data_w_to_L$variable$X2 == input$select)
    }
    
    # Create line chart showing the amount of each energy source per year from 1990 - 2019
    ggplot(data=new_data, aes(x=variable$X1, y=value, colour=variable$X2)) +
      stat_summary(fun=sum, geom="line") +
      scale_y_continuous(labels = function(y) paste0(y / 1e6, " Million")) +
      labs(x="Time", y= "Amount", title="Energy Production in Amounts", subtitle="Amount vs. Time") + 
      scale_color_manual(values=myColors, 
                         name="Energy Source(s)")
  })
  # Render the 4th plot in part 1
  output$plot4 <- renderPlot({
    new_data <- 0
    # extract all energy sources or specified energy source by specified input
    if (input$select == "All") {
      new_data <- subset(percents_w_to_L, percents_w_to_L$variable$X2 != "Total")
    } else {
      new_data <- subset(percents_w_to_L, percents_w_to_L$variable$X2 == input$select)
    }
    
    # Create line chart showing the percent of the total production for each energy source per year from 1990 - 2019
    ggplot(data=new_data, aes(x=variable$X1, y=value, colour=variable$X2)) +
      stat_summary(fun=sum, geom="line") +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(x="Time", y= "Percent", title="Percent of total Energy Production", subtitle="Percent vs. Time") +
      scale_color_manual(values=myColors, 
                         name="Energy Source(s)")
  })
  # Render the amount and percents table in part 1 with the below requirements:
  # Create table of raw numbers for the amount of each energy source per year from 1990 - 2019
  # Create table of raw numbers for the percent of the total production for each energy source per year from 1990 - 2019
  output$mytable <- DT::renderDataTable({
    new_data <- 0
    # extract all energy sources or specified energy source by specified input
    if (input$tab == "Amounts") {
      new_data <- data_w_to_L
    } else {
      new_data <- percents_w_to_L
    }
    
    if (input$select == "All") {
      new_data <- subset(new_data, new_data$variable$X2 != "Total")
    } else {
      new_data <- subset(new_data, new_data$variable$X2 == input$select)
    }
    # subset appropriately
    new_data$value <- format(new_data$value, big.mark=",",scientific=FALSE)
    new_data_l_to_w <- dcast(new_data, STATE ~ variable$X1 + variable$X2, value.var = "value", drop = TRUE)
    
    DT::datatable(new_data_l_to_w, rownames = FALSE,
                  options = list(paging=FALSE,
                                 scrollY = "720px",
                                 scrollX = "500",
                                 targets = "_all",
                                 columnDefs = list(list(className = 'dt-right', targets="_all"))))
    
  })
  # Render the 1st plot for region 1
  output$compR1_plot1 <- renderPlot({
    new_data <- data_w_to_L
    # extract all energy sources or specified energy source by specified input
    if (input$select_compR1_categories == "All") {
      new_data <- subset(new_data, new_data$variable$X2 != "Total")
    } else {
      new_data <- subset(new_data, new_data$variable$X2 == input$select_compR1_categories)
    }
    # subset appropriately
    new_data <- subset(new_data, new_data$STATE == input$select_compR1_states)
    new_data <- subset(new_data, new_data$variable$X1 == input$select_compR1_years)
    
    ggplot(data=new_data, aes(x=variable$X1, y=value, fill=variable$X2)) +
      geom_bar(position="stack", stat="identity") +
      scale_y_continuous(labels = function(y) paste0(y / 1e6, " Million")) +
      labs(x="Time", y= "Amount", title="Energy Production in Amounts", subtitle="Amount vs. Time") + 
      scale_fill_manual(values=myColors, 
                        name="Energy Source(s)")
    
  })
  # Render the 2nd plot for region 1
  output$compR1_plot2 <- renderPlot({
    new_data <- percents_w_to_L
    pos <- "identity"
    # extract all energy sources or specified energy source by specified input
    if (input$select_compR1_categories == "All") {
      new_data <- subset(new_data, new_data$variable$X2 != "Total")
      pos <- "stack"
    } else {
      new_data <- subset(new_data, new_data$variable$X2 == input$select_compR1_categories)
    }
    # subset appropriately
    new_data <- subset(new_data, new_data$STATE == input$select_compR1_states)
    new_data <- subset(new_data, new_data$variable$X1 == input$select_compR1_years)
    
    ggplot(data=new_data, aes(x=variable$X1, y=value, fill=factor(variable$X2))) +
      geom_bar(position=pos, stat="identity") +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(x="Time", y= "Percent", title="Percent of total Energy Production", subtitle="Percent vs. Time") +
      scale_fill_manual(values=myColors, 
                        name="Energy Source(s)")
  })
  # Render the 3rd plot for region 1
  output$compR1_plot3 <- renderPlot({
    new_data <- data_w_to_L
    # extract all energy sources or specified energy source by specified input
    if (input$select_compR1_categories == "All") {
      new_data <- subset(new_data, new_data$variable$X2 != "Total")
    } else {
      new_data <- subset(new_data, new_data$variable$X2 == input$select_compR1_categories)
    }
    # subset appropriately
    new_data <- subset(new_data, new_data$STATE == input$select_compR1_states)
    new_data <- subset(new_data, new_data$variable$X1 == input$select_compR1_years)
    
    ggplot(data=new_data, aes(x=variable$X1, y=value, colour=variable$X2)) +
      stat_summary(fun=sum, geom="point") +
      scale_y_continuous(labels = function(y) paste0(y / 1e6, " Million")) +
      labs(x="Time", y= "Amount", title="Energy Production in Amounts", subtitle="Amount vs. Time") + 
      scale_color_manual(values=myColors, 
                         name="Energy Source(s)")
  })
  # Render the 4th plot for region 1
  output$compR1_plot4 <- renderPlot({
    new_data <- percents_w_to_L
    # extract all energy sources or specified energy source by specified input
    if (input$select_compR1_categories == "All") {
      new_data <- subset(new_data, new_data$variable$X2 != "Total")
    } else {
      new_data <- subset(new_data, new_data$variable$X2 == input$select_compR1_categories)
    }
    # subset appropriately
    new_data <- subset(new_data, new_data$STATE == input$select_compR1_states)
    new_data <- subset(new_data, new_data$variable$X1 == input$select_compR1_years)
    
    ggplot(data=new_data, aes(x=variable$X1, y=value, colour=variable$X2)) +
      stat_summary(fun=sum, geom="point") +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(x="Time", y= "Percent", title="Percent of total Energy Production", subtitle="Percent vs. Time") +
      scale_color_manual(values=myColors, 
                         name="Energy Source(s)")
  })
  # Render the 1st table (Amounts) for region 1
  output$compR1_dt1 <- renderDataTable({
    new_data <- data_w_to_L
    # extract all energy sources or specified energy source by specified input
    if (input$select_compR1_categories == "All") {
      new_data <- subset(new_data, new_data$variable$X2 != "Total")
    } else {
      new_data <- subset(new_data, new_data$variable$X2 == input$select_compR1_categories)
    }
    # subset appropriately
    new_data <- subset(new_data, new_data$STATE == input$select_compR1_states)
    new_data <- subset(new_data, new_data$variable$X1 == input$select_compR1_years)
    # format the numbers then convert long format to wide format
    new_data$value <- format(new_data$value, big.mark=",",scientific=FALSE)
    new_data_l_to_w <- dcast(new_data, STATE ~ variable$X1 + variable$X2, value.var = "value", drop = TRUE)
    
    DT::datatable(new_data_l_to_w, rownames = FALSE,
                  options = list(paging=FALSE,
                                 scrollY = "200px",
                                 scrollX = "200px",
                                 targets = "_all",
                                 columnDefs = list(list(className = 'dt-right', targets="_all"))))
    
  })
  # Render the 2nd table (Percents) for region 1
  output$compR1_dt2 <- renderDataTable({
    new_data <- percents_w_to_L
    # extract all energy sources or specified energy source by specified input
    if (input$select_compR1_categories == "All") {
      new_data <- subset(new_data, new_data$variable$X2 != "Total")
    } else {
      new_data <- subset(new_data, new_data$variable$X2 == input$select_compR1_categories)
    }
    
    # subset appropriately
    new_data <- subset(new_data, new_data$STATE == input$select_compR1_states)
    new_data <- subset(new_data, new_data$variable$X1 == input$select_compR1_years)
    # format the numbers then convert long format to wide format
    new_data$value <- format(new_data$value, big.mark=",",scientific=FALSE)
    new_data_l_to_w <- dcast(new_data, STATE ~ variable$X1 + variable$X2, value.var = "value", drop = TRUE)
    
    DT::datatable(new_data_l_to_w, rownames = FALSE,
                  options = list(paging=FALSE,
                                 scrollY = "200px",
                                 scrollX = "200px",
                                 targets = "_all",
                                 columnDefs = list(list(className = 'dt-right', targets="_all"))))
  })
  # Render the 1st plot for region 2
  output$compR2_plot1 <- renderPlot({
    new_data <- data_w_to_L
    # extract all energy sources or specified energy source by specified input
    if (input$select_compR2_categories == "All") {
      new_data <- subset(new_data, new_data$variable$X2 != "Total")
    } else {
      new_data <- subset(new_data, new_data$variable$X2 == input$select_compR2_categories)
    }
    # subset appropriately
    new_data <- subset(new_data, new_data$STATE == input$select_compR2_states)
    new_data <- subset(new_data, new_data$variable$X1 == input$select_compR2_years)
    
    ggplot(data=new_data, aes(x=variable$X1, y=value, fill=variable$X2)) +
      geom_bar(position="stack", stat="identity") +
      scale_y_continuous(labels = function(y) paste0(y / 1e6, " Million")) +
      labs(x="Time", y= "Amount", title="Energy Production in Amounts", subtitle="Amount vs. Time") + 
      scale_fill_manual(values=myColors, 
                        name="Energy Source(s)")
    
  })
  # Render the 2nd plot for region 2
  output$compR2_plot2 <- renderPlot({
    new_data <- percents_w_to_L
    pos <- "identity"
    # extract all energy sources or specified energy source by specified input
    if (input$select_compR2_categories == "All") {
      new_data <- subset(new_data, new_data$variable$X2 != "Total")
      pos <-"stack"
    } else {
      new_data <- subset(new_data, new_data$variable$X2 == input$select_compR2_categories)
    }
    # subset appropriately
    new_data <- subset(new_data, new_data$STATE == input$select_compR2_states)
    new_data <- subset(new_data, new_data$variable$X1 == input$select_compR2_years)
    
    ggplot(data=new_data, aes(x=variable$X1, y=value, fill=factor(variable$X2))) +
      geom_bar(position=pos, stat="identity") +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(x="Time", y= "Percent", title="Percent of total Energy Production", subtitle="Percent vs. Time") +
      scale_fill_manual(values=myColors, 
                        name="Energy Source(s)")
  })
  # Render the 3rd plot for region 2
  output$compR2_plot3 <- renderPlot({
    new_data <- data_w_to_L
    # extract all energy sources or specified energy source by specified input
    if (input$select_compR2_categories == "All") {
      new_data <- subset(new_data, new_data$variable$X2 != "Total")
    } else {
      new_data <- subset(new_data, new_data$variable$X2 == input$select_compR2_categories)
    }
    # subset appropriately
    new_data <- subset(new_data, new_data$STATE == input$select_compR2_states)
    new_data <- subset(new_data, new_data$variable$X1 == input$select_compR2_years)
    
    ggplot(data=new_data, aes(x=variable$X1, y=value, colour=variable$X2)) +
      stat_summary(fun=sum, geom="point") +
      scale_y_continuous(labels = function(y) paste0(y / 1e6, " Million")) +
      labs(x="Time", y= "Amount", title="Energy Production in Amounts", subtitle="Amount vs. Time") + 
      scale_color_manual(values=myColors, 
                         name="Energy Source(s)")
  })
  # Render the 4th plot for region 2
  output$compR2_plot4 <- renderPlot({
    new_data <- percents_w_to_L
    # extract all energy sources or specified energy source by specified input
    if (input$select_compR2_categories == "All") {
      new_data <- subset(new_data, new_data$variable$X2 != "Total")
    } else {
      new_data <- subset(new_data, new_data$variable$X2 == input$select_compR2_categories)
    }
    # subset appropriately
    new_data <- subset(new_data, new_data$STATE == input$select_compR2_states)
    new_data <- subset(new_data, new_data$variable$X1 == input$select_compR2_years)
    
    ggplot(data=new_data, aes(x=variable$X1, y=value, colour=variable$X2)) +
      stat_summary(fun=sum, geom="point") +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(x="Time", y= "Percent", title="Percent of total Energy Production", subtitle="Percent vs. Time") +
      scale_color_manual(values=myColors, 
                         name="Energy Source(s)")
  })
  # Render the 1st table (Amounts) for region 2
  output$compR2_dt1 <- renderDataTable({
    new_data <- data_w_to_L
    # extract all energy sources or specified energy source by specified input
    if (input$select_compR2_categories == "All") {
      new_data <- subset(new_data, new_data$variable$X2 != "Total")
    } else {
      new_data <- subset(new_data, new_data$variable$X2 == input$select_compR2_categories)
    }
    # subset appropriately
    new_data <- subset(new_data, new_data$STATE == input$select_compR2_states)
    new_data <- subset(new_data, new_data$variable$X1 == input$select_compR2_years)
    # format the numbers then convert long format to wide format
    new_data$value <- format(new_data$value, big.mark=",",scientific=FALSE)
    new_data_l_to_w <- dcast(new_data, STATE ~ variable$X1 + variable$X2, value.var = "value", drop = TRUE)
    
    DT::datatable(new_data_l_to_w, rownames = FALSE,
                  options = list(paging=FALSE,
                                 scrollY = "200px",
                                 scrollX = "200px",
                                 targets = "_all",
                                 columnDefs = list(list(className = 'dt-right', targets="_all"))))
    
  })
  # Render the 2nd table (Percents) for region 2
  output$compR2_dt2 <- renderDataTable({
    new_data <- percents_w_to_L
    # extract all energy sources or specified energy source by specified input
    if (input$select_compR2_categories == "All") {
      new_data <- subset(new_data, new_data$variable$X2 != "Total")
    } else {
      new_data <- subset(new_data, new_data$variable$X2 == input$select_compR2_categories)
    }
    # subset appropriatly
    new_data <- subset(new_data, new_data$STATE == input$select_compR2_states)
    new_data <- subset(new_data, new_data$variable$X1 == input$select_compR2_years)
    # format the numbers then convert long format to wide format
    new_data$value <- format(new_data$value, big.mark=",",scientific=FALSE)
    new_data_l_to_w <- dcast(new_data, STATE ~ variable$X1 + variable$X2, value.var = "value", drop = TRUE)
    
    DT::datatable(new_data_l_to_w, rownames = FALSE,
                  options = list(paging=FALSE,
                                 scrollY = "200px",
                                 scrollX = "200px",
                                 targets = "_all",
                                 columnDefs = list(list(className = 'dt-right', targets="_all"))))
  })
  
  # Render the heat map for region 1
  output$geoR1_hm <- renderPlot({
    new_data <- data_w_to_L
    # extract all energy sources or specified energy source by specified input
    if (input$select_geoR1_categories == "All") {
      new_data <- subset(new_data, new_data$variable$X2 != "Total")
    } else {
      new_data <- subset(new_data, new_data$variable$X2 == input$select_geoR1_categories)
    }
    
    new_data <- subset(new_data, new_data$variable$X1 == input$select_geoR1_years)
    new_data <- as.data.frame.matrix(new_data)                   # usmap requires data to be in data frame format
    colnames(new_data)[colnames(new_data) == "STATE"] <- "state" # state column name is required by usmap
    
    color <- myColors[input$select_geoR1_categories]
    
    plot_usmap(data = new_data, values = "value", color = color, labels=TRUE) + 
      scale_fill_continuous(low = "white", high = color, name = input$select_geoR1_categories, label = scales::comma) + 
      theme(legend.position = "right") +
      labs(title = "Region 1 Energy Production", subtitle = new_data$variable$X1, caption = "** Grey states signify missing data")
  })
  # Render the heat map for region 2
  output$geoR2_hm <- renderPlot({
    new_data <- data_w_to_L
    # extract all energy sources or specified energy source by specified input
    if (input$select_geoR2_categories == "All") {
      new_data <- subset(new_data, new_data$variable$X2 != "Total")
    } else {
      new_data <- subset(new_data, new_data$variable$X2 == input$select_geoR2_categories)
    }
    
    new_data <- subset(new_data, new_data$variable$X1 == input$select_geoR2_years)
    new_data <- as.data.frame.matrix(new_data)                   # usmap requires data to be in data frame format
    colnames(new_data)[colnames(new_data) == "STATE"] <- "state" # state column name is required by usmap
    
    color <- myColors[input$select_geoR2_categories]
    
    plot_usmap(data = new_data, values = "value", color = color, labels=TRUE) + 
      scale_fill_continuous(low = "white", high = color, name = input$select_geoR2_categories, label = scales::comma) + 
      theme(legend.position = "right") +
      labs(title = "Region 2 Energy Production", subtitle = new_data$variable$X1, caption = "*** Grey states signify missing data")
  })
  # Render the heat map for region 3
  output$geoR3_hm <- renderPlot({
    new_data <- data_w_to_L
    # extract all energy sources or specified energy source by specified input
    if (input$select_geoR3_categories == "All") {
      new_data <- subset(new_data, new_data$variable$X2 != "Total")
    } else {
      new_data <- subset(new_data, new_data$variable$X2 == input$select_geoR3_categories)
    }
    
    new_data <- subset(new_data, new_data$variable$X1 == input$select_geoR3_years)
    new_data <- as.data.frame.matrix(new_data)                   # usmap requires data to be in data frame format
    colnames(new_data)[colnames(new_data) == "STATE"] <- "state" # state column name is required by usmap
    
    color <- myColors[input$select_geoR3_categories]
    
    plot_usmap(data = new_data, values = "value", color = color, labels=TRUE) + 
      scale_fill_continuous(low = "white", high = color, name = input$select_geoR3_categories, label = scales::comma) + 
      theme(legend.position = "right") +
      labs(title = "Region 3 Energy Production", subtitle = new_data$variable$X1, caption = "** Grey states signify missing data")
  })
  # Render the heat map for region 4
  output$geoR4_hm <- renderPlot({
    new_data <- data_w_to_L
    # extract all energy sources or specified energy source by specified input
    if (input$select_geoR4_categories == "All") {
      new_data <- subset(new_data, new_data$variable$X2 != "Total")
    } else {
      new_data <- subset(new_data, new_data$variable$X2 == input$select_geoR4_categories)
    }
    
    new_data <- subset(new_data, new_data$variable$X1 == input$select_geoR4_years)
    new_data <- as.data.frame.matrix(new_data)                   # usmap requires data to be in data frame format
    colnames(new_data)[colnames(new_data) == "STATE"] <- "state" # state column name is required by usmap
    
    
    color <- myColors[input$select_geoR4_categories]
    
    plot_usmap(data = new_data, values = "value", color=color, labels=TRUE) + 
      scale_fill_continuous(low = "white", high = color, name = input$select_geoR4_categories, label = scales::comma) + 
      theme(legend.position = "right") +
      labs(title = "Region 4 Energy Production", subtitle = new_data$variable$X1, caption = "** Grey states signify missing data")
  })
}



shinyApp(ui, server)





