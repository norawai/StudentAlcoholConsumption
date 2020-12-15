#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(plotly)
library(shinycssloaders)
library(leaflet)

header = dashboardHeader(title = "Ahmnesty Interactive Graphics Project")

sidebar = dashboardSidebar(
                sidebarMenu(
                    menuItem("About", icon = icon("info"), tabName = "About"),
                    menuItem("Alcohol Consumption by Location", tabName = "hist"),
                    menuItem("Similarities - Dendogram", tabName = "dend"),
                    menuItem("Student Clusters", tabName = "scatterplot"),
                    menuItem("Correlations", tabName = "mosaic"),
                    menuItem("Box Plots of the Data", tabName = "austinplotly"),
                    menuItem("Heat Maps", tabName = "grapha")
                 )
        )
# About me page


about = tabItem(tabName = "About", 
                h1("A(h)mnesty Group Members"), 
                p("Nora Wai, Austin Hsu, Hyun Kang, Michelle Wong"),
                h1("About Our Data"),
                p("This data was obtained by surveying students in math and protuguese language courses in two different highschools. 
                  The highschools are pinned below, by clicking on the pins you will be able to see which highschool is at which location."),
                p("This data has 1,044 students and 33 categorical/continuous data for each of the students"),
                p("Some of the data include the students lifestyle, home environment, relationship status, personal information, and alcohol intake."),
                bootstrapPage(
                  withSpinner(leafletOutput(outputId = "map")))
                )

# Hk Plots
barChart = tabItem(tabName = "hist",
                   bootstrapPage(
                       plotOutput(outputId = "hk_barplot", height = "300px"),
                       checkboxInput(inputId = "address",
                                     label = "Show Home Environment Breakdown")))

dendogram = tabItem(tabName = "dend",
                    bootstrapPage(
                     plotOutput(outputId = "hk_dend", height = "600px"),
                     selectInput(inputId = "orientation",
                                 label = "Plot Orientation",
                                 choices = c("Horizontal", "Vertical"),
                                 selected = "Horizontal")), 
                    p("Walc - weekend alcohol consumption (numeric: from 1 - very low to 5 - very high)"),
                    p("Dalc - workday alcohol consumption (numeric: from 1 - very low to 5 - very high)"),
                    p("studytime - weekly study time (numeric: 1 - 10 hours)"),
                    p("G1 - first period grade (numeric: from 0 to 20)"),
                    p("G2 - second period grade (numeric: from 0 to 20)"),
                    p("G3 - final grade (numeric: from 0 to 20, output target)"),
                    p("failures - number of past class failures (numeric: n if 1<=n<3, else 4)"),
                    p("goout - going out with friends (numeric: from 1 - very low to 5 - very high)"),
                    p("freetime - free time after school (numeric: from 1 - very low to 5 - very high)"),
                    p("health - current health status (numeric: from 1 - very bad to 5 - very good)"),
                    p("famrel - quality of family relationships (numeric: from 1 - very bad to 5 - excellent)"),
                    p("absences - number of school absences (numeric: from 0 to 93)"),
                    p("age - student's age (numeric: from 15 to 22)"),
                    p("Medu - mother's education (numeric: 0 - none, 1 - primary education (4th grade), 2 – 5th to 9th grade, 3 – secondary education or 4 – higher education)"),
                    p("Fedu - father's education (numeric: 0 - none, 1 - primary education (4th grade), 2 – 5th to 9th grade, 3 – secondary education or 4 – higher education)"),
                    p("traveltime - home to school travel time (numeric: 1 - 1 hour)")
                    )

# Nora Plots 
scatterplot = tabItem(tabName = "scatterplot", 
                      bootstrapPage(
                          selectInput(inputId = "color_code",
                                      label = "Break Down of Data Points:",
                                      choices = c("School", "Address", "Gender", "Activity Participation", 
                                                  "Relationship Status", "Day Alcohol", "Weekend Alcohol"),
                                      selected = "School"),
                          withSpinner(plotlyOutput(outputId = "nora_scatter"))))

mosaicplot = tabItem(tabName = "mosaic", 
                     tabsetPanel(tabPanel(title = "Day Alcohol", 
                                          bootstrapPage(
                                            selectInput(inputId = "breakdown",
                                                        label = "Correlations Between",
                                                        choices = c("Family Relationships",
                                                                    "Romantic Relationships", 
                                                                    "Family Educational Support", 
                                                                    "Age"),
                                                        selected = "Family Relationships"),
                                            plotOutput(outputId = "nora_mosaic1"))), 
                                 tabPanel(title = "Weekend Alcohol", 
                                           bootstrapPage(
                                             selectInput(inputId = "breakdown2",
                                                         label = "Correlations Between",
                                                         choices = c("Family Relationships",
                                                                     "Romantic Relationships",
                                                                     "Family Educational Support",
                                                                     "Age"),
                                                         selected = "Family Relationships"),
                                             plotOutput(outputId = "nora_mosaic2"))))
                                 )


# Austins Plots 
boxPlot1 = tabItem(tabName = "austinplotly", 
                   tabsetPanel(tabPanel(title = "Weekday", 
                                        bootstrapPage(
                                          selectInput(inputId = "var1",
                                                      label = "Choose a Semester Grade to display",
                                                      choices = c("G1", "G2", "G3"),
                                                      selected = "G1"),
                                          withSpinner(plotlyOutput(outputId = "weekday")))
                                        ),
                               tabPanel(title = "Weekend", 
                                        bootstrapPage(
                                          selectInput(inputId = "var2",
                                                      label = "Choose a Semester Grade to display",
                                                      choices = c("G1", "G2", "G3"),
                                                      selected = "G1"),
                                          withSpinner(plotlyOutput(outputId = "weekend")))))
                 )
# Michelles Plots 
heatmap1 = tabItem(tabName = "grapha", 
                   tabsetPanel(tabPanel(title = "Heat Map 1",
                   bootstrapPage(
                       
                       # Change Grade Period
                       selectInput(inputId = "grade",
                                   label = "Change Grade Period:",
                                   choices = c("First Period", "Second Period", "Final"),
                                   selected = "First Period"),
                       
                       #Change Bandwidth of X
                       sliderInput(inputId = "bw_adjustx",
                                   label = "Bandwidth of Absences:",
                                   min = 0.1, 
                                   max = 5, 
                                   value = 3.4, 
                                   step = 0.1),
                       
                       #Change Bandwidth of Y
                       sliderInput(inputId = "bw_adjusty",
                                   label = "Bandwidth of Grade Period:",
                                   min = 0.1, 
                                   max = 5, 
                                   value = 2.2, 
                                   step = 0.1),
                       
                       withSpinner(plotOutput(outputId = "m_plot", height = "600px"))
                       
                   )), 
                   tabPanel(title = "Heat Map 2",
                               bootstrapPage(
                                 # Change Grade Period
                                 selectInput(inputId = "grade2",
                                             label = "Change Grade Period:",
                                             choices = c("First Period", "Second Period", "Final"),
                                             selected = "First Period"),
                                 
                                 #Change Bandwidth of X
                                 sliderInput(inputId = "bw_adjustx2",
                                             label = "Bandwidth of Absences:",
                                             min = 0.1, 
                                             max = 5, 
                                             value = 3.4, 
                                             step = 0.1),
                                 
                                 #Change Bandwidth of Y
                                 sliderInput(inputId = "bw_adjusty2",
                                             label = "Bandwidth of Grade Period:",
                                             min = 0.1, 
                                             max = 5, 
                                             value = 2.2, 
                                             step = 0.1),
                                 
                                 withSpinner(plotOutput(outputId = "m2_plot", height = "600px"))
                                 
                               )))
                )


body = dashboardBody(tabItems(about,barChart, dendogram, scatterplot, mosaicplot, boxPlot1, heatmap1))

dashboardPage(skin = "purple", header, sidebar, body)

