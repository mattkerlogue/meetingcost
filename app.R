#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)

salary_dept <- read_csv("data/salary_dept.csv")

# Define UI for application that draws a histogram
ui <- navbarPage(
    title = "HMG Meeting Cost Calculator",
    theme = bslib::bs_theme(version = 4, bootswatch = "sandstone"),
    tabPanel(
        title = "Calculator",
        sidebarLayout(
            sidebarPanel(
             h4("Add a participant"),
             textInput("name", "Name"),
             selectizeInput("dept", "Department/Agency", 
                            choices = sort(unique(salary_dept$dept)), selected = "All employees"),
             selectInput("grade", "Grade", choices = c("", unique(salary_dept$grade_label))),
             actionButton("add", "Add attendee"),
             hr(),
             h4("Meeting length"),
             selectInput("time", "Length of meeting", 
                         choices = c("15 minutes" = 0.25, 
                                     "30 minutes" = 0.5,
                                     "45 minutes" = 0.75,
                                     "1 hour" = 1,
                                     "1 hour 30 minutes" = 1.5,
                                     "2 hours" = 2,
                                     "Half-day (3 hours)" = 3,
                                     "Full-day (6 hours)" = 6)
            ),
            hr(),
            h4(strong("Meeting cost")),
            HTML("<div class=\"card text-white bg-info mb-3\">",
                 "<div class=\"card-body\">",
                 "<h5>Total cost: <strong>"),
            textOutput("cost", inline = TRUE), 
            HTML("</strong></h5>"),
            HTML("<p>Per-minute: <strong>"), 
            textOutput("cost_minute", inline = TRUE), 
            HTML("</strong>"),
            HTML("</p></div></div>"),
            hr(),
            actionButton("reset", "Reset calculator"),
            ),
            mainPanel(
                h1("HMG Meeting Cost Calculator"),
                HTML("<p>This is an unofficial calculator for estimating the cost of Civil Service meetings.",
                     "It uses published data on salary levels to estimate the hourly rate of civil servants",
                     "by department and grade.</p>",
                     "<p>To use the calculator use the sidebar to add a participant and ",
                     "set the length of the meeting. The table below will show each attendee, ",
                     "their department/agency, grade, hourly rate and the cost of their attendance. ",
                     "In the sidebar you can see the total cost of the meeting, and the per-minute ",
                     "'rate' for the meeting (the cost of each minute).</p>",
                     "<p>This is purely for illustrative purposes and may not reflect the true costs ",
                     "of the meeting as other factors (e.g. length of service, profession, location),",
                     "have a strong influence on individual salaries. Similarly variations in contractual",
                     "arrangements (working hours, annual leave allowance) might result in an ",
                     "individual having longer or shorter working hours than has been assumed ",
                     "which will affect the calculation of the hourly rate from annual salary figures.</p>"),
                DT::DTOutput("attendees")
            )
        )
    ),
    tabPanel(
        title = "About",
        sidebarLayout(
            sidebarPanel(
                h5("Data source"),
                HTML("<p>Cabinet Office (2020)",
                     "<a href=\"https://www.gov.uk/government/statistics/civil-service-statistics-2020\">",
                     "Civil Service Statistics 2020</a>.</p>"),
                h5("Hosting"),
                HTML("The app is hosted on <a href=\"https://shinyapps.io\">shinyapps.io</a>."),
                h5("Code"),
                HTML("The code is available on <a href=\"\">Github</a>.")
            ),
            mainPanel(
                h1("HMG Meeting Cost Calculator: About"),
                p("This calculator provides an unofficial estimate of the cost of meetings of ",
                  "UK civil servants. It uses the ",
                  a(href="https://www.gov.uk/government/statistics/civil-service-statistics-2020",
                     "Civil Service Statistics 2020"),
                  "which provide a wide range of data and information about the workforce ",
                  "of the UK Civil Service."),
                p("It is inspired by the ",
                  a(href="https://meetingcostcalculator.ca", "Meeting Cost Calculator"),
                  "which estimates the cost of public sector meetings in Canada."),
                h2("Disclaimer"),
                p("While the author of this calculator is a civil servant, this project is ",
                  "a personal side project using published information and is not an ",
                  "official product of the Cabinet Office or HM Government."),
                h2("Estimating hourly rates"),
                p("The fundamental unit in the calculations is an estimated hourly rate for ",
                  "civil servants based on their department/agency and their grade. ",
                  "The published statistics only provide annual salaries, therefore an estimate ",
                  "must be calculated. The estimate is calculated by assuming that the median salary ",
                  "is for a full-time employee and they work a 37 hour week and have 30 days annual leave ",
                  "plus 8 public holidays and 1 privelige day (1635.4 working hours per year)."),
                p("This estimate is purely indicative and unlikely to be representative of ",
                  "any particular individual as several factors (e.g. length of service, profession,",
                  "location, etc) will influence their salary, while contractual arrangements will ",
                  "mean they have longer or shorter annual working hours than the figure used for ",
                  "the calculation."),
                h2("Build"),
                p("The calculator is built in ", a(href="https://www.r-project.org", "R"), 
                  " and ", a(href="https://shiny.rstudio.com", "{shiny}"), "The code is ",
                  "available on ", a(href="", "Github"), ".")
            )
        )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    meeting <- reactiveValues()
    
    meeting$attendees <- tibble::tibble(
        name = character(),
        dept = character(),
        grade_label = character(),
        hourly_rate = numeric(),
        cost = numeric()
    )
    
    meeting$cost <- 0
    
    

    observeEvent(input$add, {
        
        meeting$time <- as.numeric(input$time)
        
        this_attendee <- tibble::tibble(
            name = input$name,
            dept = input$dept,
            grade_label = input$grade
        ) %>% left_join(salary_dept, by = c("dept", "grade_label")) %>%
            select(name, dept, grade_label, hourly_rate) %>%
            mutate(cost = janitor::round_half_up(hourly_rate * meeting$time))
        
        meeting$attendees <- bind_rows(meeting$attendees, this_attendee)
        meeting$minutes <- meeting$time * 60
        meeting$cost <- sum(meeting$attendees$cost)
        
        updateTextInput(session = getDefaultReactiveDomain(), inputId = "name", value = "")
        
    })
    
    observeEvent(input$time, {
        meeting$time <- as.numeric(input$time)
        meeting$minutes <- meeting$time * 60
        meeting$attendees$cost <- meeting$attendees$hourly_rate * meeting$time
        meeting$cost <- sum(meeting$attendees$cost)
    })
    
    observeEvent(input$reset, {
        
        meeting$attendees <- tibble::tibble(
            name = character(),
            dept = character(),
            grade_label = character(),
            hourly_rate = numeric(),
            cost = numeric()
        )
        
        meeting$time <- as.numeric(input$time)
        meeting$minutes <- meeting$time * 60
        meeting$cost <- 0
        
        updateTextInput(session = getDefaultReactiveDomain(), inputId = "name", value = "")
        
        updateSelectInput(session = getDefaultReactiveDomain(), inputId = "dept", selected =  "All employees")
        
        updateSelectInput(session = getDefaultReactiveDomain(), inputId = "grade", selected = "")
        
        updateSelectInput(session = getDefaultReactiveDomain(), inputId = "time", selected = 0.25)
        
        
        
    })
    
    output$attendees <- DT::renderDT(
        DT::datatable(meeting$attendees,
        rownames = FALSE,
        colnames = c("Name", "Department/Agency", "Grade", "Hourly Rate", "Cost"),
        options = list(pageLength = 20, dom = "tip")) %>%
            DT::formatCurrency(4:5, "£"))
    
    output$cost <- renderText(paste0("£", janitor::round_half_up(meeting$cost, 2)))
    
    output$cost_minute <- renderText(paste0("£", janitor::round_half_up(meeting$cost / meeting$minutes, 2)))
        
}

# Run the application 
shinyApp(ui = ui, server = server)
