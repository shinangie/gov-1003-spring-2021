library(shiny)
library(ggforce)
library(tidyverse)
library(shinydashboard)
library(stargazer)
library(haven)
library(Hmisc)
library(tidymodels)
library(broom)

pre <- read_csv("pre.csv")
post <- read_csv("post.csv")

ui <- navbarPage(
    "DPI 610 FINAL PROJECT",
    tabPanel("Introduction", 
             titlePanel("Introduction"),
             
             # intro
             p("The National Asian American Survey (NAAS) is a scientific and nonpartisan effort to poll the opinions of Asian Americans and Pacific Islanders. This project will concern the 2016 public opinion surveys that the NAAS has organized before and after the 2016 presidential election. The purpose of this project is to measure AAPI political opinions intentionally and spotlight them in a deliberately academic manner, in order to help close the gap in research and academia on AAPI experiences.")
             
             # end of tab panel
    ),
    
    tabPanel("Data",
             titlePanel("Data"),
             
             # naas
             p("The 2016 National Asian American Survey is composed of two waves. Individual-level data from the Fall 2016 National Asian American Survey (pre-election) was fielded from August through October 2016. Individual-level data from the NAAS 2016 Post-Election Survey was fielded from November 2016 through February 2017. For both surveys, the NAAS collaborated with Catalist, a for-profit corporation based in Washington, D.C. that operates a voter database and provides data and data-related services to progressive organizations, to utilize registered voter and commercial vendor samples; these were classified according to a variety of ethnicity variables. Both surveys were weighted by ethnicity and gender, age, state of residence, education, and nativity by a unique ranking method."),
             
             # pre
             h3("The 2016 NAAS Pre-Election Survey"),
             p("The 2016 NAAS Pre-Election Survey conducted 2,238 telephone interviews to AAPI adults (respondents were required to be 18 years of age or older to proceed with the survey) between August 10 and September 29, 2016."),
             fluidPage(selectInput("x", "Demographic", choices = c("ethnicity", "gender", "education", "nativity1", "nativity2", "legality", "income1", "income2", "age1", "age2")),
                       plotOutput("plot1")),
             
             # pre
             h3("The 2016 NAAS Post-Election Survey"),
             p("The 2016 NAAS Post-Election Survey conducted 4,393 telephone interviews to AAPI adults (respondents were required to be 18 years of age or older to proceed with the survey) between November 10, 2016 and March 2, 2017."),
             fluidPage(selectInput("x", "Demographic", choices = c("ethnicity", "gender", "education", "nativity1", "nativity2", "legality", "income1", "income2", "age1", "age2")),
                       plotOutput("plot2"))
             
             # end of tab panel
    ),
    
    tabPanel("Analysis",
             titlePanel("Analysis"),
             
             # begin fluid page
             # fluidPage(selectInput("x", "X variable", choices = names(df)),
             #           selectInput("y", "Y variable", choices = names(df)),
             #           selectInput("geom", "geom", c("point", "column", "jitter")),
             # 
             #           # plot outputs
             #           plotOutput("plot"),
             #           p("something about the first plot"),
             # 
             #           plotOutput("plot2"),
             #           p("something about the second plot"),
             #           
             #           plot regression table
             #           uiOutput("lm1"),#A regression table
             #           p("something about the regression table"),
             #           )
             
             # end of tab panel
    ),
    
    tabPanel("Conclusion", 
             titlePanel("Conclusion"),
             
             p("Text")
             
             # end of tab panel
    ),
    
    tabPanel("Details", 
             titlePanel("Details"),
             
             p("Angie Shin is a sophomore studying Data Science in the Government department at Harvard College. This Shiny App is a project for the course 'DPI 610: Data Science in Politics' from the department of Democracy, Politics and Institutions at Harvard Kennedy School. This course is taken in cross-registered enrollment with the Faculty of Arts and Sciences at Harvard College as Government 1003."),
             
             # references
             h3("References"),
             p("Ramakrishnan, Karthick, Jennifer Lee, Taeku Lee, and Janelle Wong. “National Asian American Survey (NAAS) 2016 Pre-Election Survey.” Riverside, CA: National Asian American Survey. 2017-12-05."),
             p("Ramakrishnan, Karthick, Jennifer Lee, Taeku Lee, and Janelle Wong. “National Asian American Survey (NAAS) 2016 Post-Election Survey.” Riverside, CA: National Asian American Survey. 2018-03-03."),
             p("Both datasets are available at the Resource Center for Minority Data, curated by ICPSR.")
             
             # end of tab panel
    )
)

server <- function(input, output, session) {
    
    output$plot1 <- renderPlot({
        ggplot(pre, aes(.data[[input$x]])) +
            geom_bar()
    })
    
    output$plot2 <- renderPlot({
        ggplot(post, aes(.data[[input$x]])) +
            geom_bar()
    })
    # 
    # Note plot 3 is just a basic histogram with .data[[input$x]] instead
    # You can totally make a regular ggplot without the drop option by
    # changing .data[[input$x]] to x = your variable
    # output$plot3 <- renderPlot({
    #     ggplot(df, aes(.data[[input$x]])) +
    #         geom_histogram()
    # }, res = 96)
}

shinyApp(ui = ui, server = server)