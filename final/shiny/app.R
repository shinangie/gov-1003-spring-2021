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
  tabPanel("Data", 
           titlePanel("Data"),
           
           # intro
           p("The National Asian American Survey (NAAS) is a scientific and nonpartisan effort to poll the opinions of Asian Americans and Pacific Islanders. This project will concern the 2016 public opinion surveys that the NAAS has organized before and after the 2016 presidential election. The purpose of this project is to measure AAPI political opinions intentionally and spotlight them in a deliberately academic manner, in order to help close the gap in research and academia on AAPI experiences."),
           p("The 2016 National Asian American Survey is composed of two waves. Individual-level data from the Fall 2016 National Asian American Survey (pre-election) was fielded from August through October 2016. Individual-level data from the NAAS 2016 Post-Election Survey was fielded from November 2016 through February 2017. For both surveys, the NAAS collaborated with Catalist, a for-profit corporation based in Washington, D.C. that operates a voter database and provides data and data-related services to progressive organizations, to utilize registered voter and commercial vendor samples; these were classified according to a variety of ethnicity variables. Both surveys were weighted by ethnicity and gender, age, state of residence, education, and nativity by a unique ranking method."),
           
           # pre
           h3("The 2016 NAAS Pre-Election Survey"),
           p("The 2016 NAAS Pre-Election Survey conducted 2,238 telephone interviews to AAPI adults (respondents were required to be 18 years of age or older to proceed with the survey) between August 10 and September 29, 2016. Interact with the visualization below to observe the distribution of the data according to some demographic characteristics."),
           fluidPage(selectInput("x1", "Demographic", choices = c("ethnicity", "gender", "education", "nativity1", "nativity2", "legality", "income1", "income2", "age1", "age2")),
                     plotOutput("plot1")),
           
           # post
           h3("The 2016 NAAS Post-Election Survey"),
           p("The 2016 NAAS Post-Election Survey conducted 4,393 telephone interviews to AAPI adults (respondents were required to be 18 years of age or older to proceed with the survey) between November 10, 2016 and March 2, 2017. Interact with the visualization below to observe the distribution of the data according to some demographic characteristics."),
           fluidPage(selectInput("x2", "Demographic", choices = c("ethnicity", "gender", "education", "nativity1", "nativity2", "legality", "income1", "income2", "age1", "age2")),
                     plotOutput("plot2"))
           
           # end of tab panel
  ),
  
  tabPanel("The 2016 Elections",
           titlePanel("The 2016 Elections"),
           
           h3("Party Identification"),
           fluidPage(selectInput("x3", "Demographic", choices = c("ethnicity", "gender", "education", "nativity1", "nativity2", "legality", "income1", "income2", "age1", "age2")),
                     plotOutput("plot3a"),
                     plotOutput("plot3b")),

           h3("Candidate Choice"),
           fluidPage(selectInput("x4", "Demographic", choices = c("ethnicity", "gender", "education", "nativity1", "nativity2", "legality", "income1", "income2", "age1", "age2")),
                     plotOutput("plot4a"),
                     plotOutput("plot4b")),

           h3("Voter Contact"),
           fluidPage(selectInput("x5", "Demographic", choices = c("ethnicity", "gender", "education", "nativity1", "nativity2", "legality", "income1", "income2", "age1", "age2")),
                     plotOutput("plot5a"),
                     plotOutput("plot5b")),
           
           p("As shown above, despite generally affiliating with progressive leadership and parties, AAPI voices are largely uncontacted. Asian Americans and Pacific Islanders were less likely to be contacted by political parties and other partisan and nonpartisan voter organizations, with large variations across national origin lines.")
           
           # end of tab panel
  ),
  
  tabPanel("Policy Views",
           titlePanel("Policy Views"),
           p(""),
           
           h3("Pre-Election"),
           p("One question in the Pre-Election survey asked respondents on their opinion for a certain selection of policies, phrased in a segmented form as follows:"),
           tags$ol(
             tags$li("Do you support or oppose the health care law passed by Barack Obama and Congress in 2010?"),
             tags$li("Do you support or oppose major new spending by the federal government that would help undergraduates pay tuition at public colleges without needing loans?"),
             tags$li("Do you support or oppose accepting Syrian refugees into the United States?"),
             tags$li("Do you support or oppose legalizing the possession of small amounts of marijuana for personal use?"),
             tags$li("Do you support or oppose banning people who are Muslim from entering the United States?"),
             tags$li("Do you support or oppose setting stricter emission limits on power plants in order to address climate change?"),
             tags$li("Do you support or oppose the government doing more to give blacks equal rights with whites?")
           ),
           fluidPage(selectInput("x6a", "Policy", choices = c("p1", "p2", "p3", "p4", "p5", "p6", "p7")),
                     selectInput("x6b", "Demographic", choices = c("ethnicity", "gender", "education", "nativity1", "nativity2", "legality", "income1", "income2", "age1", "age2")),
                     plotOutput("plot6")),
           
           h3("Post-Election"),
           p("One question in the Post-Election survey asked respondents on their agreement with a certain selection of economic policies, phrased in a segmented form as follows:"),
           tags$ol(
             tags$li("The federal government should do more to reduce income differences between the richest and the poorest households."),
             tags$li("The federal government should do more to regulate banks."),
             tags$li("The federal government should raise the minimum wage to allow every working American a decent standard of living."),
             tags$li("The federal government should increase income taxes on people making over a million dollars a year."),
             tags$li("The federal government should do more to discourage big American companies from hiring foreign workers to replace workers in the U.S."),
             tags$li("The federal government should enact major new spending that would help undergraduates pay tuition at public colleges without needing loans."),
           ),
           fluidPage(selectInput("x7a", "Policy", choices = c("e1", "e2", "e3", "e4", "e5", "e6")),
                     selectInput("x7b", "Demographic", choices = c("ethnicity", "gender", "education", "nativity1", "nativity2", "legality", "income1", "income2", "age1", "age2")),
                     plotOutput("plot7")),
           
           p("Another question in the Post-Election survey asked respondents on their agreement with a certain selection of immigrant policies, phrased in a segmented form as follows:"),
           tags$ol(
             tags$li("Undocumented or illegal immigrants should be allowed to have an opportunity to eventually become U.S. citizens."),
             tags$li("Congress needs to increase the number of work visas it issues every year."),
             tags$li("Congress needs to increase the number of family visas it issues every year."),
             tags$li(" States should provide driver’s licenses to all residents, regardless of their immigration status."),
           ),
           fluidPage(selectInput("x8a", "Policy", choices = c("i1", "i2", "i3", "i4")),
                     selectInput("x8b", "Demographic", choices = c("ethnicity", "gender", "education", "nativity1", "nativity2", "legality", "income1", "income2", "age1", "age2")),
                     plotOutput("plot8")),

           h3("Literacy and Access"),
           p("Several of the AAPI respondents participated in the 2016 NAAS surveys by speaking in a language other than English. Language barriers are capable of limiting AAPI exposure to current events, news and politics with the lack of accessibility in American news outlets. This question asks AAPI respondents if they depend more on American television, radio, and newspapers or more on AAPI television, radio, and newspapers."),
           fluidPage(selectInput("x9", "Demographic", choices = c("ethnicity", "gender", "education", "nativity1", "nativity2", "legality", "income1", "income2", "age1", "age2")),
                     plotOutput("plot9a"),
                     plotOutput("plot9b"))
  ),
  
  tabPanel("Society and Racism",
           titlePanel("Society and Racism"),
           
           h3("Perceived Societal Issues"),
           p("One question in the Pre-Election survey gauged what respondents believed to be the most important problem facing the United States today as well as the issue that is the most important to each respondent personally."),
           fluidPage(selectInput("x10", "Demographic", choices = c("ethnicity", "gender", "education", "nativity1", "nativity2", "legality", "income1", "income2", "age1", "age2")),
                     plotOutput("plot10a"),
                     plotOutput("plot10b")),

           h3("Microaggressions"),
           p("The Post-Election survey also measured day to day encounters with strangers that AAPI respondents had in the United States. Questions were segmented to gauge occurrence of the following in an average month:"),
           tags$ol(
             tags$li("You receive poorer service than other people at restaurants or stores."),
             tags$li("People act as if you don’t speak English."),
             tags$li("People act as if they are afraid of you."),
             tags$li("People act as if they think you are dishonest."),
             tags$li("You are called names or insulted."),
             tags$li("You are threatened or harassed."),
             tags$li("People mispronounce your name."),
             tags$li("People assume you are good at math and science."),
             tags$li("People assume you are not a creative thinker.")
           ),
           fluidPage(selectInput("x11a", "Microaggression", choices = c("mi1", "mi2", "mi3", "mi4", "mi5", "mi6", "mi7", "mi8", "mi9")),
                     selectInput("x11b", "Demographic", choices = c("ethnicity", "gender", "education", "nativity1", "nativity2", "legality", "income1", "income2", "age1", "age2")),
                     plotOutput("plot11")),

           h3("Macroaggressions"),
           p("The Post-Election survey also measured ways in which AAPI respondents may have been treated poorly or unfairly. Questions were segmented to gauge occurrence of the following:"),
           tags$ol(
             tags$li("Have you ever been unfairly denied a promotion?"),
             tags$li("Have you ever been unfairly fired from a job?"),
             tags$li("For unfair reasons, do you think you have ever not been hired for a job?"),
             tags$li("Have you ever been unfairly stopped, searched, questioned, physically threatened or abused by the police?"),
             tags$li("Do you think you have ever been unfairly prevented from moving into a neighborhood because the landlord or a realtor refused to sell or rent you a house or apartment?"),
             tags$li("Have you ever moved into a neighborhood where neighbors made life difficult for you or your family?"),
             tags$li("Have you ever been denied entry into a religious institution?")
           ),
           fluidPage(selectInput("x12a", "Macroaggression", choices = c("ma1", "ma2", "ma3", "ma4", "ma5", "ma6", "ma7")),
                     selectInput("x12b", "Demographic", choices = c("ethnicity", "gender", "education", "nativity1", "nativity2", "legality", "income1", "income2", "age1", "age2")),
                     plotOutput("plot12")),

           # h3("AAPI Identities"),
           # p(""),
           # fluidPage(selectInput("x13", "Demographic", choices = c("ethnicity", "gender", "education", "nativity1", "nativity2", "legality", "income1", "income2", "age1", "age2")),
           #           plotOutput("plot13")),
  ),
  
  tabPanel("Conclusion", 
           titlePanel("Conclusion"),
           
           p("From the Pre- and Post-Election surveys, the key findings were:"),
           tags$ol(
             tags$li("Compared to the national average, Asian American registered voters hold more favorable views of Obama and Clinton, and much more unfavorable views of Trump."),
             tags$li("Asian American registered voters rank the economy, national security, racism, government, and immigration as the most important problem facing the country."),
             tags$li("Asian Americans hold progressive views on many policy issues, including health care, education spending, racial justice, and bans on Muslim immigrants. However, they are split on Syrian refugees and are conservative on marijuana legalization."),
             tags$li("Ethnic media is an important source of information for particular Asian groups. There is also pressing need for Asian American voter outreach, especially given the reported lack of contact by parties and candidates."),
             tags$li("The most serious problems facing Asian Americans include the affordability of college, health care, and elder care."),
             tags$li("Experiences with discrimination and micro-aggression vary significantly across groups, and there has been an increase in job-related discrimination experiences for some groups since 2008."),
             tags$li("The question of Asian American identity is contested, with South Asian groups (Indians and Pakistanis) finding it more challenging for American society to view them as Asian American.")
           ),
           p("For the AAPI communities across the country, the political path is not uniform, but, rather, depends more strongly on socioeconomic standing, heritage, and age. Perhaps even more importantly, they are much less likely to be mobilized to participate in politics than most Americans. The AAPI communities have always been aggregately portrayed as a passive demographic group, but the 2016 NAAS data displays them in a more accurate light of strong, political voices and increasingly vital bodies to American politics today."),
           
           # references
           h3("References"),
           p("Ramakrishnan, Karthick, Jennifer Lee, Taeku Lee, and Janelle Wong. “National Asian American Survey (NAAS) 2016 Pre-Election Survey.” Riverside, CA: National Asian American Survey. 2017-12-05."),
           p("Ramakrishnan, Karthick, Jennifer Lee, Taeku Lee, and Janelle Wong. “National Asian American Survey (NAAS) 2016 Post-Election Survey.” Riverside, CA: National Asian American Survey. 2018-03-03."),
           p("Both datasets are available at the Resource Center for Minority Data, curated by ICPSR."),
           
           # details
           h3("About"),
           p("Angie Shin is a sophomore studying Data Science in the Government department at Harvard College. This Shiny App is a project for the course 'DPI 610: Data Science in Politics' from the department of Democracy, Politics and Institutions at Harvard Kennedy School. This course is taken in cross-registered enrollment with the Faculty of Arts and Sciences at Harvard College as Government 1003.")
           
           # end of tab panel
  )
)

server <- function(input, output, session) {
  
  # demographics distribution (dd) for pre
  
  output$plot1 <- renderPlot({
    ggplot(pre, aes(.data[[input$x1]])) +
      geom_bar() +
      theme_classic()
  })
  
  # demographics distribution (dd) for post
  
  output$plot2 <- renderPlot({
    ggplot(post, aes(.data[[input$x2]])) +
      geom_bar() +
      theme_classic()
  })
  
  # dds for party identification
  
  output$plot3a <- renderPlot({
    pre %>% 
      filter(racegroup %in% c("(1) ASIAN AMERICAN", "(02) NHPI")) %>% 
      ggplot(aes(.data[[input$x3]], fill = party)) +
      geom_bar() +
      theme_classic()
  })

  output$plot3b <- renderPlot({
    post %>% 
      filter(racegroup %in% c("(1) Asian American", "(2) Pacific Islander")) %>% 
      ggplot(aes(.data[[input$x3]], fill = party)) +
      geom_bar() +
      theme_classic()
  })

  # dd for candidate choice

  output$plot4a <- renderPlot({
    pre %>% 
      filter(racegroup %in% c("(1) ASIAN AMERICAN", "(02) NHPI")) %>% 
      ggplot(aes(.data[[input$x3]], fill = votepred)) +
      geom_bar() +
      theme_classic()
  })
  
  output$plot4b <- renderPlot({
    post %>% 
      filter(racegroup %in% c("(1) Asian American", "(2) Pacific Islander")) %>% 
      ggplot(aes(.data[[input$x3]], fill = voteres)) +
      geom_bar() +
      theme_classic()
  })

  # dd for voter contact

  output$plot5a <- renderPlot({
    post %>% 
      filter(racegroup %in% c("(1) Asian American", "(2) Pacific Islander")) %>% 
      ggplot(aes(.data[[input$x3]], fill = contact1)) +
      geom_bar() +
      theme_classic()
  })
  
  output$plot5b <- renderPlot({
    post %>% 
      filter(racegroup %in% c("(1) Asian American", "(2) Pacific Islander")) %>% 
      ggplot(aes(.data[[input$x3]], fill = contact2)) +
      geom_bar() +
      theme_classic()
  })

  # dd for pre policies

  output$plot6 <- renderPlot({
    pre %>% 
      filter(racegroup %in% c("(1) ASIAN AMERICAN", "(02) NHPI")) %>% 
      ggplot(aes(.data[[input$x6b]], fill = .data[[input$x6a]])) +
      geom_bar() +
      theme_classic()
  })

  # dd for post ec policies

  output$plot7 <- renderPlot({
    post %>% 
      filter(racegroup %in% c("(1) Asian American", "(2) Pacific Islander")) %>% 
      ggplot(aes(.data[[input$x7b]], fill = .data[[input$x7a]])) +
      geom_bar() +
      theme_classic()
  })

  # dd for post imm policies

  output$plot8 <- renderPlot({
    post %>% 
      filter(racegroup %in% c("(1) Asian American", "(2) Pacific Islander")) %>% 
      ggplot(aes(.data[[input$x8b]], fill = .data[[input$x8a]])) +
      geom_bar() +
      theme_classic()
  })
  
  # dds for media
  
  output$plot9a <- renderPlot({
    pre %>% 
      filter(racegroup %in% c("(1) ASIAN AMERICAN", "(02) NHPI"),
             !is.na(media1)) %>%  
      ggplot(aes(.data[[input$x9]], fill = media1)) +
      geom_bar() +
      theme_classic()
  })
  
  # output$plot9b <- renderPlot({
  #   pre %>% 
  #     filter(racegroup %in% c("(1) ASIAN AMERICAN", "(02) NHPI"),
  #            !is.na(media1)) %>%  
  #     ggplot(aes(.data[[input$x9]], fill = media2)) +
  #     geom_bar() +
  #     theme_classic()
  # })
  
  # dds for most important problem (pre)

  output$plot10a <- renderPlot({
    pre %>% 
      filter(racegroup %in% c("(1) ASIAN AMERICAN", "(02) NHPI")) %>% 
      ggplot(aes(.data[[input$x10]], fill = soc1)) +
      geom_bar()
  })
  
  output$plot10b <- renderPlot({
    pre %>% 
      filter(racegroup %in% c("(1) ASIAN AMERICAN", "(02) NHPI")) %>% 
      ggplot(aes(.data[[input$x10]], fill = soc2)) +
      geom_bar()
  })
  
  # dd for microaggressions (post)

  output$plot11 <- renderPlot({
    post %>% 
      filter(racegroup %in% c("(1) Asian American", "(2) Pacific Islander")) %>% 
      ggplot(aes(.data[[input$x11b]], fill = .data[[input$x11a]])) +
      geom_bar() +
      theme_classic()
  })

  # dd for macroaggressions (post)

  output$plot12 <- renderPlot({
    post %>% 
      filter(racegroup %in% c("(1) Asian American", "(2) Pacific Islander")) %>% 
      ggplot(aes(.data[[input$x12b]], fill = .data[[input$x12a]])) +
      geom_bar() +
      theme_classic()
  })

  # # dd for aapi identities (post)
  # 
  # output$plot13 <- renderPlot({
  #   ggplot(post, aes(voteres)) +
  #     geom_bar()
  # })
}

shinyApp(ui = ui, server = server)