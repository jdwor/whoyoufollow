library(shiny)
library(ggplot2)
library(shinythemes)
library(shinyalert)
library(dplyr)
library(httr)
library(shinyalert)
library(shinycssloaders)
library(V8)
library(rtweet)
library(gtools)
library(uuid)
library(jsonlite)
library(scales)
library(shinyjs)
library(gender)
library(genderdata)
library(wru)
library(shinybusy)
library(extrafont)

shinyUI(
  fluidPage(title="Who do you follow? Estimating race and gender on Twitter",
            useShinyalert(),
            tags$style(type="text/css",".recalculating {opacity: 1.0;}"),
            tags$style("#wordtab {font-size: 1.1em; font-family: Garamond}"),
            tags$style("#sumtab {font-size: 1.1em; font-family: Garamond}"),
            tags$style("#thisfriend {font-size: 1.1em; font-family: Garamond}"),
            tags$style("#counter {font-size: 1em; font-family: Garamond}"),
            tags$style("#gend {font-size: 1em; font-family: Garamond}"),
            tags$style("#race {font-size: 1em; font-family: Garamond}"),
            tags$style("#relweight {font-size: 1.2em; font-family: Garamond}"),
            tags$style(type="text/css", "text {font-family: sans-serif}"),
            theme = shinytheme("paper"),
            shinyjs::useShinyjs(),
            tags$head(
              tags$head(tags$style("h1{font-family: Garamond; font-weight: 800; font-size: 2.5em;margin-left:2%; margin-bottom:0px}")),
              tags$head(tags$style("h2{font-family: Garamond; font-size: 2.0em;margin-left:2%;margin-top:2px}")),
              tags$head(tags$style("h3{font-family: Garamond; font-size: 1.6em;margin-left:2%;margin-top:0px; margin-bottom:0px}")),
              tags$head(tags$style("p{font-family: Garamond; font-size: 1.2em; margin-left:2%;
                                   margin-top:0px; margin-bottom:0px; padding-top:0px; padding-bottom:0px}"))
            ),
            fluidRow(
              style = "background-color:rgb(47,79,79,.75);",
              column(
                align="left",
                width=3,
                br(),
                actionButton("descrip","Description",
                             style="font-size: 1em; font-family: Garamond"),
                actionButton("methods","Methods",
                             style="font-size: 1em; font-family: Garamond")
              ),
              column(
                align="center",
                width=6,
                h1("Who do you follow?", class="title", style = "color:white"),
                h2("Estimating race and gender on Twitter",style="color:white"),
                br()
              ),
              column(
                align="right",
                width=3,
                br(),
                actionButton("changeuser","Change user",
                             style="font-size: 1em; font-family: Garamond"),
                actionButton("editgoals","Edit benchmarks",
                             style="font-size: 1em; font-family: Garamond")
              )
            ),
            fluidRow(id="row1",
              br(),
              uiOutput("resultshead"),
              p("Automatic assessments of race and gender can be highly flawed, so we recommend adding manual data below"),
              br()
            ),
            fluidRow(id="row3",
              column(
                align="center",
                width=4,
                h3("Race"),
                withSpinner(plotOutput("raceplot",width="90%"),color="#2f4f4f"),
                br(),
                br()
                ),
              column(
                align="center",
                width=4,
                h3("Gender"),
                withSpinner(plotOutput("gendplot",width="90%"),color="#2f4f4f"),
                br(),
                br()
              ),
              column(
                align="center",
                width=4,
                h3("Race x Gender"),
                withSpinner(plotOutput("allplot",width="90%"),color="#2f4f4f"),
                br(),
                br()
              )
            ),
            fluidRow(id="row4",
              column(
                width=1
              ),
              column(
                align="center",
                width=10,
                h3("Estimated proportion in each group, with 95% confidence intervals"),
                withSpinner(tableOutput("sumtab"),color="#2f4f4f"),
                br()
              ),
              column(
                width=1
              )
            ),
            fluidRow(id="row2",
             column(
               width=2
             ),
             column(
               align="center",
               width=8,
               uiOutput("sumhead"),
               withSpinner(tableOutput("wordtab"),color="#2f4f4f"),
               p("*Benchmarks can be edited using the 'Edit benchmarks' button",
                 style="font-size: 1.1em; margin-left:0%"),
               br(),
               br(),
               uiOutput("weights"),
               br()
             ),
             column(
               width=2
             )
            ),
            fluidRow(
              style = "background-color:#D4DADA;",
              br()
            ),
            fluidRow(
              style = "background-color:#D4DADA;",
              column(
                align="left",
                width=4,
                h3(strong("Enter data for a sample of followees")),
                p("Though optional, adding manual data will improve accuracy"),
                br()
              ),
              column(
                align="center",
                width=4,
                actionButton("showres", "Hide results (hide for faster updating)",
                             style="font-size: 1em; font-family: Garamond"),
                br()
              ),
              column(
                align="center",
                width=4
              )
            ),
            fluidRow(
              style = "background-color:#D4DADA;",
              column(
                width=2,
                br(),
                align="right",
                p("Random Followee:")
              ),
              column(
                width=1,
                align="center",
                htmlOutput("picture")
              ),
              column(
                width=9,
                align="left",
                tableOutput('thisfriend')
              )
            ),
            fluidRow(
              style = "background-color:#D4DADA;",
              column(
                align="center",
                width=2
              ),
              column(
                align="center",
                width=3,
                hr(),
                radioButtons('gend', label="Select gender", 
                             choices=c("Man","Woman","Non-binary","Unknown/NA"),
                             selected="Woman",inline=T)
              ),
              column(
                align="center",
                width=3,
                hr(),
                radioButtons('race', label="Select race", 
                             choices=c("White","Black","Asian","Hispanic/Latinx",
                                       "Middle Eastern","Native American","Unknown/NA"),
                             selected="Black",inline=T),
                br()
              ),
              column(
                align="center",
                width=2,
                hr(),
                textOutput("counter"),
                actionButton("submit", "Submit",
                             style="font-size: 1em; font-family: Garamond"),
                br(),
                br()
              ),
              column(
                align="center",
                width=2
              )
            )
  )
)
