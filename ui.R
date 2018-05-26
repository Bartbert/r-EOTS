
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "EOTS: South Pacific"),
  dashboardSidebar(width = "240px", collapsed = TRUE,
                   sidebarMenu(id = "sidebar_tabs",
                               menuItem("Battles", tabName = "battles"))),
  dashboardBody(
    tabItems(
      tabItem("battles",
              wellPanel(
                fluidRow(
                  column(width = 6,
                         radioButtons("radioBattleType", label = "Battle Type",
                                      choices = list("Air/Naval" = "an", "Ground" = "ground"),
                                      selected = "an", inline = TRUE)
                  ),
                  column(width = 6,
                         radioButtons("radioReactionPlayer", label = "Reaction Player",
                                      choices = list("Allies" = "Allies", "Japan" = "Japan"),
                                      selected = "Allies", inline = TRUE)
                  )
                ),
                fluidRow(
                  column(width = 6,
                         radioButtons("radioIntelCondition", label = "Intel Condition",
                                      choices = list("Intercept" = "Intercept", "Surprise" = "Surprise", "Ambush" = "Ambush"),
                                      selected = "Intercept", inline = TRUE)
                  ),
                  column(width = 6,
                         radioButtons("radioUSAir", label = "US Airpower DRM",
                                      choices = list("+0 (1942)" = 0, "+1: (1943)" = 1, "+3: (1944/1945)" = 3),
                                      selected = 0, inline = TRUE))
                ),
                fluidRow(
                  column(width = 6,
                         numericInput("numAlliedECmodifier", label = "Allied EC DRM", 
                                      value = 0, min = 0, max = 3, step = 1)),
                  column(width = 6,
                         numericInput("numJapanECmodifier", label = "Japan EC DRM", 
                                      value = 0, min = 0, max = 3, step = 1))
                )
              ),
              fluidRow(
                box(
                  width = 12, status = "info", solidHeader = TRUE, title = "Select Forces",
                  fluidRow(
                    column(width = 6, 
                           wellPanel(
                             fluidRow(
                               column(width = 12, uiOutput("alliedForceList"))
                             )
                           )),
                    column(width = 6, 
                           wellPanel(
                             fluidRow(
                               column(width = 12, uiOutput("japanForceList"))
                             )
                           ))
                  ),
                  fluidRow(
                    column(width = 6, wellPanel(htmlOutput("txtAlliedInfo"))),
                    column(width = 6, wellPanel(htmlOutput("txtJapanInfo")))
                  ),
                  fluidRow(
                    column(width = 6, wellPanel(tags$div(id = "divAlliedUnits"))),
                    column(width = 6, wellPanel(tags$div(id = "divJapanUnits")))
                  )
                )
              ),
              fluidRow(
                box(
                  width = 12, status = "info", solidHeader = TRUE, title = "Battle Analysis",
                  fluidRow(
                    column(width = 12, align = "center",
                           inputPanel(
                             actionButton("btnAnalyzeBattle", label = "Analyze Battle")
                           )
                    )
                  ),
                  fluidRow(
                    column(width = 12,
                           wellPanel(
                             plotOutput("plotExpectedBattleWins", width = "100%", height = "400px")
                           )
                    )
                  ),
                  fluidRow(
                    column(width = 6,
                           wellPanel(
                             plotOutput("plotExpectedBattleDamageInflicted_Allies", width = "100%", height = "400px")
                           )
                    ),
                    column(width = 6,
                           wellPanel(
                             plotOutput("plotExpectedBattleDamageInflicted_Japan", width = "100%", height = "400px")
                           )
                    )
                  ),
                  fluidRow(
                    column(width = 6,
                           wellPanel(
                             plotOutput("plotExpectedBattleDamageTaken_Allies", width = "100%", height = "400px")
                           )
                    ),
                    column(width = 6,
                           wellPanel(
                             plotOutput("plotExpectedBattleDamageTaken_Japan", width = "100%", height = "400px")
                           )
                    )
                  ),
                  fluidRow(
                    column(width = 12,
                           wellPanel(
                             plotOutput("plotExpectedBattleDamageTaken_ByUnit", width = "100%", height = "400px")
                           )
                    )
                  ),
                  fluidRow(
                    column(width = 12,
                           dataTableOutput("tblBattleResults"))
                  )
                )
              ))
    ))
)