unitStatusUI <- function(id, team.name)
{
  ns <- NS(id)
  
  print(ns("chkUnitStatus"))
  
  tagList(
    fluidRow(
      tags$div(id = paste0(team.name,"UnitInfo"),
               column(width = 4, plotOutput(ns("imgUnit"), width = 65, height = 65)),
               column(width = 8, checkboxGroupInput(ns("chkUnitStatus"), label = NULL, 
                                                    choiceNames = c("Flipped?", "Extended Range?", "In Battle Hex?"), 
                                                    choiceValues = c("is_flipped", "is_extended", "is_in_battle_hex")))
      )
    )
  )
  
}

unitStatus <- function(input, output, session, unit.id, unit.data)
{
  output$imgUnit <- renderImage({
    
    cat("Unit ID:", unit.id, "\n")
    
    image.front <- unit.data %>%
      filter(id == unit.id) %>%
      select(image_name_front) %>%
      pull()
    
    image.back <- unit.data %>%
      filter(id == unit.id) %>%
      select(image_name_back) %>%
      pull()
    
    if ("is_flipped" %in% input$chkUnitStatus) {
      name.file <- normalizePath(file.path("images", image.back))
    } else {
      name.file <- normalizePath(file.path("images", image.front))
    }
    
    list(src = name.file, alt = "This is a test")
    
  }, deleteFile = FALSE)
  
}


