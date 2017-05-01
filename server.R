
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {

  output$alliedForceList <- renderUI({
    selectizeInput("alliedForceList", 
                label = "Allied Forces", 
                multiple = TRUE,
                choices = getForceList(input$radioBattleType, team.name = "Allies", unit.data = unit.data),
                options = list(
                  placeholder = "Select one or more units"
                ))
  })

  output$japanForceList <- renderUI({
    selectizeInput("japanForceList", 
                label = "Japanese Forces", 
                multiple = TRUE,
                choices = getForceList(input$radioBattleType, team.name = "Japan", unit.data = unit.data),
                options = list(
                  placeholder = "Select one or more units"
                ))
  })
  
  observeEvent(input$btnAnalyzeBattle, {
    
    forces.allies <- unit.data %>%
      filter(id %in% input$alliedForceList) %>%
      mutate(is_flipped = FALSE,
             is_extended = FALSE)
    
    forces.japan <- unit.data %>%
      filter(id %in% input$japanForceList) %>%
      mutate(is_flipped = FALSE,
             is_extended = FALSE)
    
    print(input$radioReactionPlayer)
    print(input$radioIntelCondition)
    print(input$radioUSAir)
    print(input$numAlliedECmodifier)
    print(input$numJapanECmodifier)
    
    dr.mods <- getDieRollMods(reaction.team = input$radioReactionPlayer, 
                              intel.condition = input$radioIntelCondition, 
                              us.airpower = as.integer(input$radioUSAir), 
                              ec.allies = input$numAlliedECmodifier, 
                              ec.japan = input$numJapanECmodifier)
    
    result <- analyzeAirNavalBattle(forces.allies = forces.allies, 
                                    forces.japan = forces.japan, 
                                    reaction.team = input$radioReactionPlayer, 
                                    intel.condition = input$radioIntelCondition, 
                                    drm.allies = dr.mods$drm.allies, 
                                    drm.japan = dr.mods$drm.japan)
    
    print(forces.allies)
    print(forces.japan)
    print(result)

  })
  
})
