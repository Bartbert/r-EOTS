library(shiny)
library(caTools)
library(png)

shinyServer(function(input, output, session) {
  
  battle.analysis <- reactiveValues(battle_analysis = NULL)

  output$alliedForceList <- renderUI({
    selectizeInput("alliedForceList", 
                label = "Allied Forces", 
                multiple = TRUE,
                choices = NULL,
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
    
    battle.analysis$battle_results <- result
    
    print(forces.allies)
    print(forces.japan)
    print(result)
    
    write.csv(result, "data/battle_results.csv", row.names = FALSE)

  })
  
  output$plotExpectedBattleWins <- renderPlot({
    req(battle.analysis$battle_results)
    
    plotExpectedBattleWins(prepDataExpectedBattleWins(battle.analysis$battle_results))
  })
  
  output$plotExpectedBattleDamageInflicted_Allies <- renderPlot({
    req(battle.analysis$battle_results)
    result <- prepDataExpectedBattleDamageInflicted(battle.analysis$battle_results)
    plotExpectedBattleDamageInflicted(result$result.allies)
  })
  
  output$plotExpectedBattleDamageInflicted_Japan <- renderPlot({
    req(battle.analysis$battle_results)
    result <- prepDataExpectedBattleDamageInflicted(battle.analysis$battle_results)
    plotExpectedBattleDamageInflicted(result$result.japan)
  })
  
  output$tblBattleResults <- renderDataTable({
    battle.analysis$battle_results
  })

  getUnitImage <- session$registerDataObj(
    
    name   = 'arrests', # an arbitrary but unique name for the data object
    data   = unit.data,
    filter = function(data, req) {
      
      query <- parseQueryString(req$QUERY_STRING)
      id <- query$id  
      
      print(paste("id:", id))

      name.file <- "images/WaspF.gif"
      # image.unit <- readPNG(name.file, native = TRUE)
      
      # image <- tempfile()
      # tryCatch({
      #   png(image, width = 65, height = 65, bg = 'transparent')
      #   image.unit
      # }, finally = dev.off())
 
      # send the PNG image back in a response
      shiny:::httpResponse(
        200, 'image/gif', readBin(name.file, 'raw', file.info(name.file)[, 'size'])
      )
      
    }
  )
  
  # update the render function for selectize
  updateSelectizeInput(
    session, 'alliedForceList', server = TRUE,
    choices = getForceList("an", team.name = "Allies", unit.data = unit.data),
    options = list(render = I(
      sprintf(
        "{
        option: function(item, escape) {
        return '<div><img width=\"65\" height=\"65\" ' +
        'src=\"%s&id=' + escape(item.value) + '\" />' +
        escape(item.value) + '</div>';
        }}",
      getUnitImage
      ))
      )    
    )
})
