library(shiny)


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
                   choices = NULL,
                   options = list(
                     placeholder = "Select one or more units")
    )
  })
  
  observeEvent(input$alliedForceList, {
    
    removeUI(selector = "div:has(> #alliesUnitInfo)", multiple = TRUE)

    unit.list <- input$alliedForceList
    
    for (unit.id in unit.list) {
      insertUI(selector = "#divAlliedUnits", 
               where = "afterBegin", 
               ui = tagList(unitStatusUI(paste0("allies", unit.id), "allies"))
      )
    }
    
    unit.status <- callModule(unitStatus, paste0("allies", unit.id), unit.id, unit.data)

  })
  
  observeEvent(input$japanForceList, {
    
    removeUI(selector = "div:has(> #japanUnitInfo)", multiple = TRUE)
    
    unit.list <- input$japanForceList
    
    for (unit.id in unit.list) {
      insertUI(selector = "#divJapanUnits", 
               where = "afterBegin", 
               ui = tagList(unitStatusUI(paste0("japan", unit.id), "japan"))
      )
    }
    
    unit.status <- callModule(unitStatus, paste0("japan", unit.id), unit.id, unit.data)
    
  })
  
  observeEvent(input$btnAnalyzeBattle, {
    
    req(input$alliedForceList)
    req(input$japanForceList)
    
    units.flipped <- -1
    units.extended <- -1
    units.inhex <- -1
    
    for (unit.id in input$alliedForceList)
    {
      control.name <- paste0("allies", unit.id, "-chkUnitStatus")
      
      if ("is_flipped" %in% input[[control.name]])
        units.flipped <- c(units.flipped, unit.id)
      
      if ("is_extended" %in% input[[control.name]])
        units.extended <- c(units.extended, unit.id)
      
      if ("is_in_battle_hex" %in% input[[control.name]])
        units.inhex <- c(units.inhex, unit.id)
    }
    
    for (unit.id in input$japanForceList)
    {
      control.name <- paste0("japan", unit.id, "-chkUnitStatus")
      
      if ("is_flipped" %in% input[[control.name]])
        units.flipped <- c(units.flipped, unit.id)
      
      if ("is_extended" %in% input[[control.name]])
        units.extended <- c(units.extended, unit.id)
      
      if ("is_in_battle_hex" %in% input[[control.name]])
        units.inhex <- c(units.inhex, unit.id)
    }
    
    print(units.flipped)
    print(units.extended)
    print(units.inhex)
    
    forces.allies <- unit.data %>%
      filter(id %in% input$alliedForceList) %>%
      mutate(is_flipped = (id %in% units.flipped),
             is_extended = (id %in% units.extended),
             is_in_battle_hex = (id %in% units.inhex))
    
    forces.japan <- unit.data %>%
      filter(id %in% input$japanForceList) %>%
      mutate(is_flipped = (id %in% units.flipped),
             is_extended = (id %in% units.extended),
             is_in_battle_hex = (id %in% units.inhex))
    
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
    
    write.csv(forces.allies, "output/forces_allies.csv", row.names = FALSE)
    write.csv(forces.japan, "output/forces_japan.csv", row.names = FALSE)
    write.csv(result, "output/battle_results.csv", row.names = FALSE)
    
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
      unit.id <- as.integer(query$id)
      
      name.file <- file.path("images", data[id == unit.id, image_name_front])

      shiny:::httpResponse(
        200, 'image/gif', readBin(name.file, 'raw', file.info(name.file)[, 'size'])
      )
      
    }
  )
  
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
  
  
  updateSelectizeInput(
    session, 'japanForceList', server = TRUE,
    choices = getForceList("an", team.name = "Japan", unit.data = unit.data),
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
