library(shiny)


shinyServer(function(input, output, session) {
  
  battle.analysis <- reactiveValues(battle_analysis = NULL)
  battle.forces <- reactiveValues(allied_units = NULL,
                                  japan_units = NULL)
  
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
  
  output$txtAlliedInfo <- renderUI({

    req(input$alliedForceList)
    req(input$japanForceList)
    
    getBattleUnits()
    
    forces.allies <- battle.forces$allied_units %>%
      mutate(modifier = if_else(is_extended, 0.5, 1.0),
             af_front = attack_front * modifier,
             af_back =  attack_back * modifier,
             af = if_else(is_flipped, af_back, af_front)) 
    
    af.allies <- sum(forces.allies$af)
    
    damage.min <- NA_integer_
    
    if (!is.null(battle.analysis$battle_losses)) 
    {
      result.w.crit <- prepDataExpectedBattleDamageTaken(battle.analysis$battle_losses, unit.data) %>%
        filter(team == "Japan")
      
      result.wo.crit <- prepDataExpectedBattleDamageTaken(battle.analysis$battle_losses, unit.data, exclude.critical = TRUE) %>%
        filter(team == "Japan")
      
      damage.min <- min(result.w.crit$damage_taken)
      damage.max.w.crit <- max(result.w.crit$damage_taken)
      damage.max.wo.crit <- max(result.wo.crit$damage_taken)
    }
    
    text <- paste("Allied AF =", af.allies)
    
    if (!is.na(damage.min))
    {
      text <- paste(text, "<br/>", 
                    "Min Damage =", damage.min, "<br/>", 
                    "Max Damage (with Critical) =", damage.max.w.crit, "<br/>",
                    "Max Damage (w/o Critical =", damage.max.wo.crit)
    }
    
    HTML(text)

  })
  
  output$txtJapanInfo <- renderText({
    
    req(input$alliedForceList)
    req(input$japanForceList)
    
    getBattleUnits()
    
    forces.japan <- battle.forces$japan_units %>%
      mutate(modifier = if_else(is_extended, 0.5, 1.0),
             af_front = attack_front * modifier,
             af_back =  attack_back * modifier,
             af = if_else(is_flipped, af_back, af_front)) 
    
    af.japan <- sum(forces.japan$af)
    
    damage.min <- NA_integer_
    
    if (!is.null(battle.analysis$battle_losses)) 
    {
      result.w.crit <- prepDataExpectedBattleDamageTaken(battle.analysis$battle_losses, unit.data) %>%
        filter(team == "Allies")

      result.wo.crit <- prepDataExpectedBattleDamageTaken(battle.analysis$battle_losses, unit.data, exclude.critical = TRUE) %>%
        filter(team == "Allies")
      
      damage.min <- min(result.w.crit$damage_taken)
      damage.max.w.crit <- max(result.w.crit$damage_taken)
      damage.max.wo.crit <- max(result.wo.crit$damage_taken)
    }
    
    text <- paste("Japan AF =", af.japan)
    
    if (!is.na(damage.min))
    {
      text <- paste(text, "<br/>", 
                    "Min Damage =", damage.min, "<br/>", 
                    "Max Damage (with Critical) =", damage.max.w.crit, "<br/>",
                    "Max Damage (w/o Critical =", damage.max.wo.crit)
    }
    
    HTML(text)
    
  })
  
  getBattleUnits <- reactive({
    
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
    
    battle.forces$allied_units <- forces.allies
    battle.forces$japan_units <- forces.japan
  })

  observeEvent(input$btnAnalyzeBattle, {
    
    req(input$alliedForceList)
    req(input$japanForceList)
    
    getBattleUnits()
    
    forces.allies <- battle.forces$allied_units
    forces.japan <- battle.forces$japan_units

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
    
    battle.analysis$battle_results <- result$battle.results
    battle.analysis$battle_losses <- result$battle.losses
    
    dir.create("output", showWarnings = FALSE, recursive = TRUE)
    
    write.csv(forces.allies, "output/forces_allies.csv", row.names = FALSE)
    write.csv(forces.japan, "output/forces_japan.csv", row.names = FALSE)
    write.csv(result$battle.results, "output/battle_results.csv", row.names = FALSE)
    write.csv(result$battle.losses, "output/battle_losses.csv", row.names = FALSE)
    
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
  
  output$plotExpectedBattleDamageTaken_Allies <- renderPlot({
    req(battle.analysis$battle_losses)
    result <- prepDataExpectedBattleDamageTaken(battle.analysis$battle_losses, unit.data)
    plotExpectedBattleDamageTaken(result, "Allies")
  })
  
  output$plotExpectedBattleDamageTaken_Japan <- renderPlot({
    req(battle.analysis$battle_losses)
    result <- prepDataExpectedBattleDamageTaken(battle.analysis$battle_losses, unit.data)
    plotExpectedBattleDamageTaken(result, "Japan")
  })
  
  output$plotExpectedBattleDamageTaken_ByUnit <- renderPlot({
    req(battle.analysis$battle_losses)
    result <- prepDataExpectedBattleDamageTaken_ByUnit(battle.analysis$battle_losses, unit.data)
    plotExpectedBattleDamageTaken_ByUnit(result)
  })
  
  output$tblBattleResults <- renderDataTable({
    req(battle.analysis$battle_results)
    prepDataBattleResultsGrid(battle.analysis$battle_results)
  })
  
  getUnitImage <- session$registerDataObj(
    
    name   = 'arrests', # an arbitrary but unique name for the data object
    data   = unit.data,
    filter = function(data, req) {
      
      query <- parseQueryString(req$QUERY_STRING)
      unit.id <- as.integer(query$id)
      
      name.file <- file.path("images", data %>% filter(id == unit.id) %>% select(image_name_front) %>% pull())

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
