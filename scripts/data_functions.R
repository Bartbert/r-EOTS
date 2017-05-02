getForceList <- function(battle.type, team.name, unit.data)
{
  result <- unit.data %>%
    filter(team == team.name,
           battle_type == battle.type) %>%
    arrange(nationality, type, branch, desc(attack_front)) %>%
    mutate(unit_stats = mapply(concatUnitStats, attack_front, defense, attack_back, range),
           type = paste0("[", substr(type, 1, 1), "]"),
           label = paste(unit_name, ":", unit_stats)) %>%
    select(id, label)
  
  result <- setNames(result$id, result$label)
}

concatUnitStats <- function(attack.front, defense, attack.back, range.value)
{
  result <- paste0(attack.front, "-", defense)
  
  if (!is.na(range.value))
    result <- paste0(result, "-", range.value)
  
  if (!is.na(attack.back)) {
    result <- paste0(result, " (", attack.back, "-", defense)
    
    if (!is.na(range.value)) {
      result <- paste0(result, "-", range.value)
    }
    
    result <- paste0(result, ")")
  }
  
  result
}

analyzeAirNavalBattle <- function(forces.allies, forces.japan, 
                                  reaction.team = "Japan", intel.condition = "Intercept",
                                  drm.allies = 0, drm.japan = 0)
{
  forces.allies <- forces.allies %>%
    mutate(modifier = if_else(is_extended, 0.5, 1.0),
           af_front = attack_front * modifier,
           af_back =  attack_back * modifier,
           af = if_else(is_flipped, af_back, af_front)) 
  
  af.allies <- sum(forces.allies$af)
  
  forces.japan <- forces.japan %>%
    mutate(modifier = if_else(is_extended, 0.5, 1.0),
           af_front = attack_front * modifier,
           af_back =  attack_back * modifier,
           af = if_else(is_flipped, af_back, af_front)) 
  
  af.japan <- sum(forces.japan$af)
  
  battle.results <- data.table()
  
  for (dr.allies in 0:9)
  {
    for (dr.japan in 0:9)
    {
      damage.allies <- calcuateAirNavalDamage(af.allies, drm.allies, dr.allies)
      damage.japan <- calcuateAirNavalDamage(af.japan, drm.japan, dr.japan)
      
      battle.results <- battle.results %>%
        bind_rows(data.table(dr_allies = dr.allies,
                             dr_japan = dr.japan,
                             damage_allies = damage.allies,
                             damage_japan = damage.japan))
    }
  }
  
  
  result <- determineBattleWins(battle.results, forces.allies, forces.japan,
                                reaction.team, intel.condition,
                                drm.allies, drm.japan)
}

calcuateAirNavalDamage <- function(af, drm, dr)
{
  result <- ceiling(af * getBattleMultiplier(drm, dr))
}

getBattleMultiplier <- function(drm, dr)
{
  total <- dr + drm
  
  multiplier <- case_when(
    total <= 2 ~ 0.25,
    between(total, 3, 5) ~ 0.5,
    total >= 0 ~ 1.0 
  )
}

determineBattleWins <- function(battle.results, forces.allies, forces.japan,  
                                reaction.team = "Japan", intel.condition = "Intercept", 
                                drm.allies = 0, drm.japan = 0)
{
  result <- battle.results %>%
    mutate(allies_post_battle_af = 0,
           japan_post_battle_af = 0,
           battle_winner = NA_character_)
  
  for (i in 1:nrow(result))
  {
    pb.allies <- applyDamage(forces.allies, 
                             battle.results[i, damage_japan],
                             critical = (battle.results[i, dr_japan] == 9), 
                             max_damage = (reaction.team == "Japan"))
    
    pb.japan <- applyDamage(forces.japan, 
                            battle.results[i, damage_allies],
                            critical = (battle.results[i, dr_allies] == 9), 
                            max_damage = (reaction.team == "Allies"))
    
    if (intel.condition == "Surprise")
    {
      if (reaction.team == "Japan") {
        damage <- calcuateAirNavalDamage(sum(pb.japan$af_post_battle), drm.japan, battle.results[i, dr_japan])
        
        pb.allies <- applyDamage(forces.allies, 
                                 damage,
                                 critical = (battle.results[i, dr_japan] == 9), 
                                 max_damage = (reaction.team == "Japan"))
      } else {
        damage <- calcuateAirNavalDamage(sum(pb.allies$af_post_battle), drm.allies, battle.results[i, dr_allies])
        
        pb.allies <- applyDamage(forces.japan, 
                                 damage,
                                 critical = (battle.results[i, dr_allies] == 9), 
                                 max_damage = (reaction.team == "Japan"))
      }
    }
    
    if (intel.condition == "Ambush")
    {
      if (reaction.team != "Japan") {
        damage <- calcuateAirNavalDamage(sum(pb.japan$af_post_battle), drm.japan, battle.results[i, dr_japan])
        
        pb.allies <- applyDamage(forces.allies, 
                                 damage,
                                 critical = (battle.results[i, dr_japan] == 9), 
                                 max_damage = (reaction.team == "Japan"))
      } else {
        damage <- calcuateAirNavalDamage(sum(pb.allies$af_post_battle), drm.allies, battle.results[i, dr_allies])
        
        pb.allies <- applyDamage(forces.japan, 
                                 damage,
                                 critical = (battle.results[i, dr_allies] == 9), 
                                 max_damage = (reaction.team == "Japan"))
      }
    }
    
    result[i, ]$allies_post_battle_af <- sum(pb.allies$af_post_battle)
    result[i, ]$japan_post_battle_af <- sum(pb.japan$af_post_battle)
    result[i, ]$battle_winner <- if_else(result[i, ]$allies_post_battle_af > result[i, ]$japan_post_battle_af,
                                          "Allies", "Japan")
  }
  
  result
}

applyDamage <- function(forces, damage, critical = FALSE, max_damage = FALSE)
{
  forces.damaged <- data.table()
  
  damage.remaining <- damage
  
  forces.all <- forces %>%
    mutate(flipped = FALSE,
           eliminated = FALSE)
  
  if (critical)
  {
    forces.critical <- forces.all %>%
      mutate(loss_delta = if_else(is_flipped, af_back, af_front - af_back)) %>%
      arrange(desc(loss_delta), defense)
    
    critical_applied <- FALSE
    
    for (i in 1:nrow(forces.critical))
    {
      if (damage.remaining >= forces.critical[i, defense])
      {
        damage.remaining <- damage.remaining - forces.critical[i, defense]
        
        if (forces.critical[i, ]$is_flipped) {
          forces.critical[i, ]$eliminated <- TRUE
        } else {
          forces.critical[i, ]$flipped <- TRUE
        }
        
        forces.damaged <- bind_rows(forces.damaged, forces.critical[i, ])
        critical_applied <- TRUE
        break()
      }
    }
    
    if (!critical_applied)
    {
      # Apply a hit to the unit with lowest defense
      forces.critical <- forces.critical %>%
        mutate(max_damage = max_damage,
               af_sort = if_else(max_damage, loss_delta, (loss_delta * -1))) %>%
        filter(defense == min(defense),
               row_number(desc(af_sort)) == 1)
      
      damage.remaining <- damage.remaining - forces.critical[1, defense]
      
      if (forces.critical[1, ]$is_flipped) {
        forces.critical[1, ]$eliminated <- TRUE
      } else {
        forces.critical[1, ]$flipped <- TRUE
      }
      
      forces.damaged <- bind_rows(forces.damaged, forces.critical[1, ])
      
    }
    
    forces.all <- forces.all %>%
      mutate(is_flipped = if_else(id %in% filter(forces.critical, flipped)$id, TRUE, is_flipped),
             flipped = if_else(id %in% filter(forces.critical, flipped)$id, TRUE, flipped),
             eliminated = if_else(id %in% filter(forces.critical, eliminated)$id, TRUE, eliminated)) 
    
  }
  
  forces.full <- forces.all %>%
    filter(!is_flipped,
           !eliminated) %>%
    arrange(desc(af_front), defense)
  
  for (i in 1:nrow(forces.full))
  {
    if (damage.remaining >= forces.full[i, defense])
    {
      damage.remaining <- damage.remaining - forces.full[i, defense]
      forces.full[i, ]$flipped <- TRUE
      forces.damaged <- bind_rows(forces.damaged, forces.full[i, ])
    }
  }
  
  forces.all <- forces.all %>%
    mutate(is_flipped = if_else(id %in% filter(forces.full, flipped)$id, TRUE, is_flipped),
           flipped = if_else(id %in% filter(forces.full, flipped)$id, TRUE, flipped)) 
  
  if (nrow(forces.all) == sum(forces.all$is_flipped))
  {
    forces.flipped <- forces.all %>%
      filter(is_flipped,
             !eliminated) %>%
      arrange(desc(af_back), defense)
    
    for (i in 1:nrow(forces.flipped))
    {
      if (damage.remaining >= forces.flipped[i, defense])
      {
        damage.remaining <- damage.remaining - forces.flipped[i, defense]
        forces.flipped[i, ]$eliminated <- TRUE
        forces.damaged <- bind_rows(forces.damaged, forces.flipped[i, ])
      }
    }
    
    forces.all <- forces.all %>%
      mutate(eliminated = if_else(id %in% filter(forces.flipped, eliminated)$id, TRUE, eliminated)) 
  }
  
  result <- forces.all %>%
    group_by(id) %>%
    filter(row_number(desc(eliminated)) == 1) %>%
    mutate(af_post_battle = case_when(eliminated ~ 0, 
                                      is_flipped ~ af_back,
                                      TRUE ~ af_front)) %>%
    select(id, flipped, eliminated, af_post_battle)
}

getDieRollMods <- function(reaction.team, intel.condition, us.airpower, ec.allies, ec.japan)
{
  drm.allies <- us.airpower + ec.allies
  drm.japan <- ec.japan
  
  if (intel.condition == "Surprise")
  {
    if (reaction.team == "Allies") {
      drm.japan <- drm.japan + 3
    } else {
      drm.allies <- drm.allies + 3
    }
  }
  
  if (intel.condition == "Ambush")
  {
    if (reaction.team == "Allies") {
      drm.allies <- drm.allies + 4
    }
  }
  
  list(drm.allies = drm.allies, drm.japan = drm.japan)
}

prepDataExpectedBattleWins <- function(battle.results)
{
  result <- battle.results %>%
    group_by(battle_winner) %>%
    summarise(win_probability = n()) %>%
    mutate(win_probability = win_probability/100)
  
  if (nrow(filter(result, battle_winner == "Allies")) == 0)
    bind_rows(data.table(battle_winner = "Allies", win_probability = 0))
  
  if (nrow(filter(result, battle_winner == "Japan")) == 0)
    bind_rows(data.table(battle_winner = "Japan", win_probability = 0))
  
  result
}

prepDataExpectedBattleDamageInflicted <- function(battle.results)
{
  result.japan <- battle.results %>%
    group_by(damage_japan) %>%
    summarise(damage_probability = n()) %>%
    mutate(damage_probability = damage_probability/100,
           team_name = "Japan") %>%
    rename(damage_inflicted = damage_japan)
  
  result.allies <- battle.results %>%
    group_by(damage_allies) %>%
    summarise(damage_probability = n()) %>%
    mutate(damage_probability = damage_probability/100,
           team_name = "Allies") %>%
    rename(damage_inflicted = damage_allies)
  
  list(result.allies = result.allies, result.japan = result.japan)
}

