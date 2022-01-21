getForceList <- function(battle.type, team.name, unit.data)
{
  result <- unit.data %>%
    filter(team == team.name,
           battle_type == battle.type) %>%
    arrange(nationality, branch, type, unit_name, desc(attack_front)) %>%
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
  
  battle.results <- data.frame()
  
  for (dr.allies in 0:9)
  {
    for (dr.japan in 0:9)
    {
      damage.allies <- calcuateAirNavalDamage(af.allies, drm.allies, dr.allies)
      damage.japan <- calcuateAirNavalDamage(af.japan, drm.japan, dr.japan)
      
      battle.results <- battle.results %>%
        bind_rows(data.frame(dr_allies = dr.allies,
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
  
  battle.losses <- data.frame()
  
  for (i in 1:nrow(result))
  {
    pb.allies <- applyDamage(forces.allies, 
                             damage = battle.results[i, ]$damage_japan,
                             critical = (battle.results[i, ]$dr_japan == 9), 
                             max_damage = (reaction.team == "Japan"), 
                             enemy_air_unit_count = getAirUnitCount(forces.japan, remote_only = FALSE))
    
    pb.japan <- applyDamage(forces.japan, 
                            damage = battle.results[i, ]$damage_allies,
                            critical = (battle.results[i, ]$dr_allies == 9), 
                            max_damage = (reaction.team == "Allies"), 
                            enemy_air_unit_count = getAirUnitCount(forces.allies, remote_only = FALSE))
    
    if (intel.condition == "Surprise")
    {
      if (reaction.team == "Japan") {
        
        damage <- calcuateAirNavalDamage(sum(pb.japan$af_post_battle), drm.japan, battle.results[i, ]$dr_japan)
        
        forces.japan.post.battle <- pb.japan %>%
          inner_join(unit.data, "id") %>%
          filter(!eliminated)
        
        pb.allies <- applyDamage(forces.allies, 
                                 damage,
                                 critical = (battle.results[i, ]$dr_japan == 9), 
                                 max_damage = (reaction.team == "Japan"), 
                                 enemy_air_unit_count = getAirUnitCount(forces.japan.post.battle, remote_only = FALSE))
      } else {
        
        damage <- calcuateAirNavalDamage(sum(pb.allies$af_post_battle), drm.allies, battle.results[i, ]$dr_allies)
        
        forces.allies.post.battle <- pb.allies %>%
          inner_join(unit.data, "id") %>%
          filter(!eliminated)
        
        pb.japan <- applyDamage(forces.japan, 
                                 damage,
                                 critical = (battle.results[i, ]$dr_allies == 9), 
                                 max_damage = (reaction.team == "Japan"), 
                                 enemy_air_unit_count = getAirUnitCount(forces.allies.post.battle, remote_only = FALSE))
      }
    }
    
    if (intel.condition == "Ambush")
    {
      if (reaction.team != "Japan") {
        
        damage <- calcuateAirNavalDamage(sum(pb.japan$af_post_battle), drm.japan, battle.results[i, ]$dr_japan)
        
        forces.japan.post.battle <- pb.japan %>%
          inner_join(unit.data, "id") %>%
          filter(!eliminated)
        
        pb.allies <- applyDamage(forces.allies, 
                                 damage,
                                 critical = (battle.results[i, ]$dr_japan == 9), 
                                 max_damage = (reaction.team == "Japan"), 
                                 enemy_air_unit_count = getAirUnitCount(forces.japan.post.battle, remote_only = FALSE))
      } else {
        
        damage <- calcuateAirNavalDamage(sum(pb.allies$af_post_battle), drm.allies, battle.results[i, ]$dr_allies)
        
        forces.allies.post.battle <- pb.allies %>%
          inner_join(unit.data, "id") %>%
          filter(!eliminated)
        
        pb.japan <- applyDamage(forces.japan, 
                                 damage,
                                 critical = (battle.results[i, ]$dr_allies == 9), 
                                 max_damage = (reaction.team == "Japan"), 
                                enemy_air_unit_count = getAirUnitCount(forces.allies.post.battle, remote_only = FALSE))
      }
    }
    
    result[i, ]$allies_post_battle_af <- sum(pb.allies$af_post_battle)
    result[i, ]$japan_post_battle_af <- sum(pb.japan$af_post_battle)
    result[i, ]$battle_winner <- if_else(result[i, ]$allies_post_battle_af > result[i, ]$japan_post_battle_af,
                                          "Allies", "Japan")
    
    pb.allies <- pb.allies %>%
      mutate(dr_allies = battle.results[i, ]$dr_allies,
             dr_japan = battle.results[i, ]$dr_japan)
    
    pb.japan <- pb.japan %>%
      mutate(dr_allies = battle.results[i, ]$dr_allies,
             dr_japan = battle.results[i, ]$dr_japan)
    
    battle.losses <- battle.losses %>%
      bind_rows(pb.allies, pb.japan)
  }
  
  list(battle.results = result, battle.losses = battle.losses)
}

applyDamage <- function(forces, damage, critical = FALSE, max_damage = FALSE, enemy_air_unit_count = 0)
{
  damage.remaining <- damage
  
  forces.all <- forces %>%
    mutate(flipped = FALSE,
           eliminated = FALSE)

  # Apply hits regardless of being flipped if critical
  if (critical)
  {
    forces.critical <- forces.all %>%
      mutate(loss_delta = if_else(is_flipped, af_back, af_front - af_back)) %>%
      arrange(desc(loss_delta), defense)

    for (i in 1:nrow(forces.critical))
    {
      if (isIneligibleForDamage(forces.critical, unit.record = forces.critical[i, ], remote.air.unit.limit = enemy_air_unit_count))
        next()
      
      if (damage.remaining >= forces.critical[i, ]$defense)
      {
        damage.remaining <- damage.remaining - forces.critical[i, ]$defense
        
        if (forces.critical[i, ]$is_flipped) {
          forces.critical[i, ]$eliminated <- TRUE
        } else {
          forces.critical[i, ]$flipped <- TRUE
        }
      }
    }
    
    forces.all <- updateForcesDamaged(forces.all, forces.critical) 
    
  }
  
  # Apply hits to full strength units
  forces.full <- forces.all %>%
    filter(!is_flipped,
           !eliminated) %>%
    arrange(desc(af_front), defense)
  
  for (i in 1:nrow(forces.full))
  {
    if (nrow(forces.full) == 0)
      break()

    if (isIneligibleForDamage(forces.full, unit.record = forces.full[i, ], remote.air.unit.limit = enemy_air_unit_count))
      next()
    
    if (damage.remaining >= forces.full[i, ]$defense)
    {
      damage.remaining <- damage.remaining - forces.full[i, ]$defense
      forces.full[i, ]$flipped <- TRUE
    }
  }
  
  forces.all <- updateForcesDamaged(forces.all, forces.full)
  
  # Apply hits to flipped units
  if (nrow(forces.all) == sum(forces.all$is_flipped) | critical)
  {
    forces.flipped <- forces.all %>%
      filter(is_flipped,
             !eliminated) %>%
      arrange(desc(af_back), defense)
    
    for (i in 1:nrow(forces.flipped))
    {
      if (nrow(forces.flipped) == 0)
        break()
 
      if (isIneligibleForDamage(forces.flipped, unit.record = forces.flipped[i, ], remote.air.unit.limit = enemy_air_unit_count))
        next()
      
      if (damage.remaining >= forces.flipped[i, ]$defense)
      {
        damage.remaining <- damage.remaining - forces.flipped[i, ]$defense
        forces.flipped[i, ]$eliminated <- TRUE
      }
    }
    
    forces.all <- updateForcesDamaged(forces.all, forces.flipped) 
  }
  
  # Apply a step loss if critical hit, but no units damaged yet
  if (critical & nrow(filter(forces.all, (flipped | eliminated))) == 0)
  {
    # Apply a hit to the unit with lowest defense
    forces.critical <- forces.critical %>%
      mutate(max_damage = max_damage,
             af_sort = if_else(max_damage, loss_delta, (loss_delta * -1))) %>%
      filter(defense == min(defense)) %>%
      arrange(af_sort)
    
    for (i in 1:nrow(forces.critical))
    {
      if (isIneligibleForDamage(forces.critical, unit.record = forces.critical[i, ], remote.air.unit.limit = enemy_air_unit_count))
        next()

      if (forces.critical[1, ]$is_flipped) {
        forces.critical[1, ]$eliminated <- TRUE
      } else {
        forces.critical[1, ]$flipped <- TRUE
      }
      
      break()
    }

    forces.all <- updateForcesDamaged(forces.all, forces.critical) 
    
  }
  
  result <- forces.all %>%
    group_by(id) %>%
    filter(row_number(desc(eliminated)) == 1) %>%
    mutate(af_post_battle = case_when(eliminated ~ 0, 
                                      is_flipped ~ af_back,
                                      TRUE ~ af_front)) %>%
    select(id, flipped, eliminated, af_post_battle)
}


applyDamage_noncritial <- function(forces, damage, enemy_air_unit_count = 0)
{
  damage.remaining <- damage
  
  forces.all <- forces %>%
    mutate(flipped = FALSE,
           eliminated = FALSE)
  
  forces.full.strength <- forces.all %>%
    filter(!is_flipped)
  
  total.defense <- sum(forces.full.strength$defense)
  mininum.defense <- ifelse(total.defense == 0, 0, min(forces.full.strength$defense))
  
  # If it's not possible to damage ANY full-strength unit, then just bail out now
  if (mininum.defense > 0 && damage.remaining < mininum.defense)
  {
    # return results
  }
  
  # If it's not possible to flip all full-strength units, then find the 
  # best ones that CAN be flipped and then return
  if (damage.remaining < total.defense)
  {
    # permute
    # return results
  }
  
  # Try to apply damage to each full-strength unit. Air unit limitations may prevent
  # flipping all units.
  if (total.defense > 0 && damage.remaining >= total.defense)
  {
    for (i in 1:nrow(forces.full.strength))
    {
      if (isIneligibleForDamage(forces.full.strength, 
                                unit.record = forces.full.strength[i, ], 
                                remote.air.unit.limit = enemy_air_unit_count))
        next()
      
      if (damage.remaining >= forces.full.strength[i, ]$defense)
      {
        damage.remaining <- damage.remaining - forces.full.strength[i, ]$defense
        forces.full.strength[i, ]$flipped <- TRUE
      }
    }
    
    forces.all <- updateForcesDamaged(forces.all, forces.full.strength) 
    
    # If it was not possible to flip all full-strength units, then no further
    # damage is possible, so return the results
    if (sum(forces.full.strength$is_flipped) != nrow(forces.full.strength))
    {
      # return results
    }
  }
  
  # If we made it this far, then it should mean that all full-strength units
  # have been flipped, or there were no full-strength units to begin with.
  # Either way, we can now try to apply damage to flipped units
  
  if (sum(!forces.all$is_flipped) > 0)
  {
    # Something went wrong. All units should be flipped by now.
  }
  
  # Permute through flipped units
  
}

permuteUnits <- function(forces.test, damage, enemy_air_unit_count = 0)
{
  result <- data.frame()
  
  permutations <- permn(forces.test$id)
  
  for (i in 1:length(permutations))
  {
    forces.permute <- forces.test
    damage.remaining <- damage
    damage.applied <- 0

    for (unit.id in permutations[[i, ]])
    {
      unit.current <- forces.permute %>% filter(id == unit.id)
      
      if (isIneligibleForDamage(forces.permute, 
                                unit.record = unit.current, 
                                remote.air.unit.limit = enemy_air_unit_count))
        next()
      
      if (damage.remaining >= unit.current[1]$defense)
      {
        if ( forces.permute %>% filter(id == unit.id)$is_flipped) {
          forces.permute <- forces.permute %>%
            filter(id == unit.id) %>%
            mutate(eliminated = TRUE)
        } else {
          forces.permute <- forces.permute %>%
            filter(id == unit.id) %>% 
            mutate(flipped = TRUE)
        }
        
        damage.remaining <- damage.remaining - unit.current[1, defense]
        damage.applied <- damage.applied + unit.current[1, defense]

      }
      
    }
    
    forces.permute <- forces.permute %>%
      mutate(af_post_battle = case_when(eliminated ~ 0, 
                                        flipped ~ af_back,
                                        TRUE ~ af_front))
    
    result <- result %>%
      bind_rows(data.frame(index = i, damage_applied = damage.applied, af_post_battle = sum(forces.permute$af_post_battle)))
  }
  
  result <- result %>%
    arrange(desc(damage_applied), af_post_battle, index)
}

updateForcesDamaged <- function(forces.all, forces.damaged)
{
  result <- forces.all %>%
    mutate(is_flipped = if_else(id %in% filter(forces.damaged, flipped)$id, TRUE, is_flipped),
           flipped = if_else(id %in% filter(forces.damaged, flipped)$id, TRUE, flipped),
           eliminated = if_else(id %in% filter(forces.damaged, eliminated)$id, TRUE, eliminated)) 
}

getRemoteAirUnitCount <- function(forces.all, damaged_only = FALSE)
{
  air.unit.count <- forces.all %>%
    filter(range > 0, !is_in_battle_hex)
  
  if (damaged_only)
  {
    air.unit.count <- air.unit.count %>%
      filter(flipped | eliminated) 
  }
  
  air.unit.count <- air.unit.count %>%
    summarise(cnt = n(), .groups = "drop") 
  
  air.unit.count <- sum(air.unit.count$cnt)
  
}

getAirUnitCount <- function(forces.all, remote_only = FALSE)
{
  air.unit.count <- forces.all %>%
    filter(range > 0)
  
  if (remote_only)
  {
    air.unit.count <- air.unit.count %>%
      filter(!is_in_battle_hex) 
  }
  
  air.unit.count <- air.unit.count %>%
    summarise(cnt = n(), .groups = "drop") 
  
  air.unit.count <- sum(air.unit.count$cnt)
  
}

isIneligibleForDamage <- function(forces.all, unit.record, remote.air.unit.limit)
{
  if (nrow(filter(unit.record, range > 0, !is_in_battle_hex)) > 0)
  {
    damaged.remote.aircraft.count <- getRemoteAirUnitCount(forces.all, damaged_only = TRUE)
    
    return(damaged.remote.aircraft.count >= remote.air.unit.limit) 
    
  } else {
    return(FALSE)
  }
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
    summarise(win_probability = n(), .groups = "drop") %>%
    mutate(win_probability = win_probability/100)
  
  if (nrow(filter(result, battle_winner == "Allies")) == 0)
    bind_rows(data.frame(battle_winner = "Allies", win_probability = 0))
  
  if (nrow(filter(result, battle_winner == "Japan")) == 0)
    bind_rows(data.frame(battle_winner = "Japan", win_probability = 0))
  
  result
}

prepDataExpectedBattleDamageInflicted <- function(battle.results)
{
  result.japan <- battle.results %>%
    group_by(damage_japan) %>%
    summarise(damage_probability = n(), .groups = "drop") %>%
    mutate(damage_probability = damage_probability/100,
           team_name = "Japan") %>%
    rename(damage_inflicted = damage_japan)
  
  result.allies <- battle.results %>%
    group_by(damage_allies) %>%
    summarise(damage_probability = n(), .groups = "drop") %>%
    mutate(damage_probability = damage_probability/100,
           team_name = "Allies") %>%
    rename(damage_inflicted = damage_allies)
  
  list(result.allies = result.allies, result.japan = result.japan)
}

prepDataExpectedBattleDamageTaken <- function(battle.losses, unit.data, exclude.critical = FALSE)
{
  result <- battle.losses
  
  if (exclude.critical)
  {
    result <- result %>%
      filter(dr_allies != 9, dr_japan != 9)
  }

  result <- result %>% 
    inner_join(unit.data, "id") %>%
    mutate(damage_taken = (flipped * defense + eliminated * defense)) %>%
    group_by(team, dr_allies, dr_japan) %>%
    summarise(cnt = n(),
              damage_taken = sum(damage_taken), .groups = "drop") %>%
    group_by(team, damage_taken) %>%
    summarise(cnt = n(), .groups = "drop") %>%
    mutate(damage_probability = cnt/100)
  
}

prepDataExpectedBattleDamageTaken_ByUnit <- function(battle.losses, unit.data)
{
  result <- battle.losses %>% 
    inner_join(unit.data, "id") %>%
    mutate(damage_taken = (flipped * defense + eliminated * defense)) %>%
    group_by(id, team, unit_name, damage_taken) %>%
    summarise(cnt = n(), .groups = "drop") %>%
    mutate(damage_probability = cnt/100)
  
}

prepDataBattleResultsGrid <- function(battle.results)
{
  result <- battle.results %>%
    select(`Allied DR` = dr_allies,
           `Japan DR` = dr_japan,
           `Allied Damage Inflicted` = damage_allies,
           `Japan Damage Inflicted` = damage_japan,
           `Allied Post-Battle AF` = allies_post_battle_af,
           `Japan Post-Battle AF` = japan_post_battle_af,
           `Battle Winner`  = battle_winner)
}

testBattleAnalysis <- function()
{
  forces.allies <- fread("output/forces_allies.csv")
  forces.japan <- fread("output/forces_japan.csv")
  battle.results <- fread("output/battle_results.csv")
  battle.losses <- fread("output/battle_losses.csv")
  
  reaction.team <- "Japan"
  intel.condition <- "Intercept"
  drm.allies <- 0
  drm.japan <- 0
  
  result <- analyzeAirNavalBattle(forces.allies = forces.allies, 
                                  forces.japan = forces.japan, 
                                  reaction.team = reaction.team, 
                                  intel.condition = intel.condition, 
                                  drm.allies = drm.allies, 
                                  drm.japan = drm.allies)
}

