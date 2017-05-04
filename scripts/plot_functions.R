plotExpectedBattleWins <- function(data.all)
{
  plot.data <- data.all
  
  p <- ggplot(plot.data, aes(x = battle_winner, y = win_probability))
  p <- p + geom_bar(stat = "identity")
  p <- p + xlab("Player Team")
  p <- p + ylab("Probability of Winning Battle")
  p <- p + labs(fill = "")
  p <- p + scale_y_continuous(labels = percent)
  p <- p + ggtitle(paste("Battle Victory Analysis"))
  p <- p + geom_text(aes(label=percent(win_probability), y = win_probability), vjust=-.5, colour="black", size=4)
  p <- p + scale_fill_brewer(palette = "Set1")
  p <- p + theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1), legend.position = "top")

  p
  
}

plotExpectedBattleDamageInflicted <- function(data.all)
{
  plot.data <- data.all %>%
    mutate(damage_inflicted = factor(damage_inflicted))
  
  team.name <- data.all[1, team_name]
  
  p <- ggplot(plot.data, aes(x = damage_inflicted, y = damage_probability))
  p <- p + geom_bar(stat = "identity")
  p <- p + xlab("Inflicted Damage")
  p <- p + ylab("Probability")
  p <- p + labs(fill = "")
  p <- p + scale_y_continuous(labels = percent)
  p <- p + ggtitle(paste("Expected Damage Inflicted By", team.name))
  p <- p + geom_text(aes(label=percent(damage_probability), y = damage_probability), vjust=-.5, colour="black", size=4)
  p <- p + scale_fill_brewer(palette = "Set1")
  p <- p + theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1), legend.position = "top")
  
  p
  
}

plotExpectedBattleDamageTaken <- function(data.all, team.name)
{
  plot.data <- data.all %>%
    mutate(damage_taken = factor(damage_taken)) %>%
    filter(team == team.name)

  p <- ggplot(plot.data, aes(x = damage_taken, y = damage_probability))
  p <- p + geom_bar(stat = "identity")
  p <- p + xlab("Damage Taken")
  p <- p + ylab("Probability")
  p <- p + labs(fill = "")
  p <- p + scale_y_continuous(labels = percent)
  p <- p + ggtitle(paste("Expected Damage Taken By", team.name))
  p <- p + geom_text(aes(label=percent(damage_probability), y = damage_probability), vjust=-.5, colour="black", size=4)
  p <- p + scale_fill_brewer(palette = "Set1")
  p <- p + theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1), legend.position = "top")
  
  p
  
}

plotExpectedBattleDamageTaken_ByUnit <- function(data.all)
{
  plot.data <- data.all %>%
    data.table() %>%
    mutate(unit_name = as.factor(unit_name),
           sort_val = if_else(team == "Allies", 0, 1),
           unit_name = reorder(unit_name, sort_val, FUN = sum))
  
  p <- ggplot(plot.data, aes(x = damage_taken, y = unit_name, colour = team))
  p <- p + geom_point(size = 3)
  p <- p + xlab("Damage Taken")
  p <- p + ylab("")
  p <- p + labs(colour = "")
  p <- p + geom_text(aes(label = percent(damage_probability)), vjust = 1.5, hjust = .5, colour = "black", size = 4)
  p <- p + geom_text(aes(label = damage_taken), vjust = -.75, hjust = .5, colour = "blue", size = 4)
  p <- p + scale_fill_brewer(palette = "Set1")
  p <- p + theme(panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.y = element_line(colour = "grey60", linetype = "dashed"))
  
  p
}


