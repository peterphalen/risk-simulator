


playRisk <- function( attack.dice = 3, # num. of attack dice
                      defend.dice = 2, # num. of defend dice
                      rolls = 100 # num. of simulations
                      ){
  
  attacker.total.kills <- 0 #attacker kills across rolls
  total.casualties <- 0 # number of soldiers lost across rolls
  
  for ( roll in 1:rolls ) {
  
  # roll the dice
  a.rolls <- sample(1:6, size = attack.dice )
  d.rolls <- sample(1:6, size = defend.dice )
  
  # order the values from highest to lowest
  a.rolls <- a.rolls[order(a.rolls, decreasing = TRUE)]
  d.rolls <- d.rolls[order(d.rolls, decreasing = TRUE)]
  
  # number of comparisons needed to score
  # is same as the lowest num. of die between attacker
  # and defender
  comparisons <- min( attack.dice, defend.dice ) 
  total.casualties <- total.casualties + comparisons
  
  # number of kills for the attacker this roll
  attacker.kills.this.roll <- 0

  for ( i in comparisons ){
    
    attacker.kills.this.roll <- attacker.kills.this.roll + 
                                 ifelse(a.rolls[i] > d.rolls[i], 
                                        1, #if attacker roll > than defender roll, give attacker a kill 
                                        0) #attacker loses draws
  }
  
  # add result of this roll to the tally 
  attacker.total.kills <- attacker.total.kills + attacker.kills.this.roll

  }
  
  attacker.casualties <- total.casualties - attacker.total.kills

  
  # total attacker kills / total possible kills = % success for attacker
  percent.attacker.kills <- (attacker.total.kills / total.casualties)
  #format as percent
  percent.attacker.kills <-   paste0(formatC(100 * percent.attacker.kills, format = "f", digits = 2), "%")
  
  # kills:deaths ratio
  kill.death.ratio <- round( ( attacker.total.kills / attacker.casualties ), digits = 2)

  
  return (  data.frame(attacker.total.kills, attacker.casualties, percent.attacker.kills, kill.death.ratio)  )
  
}