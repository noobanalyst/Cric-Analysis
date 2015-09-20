library(ggplot2)
library(dplyr)

# Creating a function to simplify reordering the factor variables present in the data frame, in descending order
rearrange <- function(x) {reorder(x,x,function(y)-length(y))}

# Table showcasing number of matches won by each team. 
winningTeams <- addmargins(xtabs(~ rearrange(matchTable$Winner), matchTable))
names(dimnames(winningTeams)) <- "Win by Teams"
winningTeams

# Same stat as a percentage. It can also be achieved by rowPerc function from tigerstats package
teamsPerc <- prop.table(xtabs(~ rearrange(matchTable$Winner), matchTable))*100
names(dimnames(teamsPerc)) <- "Win by Teams"
teamsPerc

#Splitting the number of matches won by match type
winningType <- addmargins(xtabs(~ rearrange(matchTable$MatchType) + rearrange(matchTable$Winner), matchTable))
names(dimnames(winningType)) <- c("MatchType","Team")
winningType

#Creating a list of opponent wise distribution of matches won
temp <- sort(levels(matchTable$`Team 1`))
Opponent <- temp[temp!="India"]
matchTableOpp <- lapply(seq(Opponent), function(x) filter(matchTable, (matchTable$`Team 1`==Opponent[x] | matchTable$`Team 2`==Opponent[x])))
names(matchTableOpp) <- Opponent

# Creating table for number of matches won by matches type and opponent
winTeam <- lapply(seq(Opponent), function(x) addmargins(xtabs(~ matchTableOpp[[Opponent[x]]]$MatchType + matchTableOpp[[Opponent[x]]]$Winner, matchTableOpp[[Opponent[x]]], drop.unused.levels = TRUE)))
names(winTeam) <- Opponent
for ( i in seq(Opponent))
{
        names(dimnames(winTeam[[Opponent[i]]])) <- c("MatchType", "Results")
}
winTeam

# The same stat's as percentage
winTeamPerc <- lapply(seq(Opponent), function(x) prop.table(xtabs(~ matchTableOpp[[Opponent[x]]]$MatchType + matchTableOpp[[Opponent[x]]]$Winner, matchTableOpp[[Opponent[x]]], drop.unused.levels = TRUE),margin = 1)*100)
names(winTeamPerc) <- Opponent
for ( i in seq(Opponent))
{
        names(dimnames(winTeamPerc[[Opponent[i]]])) <- c("MatchType", "Results")
}
winTeamPerc

# Histogram to showcase number of matches won by India and other teams in descending order
matchHist <- ggplot(matchTable, aes(x=rearrange(matchTable$Winner)))+geom_histogram(fill="red")+labs(x="Country", y="Number of Matches")+stat_bin(geom = "text", aes(label=..count..), vjust=-0.5)

