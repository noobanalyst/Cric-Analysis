matchData <- function(a)
{
library(XML)
        
matchTable <- a

matchTree <- lapply(as.character(levels(matchTable$MatchURL)[matchTable$MatchURL]),htmlTreeParse, useInternalNodes = TRUE)

# Finding who won the toss
tossList <- lapply(matchTree, xpathSApply, "//*[text()[contains(.,'Toss')]]", xmlValue)

# Player of the match
MOM <- lapply(matchTree, xpathSApply, "//*[text()[contains(.,'Player of the match')]]", xmlValue)

# To extract captain name, search using grep function by passing it "*"

matchList <- lapply(matchTree,readHTMLTable)

# Removing unnecessary elements from the list
matchList <- lapply(seq(matchList), function(x) {
                y <- matchList[[x]]
                y <- lapply(1:4, function(z) y[[z]])
                return(y)
                })

# Removing the NA values from the list
matchList <- lapply(seq(matchList), function(x)
        { y <- matchList[[x]]
          lapply(seq(y), function(z){
                  a <- grep("Total", y[[z]][[2]], fixed = TRUE)
                  if(length(a)==1)
                  {
                    y[[z]][a,][,is.na(y[[z]][a,])] <- 
                         rep("", length(y[[z]][a,][,is.na(y[[z]][a,])]))
                    na.omit(y[[z]])
                  }
                  else na.omit(y[[z]])
          })})

# Renaming the columns of the match list. Need to find a solution for this
columnList <- lapply(matchTree, xpathSApply, "//tr[@class='tr-heading']/th", xmlValue)

# Removing unecessary columns and rownames from the list
matchList <- lapply(seq(matchList), function(x) 
        { y <- matchList[[x]]
          y <- lapply(seq(y), function(z) {
                  rownames(y[[z]]) <- NULL
                  y[[z]][[1]] <- NULL
                  return(y[[z]])
          })
          return(y)
        })

# Renaming the columns in the match list
battingNames <- c("Batsman", "Dismissal", "Runs", "Min", "Balls", "4s", "6s", "SR")
bowlingNames <- c("Bowling", "O", "M", "R", "W", "Econ", "0s", "4s", "6s", "Extras")

matchList <- lapply(seq(matchList), function(x){
        y <- matchList[[x]]
        if(x%%2==0) colnames(y) <- bowlingNames
        else colnames(y) <- battingNames
        return(y)
})

# Naming the list elements based on the match number
names(matchList) <- as.character(levels(matchTable$Scorecard)[matchTable$Scorecard])


}
