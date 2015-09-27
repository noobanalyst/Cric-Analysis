cricData <- function(z)
{
library(XML)
library(xlsx)

# Creating a list of countries and their respective ID's to build CricInfo URL
countryList <- data.frame(Country=character(9), ID=numeric(9));
countryList$Country <- c("England", "Australia", "South Africa", "West Indies", "New Zealand","India", "Pakistan","Sri Lanka","Zimbabwe")
countryList$ID <- c(1:9)

ID <- paste("id=", countryList$ID[countryList$Country==z], sep = "")

yearURL <- paste("http://stats.espncricinfo.com/ci/engine/records/team/match_results_year.html?class=11",ID, "type=team", sep=";")

# Read table data from yearURL
yearTableParse <- htmlTreeParse(yearURL, useInternalNodes = TRUE)

# Read year values from the table
yearListParse <- xpathSApply(yearTableParse, "//a[contains(@href, '/match_results.html?')]", xmlValue)

# Convert the list to a numeric vector for convinience
yearList <- as(yearListParse, "numeric")

# Build URL's for the pages
urlVector <- character(length(yearList))
for (i in 1: length(yearList))
{
yearList[i] = paste("id=", yearList[i],sep="")

team <- paste("team=", countryList$ID[countryList$Country==z],sep = "")

urlVector[i]= paste("http://stats.espncricinfo.com/ci/engine/records/team/match_results.html?class=11", yearList[i], team, "type=year", sep=";")
}

# Since the records on the pages are found in the form of tables containing links which then redirects to the match result, we need to parse the html page before reading the table
yearHTML <- lapply(urlVector, htmlTreeParse, useInternalNodes = TRUE)

# Name the list elements 
names(yearHTML) <- yearListParse

#Read records table from the URLs created into a list
tempMatchList <- lapply(yearHTML, readHTMLTable)

# Removing unwanted elements from this list
matchList <- lapply(seq(tempMatchList), function(x) tempMatchList[[x]][[1]])

# Extracting URL for the specific match pages
matchURL <- lapply(yearHTML, xpathSApply, "//a[contains(@href, '/ci/engine/match/')]/@href")

# Removing element name "href"
matchURL <- lapply(seq(matchURL), function(x) unname(matchURL[[x]]))

# Removing duplicates from the list
matchURL <- lapply(seq(matchURL), function(x) unique(matchURL[[x]]))
matchList <- lapply(seq(matchList), function(x) unique(matchList[[x]]))

# Removing the first 4 unwanted URls from each nested list 
matchURL <- lapply(seq(matchURL), function(x) matchURL[[x]][-1:-4])

#Adding the text "http://stats.cricinfo.com" to complete the URL's
matchURL <- lapply(seq(matchURL), function(x) paste("http://stats.cricinfo.com", matchURL[[x]], sep = ""))

# Naming the list elements Year wise
names(matchList) <- yearListParse

# Adding the individual URL's, Year,Month and Match Type as a column to their respective matches in matchList
matchYear <- lapply(seq(matchList), function(x) rep(names(matchList[x]),nrow(matchList[[x]])))
matchMonth <- lapply(seq(matchList), function(x) substr(matchList[[x]][[6]],1,3))
matchType <- lapply(seq(matchList), function(x) substr(matchList[[x]][[7]],1,4))
matchList <- lapply(seq(matchURL), function(x){cbind(matchList[[x]], matchURL[[x]],matchYear[[x]], matchMonth[[x]], gsub(" ","",matchType[[x]]))})

# Renaming the matchURL and matchYear column in the list elements
matchLabel <- colnames(matchList[[1]])
matchLabel[8] <- "MatchURL"
matchLabel[9] <- "MatchYear"
matchLabel[10] <- "MatchMonth"
matchLabel[11] <- "MatchType"
matchList <- lapply(seq(matchList), function(x)
        { y <- matchList[[x]]
          colnames(y) <- matchLabel
          return(y)
        })

# Converting matchList to data frame for convinience
# tempDF1 <- data.frame()
for (i in 1:length(matchList))
{
  tempDF2 <- matchList[[i]]
  matchTable <- rbind(tempDF1,tempDF2)
  tempDF1 <- matchTable
}

# Removing duplicates from the data frame due to year end test matches being counted twice
for (i in 1:(nrow(matchTable)-1))
{
        if (matchTable$`Match Date`[i]==matchTable$`Match Date`[(i+1)])
                matchTable$MatchYear[i+1] <- matchTable$MatchYear[i]
}
matchTable <- unique(matchTable)
return(matchTable)
}

# Exporting the data frame to a XLSX file in the working directory
write.xlsx(matchTable, "./IndiaCricRecords.xlsx", row.names = FALSE)
