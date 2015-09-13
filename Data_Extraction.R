library(XML)
yearURL <- "http://stats.espncricinfo.com/ci/engine/records/team/match_results_year.html?class=11;id=6;type=team"

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

urlVector[i]= paste("http://stats.espncricinfo.com/ci/engine/records/team/match_results.html?class=11", yearList[i], "team=6", "type=year", sep=";")
}

# Since the records on the pages are found in the form of tables containing links which then redirects to the match result, we need to parse the html page before reading the table
yearHTML <- lapply(urlVector, htmlTreeParse, useInternalNodes = TRUE)

# Name the list elements 
names(yearHTML) <- yearListParse

#Read records table from the URLs created into a list
tempMatchList <- lapply(yearHTML, readHTMLTable)

# Removing unwanted elements from this list
matchList <- lapply(1:69, function(x) tempMatchList[[x]][[1]])

# Extracting URL for the specific match pages
matchURL <- lapply(yearHTML, xpathSApply, "//a[contains(@href, '/ci/engine/match/')]/@href")

# Removing element name "href"
tempMatchURL <- lapply(1:69, function(x) unname(matchURL[[x]]))

# Removing duplicates from the list
tempMatchURL2 <- lapply(1:69, function(x) unique(matchURL[[x]]))

# Removing the first 4 unwanted URls from each nested list 
macthURL <- lapply(1:69, function(x) tempMatchURL2[[x]][-1:-4])

# Adding the individual URL's to their respective matches in matchList
matchList <- lapply(1:69, function(x){cbind(matchList[[x]], matchURL[[x]])})

# Converting matchList to data frame for convinience
tempDF1 <- data.frame()
for (i in 1:length(matchList))
{
  tempDF2 <- matchTable[[i]]
  matchTable <- rbind(tempDF1,tempDF2)
  tempDF1 <- tempDF
}
