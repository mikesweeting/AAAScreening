# Do "source" on all .R files in the "functions" directory, 
# excluding this one and massDataFunctions.R (if that still exists).

dirName <- "functions"

allFiles <- list.files(path=dirName, pattern="\\.R$")

onesToExclude <- c("allFunctions.R", "massDataFunctions.R")

for (fileName in setdiff(allFiles, onesToExclude)) 
	source(file.path(dirName, fileName))

#library(methods, quietly=TRUE)  # was needed to make "is" work on Cardio

