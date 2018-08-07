################################################################################
# Functions for generating time till non-AAA death.
# There are three sets of functions, one for each model, plus one function, 
# generateTimeTillNonAaaDeath, that is used whichever model is used.
# 
# In all three models, the idea is that you run one function once, to get 
# the rates or whatever from a file, and then another function lots of times, 
# to generate individual death times.
# 
# See also nonAaaDeathMethod in processPersons.

################################################################################
# A single function that is used whatever model for non-AAA death is used.

generateTimeTillNonAaaDeath <- function(v0, v1other) {
	if (v1other$nonAaaDeathMethod == "mass") {
		return(generateTimeTillNonAaaDeathFromSurvProbs(
				v1other$nonAaaSurvProbs, 0.25))
		
	} else if (v1other$nonAaaDeathMethod == "onsIntegerStart") { 
		return(generateTimeTillNonAaaDeathFromSurvProbs(
				v1other$nonAaaSurvProbs, 1))
		
	} else if (v1other$nonAaaDeathMethod == "onsNonintegerStart") {
		return(generateTimeTillNonAaaDeathWithNonIntegerStartAge(
				v1other$nonAaaMortalityRates, v0$generateAgeAtBaseline()))
		
	} else {
		stop("v1other$nonAaaDeathMethod=", v1other$nonAaaDeathMethod, 
				" is illegal")
	}
}

################################################################################
# Use three-monthly survival probabilities from MASS.
# 
# This corresponds to v1other$nonAaaDeathMethod == "mass". 
# The idea is that you do getMassSurvivalProbabilities() once (or once per 
# run of processPersons) and then generateTimeTillNonAaaDeathFromSurvProbs 
# lots of times. 

# getMassSurvivalProbabilities is designed to be used on one specific file of 
# numbers from MASS. 
# generateTimeTillNonAaaDeathFromSurvProbs is designed to work on any vector 
# of survival probabilities, whether the periodLength is 0.25 or 1 (it would 
# probably work with other periodLength values but this has not been checked).
# It generates the integer part of the time using findInterval and a single 
# runif(1). 

# Get the survival probabilities from the CSV file and return them as a vector. 
# This function only needs to be run once.
getMassSurvivalProbabilities <- function(fileName=file.path("input", 
		"MASS - 10yr - non AAA deaths_seeEmail20150721.csv")) {
	fileContents <- read.csv(fileName, comment.char="#")
	names(fileContents) <- c("cycle", "followUpAtLeast", 
			"survProbAdjustedForCensAndAaa", "probDeathInThisCycle")
	survProbs <- fileContents$survProbAdjustedForCensAndAaa
	survProbs[!is.na(survProbs)]
}

# Generate a non-AAA death time from a vector of survival probabilities. 
# This can be used on MASS numbers or other numbers. 
generateTimeTillNonAaaDeathFromSurvProbs <- 
		function(survivalProbs, periodLength) {
	# Check that the arguments are legal. 
	if (!is.vector(survivalProbs) || !is.numeric(survivalProbs) || 
			!identical(survivalProbs, sort(survivalProbs, decreasing=TRUE)) ||
			any(survivalProbs < 0) || any(survivalProbs > 1))
		stop("survivalProbs must be a decreasing vector of probabilities")
	if (!identical(periodLength, 0.25) && !identical(periodLength, 1))
		stop("periodLength should almost certainly be either 0.25 or 1")
	
	# Calculate how many periods they survive. 
	# If runif(1) is greater than survivalProbs[1], they survive 0 periods. 
	# If it is between survivalProbs[1] and [2], they survive 1 period. Etc. 
	numberOfperiodsSurvived <- 
			length(survivalProbs) - findInterval(runif(1), sort(survivalProbs))
	
	# Calculate how long they survive. The runif in the following line is 
	# because they die part-way through the period.
	numberOfperiodsSurvived * periodLength + runif(n=1, min=0, max=periodLength)
	
	# If the first runif(1) says they are beyond the range of survivalProbs, 
	# then they die within one year of the end of that range. This is equivalent 
	# to there being one extra survival probability on the end, which is zero.  
}

################################################################################
# Functions for use on ONS-style data, with start-age being different for 
# different people, and not necessarily integer. (See also notes below.)
# 
# This corresponds to v1other$nonAaaDeathMethod == "onsNonintegerStart". 
# Warning: this has not been used or looked at for a long time and should be 
# treated with caution. Hopefully it will not be used.  

# Given a file that is probably in a similar format to what you get from ONS, 
# make a data-frame of mortality rates in which the row-names are the ages. 
readMortalityRatesFromFile <- function(fileName) {
	mortalityRates <- read.csv(fileName, header=FALSE, blank.lines.skip=TRUE, 
			comment.char="#")
	if (ncol(mortalityRates) < 2) 
		stop("the file must be a CSV file with age in the first column and ",
				"mortality rate in the second")
	names(mortalityRates)[1:2] <- c("age", "mortalityRate")
	if (any(mortalityRates$mortalityRate > 1))
		stop("the mortality rates must be per 1, not per 100,000")
	ageColumn <- mortalityRates$age
	if (ageColumn[1] != floor(ageColumn[1]) ||
			!identical(ageColumn, seq(min(ageColumn), max(ageColumn))))
		stop("mortalityRates$age must be of the form x x+1 x+2 ..., ",
				"where x is an integer")
	return(mortalityRates)
}

# Given a table of mortality rates (whose first two columns are age and 
# mortalityRate) and an initial age, generate time till death. 
generateTimeTillNonAaaDeathWithNonIntegerStartAge <- 
		function(mortalityRates, startAge, verbose=FALSE) {
	# (Main checking of mortalityRates is done in readMortalityRatesFromFile.) 
	if (!is.data.frame(mortalityRates) || 
			!identical(names(mortalityRates)[1:2], c("age","mortalityRate")))
		stop("mortalityRates must be a data-frame whose first two columns are ",
				"age and mortalityRate")
	if (startAge < min(mortalityRates$age) || 
			startAge >= max(mortalityRates$age) + 1)
		stop("startAge is outside the range that appears in mortalityRates")
	
	# Find the first row to use.  
	integerStartAge <- floor(startAge)
	rowNumber <- match(integerStartAge, mortalityRates$age)
	
	# Deal with the first part-year, if startAge is not an integer. 
	if (startAge > integerStartAge) { 
		mortalityRate <- mortalityRates$mortalityRate[rowNumber]
		proportionOfYearLeft <- integerStartAge + 1 - startAge
		if (verbose) cat("probability of dying in first part-year:", 
				mortalityRate * proportionOfYearLeft, "\n")
		if (runif(1) < mortalityRate * proportionOfYearLeft)
			return(runif(n=1, min=0, max=proportionOfYearLeft))
		rowNumber <- rowNumber + 1
	}
	
	# Deal with the subsequent years. 
	repeat {
		# If they have survived all the rows, they die during the next year:
		if (rowNumber > nrow(mortalityRates))
			return(max(mortalityRates$age) + 1 + runif(1) - startAge)
		# Otherwise, they may or may not die in this one-year age-group:
		if (verbose) cat("probability of dying in age-group ", 
				mortalityRates$age[rowNumber], ": ", 
				mortalityRates$mortalityRate[rowNumber], "\n", sep="")
		if (runif(1) < mortalityRates$mortalityRate[rowNumber])
			return(mortalityRates$age[rowNumber] + runif(1) - startAge)
		rowNumber <- rowNumber + 1
	}
}

# readMortalityRatesFromFile is a separate function because reading a file is 
# time-consuming and should only be done once. Most of it is just checking. 
# 
# One downside of this pair of functions is that it takes 50 seconds to do 
# generateTimeTillNonAaaDeathWithNonIntegerStartAge 100,000 times, with 11 
# relevant rows in mortalityRates. This is because it generates lots of 
# runif(1)s and checks them against all the mortality rates until it "fails" 
# one of them and the person dies. 
# 
# This could be solved by making the first function (which is only run once) 
# calculate the survival-probabilities starting at each integer age, and 
# returning a data-frame that is basically a lower-triangular matrix; then the 
# second function (which is run many times) would have to generate one runif(1) 
# to deal with the first part-year and, if the person survives that, one 
# runif(1) to deal with the rest of time, with all those survival-probabilities 
# multiplied by some factor if necessary to deal with the conditional 
# probability of surviving to X given that you survived the first part-year. 
# 
# (There is also the extra runif(1) that is added on just before the time is 
# returned, which corresponds to the mortality rates being the chance of 
# death in a one-year period and the assumption that deaths are uniformly 
# distributed in that period.)

################################################################################
# A newer and simpler function for use with ONS-style data that has had AAA 
# deaths "subtracted out". This assumes that everyone starts at the same age 
# and that age is an integer.  
# 
# This corresponds to v1other$nonAaaDeathMethod == "onsIntegerStart".
# The idea is that you do convertMortalityRatesToSurvProbs once (or once per 
# processPersons) and then generateTimeTillNonAaaDeathFromSurvProbs lots of 
# times.

# This function is intended to be run once, with a file of the same form as 
# "nonAaaDeathMortalityRates_seeEmail20150602.csv". It takes a file of mortality
# rates and processes them into the single vector of survival probabilities that
# is required by generateTimeTillNonAaaDeathFromSurvProbs (which is also used 
# when using MASS non-AAA death rates). In the file, the mortality rate in the 
# row with age=65 needs to be the prob. that a person aged 65 dies before 66. 

convertMortalityRatesToSurvProbs <- function(startAge, fileName) {
	# Get the mortality rates. (These are probabilities of death, not for 
	# example rates of an exponential distribution or anything like that.)
	mortalityRates <- read.csv(fileName, header=FALSE, row.names=1, 
			blank.lines.skip=TRUE, comment.char="#")
	if (ncol(mortalityRates) != 1) 
		stop("the file must be a two-column CSV file with age in the first ",
				"column and mortality rate in the second")
	if (any(mortalityRates > 1))
		stop("the mortality rates must be per 1, not per 100,000")
	
	# Work out the first survival probability. 
	firstRow <- match(startAge, rownames(mortalityRates))
	if (is.na(firstRow)) 
		stop("startAge=", startAge, " was not found in ", fileName)
	survivalProbs <- 1 - mortalityRates[firstRow, 1]
	
	# Work out the remaining survival probabilities, and return them. 
	if (firstRow==nrow(mortalityRates)) return(survivalProbs)
	for (i in (firstRow + 1):nrow(mortalityRates)) {
		nextSurvivalProb <- survivalProbs[length(survivalProbs)] * 
				(1 - mortalityRates[i, 1])
		survivalProbs <- c(survivalProbs, nextSurvivalProb)
	}
	return(survivalProbs)
}

################################################################################



