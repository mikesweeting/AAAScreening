################################################################################
# generateReinterventionTime.R

# Example (using different units from usual): 
# surgeryTime = 1000
# currentTime = 1097
# 
# Say postSurgeryInitialPeriod is 30 and the periods with different 
# exponential-distribution rates are "31-90 days after surgery", 
# "91-150 days after surgery", "151-300 days after surgery", and 
# "301+ days after surgery"). This means:
# postSurgeryInitialPeriod = 30
# timeBoundaries = c(90, 150, 300)   # (actually that divided by 365.25)
# 
# rates = c(0.8, 0.7, 0.6, 0.5)

# If there is just one rate then there is no need to pass timeBoundaries. 

generateReinterventionTime <- function(rates, 
		timeBoundaries=numeric(), surgeryTime, currentTime, 
		postSurgeryInitialPeriod, verbose=FALSE) {
	
	# Check rates, timeBoundaries, surgeryTime, and currentTime.
	checkReinterventionRatesAndTimeBoundaries(rates, timeBoundaries)
	checkIsSingleNumeric(surgeryTime)
	checkIsSingleNumeric(currentTime)
	
	# If all rates are zero, return NA. 
	# This means no reintervention will be scheduled. 
	if (all(rates == 0)) return(NA)
	
	# If currentTime is before the first possible time of reintervention, 
	# then replace it with that time. 
	if (currentTime < surgeryTime + postSurgeryInitialPeriod)
		currentTime <- surgeryTime + postSurgeryInitialPeriod
	
	# Find what period currentTime is in.
	periodNumber <- findInterval(currentTime, surgeryTime + timeBoundaries) + 1
	# Example: periodNumber = 2.
	
	# Create timeBoundariesForGenerating. This will be a vector of actual times, 
	# like in the model. It will consist of currentTime, then all the actual 
	# time-boundaries that are after that, and finally infinity.
	if (periodNumber <= length(timeBoundaries)) {
		# Two or more periods will need to be used.
		timeBoundariesForGenerating <- surgeryTime + 
				timeBoundaries[periodNumber:length(timeBoundaries)] 
		timeBoundariesForGenerating <- timeBoundariesForGenerating[
				timeBoundariesForGenerating > currentTime]
		timeBoundariesForGenerating <- 
				c(currentTime, timeBoundariesForGenerating, Inf)
	} else {
		# Only one period will need to be used.
		timeBoundariesForGenerating<- c(currentTime, Inf)
	}
	# Example: timeBoundariesForGenerating = c(1097, 1150, 1300, Inf)
	
	# Create ratesToUse and numberOfPeriodsToUse. 
	ratesToUse <- rates[periodNumber:length(rates)]
	numberOfPeriodsToUse <- length(ratesToUse)
	# Example: ratesToUse = c(0.7, 0.6, 0.5)
	
	if (verbose) {
		cat("timeBoundariesForGenerating: ", timeBoundariesForGenerating, "\n")
		cat("ratesToUse: ", ratesToUse, "\n")
	}
	
	# Generate the time. Go through each period in turn, until you get a
	# result (a reintervention time) that falls within that period. 
	for (i in 1:numberOfPeriodsToUse) {
		# Deal with the possibility that the rate in this period is zero.
		if (ratesToUse[i] == 0) {
			if (i == numberOfPeriodsToUse) {
				if (verbose) cat("zero rate in final period; returning NA\n")
				return(NA)  # there is no reintervention-time
			} else {
				next  # there is no reintervention-time in this period
			}
		} 
		# TODO: actually checkReinterventionRatesAndTimeBoundaries insists that 
		# the rates are in decreasing order; so if the rate in one period is 
		# zero then maybe this should give up straight away and return NA.
		
		# Generate a time that might be usable as the next reintervention time.
		possibleResult <- 
				timeBoundariesForGenerating[i] + rexp(n=1, rate=ratesToUse[i])
		if (verbose) cat("i=", i, "  possibleResult (using rate=", 
					ratesToUse[i], "): ", possibleResult, " ... ", sep="")
		
		# If possibleResult is before the next time boundary, return it. 
		# Otherwise, continue to the next iteration of the for loop.
		if (possibleResult < timeBoundariesForGenerating[i + 1]) {
			if (verbose) cat("accepted!\n")
			return(possibleResult)
		} else {
			if (verbose) cat("rejected\n")
		}
	}
	# Alternatively, for the piecewise exponential distribution, use rpexp, 
	# in msm package. But this prohibits rate=0. 	
	
	stop("INTERNAL ERROR: you should never get to here")
}

################################################################################



