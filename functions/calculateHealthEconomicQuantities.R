################################################################################
# Given an event-history, the costs, and the names of the required 
# quantities, calculate the required health-economic quantities.

# The purpose of passing namesOfQuantities to this function is to define 
# "result" with the appropriate names at the start and to check that the 
# function is calculating what is required by processOnePair.

calculateHealthEconomicQuantities <- function(
		eventHistory, namesOfQuantities, costs, v1other) {
	# The arguments to this function are rather messy - namesOfQuantities is in 
	# v0 and costs is in v2. The following things are used from v1other:
	# qalyFactorBoundaries, qalyFactors, lifeYearDiscountRate, costDiscountRate
	
	# Check some things. 
	if (!inherits(eventHistory, "eventHistory"))
		stop("eventHistory must be an object of class eventHistory")
	costsWithAttrsRemoved <- costs  # (vectors with attributes fail is.vector)
	attributes(costsWithAttrsRemoved) <- NULL
	if (!is.numeric(costs) || is.null(names(costs)) || 
			!is.vector(costsWithAttrsRemoved))
		stop("costs must be a numeric vector with names")
		
	# Create result. 
	result <- numeric()  # much faster than sapply(...), according to profvis
	# Previously: result <- sapply(namesOfQuantities, FUN=function(x) NA_real_)
	
	# Calculate lifeYears. 
	result["lifeYears"] <- eventHistory$times[length(eventHistory$times)]
	# The previous line calculates life-years from time zero to the last event, 
	# which is always death of some type or censoring. 
	
	# Calculate QALYs. 
	result["qalys"] <- adjustLifeYears(result["lifeYears"], 
			qalyFactorBoundaries=v1other$qalyFactorBoundaries, 
			qalyFactors=v1other$qalyFactors)
	
	# Calculate cost.
	result["cost"] <- sum(costs[eventHistory$events], na.rm=TRUE)
	# (If X happens twice, then the previous line counts the cost of X twice, 
	# as it obviously should. na.rm=TRUE deals with events that have no cost.)

	# Calculate discounted life-years.
	# The first year is discounted (it uses x^1, not x^0). 
	# See adjustLifeYears and testCalculateHealthEconomicQuantities.R. 
	# For example, if you live only 0.2 years, then that is discounted by 3.5%.
	#@ I sent an e-mail on 2015-12-29 about this, and I think it is OK. 
	result["discountedLifeYears"] <- adjustLifeYears(result["lifeYears"], 
			discountRate=v1other$lifeYearDiscountRate)
	
	# Calculate discounted QALYs. 
	result["discountedQalys"] <- adjustLifeYears(result["lifeYears"], 
			qalyFactorBoundaries=v1other$qalyFactorBoundaries, 
			qalyFactors=v1other$qalyFactors,
			discountRate=v1other$lifeYearDiscountRate)
	
	# Calculate discounted cost. 
	# Unlike with life-years, this discounting is done in a continuous way, as 
	# shown in the following formula:
	result["discountedCost"] <- sum(costs[eventHistory$events] / 
			(1 + v1other$costDiscountRate) ^ eventHistory$times, na.rm=TRUE)	

	# Return result. 
	if (!identical(names(result), namesOfQuantities))
		stop("INTERNAL ERROR: names(result) should equal namesOfQuantities")
	return(result)
}

################################################################################
# A function for calculating quality-adjusted or discounted life-years, 
# (including neither or both of quality-adjusted and discounted). 
# 
# This can cope with non-integer qalyFactorBoundaries (for example, startAge is 
# 69.4 and one of the qalyFactorBoundaries is 75). It can also cope with 
# arbitrary and person-specific qalyFactorBoundaries (for example due to 
# surgery), though that is not being done at present.

adjustLifeYears <- function(lifeYears, qalyFactorBoundaries=numeric(0), 
		qalyFactors=1, discountPeriod=1, discountRate=0) {
	# Make discountTimes and discountFactors. The first time will be 
	# "discountPeriod", the second will be "discountPeriod*2", etc. 
	# (If you want discountTimes to be something like 0.7, 1.7, 2.7, 3.7, then 
	# the next three lines will need to change, but the rest of this function 
	# should be OK.) 
	numberOfDiscountPeriods <- ceiling(lifeYears / discountPeriod)
	discountTimes <- seq_len(numberOfDiscountPeriods - 1) * discountPeriod
	discountFactors <- (1 + discountRate) ^ -(1:numberOfDiscountPeriods)
		
	# Make sortedTimes. 
	sortedTimes <- mergeSortedVectors(qalyFactorBoundaries, discountTimes)
	# Slower: sortedTimes <- sort(unique(c(qalyFactorBoundaries,discountTimes)))
	# Previously: ... <- unique(sort(c(qalyFactorBoundaries, discountTimes)))
	
	# Calculate intervalLengths. This is basically just the differences between 
	# adjacent elements of sortedTimes.
	intervalLengths <- c(sortedTimes, Inf) - c(0, sortedTimes)
	
	# Calculate numberOfIntervalsToUse. This may well be less than 
	# length(intervalLengths). The last interval will probably be incomplete. 
	numberOfIntervalsToUse <- findInterval(lifeYears, sortedTimes) + 1

	# Make factorsForEachInterval, a matrix in which each column corresponds to 
	# one interval, the first row contains the qaly factors, and the second row 
	# contains the discount factors. 
	factorsForEachInterval <- 
	        matrix(NA_real_, nrow=2, ncol=numberOfIntervalsToUse)
	timesAndFactors <- list(
	        list(times=qalyFactorBoundaries, factors=qalyFactors),
	        list(times=discountTimes, factors=discountFactors))
	for (i in 1:2) { 
	    indexes <- match(timesAndFactors[[i]]$times, sortedTimes)
	    numbersOfCopies <- 
	            c(indexes, 1 + length(sortedTimes)) - c(0, indexes)
	    factorsForEachInterval[i,] <- rep(timesAndFactors[[i]]$factors, 
	            numbersOfCopies)[1:numberOfIntervalsToUse] 
	}
	
	# Make a vector of the intervals that lifeYears consists of. 
	if (numberOfIntervalsToUse == 1) {
	    intervals <- lifeYears 
	} else {
	    intervals <- c(intervalLengths[1:(numberOfIntervalsToUse-1)],
	            lifeYears - sortedTimes[numberOfIntervalsToUse-1])
	}
	
	# Multiply each element of intervals by each element in the relevant column 
	# of factorsForEachInterval. Then sum to get the result. 
	sum(intervals * factorsForEachInterval[1,] * factorsForEachInterval[2,])
	# Much slower: sum(intervals * apply(factorsForEachInterval, 2, FUN=prod))
}


################################################################################



