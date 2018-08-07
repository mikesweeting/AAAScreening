################################################################################
# A function that can be used in place of processPersons. 

# Here "weighting" means over-sampling people who start above a threshold, 
# and then reweighting the results to get appropriate overall means.
# The idea is that people who start above the threshold have more effect on the 
# differences between the invited and notInvited groups, so this should 
# give more accurate results.

processPersonsWithWeighting <- function(v0, v1other, v2, 
		threshold=2.0, proportionToGenerateBelowThreshold) {
	# Set certain v0 booleans. This function only deals correctly with means; 
	# it does not deal with event-histories etc. properly, so you can't count 
	# the numbers of events after using it. 
	v0$returnMeanQuantities <- TRUE
	v0$returnEventHistories <- FALSE  
	v0$returnAllPersonsQuantities <- FALSE
	
	# Check proportionToGenerateBelowThreshold. 
	if (!is.numeric(proportionToGenerateBelowThreshold) || 
			length(proportionToGenerateBelowThreshold) != 1 ||
			proportionToGenerateBelowThreshold < 0 || 
			proportionToGenerateBelowThreshold > 1)
		stop("proportionToGenerateBelowThreshold must be a numeric in [0,1]")
	
	# Make v1bd and find the true proportion who are below the threshold.
	v1bd <- v1other$baselineDiameters
	trueProportionBelowThreshold <- 
			sum(v1bd$weight[v1bd$size < threshold]) / sum(v1bd$weight)
	
	# Create v0under and v1under, for the run of processPersons in which all 
	# baseline diameters are under the threshold.
	v0under <- v0
	v0under$numberOfPersons <- 
			v0$numberOfPersons * proportionToGenerateBelowThreshold
	if (round(v0under$numberOfPersons) != v0under$numberOfPersons)
		stop("v0under$numberOfPersons needs to be an integer")
	v1otherUnder <- v1other
	v1otherUnder$baselineDiameters$weight[v1bd$size >= threshold] <- 0
	# sample(..., prob=weights) does not require weights to sum to 1
	
	# Create v0over and v1over, for the run of processPersons in which all 
	# baseline diameters are over the threshold.
	v0over <- v0
	v0over$numberOfPersons <- 
			v0$numberOfPersons * (1 - proportionToGenerateBelowThreshold)
	v1otherOver <- v1other
	v1otherOver$baselineDiameters$weight[v1bd$size < threshold] <- 0
	
	# Do the two runs of processPersons.
	resultUnder <- processPersons(v0under, v1otherUnder, v2)
	resultOver <- processPersons(v0over, v1otherOver, v2)
	
	# Re-weight the means and return the 2x6 matrix.
	return(list(meanQuantities = 
			resultUnder$meanQuantities * trueProportionBelowThreshold + 
			resultOver$meanQuantities * (1 - trueProportionBelowThreshold)))
}

################################################################################
