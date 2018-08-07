# Create qalyFactors and qalyFactorBoundaries, which will be stored in v1other
# and used by adjustLifeYears.
# 
# Time when you are aged between v1other, $qalyFactorBoundaries[i] and [i+1] 
# will be adjusted by a factor of v1other$qalyFactors[i+1].
# 
# The source for the default values of qalyFactorBoundariesAsAges and 
# qalyFactorsForAges (see the first line of code below) is 
# Table A in Kind et al 1999, http://www.york.ac.uk/che/pdf/DP172.pdf. 
# For men, years aged 65-74 are multiplied by 0.78 and years aged 75+ are 
# multiplied by 0.75. 
# 
# startAge is the age at the start of the simulation.

createQalyFactors <- function(startAge, 
		qalyFactorBoundariesAsAges=c(25,35,45,55,65,75), 
		qalyFactorsForAges=c(0.94, 0.93, 0.91, 0.84, 0.78, 0.78, 0.75)) {
			
	# Check qalyFactorBoundariesAsAges and qalyFactorsForAges.
	if (length(qalyFactorBoundariesAsAges) + 1 != length(qalyFactorsForAges))
		stop("qalyFactorsForAges must be 1 longer than ",
				"qalyFactorBoundariesAsAges")
	if (!identical(qalyFactorBoundariesAsAges,sort(qalyFactorBoundariesAsAges)))
		stop("qalyFactorBoundariesAsAges must be increasing")
	if (!identical(qalyFactorsForAges, 
			sort(qalyFactorsForAges, decreasing=TRUE)))
		stop("qalyFactorsForAges must be decreasing")
	if (any(qalyFactorBoundariesAsAges != round(qalyFactorBoundariesAsAges)))
		stop("qalyFactorBoundariesAsAges must all be integers")
	
	# Work out qalyFactorBoundaries and qalyFactors. 
	# (These two are the same as qalyFactorBoundariesAsAges and 
	# qalyFactorsForAges, but with startAge subtracted and truncated at the 
	# start, so that they are in terms of time since the start instead of age.) 
	firstBoundaryToUse <- findInterval(startAge, qalyFactorBoundariesAsAges) + 1
	if (firstBoundaryToUse == length(qalyFactorBoundariesAsAges) + 1) {
		qalyFactorBoundaries <- numeric(0)
	} else {
		qalyFactorBoundaries <- qalyFactorBoundariesAsAges[
			firstBoundaryToUse:length(qalyFactorBoundariesAsAges)] - startAge
	}
	qalyFactors <- 
			qalyFactorsForAges[firstBoundaryToUse:length(qalyFactorsForAges)]
	
	# Return qalyFactorBoundaries and qalyFactors.
	return(list(qalyFactorBoundaries=qalyFactorBoundaries, 
			qalyFactors=qalyFactors))
}







