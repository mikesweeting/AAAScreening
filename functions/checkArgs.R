################################################################################
# checkArgs is used by processPersons. 
# This returns nothing and just raises errors if something is wrong. 


################################################################################
checkArgs <- function(v0, v1other, v1distributions, v2) {
	# v0 and v1other must be given, but v1distributions and v2 can be missing.
	if (missing(v0) || missing(v1other))
		stop("v0 and v1other must not be missing")
	checkV0(v0)
	checkV0andV1other(v0, v1other)
	checkV1other(v1other)
	if (!missing(v1distributions)) checkV1distributions(v1distributions)
	if (!missing(v2)) {
		checkV2(v2)
		checkV1otherAndV2(v1other, v2)
	}
}

################################################################################
checkV0 <- function(v0) {
	# Check that the v0$return... values are not all FALSE. 
	if (!any(v0$returnMeanQuantities, v0$returnEventHistories, 
			v0$returnAllPersonsQuantities))
		stop("v0$return... variables are all FALSE; nothing will be returned\n")
	
	# Check v0$method. 
	if (!(v0$method %in% c("serial", "parallel", "foreach", "parallelBatches")))
		stop("method has to be one of serial, parallel, foreach, and ",
				"parallelBatches")
	
	# Check v0$numberOfParameterIterations.
	if ("numberOfParameterIterations" %in% names(v0) && 
			(!is.numeric(v0$numberOfParameterIterations) || 
				length(v0$numberOfParameterIterations) != 1 ||
				v0$numberOfParameterIterations < 1))
		stop("v0$numberOfParameterIterations must be a positive integer")
}

################################################################################
checkV0andV1other <- function(v0, v1other) {
	# Check that the necessary file-names etc. have been supplied for
	# nonAaaSurvProbs or nonAaaMortalityRates. 
	
	if (v1other$nonAaaDeathMethod == "mass") {
		# (no checks)
		
	} else if (v1other$nonAaaDeathMethod == "onsIntegerStart") { 
		if (any(!(c("startAge","nonAaaMortalityRatesFileName") %in% 
							names(v1other))))
			stop("v1other$nonAaaDeathMethod is onsIntegerStart, so ", 
					"v1other must contain startAge and nonAaaMortalityRatesFileName")
		
	} else if (v1other$nonAaaDeathMethod == "onsNonintegerStart") {
		if (!("nonAaaMortalityRatesFileName" %in% names(v1other)) || 
				!("generateAgeAtBaseline" %in% names(v0)))
			stop("v1other$nonAaaDeathMethod is onsNonintegerStart, so ", 
					"v1other must contain nonAaaMortalityRatesFileName and\n",
					"v0 must contain generateAgeAtBaseline")
		
	} else {
		stop("v1other$nonAaaDeathMethod=", v1other$nonAaaDeathMethod, 
				" is illegal")
	}
	
}

################################################################################
checkV1other <- function(v1other) {
	# Check v1other$aortaDiameterThresholds and v1other$monitoringIntervals.
	if (length(v1other$aortaDiameterThresholds) != 
			length(v1other$monitoringIntervals) + 1)
		stop("v1other$aortaDiameterThresholds must be exactly one longer than ",
				"v1other$monitoringIntervals")
	if (!identical(v1other$aortaDiameterThresholds, 
			sort(v1other$aortaDiameterThresholds)))
		stop("v1other$aortaDiameterThresholds must be in increasing order")
	if (!identical(v1other$monitoringIntervals, 
			sort(v1other$monitoringIntervals, decreasing=TRUE)))
		stop("v1other$monitoringIntervals must be in decreasing order")
	
	# Check v1other$zeroGrowthDiameterThreshold, if it exists. 
	if ("zeroGrowthDiameterThreshold" %in% names(v1other) &&
			(!is.numeric(v1other$zeroGrowthDiameterThreshold) || 
				length(v1other$zeroGrowthDiameterThreshold) != 1 || 
				v1other$zeroGrowthDiameterThreshold < 0))
		stop("v1other$zeroGrowthDiameterThreshold must be a single ",
				"non-negative number, if it exists")
	
	# Check the reintervention time-boundaries. 
	for (varName in c("reinterventionTimeBoundariesAfterElectiveOpen",
			"reinterventionTimeBoundariesAfterElectiveEvar",
			"reinterventionTimeBoundariesAfterEmergencyOpen",
			"reinterventionTimeBoundariesAfterEmergencyEvar")) {
		v1element <- v1other[[varName]]
		if (is.null(v1element) || !is.numeric(v1element) || 
				any(is.na(v1element)) || any(v1element < 0) || 
				!identical(sort(v1element), v1element))
			stop("v1other$", varName, " must be a vector of positive numerics ",
					"in increasing order")
	}
	# TODO: these checks are also done by 
	# checkReinterventionRatesAndTimeBoundaries. 
	# See also "Check reintervention rates" in checkV2 below.
	
	# Check v1other$postSurgeryInitialPeriod.
	if (!isSingleNumeric(v1other$postSurgeryInitialPeriod))
		stop("v1other$postSurgeryInitialPeriod must be a single numeric")
	
	# Check times to do with post-surgery monitoring. 
	for (varName in c("timeToMonitoringFollowingOpenSurgery",
			"timeBetweenMonitoringFollowingEvarSurgery")) {
		v1element <- v1other[[varName]]
		if (is.null(v1element)) 
			stop("v1other$", varName, " must not be NULL")
		if (is.na(v1element)) next  # it is OK for these to be NA
		if (!is.numeric(v1element) || length(v1element) != 1 || v1element < 0)
			stop("v1other$", varName, " must be a single numeric (or NA)")
	}
}

################################################################################
checkV1distributions <- function(v1distributions) {
	# Check v1distributions$extraDiameterForCtScan. 
	if (!("extraDiameterForCtScan" %in% names(v1distributions)))
		stop("v1distributions must contain extraDiameterForCtScan")
	
	# Check the elements of v1distributions. 
	
	for (v1elementName in names(v1distributions)) {
		# Get v1element and type, check v1element is not NA.
		v1element <- v1distributions[[v1elementName]]
		if (any(is.na(v1element))) {
			cat("\nv1distributions$", v1elementName, ":\n", sep="")
			print(v1element)
			stop("v1distributions$", v1elementName, " is illegal")
		}
		type <- attr(v1element, "type")
		if (is.null(type)) 
			stop("the \"type\" element of v1distributions$", v1elementName, 
					" is NULL")
		
		# Now go through the different possibilities for type and check 
		# that v1element is legal. 
		raiseV1TypeRelatedError <- function(v1elementName) {
			cat("\nv1distributions$", v1elementName, ":\n", sep="")
			print(v1element)
			stop("v1distributions$", v1elementName, " is illegal in some way")
		}
		if (type == "fixed value") {
			# Just check that it is not NA and it does not have zero length. 
			if (is.na(v1element) || length(v1element) == 0)
				raiseV1TypeRelatedError(v1elementName)
			
		} else if (type == "beta pars for probability") {
			# It must be a list with elements alpha and beta (single numerics).
			if (!is.list(v1element) || 
					!identical(names(v1element), c("alpha", "beta")) ||
					!isSingleNumeric(v1element$alpha) || 
					!isSingleNumeric(v1element$beta))
				raiseV1TypeRelatedError(v1elementName)
			
		} else if (type == "hyperpars for logistic model for probability") {
			# - This type is not just a logistic model but is also allowed to 
			#   have logOddsAdjustment. 
			# - v1element must be a list with first element mean. 
			# - If mean is a single numeric, this is an intercept-only logistic 
			#   model and the next element must be variance, also a single 
			#   numeric. mean and variance can have names but do not have to.
			#   (However, in v2 names(beta) must be "intercept".)
			# - Otherwise, the next element must be covariance, a matrix, with 
			#   the same names as mean. 
			# - The third element is optional but if it exists it must be 
			#   logOddsAdjustment. 
	
			# Check it is a list of length 2 or 3. 
			if (!is.list(v1element) || 
					length(v1element) < 2 || length(v1element) > 3)
				raiseV1TypeRelatedError(v1elementName)
			# Check the 1st name is mean and the 2nd is variance or covariance.
			if (names(v1element)[[1]] != "mean" || 
					!(names(v1element)[[2]] %in% c("variance", "covariance")))
				raiseV1TypeRelatedError(v1elementName)
			# Check the 3rd name, if it exists, is logOddsAdjustment.
			if (length(v1element) == 3 && names(v1element)[[3]] != 
					"logOddsAdjustment")
				raiseV1TypeRelatedError(v1elementName)
			
			# Check the first two elements are numeric & the first is a vector.
			if (!is.numeric(v1element$mean) || !is.vector(v1element$mean) ||
					!is.numeric(v1element[[2]]))
				raiseV1TypeRelatedError(v1elementName)
			
			if (length(v1element$mean) == 1) {
				# Check things specific to intercept-only models.
				if (names(v1element)[[2]] != "variance" || 
						!isSingleNumeric(v1element$mean) || 
						!isSingleNumeric(v1element$variance))
					raiseV1TypeRelatedError(v1elementName)
				
			} else {
				# Check things specific to the other kind.
				if (names(v1element)[[2]] != "covariance" || 
						!is.matrix(v1element$covariance) || 
						!is.numeric(v1element$covariance) ||
						length(v1element$mean) != nrow(v1element$covariance) ||
						!identical(names(v1element$mean), 
								rownames(v1element$covariance)) ||
						!identical(names(v1element$mean), 
								colnames(v1element$covariance)))
					raiseV1TypeRelatedError(v1elementName)
			}
	
			# Check logOddsAdjustment, if it exists. 
			if (length(v1element) == 3 && 
					!isSingleNumeric(v1element$logOddsAdjustment))
				raiseV1TypeRelatedError(v1elementName)
	
		} else if (type == "gamma pars for rate") {
			# It must be a list with elements shape and scale (single numerics).
			if (!is.list(v1element) || 
					!identical(names(v1element), c("shape", "scale")) ||
					!isSingleNumeric(v1element$shape) || 
					!isSingleNumeric(v1element$scale))
				raiseV1TypeRelatedError(v1elementName)
		  
		} else if (type == "gamma pars for multiple rates") {
		  # It must be a list with elements shapes and scales.
		  if (!is.list(v1element) || 
		      !identical(names(v1element), c("shapes", "scales")) ||
		      !isMultipleNumeric(v1element$shapes) || 
		      !isMultipleNumeric(v1element$scales))
		    raiseV1TypeRelatedError(v1elementName)
			
		} else if (type == "pars for betaThenConvertThreeMonthProbToRate") {
			# It must be a list with elements alpha and beta (single numerics).
			if (!is.list(v1element) || 
					!identical(names(v1element), c("alpha", "beta")) ||
					!isSingleNumeric(v1element$alpha) || 
					!isSingleNumeric(v1element$beta))
				raiseV1TypeRelatedError(v1elementName)
			
		} else if (type == "pars for gammaThenMultiplyByFour") {
			# It must be a list with elements shape and scale (single numerics).
			if (!is.list(v1element) || 
					!identical(names(v1element), c("shape", "scale")) ||
					!isSingleNumeric(v1element$shape) || 
					!isSingleNumeric(v1element$scale))
				raiseV1TypeRelatedError(v1elementName)
			
		} else if (type == "hyperpars for aorta model") {
			# It must be either a vector or matrix with dimension 2 or 6.
			# NB a numeric vector with an attribute no longer passes is.vector!
			if (!(is.numeric(v1element) && length(v1element) %in% c(2,6)) && 
					!(is.matrix(v1element) && nrow(v1element) %in% c(2,6)))
				raiseV1TypeRelatedError(v1elementName)
			if (length(v1element) %in% c(2,6) && !hasNames(v1element))
				stop("if v1element is a vector then it must have names")
			# TODO: more checking if type == "hyperpars for aorta model" 
			
		} else if (type == "fixed value for probability") {
			# It must be a single numeric.
			if (!isSingleNumeric(v1element))
				raiseV1TypeRelatedError(v1elementName)
			
		} else if (type == "truncated normal distribution") {
		  # It must be a list with elements mean and variance (single numerics).
		  if (!is.list(v1element) || 
		      !identical(names(v1element), c("mean", "variance")) ||
		      !isSingleNumeric(v1element$mean) || 
		      !isSingleNumeric(v1element$variance))
		    raiseV1TypeRelatedError(v1elementName)
		  
		} else if (type == "fixed value for costs") {
			# TODO: think how to check this!
			
		} else if (type == "distribution for costs") {
		  # It must be a list with elements mean (multiple numerics) and variance (single numeric).
		  if (!is.list(v1element) || 
		      !identical(names(v1element), c("mean", "variance")) ||
		      !isSingleNumeric(v1element$variance))
		    raiseV1TypeRelatedError(v1elementName)
		  
		} else if (type == "fixed value for reintervention rates") {
			if (!is.numeric(v1element) || any(v1element < 0))
				raiseV1TypeRelatedError(v1elementName)

		} else if (type == "fixed value for rate") {
		  if (!is.numeric(v1element) || any(v1element < 0))
		    raiseV1TypeRelatedError(v1elementName)
		
		} else if (type == "normal distribution for logit prevalence") {
		  # It must be a list with elements mean and variance (single numerics).
		  if (!is.list(v1element) || 
		      !identical(names(v1element), c("mean", "variance")) ||
		      !isSingleNumeric(v1element$mean) || 
		      !isSingleNumeric(v1element$variance))
		    raiseV1TypeRelatedError(v1elementName)
		  
		} else {
			stop("type=", {if (is.null(type)) "NULL" else type}, 
					" is illegal; v1elementName=", v1elementName)
		}
	}
}

################################################################################
checkV2 <- function(v2) {
	# Check the elements of v2.
	
	for (v2elementName in names(v2)) {
		# Get v2element and type. 
		v2element <- v2[[v2elementName]]
		if (any(is.na(v2element))) {
			cat("\v2$", v2elementName, ":\n", sep="")
			print(v2element)
			stop("v2$", v2elementName, " is illegal")
		}
		type <- attr(v2element, "type")
		if (is.null(type)) 
			stop("the \"type\" element of v2$", v2elementName, " is NULL")
		
		# Now go through the different possibilities for type and check 
		# that element is legal. 
		raiseV2TypeRelatedError <- function(v2elementName) {
			cat("\nv2$", v2elementName, ":\n", sep="")
			print(v2element)
			stop("v2$", v2elementName, " is illegal in some way")
		}
		
		if (type == "fixed value") {
			# Just check that it is not NA and it does not have zero length. 
			if (is.na(v2element) || length(v2element) == 0)
				raiseV2TypeRelatedError(v2elementName)
			
		} else if (type == "probability") {
			if (!isSingleNumeric(v2element) || v2element < 0 || v2element > 1)
				raiseV2TypeRelatedError(v2elementName)
			
		} else if (type == "logistic model for probability") {
			# This type is not just a logistic model but is also allowed to 
			# have logOddsAdjustment. 
			# It must be of the form c(intercept=2, xxx=3) and have length at 
			# least 1 (length 1 means an intercept-only model). The last 
			# element can be "logOddsAdjustment" and is used differently. 
			if (!is.numeric(v2element) || any(is.na(v2element)) || 
					!hasNames(v2element))
				raiseV2TypeRelatedError(v2elementName)
			minimumLength <- { if (names(v2element)[length(v2element)] == 
					"logOddsAdjustment") 2 else 1 }
			if (length(v2element) < minimumLength) 
				raiseV2TypeRelatedError(v2elementName)
			# Check that logOddsAdjustment does not appear before the last name.
			if ("logOddsAdjustment" %in% names(v2element) && match(
					"logOddsAdjustment", names(v2element)) < length(v2element))
				raiseV2TypeRelatedError(v2elementName)
			# Whether the names other than logOddsAdjustment are legal will be 
			# checked in getBinaryVariable. The names are very restricted and 
			# have to be something like c("intercept", "age", "aortaSize"). 
			
		} else if (type == "rate") {
			if (!isSingleNumeric(v2element) || v2element < 0)
				raiseV2TypeRelatedError(v2elementName)
			
		} else if (type == "par for aorta model") {
			# It must be a single numeric.
			if (!isSingleNumeric(v2element))
				raiseV2TypeRelatedError(v2elementName)
			
		} else if (type == "costs") {
			# It must be a vector of named numerics.
			if (!is.numeric(v2element) || !hasNames(v2element) || 
					length(v2element) < 9)
				raiseV2TypeRelatedError(v2elementName)
			
			#} else if (type == "baseline diameters with desired prevalence") {
			#	# TODO: check that it has the right form
			#	# Also, when checkArgs is run, 
			# 	# v2$baselineDiametersWithDesiredPrevalence might not yet exist.
	
		} else if (type == "reintervention rates") {
			# It must be a vector of non-negative numerics. 
			if (!is.numeric(v2element) || any(v2element < 0))
				raiseV2TypeRelatedError(v2elementName)
			
		} else {
			stop("type=", {if (is.null(type)) "NULL" else type}, 
					" is illegal; v2elementName=", v2elementName)
		}
	}
	
	# Check that v2 contains certain elements. If the model changes, update 
	# requiredElementsOfV2 in the next line to how it should be.
	requiredElementsOfV2 <- c(
			"probOfRequireReinvitation", "probOfAttendScreen",
			"probOfNonvisualization", "probOfContraindication",
			"probOfEmergencySurgeryIfRupture", 
			"probOfElectiveSurgeryIsOpen",
			"probOfEmergencySurgeryIsOpen",
			"rateOfDropoutFromMonitoring", "rateOfIncidentalDetection", 
#			"rateOfNonAaaDeathAfterContraindication",
			"beta0", "beta1", "sigma0", "sigma1", "rho", "sigmaW", 
			"gamma", "alpha", 
			"costs", "ctMeasurementErrorSD")
			# don't require "ultrasoundMeasurementErrorSD" any more; that is 
			# now copied from sigmaW in processPersons.
	for (varName in requiredElementsOfV2)
		if (!(varName %in% names(v2)) || is.na(v2[[varName]]))
			stop("v2 must contain ", varName, " and it must not be NA")
	
	## # Check that v2 contains certain required elements and they are not NA. 
	## for (varName in c("costs", "ctMeasurementErrorSD", 
	##         "ultrasoundMeasurementErrorSD")) 
	##     if (!(varName %in% names(v2)) || is.na(v2[[varName]]))
	##         stop("v2 must contain ", varName, " and it must not be NA")
	
	# Check reintervention rates. 
	for (varName in c("reinterventionRatesAfterElectiveOpen",
			"reinterventionRatesAfterElectiveEvar",
			"reinterventionRatesAfterEmergencyOpen",
			"reinterventionRatesAfterEmergencyEvar")) {
		v2element <- v2[[varName]]
		if (is.null(v2element) || !is.numeric(v2element) || 
				any(is.na(v2element) || any(v2element < 0))) 
			## MJS 09/03/17. removed requirement for reintervention rates to be decreasing over time (since this occasionally happens in PSA)
				# || 
				#!identical(sort(v2element, decreasing=TRUE), 
				#as.numeric(v2element))))
			stop("v2element$", varName, " must be a non-negative numeric vector")
	}

}

################################################################################
checkV1otherAndV2 <- function(v1other, v2) {
	
	if (!("electiveSurgeryAaaDeathMethod" %in% names(v1other)))
		stop("v1other must contain an electiveSurgeryAaaDeathMethod element")
	if (!("emergencySurgeryAaaDeathMethod" %in% names(v1other)))
		stop("v1other must contain an emergencySurgeryAaaDeathMethod element")
	
	############################################################################
	# Check v1other$electiveSurgeryAaaDeathMethod and things associated with it.
	
	# First define some vectors. 
	v2elementsForPostElectiveSurgeryInstantDeathOnly <- 
			c("probOfDieFromElectiveSurgeryViaScreeningDetection", 
			"probOfDieFromElectiveSurgeryViaIncidentalDetection")
	v2containsElementsForPostElectiveSurgeryInstantDeathOnly <- 
			v2elementsForPostElectiveSurgeryInstantDeathOnly %in% names(v2)
	v2elementsForPostElectiveSurgerySurvivalModel <- c( 
			"probOfAaaDeathInInitialPeriodAfterElectiveOpenSurgery",
			"probOfAaaDeathInInitialPeriodAfterElectiveEvarSurgery",
			"rateOfAaaDeathAfterElectiveOpenSurgeryAndInitialPeriod",
			"rateOfAaaDeathAfterElectiveEvarSurgeryAndInitialPeriod")
	v2containsElementsForPostElectiveSurgerySurvivalModel <- 
			v2elementsForPostElectiveSurgerySurvivalModel %in% names(v2)
	# (v1other$postSurgeryInitialPeriod is now used in reintervention-related 
	# code as well as code to do with electiveSurgeryAaaDeathMethod. 
	# So don't check it here.)
	
	if (v1other$electiveSurgeryAaaDeathMethod == "instantDeathOnly") {
		# Check that the required elements are present and the non-required 
		# elements are not. 
		if (!all(v2containsElementsForPostElectiveSurgeryInstantDeathOnly))
			stop("v1other$electiveSurgeryAaaDeathMethod=", 
					v1other$electiveSurgeryAaaDeathMethod, 
					";\n and v2 does not contain all the required elements")
		if (any(v2containsElementsForPostElectiveSurgerySurvivalModel))
			stop("v1other$electiveSurgeryAaaDeathMethod=", 
					v1other$electiveSurgeryAaaDeathMethod, 
					";\n v2 contains elements that it should not contain")
		# Check one other thing.
		if (v2$probOfElectiveSurgeryIsOpen != 1)
			stop("v1other$electiveSurgeryAaaDeathMethod=", 
					v1other$electiveSurgeryAaaDeathMethod, ",\n so ",
					"v2$probOfElectiveSurgeryIsOpen",
					"\n should be 1, but it is",
					v2$probOfElectiveSurgeryIsOpen)
		# The probability will be checked elsewhere along with all the 
		# other v2 elements whose type attribute is "probability". 
		
	} else if (v1other$electiveSurgeryAaaDeathMethod == "survivalModel") {
		# Check that the required elements are present and the non-required 
		# elements are not. 
		if (!all(v2containsElementsForPostElectiveSurgerySurvivalModel))
			stop("v1other$electiveSurgeryAaaDeathMethod=", 
					v1other$electiveSurgeryAaaDeathMethod, 
					";\n and v2 does not contain all the required elements")
		if (any(v2containsElementsForPostElectiveSurgeryInstantDeathOnly))
			stop("v1other$electiveSurgeryAaaDeathMethod=", 
					v1other$electiveSurgeryAaaDeathMethod, 
					";\n v2 contains elements that it should not contain")
		# Check that the elements all have legal types.
		for (varName in v2elementsForPostElectiveSurgerySurvivalModel) { 
			type <- getType(v2[[varName]])
			if (!(type %in%
					c("probability", "logistic model for probability", "rate")))
				stop("v2$", varName, " has an illegal type: ", type)
		}
		
	} else {
		stop("v1other$electiveSurgeryAaaDeathMethod=", 
				v1other$electiveSurgeryAaaDeathMethod, " is illegal")
	}
	
	############################################################################
	# Check v1other$emergencySurgeryAaaDeathMethod and things associated with it.
	
	# First define some vectors. 
	v2elementsForPostEmergencySurgeryInstantDeathOnly <- 
			"probOfDieFromEmergencySurgery"
	v2containsElementsForPostEmergencySurgeryInstantDeathOnly <- 
			v2elementsForPostEmergencySurgeryInstantDeathOnly %in% names(v2)
	v2elementsForPostEmergencySurgerySurvivalModel <- c( 
			"probOfAaaDeathInInitialPeriodAfterEmergencyOpenSurgery",
			"probOfAaaDeathInInitialPeriodAfterEmergencyEvarSurgery",
			"rateOfAaaDeathAfterEmergencyOpenSurgeryAndInitialPeriod",
			"rateOfAaaDeathAfterEmergencyEvarSurgeryAndInitialPeriod")
	v2containsElementsForPostEmergencySurgerySurvivalModel <- 
			v2elementsForPostEmergencySurgerySurvivalModel %in% names(v2)
	
	if (v1other$emergencySurgeryAaaDeathMethod == "instantDeathOnly") {
		# Check that the required elements are present and the non-required 
		# elements are not. 
		if (!all(v2containsElementsForPostEmergencySurgeryInstantDeathOnly))
			stop("v1other$emergencySurgeryAaaDeathMethod=", 
					v1other$emergencySurgeryAaaDeathMethod, 
					";\n and v2 does not contain all the required elements")
		if (any(v2containsElementsForPostEmergencySurgerySurvivalModel))
			stop("v1other$emergencySurgeryAaaDeathMethod=", 
					v1other$emergencySurgeryAaaDeathMethod, 
					";\n v2 contains elements that it should not contain")
		# Check one other thing.
	  if (v2$probOfEmergencySurgeryIsOpen != 1)
	    stop("v1other$emergencySurgeryAaaDeathMethod=", 
	         v1other$emergencySurgeryAaaDeathMethod, ",\n so ",
	         "v2$probOfEmergencySurgeryIsOpen",
	         "\n should be 1, but it is",
	         v2$probOfEmergencySurgeryIsOpen)
	  
	} else if (v1other$emergencySurgeryAaaDeathMethod == "survivalModel") {
		# Check that the required elements are present and the non-required 
		# elements are not. 
		if (!all(v2containsElementsForPostEmergencySurgerySurvivalModel))
			stop("v1other$emergencySurgeryAaaDeathMethod=", 
					v1other$emergencySurgeryAaaDeathMethod, 
					";\n and v2 does not contain all the required elements")
		if (any(v2containsElementsForPostEmergencySurgeryInstantDeathOnly))
			stop("v1other$emergencySurgeryAaaDeathMethod=", 
					v1other$emergencySurgeryAaaDeathMethod, 
					";\n v2 contains elements that it should not contain")
	  # Check that the elements all have legal types.
	  for (varName in v2elementsForPostEmergencySurgerySurvivalModel) { 
	    type <- getType(v2[[varName]])
	    if (!(type %in%
	          c("probability", "logistic model for probability", "rate")))
	      stop("v2$", varName, " has an illegal type: ", type)
	  }
	    
	  	} else {
		stop("v1other$emergencySurgeryAaaDeathMethod=", 
				v1other$emergencySurgeryAaaDeathMethod, " is illegal")
	}

}

################################################################################
################################################################################

checkArrayOfMeansFirstDimNames <- function(arrayOfMeans) {
	# arrayOfMeans could be either processPersonsResult$meanQuantities or 
	# psaResult$psaQuantities	
	if (identical(dimnames(arrayOfMeans)[[1]], c("screening", "noScreening")))
		stop("dimnames(arrayOfMeans)[[1]] is the wrong way round. It ",
			"should be \n c(\"noScreening\", \"screening\").",
			" (getIncrementalCostOrEffectiveness assumes\n that the first ",
			"\"row\" is the standard treatment/scenario, e.g. noScreening, ",
			"\n and the second is the novel one, e.g. screening.)")
}

################################################################################
# Check rates and timeBoundaries, which are used by generateReinterventionTime.

checkReinterventionRatesAndTimeBoundaries <- function(rates, timeBoundaries) {

	if (is.null(timeBoundaries) || !is.numeric(timeBoundaries) || 
			any(is.na(timeBoundaries)) || any(timeBoundaries < 0) || 
			!identical(sort(timeBoundaries), timeBoundaries))
		stop("timeBoundaries=", { if (is.null(timeBoundaries)) "NULL" else 
				paste(timeBoundaries, collapse=",") },
				" must be a non-negative numeric vector in ascending order")
	
	if (is.null(rates) || !is.numeric(rates) || any(is.na(rates)) || 
			any(rates < 0))
			## MJS 09/03/17. Removed requirement for rates to be decreasing over time
			#|| 
			#!identical(sort(rates, decreasing=TRUE), as.numeric(rates))) 
			# Reason for as.numeric: sort & as.numeric both discard attributes.
		stop("rates=", { if (is.null(rates)) "NULL" else 
				paste(rates, collapse=",") }, 
				" must be a non-negative numeric vector")
	
	# Logically timeBoundaries must be in ascending order.
	# As for rates, it is not illogical or impossible for them to not be 
	# in descending order, but it seems very implausible. 
	
	if (length(timeBoundaries) + 1 != length(rates))
		stop("length(timeBoundaries) must be 1 less than length(rates)")	
}

################################################################################

