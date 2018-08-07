################################################################################
# getBinaryVariable became necessary when it was decided that in the PSA some 
# probabilities should be generated from logistic regression models.
# 
# If a probability is generated from a beta distribution, then a boolean 
# element is generated for both members of each pair, in processOnePair.R. 
# This is stored in v3. 
# (This might not apply to probOfAaaDeathInInitialPeriodAfterElectiveEvarSurgery
# and the other probabilities in the "survivalModel" for operative mortality.)
# 
# If a probability is generated from a logistic model, then it can only be 
# generated at the point when it is needed. This is because most 
# logistic models involve age or aorta size. 
#
# getBinaryVariable deals with all possibilities, so it can be called 
# straight from generateEventHistory and generateEventHistory does not get 
# too cluttered. 
# 
# To cover both that possibility and the beta-distribution possibility

################################################################################
# If v3 contains varName, getBinaryVariable just returns that boolean element
# (after doing loads of checking).
# Otherwise, it calculates a probability from a logistic model, etc.

# TODO: separate the code to do with logistic models from the code that searches 
# through v2 and v3, etc.

getBinaryVariable <- function(varName, v1other, v2, v3, eventTime) {
	
	# Check the arguments.
	if (!is.character(varName) || length(varName) != 1)
		stop("varName must be a single string")
	if (!is.list(v1other) || !is.list(v2) || !is.list(v3))
		stop("v1other, v2, and v3 must all be lists")
	if (!missing(eventTime) && (is.null(eventTime) || is.na(eventTime) || 
			!is.numeric(eventTime) || length(eventTime) != 1 || eventTime < 0))
		stop("eventTime must be a single non-negative numeric")
	
	# Get the result. There are several possibilities. 
	
	if (varName %in% names(v3)) {
		if (!(getType(v3[[varName]]) %in% c("boolean", "fixed value")))
			stop("v3$", varName, " must have type boolean or fixed value")
		# There is a boolean element in v3. (This must be checked first.)
		result <- v3[[varName]]
		
	} else if (varName %in% names(v2) && 
			getType(v2[[varName]]) == "logistic model for probability") {
		# There is a "logistic model for probability" element in v2.
		
		# Make beta and logOddsAdjustment. beta is v2[[varName]] but with the 
		# logOddsAdjustment element removed if it exists. logOddsAdjustment is 
		# the logOddsAdjustment element of v2[[varName]] if that exists, or NA. 
		beta <- v2[[varName]][names(v2[[varName]]) != "logOddsAdjustment"]
		logOddsAdjustment <- v2[[varName]]["logOddsAdjustment"]  # might be NA 
		
		# Calculate the covariates.
		# (When calculating aorta size here, do use measurement error - because 
		# the logistic models were fitted using measured sizes, not exact ones.)
		covariates <- rep(NA, length(beta) - 1)
		names(covariates) <- names(beta)[-1]  # beta[1] is the intercept
		for (covariateName in names(covariates)) {
			# Calculate the appropriate number and store it in 
			# covariates[covariateName], e.g. covariates["age"].
			covariates[covariateName] <- switch(covariateName,
					
					# Names and formulas for the allowed covariates:
				  # MS ADDED 09/02/17 (ALL FITTED MODELS USE CENTRED COVARIATE (AGE-80) and (AAA DIAMETER - 6))
					# SO DO THE SAME IN THE CODE. NB MIGHT WANT TO ADD THIS AS A "PARAMETER" SO IT CAN BE CHANGED BY THE USER
					age = v1other$startAge + eventTime - 80,
					aortaSize = getAortaMeasurement(v3, eventTime, 
						v2$ultrasoundMeasurementErrorSD, method="ultrasound") - 6.0,
			
					# If covariateName is none of the allowed names, then stop: 
					stop("in v2$", varName, ", the name \"", covariateName, 
					"\" is illegal")
			)
		}
		
		# Calculate the probability and use that to generate a boolean value 
		# from the Bernoulli distribution.
		prob <- calculateProbFromLogisticModel(beta=beta, 
				covariates=covariates, logOddsAdjustment=logOddsAdjustment)
		result <- rbernoulli(prob)
		
	} else if (varName %in% names(v2) && 
			getType(v2[[varName]]) == "probability" && 
			varName %in% namesOfProbVarsForSurvivalModel) {
		# If varName is in namesOfProbVarsForSurvivalModel, then it is allowed 
		# to be a probability in v2 with no corresponding boolean in v3.
		result <- rbernoulli(v2[[varName]])
		
	} else {
		# Error. Display varName, v2, and v3, to help with debugging. 
		cat("\n\nvarName=", varName, "\nv2:\n", sep=""); print(v2); 
		cat("\nv3:\n"); print(v3)
		stop("failed to find acceptable element ", varName, " in v2 or v3")
		
	}
	
	# Check result is not NULL or NA and return it.
	if (is.null(result) || is.na(result)) {
		cat("Error in getBinaryVariable: result is NULL or NA.\n")
		browser()
		stop("result is ", {if (is.null(result)) "NULL" else result})
	}
	return(result)
}

################################################################################
# calculateProbFromLogisticModel can cope with a logistic model or a 
# logistic model in which the log-odds needs to be multiplied by a factor.
# (There are several other possible ways in which one could deal with 
# logOddsAdjustment.)

calculateProbFromLogisticModel <- 
		function(beta, covariates, logOddsAdjustment, verbose=FALSE) {
	# Check the arguments.
	if (length(beta) < 1) 
		stop("length(beta)=", length(beta), " but this must be at least 1")
	if (length(beta) != length(covariates) + 1)
		stop("length(beta)=", length(beta), " but length(covariates)=", 
				length(covariates))
	if (!identical(names(beta), c("intercept", names(covariates))))
		stop("names(beta)=", paste(names(beta), collapse=","), 
				" must be \"intercept\" followed by names(covariates)=",
				paste(names(covariates), collapse=","))
	
	# Check logOddsAdjustment, and set it to 0 if it is missing, NULL, or NA.
	# (This function just having a default value for logOddsAdjustment of 0 
	# would not suffice, because the user might pass NA or NULL, and those also 
	# need to be dealt with properly.)
	if (missing(logOddsAdjustment) || is.null(logOddsAdjustment) ||
			is.na(logOddsAdjustment))
		logOddsAdjustment <- 0
	checkIsSingleNumeric(logOddsAdjustment)
	
	# Calculate and return the probability. 
	# In logistic regression the linear predictor gives the log-odds. 
	logOdds <- beta[1] + sum(beta[-1] * covariates) + logOddsAdjustment
	prob <- plogis(logOdds)  # plogis(x) = exp(x) / (1 + exp(x))
	#prob <- exp(logOdds) / ( 1 + exp(logOdds) )
	#if (is.infinite(exp(logOdds))) prob <- 1  # deal with large logOdds 
	if (verbose) {
		cat("\ncalculateProbFromLogisticModel:\nbeta=\n")
		print(beta)
		cat("covariates=\n")
		print(covariates)
		cat("logOddsAdjustment=", logOddsAdjustment, "\n", sep="")		
		cat("logOdds=", logOdds, "\n", sep="")
		cat("prob=", prob, "\n", sep="")
	}
	return(prob)
}

################################################################################
