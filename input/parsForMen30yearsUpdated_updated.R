################################################################################
# NB do not remove comments from this file lightly!

################################################################################
# Parameters for phase 1B, 2(ii). 
#@ parsForMen30yearsUpdated.R was previously called parametersGlover2014.R. 

# If things in this file are unclear then please see 
# parsForMen4yearsValidation.R first as that has more comments and details.

if (!exists("v0")) v0 <- compactList() 
if (!exists("v1distributions")) v1distributions <- compactList() 
if (!exists("v1other")) v1other <- compactList() 
if (!exists("v2")) v2 <- compactList() 

################################################################################
# Parameters for the main analysis (processPersons). 

################################################################################
# MISCELLANEOUS

v1other$postSurgeryInitialPeriod <- 30 / 365.25  # = 0.08213552

v1other$startAge <- 65	
# this is used below by being passed to createQalyFactors

v0$generateCensoringTime <- function() { 30.000001 }  # see parsForMen30years.R

# Post-surgery monitoring. 
v1other$timeToMonitoringFollowingOpenSurgery <- 100
v1other$timeBetweenMonitoringFollowingEvarSurgery <- NA

# No growth for men with a diameter of < 2cm 
v1other$zeroGrowthDiameterThreshold <- 2.0 

################################################################################
# SCREENING

# Re-invitation proportion
v2$probOfRequireReinvitation <- setType(0.1360, "probability")


# Attendance proportion
v2$probOfAttendScreen <- setType(0.750, "probability")


# Non-visualisation proportion
v2$probOfNonvisualization <- setType(0.0121, "probability")

# Prevalence proportion
fileName <- "input/AAA max measurements 2009 to 2014 MS 17 06 2015.csv"
v1other$baselineDiameters <- read.csv(fileName, comment.char="#")[, c("size", "pw")]
names(v1other$baselineDiameters) <- c("size", "weight")
## FOR APP - SET PREVALENCE EXPLICITLY
## THIS IS SUM >=3.0
prev<-sum(v1other$baselineDiameters$weight[v1other$baselineDiameters$size>=3])
v2$prevalence <- setType(prev, "probability")

v1other$prevalenceThreshold<-3.0

################################################################################
# AAA GROWTH & RUPTURE

# AAA growth
# AAA rupture

# sigma0 (exp), sigma1 (exp), rho (tanh) and sigmaw (exp) already transformed
v2$beta0 <- 1.27152200
v2$beta1 <- 0.05838810
v2$sigma0 <- exp(-1.737735221)
v2$sigma1 <- exp(-3.318297793)
v2$rho <- tanh(0.455016818)
v2$sigmaW <- exp(-2.586854985)
v2$gamma <- -16.26293
v2$alpha <- 7.210208
for (elementName in c("beta0", "beta1", "sigma0", "sigma1", "rho", "sigmaW",
		"gamma", "alpha"))
	attr(v2[[elementName]], "type") <- "par for aorta model"
# Values for growth taken from model_parameters_growth_unwgt.xls in W:\Studies\RESCAN\model estimates\growth\women.


################################################################################
# SURVEILLANCE

# Surveillance intervals
v1other$aortaDiameterThresholds <- c(3.0, 4.5, 5.5)
v1other$monitoringIntervals <- c(1, 0.25)
## Assume no monitoring after contraindication (as in Kim et al.). 
## Leave v1other$monitoringIntervalFollowingContraindication unspecified

# Dropout rate from surveillance
v2$rateOfDropoutFromMonitoring <- setType(0.01430178 * 4, "rate")  # see below

# Incidental detection rate
v2$rateOfIncidentalDetection <- 
		setType(convertThreeMonthProbToRate(0.0114), "rate")

# Delay from 5.5+cm scan to consultation		
v1other$waitingTimeToConsultation <- 71 / 365.25

# Consultation scan
v2$ctMeasurementErrorSD <- setType(0.19, "fixed value")
v2$extraDiameterForCtScan <- setType(0.2443, "fixed value")



# Decision at consultation: proportion returned to surveillance
# No variables need to be defined here. 

# Decision at consultation: non-intervention proportion
# In the computer program, probOfContraindication is a boolean that decides 
# between contraindication and deciding to go ahead with surgery. 
## CHANGED FOR APP
v2$ probOfContraindication <- 
  setType(c(intercept = qlogis(0.0977 / (0.0977 + 0.681)), age = 0, aortaSize = 0), 
          "logistic model for probability")

# Decision at consultation: proportion elective surgery
# This is just 1 minus probOfContraindication 

# Delay from consultation to elective surgery
v1other$waitingTimeToElectiveSurgery <- 59 / 365.25

################################################################################
# ELECTIVE OPERATIONS

# Proportion receiving EVAR vs. open repair
## CHANGED BELOW FOR APP
v2$probOfElectiveSurgeryIsOpen <- setType(c("intercept"=qlogis(0.2975504),age=0,aortaSize=0), "logistic model for probability")  

# Model for peri/post-operative mortality
v1other$electiveSurgeryAaaDeathMethod <- "survivalModel"
		
# EVAR 30-day operative mortality
## CHANGED BELOW FOR APP
v2$probOfAaaDeathInInitialPeriodAfterElectiveEvarSurgery <- setType(c("intercept"=qlogis(0.01607717),age=0,aortaSize=0), "logistic model for probability")  

# Open repair 30-day operative mortality
## CHANGED BELOW FOR APP
v2$probOfAaaDeathInInitialPeriodAfterElectiveOpenSurgery <- setType(c("intercept"=qlogis(0.04109589),age=0,aortaSize=0), "logistic model for probability")  

# Re-intervention rate after successful EVAR
v2$reinterventionRatesAfterElectiveEvar <- setType(c(0,0), "reintervention rates")
v1other$reinterventionTimeBoundariesAfterElectiveEvar <- 120/365.25

# Re-intervention rate after successful open repair
v2$reinterventionRatesAfterElectiveOpen <- setType(c(0,0), "reintervention rates")
v1other$reinterventionTimeBoundariesAfterElectiveOpen <- 120/365.25

# Long-term AAA mortality rate after successful EVAR
v2$rateOfAaaDeathAfterElectiveEvarSurgeryAndInitialPeriod <- 
		setType(0.0076640849, "rate")

# Long-term AAA mortality rate after successful open repair
v2$rateOfAaaDeathAfterElectiveOpenSurgeryAndInitialPeriod <- 
		setType(0.0006991216, "rate")

################################################################################
# EMERGENCY OPERATIONS

# % operated after rupture
v2$probOfEmergencySurgeryIfRupture <- setType(0.368, "probability")

# Proportion receiving EVAR vs. open repair
# Proportion receiving EVAR vs. open repair
v2$probOfEmergencySurgeryIsOpen <- 
  setType(c(intercept = Inf, age = 0, aortaSize=0), 
          "logistic model for probability")

# Model for peri/post-operative mortality		
v1other$emergencySurgeryAaaDeathMethod <- "survivalModel"

# EVAR 30-day operative mortality
v2$probOfAaaDeathInInitialPeriodAfterEmergencyEvarSurgery <- 
  setType(c(intercept = Inf, age = 0, aortaSize=0),  
          "logistic model for probability")

# Open repair 30-day operative mortality
v2$probOfAaaDeathInInitialPeriodAfterEmergencyOpenSurgery <- 
  setType(c(intercept = qlogis(0.342), age = 0, aortaSize=0),  
          "logistic model for probability")

# Long-term AAA mortality rate after successful EVAR
v2$rateOfAaaDeathAfterEmergencyEvarSurgeryAndInitialPeriod <- 
  setType(1e-100, "rate")  

# Long-term AAA mortality rate after successful open repair
v2$rateOfAaaDeathAfterEmergencyOpenSurgeryAndInitialPeriod <- 
  setType(1e-100, "rate")


# Re-intervention rate after successful EVAR
v2$reinterventionRatesAfterEmergencyEvar <- 
  setType(c(0,0) / 100, "reintervention rates")
v1other$reinterventionTimeBoundariesAfterEmergencyEvar <- 120/365.25


# Re-intervention rate after successful open repair
v2$reinterventionRatesAfterEmergencyOpen <- setType(c(0,0) / 100, "reintervention rates")
v1other$reinterventionTimeBoundariesAfterEmergencyOpen <- 120/365.25


################################################################################
# COSTS
## COSTS RESET FROM INF TO 1e7
v2$costs <- setType(c(
		inviteToScreen=1.70, 
		requireReinvitation=1.70,  # see below
		screen=32.20, 
		monitor=68.00,
		monitorFollowingContraindication=1e7,
		consultation=435.25,
		electiveSurgeryEvar=13345.66, # NB order (evar, open) 
		electiveSurgeryOpen=11532.69,
		emergencySurgeryEvar=1e7, 
		emergencySurgeryOpen=19984.75,
		reinterventionAfterElectiveEvar=1e7,
		reinterventionAfterElectiveOpen=1e7,
		reinterventionAfterEmergencyEvar=1e7,
		reinterventionAfterEmergencyOpen=1e7,
				monitorFollowingOpenSurgery=1e7,
		monitorFollowingEvarSurgery=1e7
		), type="costs")


################################################################################
# MISCELLANEOUS

# Non-AAA mortality rate
v1other$nonAaaDeathMethod <- "onsIntegerStart"
v1other$nonAaaMortalityRatesFileName <- 
	"input/nonAaaDeathMortalityRates_seeEmail20160617.csv"  # changed 2016-06-17

# Non-AAA mortality rate in those contraindicated
v2$rateOfNonAaaDeathAfterContraindication <- setType(41 / 166, "rate")

# Overall QoL / utilities
v1other <- compactList(append(v1other, 
		createQalyFactors(startAge=v1other$startAge)))

# Discount rates
v1other$lifeYearDiscountRate <- 3.5 / 100
v1other$costDiscountRate <- 3.5 / 100

################################################################################



