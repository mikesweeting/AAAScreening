################################################################################
# Parameters for Reference Model (women). 

if (!exists("v0")) v0 <- compactList() 
if (!exists("v1distributions")) v1distributions <- compactList() 
if (!exists("v1other")) v1other <- compactList() 
if (!exists("v2")) v2 <- compactList() 


v1other$postSurgeryInitialPeriod <- 30 / 365.25

v1other$startAge <- 65

v0$generateCensoringTime <- function() { 30.000001 }  # see parsForMen30years.R


# Post-surgery monitoring. 
v1other$timeToMonitoringFollowingOpenSurgery <- (6 * 7)/ 365.25
v1other$timeBetweenMonitoringFollowingEvarSurgery <- 1

# No growth for women with a diameter of < 2cm 
v1other$zeroGrowthDiameterThreshold <- 2.0 

################################################################################
# SCREENING

# Re-invitation proportion
v2$probOfRequireReinvitation <- setType(142127/594376, "probability")

# Attendance proportion
v2$probOfAttendScreen <- setType(218/300, "probability")

# Non-visualisation proportion
v2$probOfNonvisualization <- setType(1652/470531, "probability")

# Prevalence proportion
fileName <- "input/AAA max measurements 2009 to 2014 MS 17 06 2015.csv"
v1other$baselineDiameters <- read.csv(fileName, comment.char="#")[, c("size", "pw")]
names(v1other$baselineDiameters) <- c("size", "weight")
v2$prevalence <- setType(0.0042756, "probability")

v1other$prevalenceThreshold<-3.0

################################################################################
# AAA GROWTH & RUPTURE

# AAA growth
# AAA rupture

v2$beta0 <- 1.334426035
v2$beta1 <- 0.05239619
v2$sigma0 <- exp(-1.986425701)
v2$sigma1 <- exp(-3.281826664)
v2$rho <- tanh(0.409567743)
v2$sigmaW <- exp(-2.960876192)
v2$gamma <- -12.39926
v2$alpha <- 5.468415

for (elementName in c("beta0", "beta1", "sigma0", "sigma1", "rho", "sigmaW",
                      "gamma", "alpha"))
  attr(v2[[elementName]], "type") <- "par for aorta model"

# psa 
growthParameterNames <- 
  c("beta1", "beta0", "logSigma1", "logSigma0", "atanhRho", "logSigmaW")
ruptureParameterNames <- c("alpha", "gamma")



################################################################################
# SURVEILLANCE

# Surveillance intervals
v1other$aortaDiameterThresholds <- c(3.0, 4.5, 5.5)
v1other$monitoringIntervals <- c(1, 0.25)
v1other$monitoringIntervalFollowingContraindication <- 0.5

# Dropout rate from surveillance
v2$rateOfDropoutFromMonitoring <- setType(1072/19650, "rate")

# Incidental detection rate
v2$rateOfIncidentalDetection <- setType(40/1364.25, "rate")

# Delay from 5.5+cm scan to consultation
v1other$waitingTimeToConsultation <- 10.6 / 365.25

# Consultation scan
v2$extraDiameterForCtScan <- setType(0.244, "fixed value") 
v2$ctMeasurementErrorSD <- setType(0.19, "fixed value")  

# Decision at consultation: proportion returned to surveillance
# No variables need to be defined here. 

# Decision at consultation: non-intervention proportion
# In the computer program, probOfContraindication is a boolean that decides 
# between contraindication and deciding to go ahead with surgery. 
## CHANGED FOR APP
v2$ probOfContraindication <- 
  setType(c(intercept = qlogis(0.34226), age = 0, aortaSize = 0), 
          "logistic model for probability")

# Decision at consultation: proportion elective surgery
# This is just 1 minus probOfContraindication 

# Delay from consultation to elective surgery
v1other$waitingTimeToElectiveSurgery <- 70.8 / 365.25

################################################################################
# ELECTIVE OPERATIONS

# Proportion receiving EVAR vs. open repair
v2$ probOfElectiveSurgeryIsOpen <- 
  setType(c(intercept = -0.702391, age = -0.095305, aortaSize = 0.303022), 
          "logistic model for probability")

v1other$electiveSurgeryAaaDeathMethod <- "survivalModel"

# EVAR 30-day operative mortality
v2$probOfAaaDeathInInitialPeriodAfterElectiveEvarSurgery <- 
  setType(c(intercept = -3.909914, age = 0.002297, aortaSize = -0.027854,
			logOddsAdjustment =log((37/(1642-37))/(27/(1642-27)))), 
          "logistic model for probability")

# Open repair 30-day operative mortality
v2$probOfAaaDeathInInitialPeriodAfterElectiveOpenSurgery <- 
  setType(c(intercept = -2.33601, age = 0.06416, aortaSize = 0.07745,
        logOddsAdjustment=log((75/(1066-75))/(64/(1066-64)))), 
          "logistic model for probability")

# Re-intervention rate after successful EVAR
v2$reinterventionRatesAfterElectiveEvar <- 
  setType(c(20.3, 6.4) / 100, "reintervention rates")
v1other$reinterventionTimeBoundariesAfterElectiveEvar <- 120 / 365.25

# Re-intervention rate after successful open repair
v2$reinterventionRatesAfterElectiveOpen <- setType(c(0,0), "reintervention rates")
v1other$reinterventionTimeBoundariesAfterElectiveOpen <- 120/365.25

# Long-term AAA mortality rate after successful EVAR
v2$rateOfAaaDeathAfterElectiveEvarSurgeryAndInitialPeriod <- 
  setType(1.799 / 100, "rate") 

# Long-term AAA mortality rate after successful open repair
v2$rateOfAaaDeathAfterElectiveOpenSurgeryAndInitialPeriod <- 
  setType(0.499 / 100, "rate") 

################################################################################
# EMERGENCY OPERATIONS

# % operated after rupture
v2$probOfEmergencySurgeryIfRupture <- setType(0.25, "probability")

# Proportion receiving EVAR vs. open repair
v2$probOfEmergencySurgeryIsOpen <- 
  setType(c(intercept = 1.547574, age = -0.040946, aortaSize=0), 
          "logistic model for probability")

v1other$emergencySurgeryAaaDeathMethod <- "survivalModel"

# EVAR 30-day operative mortality
v2$probOfAaaDeathInInitialPeriodAfterEmergencyEvarSurgery <- 
  setType(c(intercept = -1.14998, age = 0.06071, aortaSize=0,
		logOddsAdjustment=log((48/(244-48))/(31/(244-31)))),  
          "logistic model for probability")

# Open repair 30-day operative mortality
v2$probOfAaaDeathInInitialPeriodAfterEmergencyOpenSurgery <- 
  setType(c(intercept = -0.34268, age = 0.03293, aortaSize=0,
		logOddsAdjustment=log((319/(845-319))/(284/(845-284)))),  
          "logistic model for probability")

# Re-intervention rate after successful EVAR
v2$reinterventionRatesAfterEmergencyEvar <- 
  setType(c(15.8,15.8) / 100, "reintervention rates")
v1other$reinterventionTimeBoundariesAfterEmergencyEvar <- 120/365.25

# Re-intervention rate after successful open repair
v2$reinterventionRatesAfterEmergencyOpen <- setType(c(2.3,2.3) / 100, "reintervention rates")
v1other$reinterventionTimeBoundariesAfterEmergencyOpen <- 120/365.25

# Long-term AAA mortality rate after successful EVAR
v2$rateOfAaaDeathAfterEmergencyEvarSurgeryAndInitialPeriod <- 
  setType(1e-100, "rate")  # the document says zero; search for myRexp

# Long-term AAA mortality rate after successful open repair
v2$rateOfAaaDeathAfterEmergencyOpenSurgeryAndInitialPeriod <- 
  setType(1.613 / 100, "rate")

################################################################################
# COSTS

v2$costs <- setType(c(
  inviteToScreen=1.80, 
  requireReinvitation=1.80, 
  screen=34.11, 
  monitor=72.03,
  monitorFollowingContraindication=72.03,   # NB 
  consultation=328.64,
  electiveSurgeryEvar=13844,  # NB order (evar, open) 
  electiveSurgeryOpen=13060,
  emergencySurgeryEvar=16154,
  emergencySurgeryOpen=17613,
  reinterventionAfterElectiveEvar=7546,
  reinterventionAfterElectiveOpen=8986,
  reinterventionAfterEmergencyEvar=7546,
  reinterventionAfterEmergencyOpen=8986,
  monitorFollowingEvarSurgery=258.16,
  monitorFollowingOpenSurgery=196.79
), type="costs")



################################################################################
# MISCELLANEOUS

# Non-AAA mortality rate
v1other$nonAaaDeathMethod <- "onsIntegerStart"
v1other$nonAaaMortalityRatesFileName <- 
  "input/nonAaaDeathMortalityRatesForWomen.csv" 

# Overall QoL / utilities
v1other <- compactList(append(v1other, createQalyFactors(
  startAge=v1other$startAge,
  qalyFactorBoundariesAsAges=c(25, 35, 45, 55, 65, 75), 
  qalyFactorsForAges=c(Inf, Inf, Inf, Inf, 0.81, 0.78, 0.71)
  # These Infs are so that it gives clearly wrong results if ages 
  # below 55 are accidentally used.
)))


# Discount rates
v1other$lifeYearDiscountRate <- 3.5 / 100
v1other$costDiscountRate <- 3.5 / 100



################################################################################



