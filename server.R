
library(shiny)
library(parallel)
library(ggplot2)
library(viridis)
## other libraries that may come in useful...
# library(shinyjs)
# library(doParallel)
# library(DT)


shinyServer(function(input, output) {
  source("./functions/allFunctions.R",local=T)

  doNull<-function(object,fun,...){
    if(!is.null(object)){
      do.call(match.fun(fun),list(object,...))
    } else {
      NULL
    }
  }
  
  ## modelInputs uses either the user-specified inputs from the UI or if the radio button is clicked it will replace with default values (i.e. from parametersGlover2014.R)  
  modelInputs<-reactive({
    
    if(input$inputType==2){ ## Reference model for women
      v0<- compactList() 
      v1other <- compactList() 
      v2 <- compactList()
      source("./input/parsForWomen30years_SA.3.0.R",local=T) 
      ## Settings
      v0$numberOfPersons <- 2e4
      v0$returnEventHistories <- TRUE  # these two are so that countEvents will work
      v0$recordSizes <- TRUE
      v0$returnAllPersonsQuantities <- TRUE
      v0$method <- "parallelBatches"
      v0$randomSeed <- 2
      v0$numberOfProcesses <- detectCores()-1 ## use number cores minus 1 to allow machine to still function
      v0 <- setUnspecifiedElementsOfv0(v0)
      
    } else if(input$inputType==3){ ## Best alternative screening strategy
      v0<- compactList() 
      v1other <- compactList() 
      v2 <- compactList()
      source("./input/parsForWomen30years_SA.4.1.R",local=T) 
      ## Settings
      v0$numberOfPersons <- 2e4
      v0$returnEventHistories <- TRUE  # these two are so that countEvents will work
      v0$recordSizes <- TRUE
      v0$returnAllPersonsQuantities <- TRUE
      v0$method <- "parallelBatches"
      v0$randomSeed <- 2
      v0$numberOfProcesses <- detectCores()-1
      v0 <- setUnspecifiedElementsOfv0(v0)
    } else if(input$inputType==4){ ## Contemporary model (Glover 2014)
      # Aorta model, including parameters for PSA. 
      v0<- compactList() 
      v1other <- compactList() 
      v2 <- compactList()
      source("./input/parsForMen30yearsUpdated_updated.R",local=T) 
      ## Settings
      v0$numberOfPersons <- 2e4
      v0$returnEventHistories <- TRUE  # these two are so that countEvents will work
      v0$recordSizes <- TRUE
      v0$returnAllPersonsQuantities <- TRUE
      v0$method <- "parallelBatches"
      v0$randomSeed <- 2
      v0$numberOfProcesses <- detectCores()-1
      v0 <- setUnspecifiedElementsOfv0(v0)
    } else if(input$inputType==5){ ## 4-year model for men
      # Aorta model, including parameters for PSA. 
      v0<- compactList() 
      v1other <- compactList() 
      v2 <- compactList()
      source("./input/parsForMen4yearsValidation_updated.R",local=T) 
      ## Settings
      v0$numberOfPersons <- 2e4
      v0$returnEventHistories <- TRUE  # these two are so that countEvents will work
      v0$recordSizes <- TRUE
      v0$returnAllPersonsQuantities <- TRUE
      v0$method <- "parallelBatches"
      v0$randomSeed <- 2
      v0$numberOfProcesses <- detectCores()-1
      v0 <- setUnspecifiedElementsOfv0(v0)
    } else {
      v0<-compactList() 
      v1other <- compactList() 
      v2 <- compactList()
    }
    
    return(list(v0=v0,v1other=v1other,v2=v2))
  })
  
  NAAASP.baselineDiameters<- read.csv("input/AAA max measurements 2009 to 2014 MS 17 06 2015.csv", comment.char="#")[, c("size", "pw")]
  names(NAAASP.baselineDiameters) <- c("size", "weight")
  Uppsala.baselineDiameters<- read.csv("input/uppsala_diameters_screening.baseline.diameters.csv", comment.char="#")[, c("size", "pw")]  
  names(Uppsala.baselineDiameters) <- c("size", "weight")

  ## Make dynamic the default values for model parameters point estimates
  ## NUMBER OF SIMULATIONS
  output$modelInputSimulation <- renderUI({
    list(
      numericInput("v0numberOfPersons",value= modelInputs()$v0$numberOfPersons,label="Number of patients in simulation",min=100,step=1)
    )
  })  
  
  ## SCREENING PARAMETERS
  output$modelInputScreening <- renderUI({
    list(
      numericInput("v2probOfRequireReinvitation",value=doNull(modelInputs()$v2$probOfRequireReinvitation,fun="round",digits=4),label="Re-invitation proportion", min=0,max=1,step=0.0001),
      numericInput("v2probOfAttendScreen",value=doNull(modelInputs()$v2$probOfAttendScreen,fun="round",digits=4),label="Attendance proportion", min=0,max=1,step=0.0001),
      numericInput("v2probOfNonvisualization",value=doNull(modelInputs()$v2$probOfNonvisualization,fun="round",digits=4),label="Non-visualisation proportion", min=0,max=1,step=0.0001),    
      selectInput("v1otherbaselineDiameters", label = "AAA size distribution at screening",
                   choices = list("NAAASP" = 1,"Uppsala distribution" = 2), 
                   selected = 1),
      ## CHECK WHY toggleState DOES NOT WORK
      #useShinyjs(),
      #toggleState("v2prevalence",input$reweighted==TRUE),
      #checkboxInput("reweighted", label = "Re-weight AAA distribution?", value = !is.null(modelInputs()$v2$prevalence)), ## if true then we need to provide prevalence estimate
      numericInput("v2prevalence",value=doNull(modelInputs()$v2$prevalence,fun="round",digits=4),label="AAA prevalence (>=3.0cm)", min=0,max=1,step=0.0001)
    )})
  
  
  ## SURVEILLANCE PARAMETERS
  output$modelInputSurveillance <- renderUI({
    list(  
      numericInput("v2rateOfDropoutFromMonitoring",value=doNull(modelInputs()$v2$rateOfDropoutFromMonitoring,fun="round",digits=4),label="Drop-out rate from surveillance (per person-year)", min=0,max=1,step=0.00000001),      
      numericInput("v2rateOfIncidentalDetection",value=doNull(modelInputs()$v2$rateOfIncidentalDetection,fun="round",digits=4),label="Incidental detection rate (per person-year)", min=0,max=1,step=0.00000001)      
    )})
  
  ## SURGICAL PARAMETERS
  output$modelInputElectiveSurgeryIsEvar <- renderUI({
    list(  
      numericInput("v2probOfElectiveSurgeryIsEvarProb",value=doNull(doNull(doNull(modelInputs()$v2$probOfElectiveSurgeryIsOpen["intercept"],fun="-"),fun="plogis"),fun="round",digits=4),label="Probability at age 80, 6.0cm diameter", min=0,max=1,step=0.001),
      numericInput("v2probOfElectiveSurgeryIsEvarORage",value=doNull(doNull(doNull(modelInputs()$v2$probOfElectiveSurgeryIsOpen["age"],fun="-"),fun="exp"),fun="round",digits=3),label="Odds ratio per year increase in age", min=0,step=0.001),
      numericInput("v2probOfElectiveSurgeryIsEvarORdiam",value=doNull(doNull(doNull(modelInputs()$v2$probOfElectiveSurgeryIsOpen["aortaSize"],fun="-"),fun="exp"),fun="round",digits=3),label="Odds ratio per cm increase in aorta diameter", min=0,step=0.001)
    )})
  output$modelInputContraindication <- renderUI({
    list(
      numericInput("v2probOfContraindicationProb",value=doNull(doNull(modelInputs()$v2$probOfContraindication["intercept"],fun="plogis"),fun="round",digits=4),label="Probability at age 80, 6.0cm diameter", min=0,max=1,step=0.001),
      numericInput("v2probOfContraindicationORage",value=doNull(doNull(modelInputs()$v2$probOfContraindication["age"],fun="exp"),fun="round",digits=3),label="Odds ratio per year increase in age", min=0,step=0.001),
      numericInput("v2probOfContraindicationORdiam",value=doNull(doNull(modelInputs()$v2$probOfContraindication["aortaSize"],fun="exp"),fun="round",digits=3),label="Odds ratio per cm increase in aorta diameter", min=0,step=0.001)
    )})
  output$modelInput30dayElectiveEVAR <- renderUI({
    list(
      numericInput("v2probOfAaaDeathInInitialPeriodAfterElectiveEvarSurgeryProb",value=doNull(doNull(modelInputs()$v2$probOfAaaDeathInInitialPeriodAfterElectiveEvarSurgery["intercept"],fun="plogis"),fun="round",digits=4),label="Probability at age 80, 6.0cm diameter", min=0,max=1,step=0.001),
      numericInput("v2probOfAaaDeathInInitialPeriodAfterElectiveEvarSurgeryORage",value=doNull(doNull(modelInputs()$v2$probOfAaaDeathInInitialPeriodAfterElectiveEvarSurgery["age"],fun="exp"),fun="round",digits=3),label="Odds ratio per year increase in age", min=0,step=0.001),
      numericInput("v2probOfAaaDeathInInitialPeriodAfterElectiveEvarSurgeryORdiam",value=doNull(doNull(modelInputs()$v2$probOfAaaDeathInInitialPeriodAfterElectiveEvarSurgery["aortaSize"],fun="exp"),fun="round",digits=3),label="Odds ratio per cm increase in aorta diameter", min=0,step=0.001)
    )})
  output$modelInput30dayElectiveOpen <- renderUI({
    list(
      numericInput("v2probOfAaaDeathInInitialPeriodAfterElectiveOpenSurgeryProb",value=doNull(doNull(modelInputs()$v2$probOfAaaDeathInInitialPeriodAfterElectiveOpenSurgery["intercept"],fun="plogis"),fun="round",digits=4),label="Probability at age 80, 6.0cm diameter", min=0,max=1,step=0.001),
      numericInput("v2probOfAaaDeathInInitialPeriodAfterElectiveOpenSurgeryORage",value=doNull(doNull(modelInputs()$v2$probOfAaaDeathInInitialPeriodAfterElectiveOpenSurgery["age"],fun="exp"),fun="round",digits=3),label="Odds ratio per year increase in age", min=0,step=0.001),
      numericInput("v2probOfAaaDeathInInitialPeriodAfterElectiveOpenSurgeryORdiam",value=doNull(doNull(modelInputs()$v2$probOfAaaDeathInInitialPeriodAfterElectiveOpenSurgery["aortaSize"],fun="exp"),fun="round",digits=3),label="Odds ratio per cm increase in aorta diameter", min=0,step=0.001)
    )})
  output$modelInputLongTermElectiveMortalityRate <- renderUI({
    list(
      numericInput("v2rateOfAaaDeathAfterElectiveEvarSurgeryAndInitialPeriod",value=doNull(modelInputs()$v2$rateOfAaaDeathAfterElectiveEvarSurgeryAndInitialPeriod,fun="round",digits=4),label="Long-term rate following elective EVAR (per person-year)", min=0,max=1,step=0.001),
      numericInput("v2rateOfAaaDeathAfterElectiveOpenSurgeryAndInitialPeriod",value=doNull(modelInputs()$v2$rateOfAaaDeathAfterElectiveOpenSurgeryAndInitialPeriod,fun="round",digits=4),label="Long-term rate following elective Open (per person-year)", min=0,max=1,step=0.001)
    )})
  output$modelInputReintElectiveEVAR <- renderUI({
      list(
        numericInput("v2reinterventionRatesAfterElectiveEvar1",value=doNull(modelInputs()$v2$reinterventionRatesAfterElectiveEvar[1],fun="round",digits=4),label="Re-intervention rate 31-120 days after elective EVAR (per person-year)", min=0,max=1,step=0.001),
        numericInput("v2reinterventionRatesAfterElectiveEvar2",value=doNull(modelInputs()$v2$reinterventionRatesAfterElectiveEvar[2],fun="round",digits=4),label="Re-intervention rate >120 days after elective EVAR (per person-year)", min=0,max=1,step=0.001)
      )})
  output$modelInputReintElectiveOpen <- renderUI({
    list(
      numericInput("v2reinterventionRatesAfterElectiveOpen1",value=doNull(modelInputs()$v2$reinterventionRatesAfterElectiveOpen[1],fun="round",digits=4),label="Re-intervention rate 31-120 days after elective Open (per person-year)", min=0,max=1,step=0.001),
      numericInput("v2reinterventionRatesAfterElectiveOpen2",value=doNull(modelInputs()$v2$reinterventionRatesAfterElectiveOpen[2],fun="round",digits=4),label="Re-intervention rate >120 days after elective Open (per person-year)", min=0,max=1,step=0.001)
    )})
  output$modelInputEmergencySurgeryIfRupture <- renderUI({
    list(  
      numericInput("v2probOfEmergencySurgeryIfRupture",value=doNull(modelInputs()$v2$probOfEmergencySurgeryIfRupture,fun="round",digits=3),label="Probability of emergency operation after rupture", min=0,max=1,step=0.001)
    )})
  output$modelInputEmergencySurgeryIsEvar <- renderUI({
    list(  
      numericInput("v2probOfEmergencySurgeryIsEvarProb",value=doNull(doNull(doNull(modelInputs()$v2$probOfEmergencySurgeryIsOpen["intercept"],fun="-"),fun="plogis"),fun="round",digits=4),label="Probability at age 80, 6.0cm diameter", min=0,max=1,step=0.001),
      numericInput("v2probOfEmergencySurgeryIsEvarORage",value=doNull(doNull(doNull(modelInputs()$v2$probOfEmergencySurgeryIsOpen["age"],fun="-"),fun="exp"),fun="round",digits=3),label="Odds ratio per year increase in age", min=0,step=0.001),
      numericInput("v2probOfEmergencySurgeryIsEvarORdiam",value=doNull(doNull(doNull(modelInputs()$v2$probOfEmergencySurgeryIsOpen["aortaSize"],fun="-"),fun="exp"),fun="round",digits=3),label="Odds ratio per cm increase in aorta diameter", min=0,step=0.001)
    )})
  output$modelInput30dayEmergencyEVAR <- renderUI({
    list(
      numericInput("v2probOfAaaDeathInInitialPeriodAfterEmergencyEvarSurgeryProb",value=doNull(doNull(modelInputs()$v2$probOfAaaDeathInInitialPeriodAfterEmergencyEvarSurgery["intercept"],fun="plogis"),fun="round",digits=4),label="Probability at age 80, 6.0cm diameter", min=0,max=1,step=0.001),
      numericInput("v2probOfAaaDeathInInitialPeriodAfterEmergencyEvarSurgeryORage",value=doNull(doNull(modelInputs()$v2$probOfAaaDeathInInitialPeriodAfterEmergencyEvarSurgery["age"],fun="exp"),fun="round",digits=3),label="Odds ratio per year increase in age", min=0,step=0.001),
      numericInput("v2probOfAaaDeathInInitialPeriodAfterEmergencyEvarSurgeryORdiam",value=doNull(doNull(modelInputs()$v2$probOfAaaDeathInInitialPeriodAfterEmergencyEvarSurgery["aortaSize"],fun="exp"),fun="round",digits=3),label="Odds ratio per cm increase in aorta diameter", min=0,step=0.001)
    )})
  output$modelInput30dayEmergencyOpen <- renderUI({
    list(
      numericInput("v2probOfAaaDeathInInitialPeriodAfterEmergencyOpenSurgeryProb",value=doNull(doNull(modelInputs()$v2$probOfAaaDeathInInitialPeriodAfterEmergencyOpenSurgery["intercept"],fun="plogis"),fun="round",digits=4),label="Probability at age 80, 6.0cm diameter", min=0,max=1,step=0.001),
      numericInput("v2probOfAaaDeathInInitialPeriodAfterEmergencyOpenSurgeryORage",value=doNull(doNull(modelInputs()$v2$probOfAaaDeathInInitialPeriodAfterEmergencyOpenSurgery["age"],fun="exp"),fun="round",digits=3),label="Odds ratio per year increase in age", min=0,step=0.001),
      numericInput("v2probOfAaaDeathInInitialPeriodAfterEmergencyOpenSurgeryORdiam",value=doNull(doNull(modelInputs()$v2$probOfAaaDeathInInitialPeriodAfterEmergencyOpenSurgery["aortaSize"],fun="exp"),fun="round",digits=3),label="Odds ratio per cm increase in aorta diameter", min=0,step=0.001)
    )})
  output$modelInputLongTermEmergencyMortalityRate <- renderUI({
    list(
      numericInput("v2rateOfAaaDeathAfterEmergencyEvarSurgeryAndInitialPeriod",value=modelInputs()$v2$rateOfAaaDeathAfterEmergencyEvarSurgeryAndInitialPeriod,label="Long-term rate following emergency EVAR (per person-year)", min=1e-100,max=1,step=0.001),
      numericInput("v2rateOfAaaDeathAfterEmergencyOpenSurgeryAndInitialPeriod",value=modelInputs()$v2$rateOfAaaDeathAfterEmergencyOpenSurgeryAndInitialPeriod,label="Long-term rate following emergency Open (per person-year)", min=1e-100,max=1,step=0.001)
    )})
  output$modelInputReintEmergencyEVAR <- renderUI({
    list(
      numericInput("v2reinterventionRatesAfterEmergencyEvar1",value=doNull(modelInputs()$v2$reinterventionRatesAfterEmergencyEvar[1],fun="round",digits=4),label="Re-intervention rate 31-120 days after emergency EVAR (per person-year)", min=0,max=1,step=0.001),
      numericInput("v2reinterventionRatesAfterEmergencyEvar2",value=doNull(modelInputs()$v2$reinterventionRatesAfterEmergencyEvar[2],fun="round",digits=4),label="Re-intervention rate >120 days after emergency EVAR (per person-year)", min=0,max=1,step=0.001)
    )})
  output$modelInputReintEmergencyOpen <- renderUI({
    list(
      numericInput("v2reinterventionRatesAfterEmergencyOpen1",value=doNull(modelInputs()$v2$reinterventionRatesAfterEmergencyOpen[1],fun="round",digits=4),label="Re-intervention rate 31-120 days after emergency Open (per person-year)", min=0,max=1,step=0.001),
      numericInput("v2reinterventionRatesAfterEmergencyOpen2",value=doNull(modelInputs()$v2$reinterventionRatesAfterEmergencyOpen[2],fun="round",digits=4),label="Re-intervention rate >120 days after emergency Open (per person-year)", min=0,max=1,step=0.001)
    )})
  
  ## COSTS
  output$modelInputCosts <- renderUI({
    list(
      numericInput("v2costsInviteToScreen",value=doNull(modelInputs()$v2$costs["inviteToScreen"],fun="round",digits=2),label="Invitation to screening (GBP)", min=0,step=0.01),
      numericInput("v2costsRequireReinvitation",value=doNull(modelInputs()$v2$costs["requireReinvitation"],fun="round",digits=2),label="Re-invitation to screening (GBP)", min=0,step=0.01),
      numericInput("v2costsScreen",value=doNull(modelInputs()$v2$costs["screen"],fun="round",digits=2),label="Screening scan (GBP)", min=0,step=0.01),
      numericInput("v2costsMonitor",value=doNull(modelInputs()$v2$costs["monitor"],fun="round",digits=2),label="Surveillance scan (GBP)", min=0,step=0.01),
      numericInput("v2costsMonitorFollowingContraindication",value=doNull(modelInputs()$v2$costs["monitorFollowingContraindication"],fun="round",digits=2),label="Surveillance scan after non-intervention (GBP)", min=0,step=0.01),
      numericInput("v2costsConsultation",value=doNull(modelInputs()$v2$costs["consultation"],fun="round",digits=2),label="Consultation for elective surgery (GBP)", min=0,step=0.01),
      numericInput("v2costsElectiveSurgeryEvar",value=doNull(modelInputs()$v2$costs["electiveSurgeryEvar"],fun="round",digits=2),label="Elective EVAR (GBP)", min=0,step=0.01),
      numericInput("v2costsElectiveSurgeryOpen",value=doNull(modelInputs()$v2$costs["electiveSurgeryOpen"],fun="round",digits=2),label="Elective Open repair (GBP)", min=0,step=0.01),
      numericInput("v2costsEmergencySurgeryEvar",value=doNull(modelInputs()$v2$costs["emergencySurgeryEvar"],fun="round",digits=2),label="Emergency EVAR repair (GBP)", min=0,step=0.01),
      numericInput("v2costsEmergencySurgeryOpen",value=doNull(modelInputs()$v2$costs["emergencySurgeryOpen"],fun="round",digits=2),label="Emergency Open repair (GBP)", min=0,step=0.01),
      numericInput("v2costsReinterventionAfterElectiveEvar",value=doNull(modelInputs()$v2$costs["reinterventionAfterElectiveEvar"],fun="round",digits=2),label="Re-intervention after elective EVAR (GBP)", min=0,step=0.01),
      numericInput("v2costsReinterventionAfterElectiveOpen",value=doNull(modelInputs()$v2$costs["reinterventionAfterElectiveOpen"],fun="round",digits=2),label="Re-intervention after elective Open repair (GBP)", min=0,step=0.01),
      numericInput("v2costsReinterventionAfterEmergencyEvar",value=doNull(modelInputs()$v2$costs["reinterventionAfterEmergencyEvar"],fun="round",digits=2),label="Re-intervention after emergency EVAR (GBP)", min=0,step=0.01),
      numericInput("v2costsReinterventionAfterEmergencyOpen",value=doNull(modelInputs()$v2$costs["reinterventionAfterEmergencyOpen"],fun="round",digits=2),label="Re-intervention after emergency Open repair (GBP)", min=0,step=0.01),
      numericInput("v2costsMonitorFollowingEvarSurgery",value=doNull(modelInputs()$v2$costs["monitorFollowingEvarSurgery"],fun="round",digits=2),label="Annual surveillance cost following EVAR (GBP)", min=0,step=0.01),
      numericInput("v2costsMonitorFollowingOpenSurgery",value=doNull(modelInputs()$v2$costs["monitorFollowingOpenSurgery"],fun="round",digits=2),label="Surveillance cost (at 6 weeks) following Open repair (GBP)", min=0,step=0.01)
    )})
  
  ########### Other model parameters
  ## OTHER Simulation parameters
  output$modelInputSimulationExtra <- renderUI({
    list(
      #checkboxInput("v0returnEventHistories", label = "Count events (output table may take a long time to appear)", value = modelInputs()$v0$returnEventHistories), ## if true then we need to summarise information
      numericInput("v0randomSeed", value=modelInputs()$v0$randomSeed,label="Random seed",min=1,step=1),
      selectInput("v0method", label = "Serial / parallel processing",
                  choices = list("Serial" = 1,"Parallel" = 2), 
                  selected = 2),
      numericInput("v0numberOfProcesses",value=modelInputs()$v0$numberOfProcesses,label="Number of parallel processes",min=1,step=1)    
    )
  })
  output$modelInputGrowth <- renderUI({
    list(
      ## TO DO: CREATE GREEK SYMBOLS. SEE HTML("&rho;: Correlation between intercept and slope")
      numericInput("v2beta0", value=modelInputs()$v2$beta0,label="beta0: Average log aortic diameter at time of screening (log(cm))",step=0.001),
      numericInput("v2beta1", value=modelInputs()$v2$beta1,label="beta1: Average rate of log diameter growth (log(cm)/year)",step=0.001),
      numericInput("v2sigma0", value=modelInputs()$v2$sigma0,label="sigma0: Between-person standard deviation of intercepts",min=0,step=0.001),
      numericInput("v2sigma1", value=modelInputs()$v2$sigma1,label="sigma1: Between-person standard deviation of rate of growth",min=0,step=0.001),
      numericInput("v2rho", value=modelInputs()$v2$rho,label="rho: Correlation between intercept and slope",min=-1,max=1,step=0.001),
      numericInput("v2sigmaW", value=modelInputs()$v2$sigmaW,label="sigma_w: Residual standard deviation",min=0,step=0.001)    
    )
  })
  output$modelInputRupture <- renderUI({
    list(
      ## TO DO: CREATE GREEK SYMBOLS. SEE HTML("&rho;: Correlation between intercept and slope")
      numericInput("v2gamma", value=modelInputs()$v2$gamma,label="gamma: log baseline hazard of rupture",step=0.001),
      numericInput("v2alpha", value=modelInputs()$v2$alpha,label="alpha: log hazard ratio associated with one unit increase in log aortic diameter",step=0.001)
    )
  })
  output$modelInputSurveillanceExtra <- renderUI({
    list(
      numericInput("v1otherwaitingTimeToConsultationDays",value=doNull(365.25*modelInputs()$v1other$waitingTimeToConsultation,fun="round",digits=1),label="Delay from 5.5+cm scan to consultation (days)", min=0,step=0.1),
      numericInput("v1otherwaitingTimeToElectiveSurgery",value=doNull(365.25*modelInputs()$v1other$waitingTimeToElectiveSurgery,fun="round",digits=1),label="Delay from consultation to elective surgery (days)", min=0,step=0.1)                
    )
  })
  output$modelInputMiscellaneous <- renderUI({
    list(
      numericInput("v1otherlifeYearDiscountRate",value=modelInputs()$v1other$lifeYearDiscountRate,label="Discount rate for life-years", min=0,max=1,step=0.001),
      numericInput("v1othercostDiscountRate",value=modelInputs()$v1other$costDiscountRate,label="Discount rate for costs", min=0,max=1,step=0.001),
      numericInput("v2extraDiameterForCtScan",value=modelInputs()$v2$extraDiameterForCtScan,label="Mean difference between CT and US diameter (cm)", step=0.001),
      numericInput("v2ctMeasurementErrorSD",value=modelInputs()$v2$ctMeasurementErrorSD,label="Measurement error (SD) of CT scan (cm)", min=0,step=0.01)
    )
  })
 
  v0<-reactive({
    v0<-modelInputs()$v0  
    if(!is.null(input$v0numberOfPersons)){
      v0$numberOfPersons<-input$v0numberOfPersons
    }
    if(!is.null(input$v0returnEventHistories)){
      v0$returnEventHistories<-input$v0returnEventHistories
    }
    if(!is.null(input$v0randomSeed)){
      v0$randomSeed<-input$v0randomSeed
    }
    if(!is.null(input$v0numberOfProcesses)){
      v0$numberOfProcesses<-input$v0numberOfProcesses
    }
    if(!is.null(input$v0method)){
      if(input$v0method==1){ ## Serial
        v0$method<-"serial"
      } else {
        v0$method<-"parallelBatches"
      }
    }
    v0  
  })
  v1other<-reactive({
    v1other<-modelInputs()$v1other  
    if(!is.null(input$v1otherwaitingTimeToConsultationDays)){
      v1other$waitingTimeToConsultation<-input$v1otherwaitingTimeToConsultationDays/365.25
    }
    if(!is.null(input$v1otherwaitingTimeToElectiveSurgery)){
      v1other$waitingTimeToElectiveSurgery<-input$v1otherwaitingTimeToElectiveSurgery/365.25
    }
    if(!is.null(input$v1otherlifeYearDiscountRate)){
      v1other$lifeYearDiscountRate<-input$v1otherlifeYearDiscountRate
    }
    if(!is.null(input$v1othercostDiscountRate)){
      v1other$costDiscountRate<-input$v1othercostDiscountRate
    }
    if(!is.null(input$v1otherbaselineDiameters)){
      if(input$v1otherbaselineDiameters==1){ ## NAAASP
        v1other$baselineDiameters<-NAAASP.baselineDiameters   
      } else {
        v1other$baselineDiameters<-Uppsala.baselineDiameters
      }
    }
    v1other  
  })
  v2<-reactive({
    v2<-modelInputs()$v2
    if(!is.null(input$v2probOfRequireReinvitation)){
      v2$probOfRequireReinvitation<-setType(input$v2probOfRequireReinvitation, "probability")
    }
    if(!is.null(input$v2probOfAttendScreen)){
      v2$probOfAttendScreen<-setType(input$v2probOfAttendScreen,"probability")
    }
    if(!is.null(input$v2probOfNonvisualization)){
      v2$probOfNonvisualization<-setType(input$v2probOfNonvisualization,"probability")
    }
    if(!is.null(input$v2prevalence)){
      v2$prevalence<-setType(input$v2prevalence,"probability")
    }
    if(!is.null(input$v2rateOfDropoutFromMonitoring)){
      v2$rateOfDropoutFromMonitoring<-setType(input$v2rateOfDropoutFromMonitoring,"rate")
    }
    if(!is.null(input$v2rateOfIncidentalDetection)){
      v2$rateOfIncidentalDetection<-setType(input$v2rateOfIncidentalDetection,"rate")
    }
    if(!is.null(input$v2beta0)){
      v2$beta0<-setType(input$v2beta0,"par for aorta model")
    }
    if(!is.null(input$v2beta1)){
      v2$beta1<-setType(input$v2beta1,"par for aorta model")
    }
    if(!is.null(input$v2sigma0)){
      v2$sigma0<-setType(input$v2sigma0,"par for aorta model")
    }
    if(!is.null(input$v2sigma1)){
      v2$sigma1<-setType(input$v2sigma1,"par for aorta model")
    }
    if(!is.null(input$v2rho)){
      v2$rho<-setType(input$v2rho,"par for aorta model")
    }
    if(!is.null(input$v2sigmaW)){
      v2$sigmaW<-setType(input$v2sigmaW,"par for aorta model")
    }
    if(!is.null(input$v2gamma)){
      v2$gamma<-setType(input$v2gamma,"par for aorta model")
    }
    if(!is.null(input$v2alpha)){
      v2$alpha<-setType(input$v2alpha,"par for aorta model")
    }
    if(!is.null(input$v2extraDiameterForCtScan)){
      v2$extraDiameterForCtScan<-setType(input$v2extraDiameterForCtScan,"fixed value")
    }
    if(!is.null(input$v2ctMeasurementErrorSD)){
      v2$ctMeasurementErrorSD<-setType(input$v2ctMeasurementErrorSD,"fixed value")
    }
    if(!is.null(input$v2probOfElectiveSurgeryIsEvarProb)){
      v2$probOfElectiveSurgeryIsOpen<- 
        setType(c(intercept = qlogis(1-input$v2probOfElectiveSurgeryIsEvarProb), 
                  age = -log(input$v2probOfElectiveSurgeryIsEvarORage), 
                  aortaSize = -log(input$v2probOfElectiveSurgeryIsEvarORdiam)), 
                "logistic model for probability")
    }
    if(!is.null(input$v2probOfContraindicationProb)){
      v2$probOfContraindication<- 
        setType(c(intercept = qlogis(input$v2probOfContraindicationProb), 
                  age = log(input$v2probOfContraindicationORage), 
                  aortaSize = log(input$v2probOfContraindicationORdiam)), 
                "logistic model for probability")
    }    
    if(!is.null(input$v2probOfAaaDeathInInitialPeriodAfterElectiveEvarSurgeryProb)){
      v2$probOfAaaDeathInInitialPeriodAfterElectiveEvarSurgery<- 
        setType(c(intercept = qlogis(input$v2probOfAaaDeathInInitialPeriodAfterElectiveEvarSurgeryProb), 
                  age = log(input$v2probOfAaaDeathInInitialPeriodAfterElectiveEvarSurgeryORage), 
                  aortaSize = log(input$v2probOfAaaDeathInInitialPeriodAfterElectiveEvarSurgeryORdiam)), 
                "logistic model for probability")
    }
    if(!is.null(input$v2probOfAaaDeathInInitialPeriodAfterElectiveOpenSurgeryProb)){
      v2$probOfAaaDeathInInitialPeriodAfterElectiveOpenSurgery<- 
        setType(c(intercept = qlogis(input$v2probOfAaaDeathInInitialPeriodAfterElectiveOpenSurgeryProb), 
                  age = log(input$v2probOfAaaDeathInInitialPeriodAfterElectiveOpenSurgeryORage), 
                  aortaSize = log(input$v2probOfAaaDeathInInitialPeriodAfterElectiveOpenSurgeryORdiam)), 
                "logistic model for probability")
    }    
    if(!is.null(input$v2rateOfAaaDeathAfterElectiveEvarSurgeryAndInitialPeriod)){
      v2$rateOfAaaDeathAfterElectiveEvarSurgeryAndInitialPeriod<-setType(input$v2rateOfAaaDeathAfterElectiveEvarSurgeryAndInitialPeriod,"rate")
    }
    if(!is.null(input$v2rateOfAaaDeathAfterElectiveOpenSurgeryAndInitialPeriod)){
      v2$rateOfAaaDeathAfterElectiveOpenSurgeryAndInitialPeriod<-setType(input$v2rateOfAaaDeathAfterElectiveOpenSurgeryAndInitialPeriod,"rate")
    }
    if(!is.null(input$v2reinterventionRatesAfterElectiveEvar1)){
      v2$reinterventionRatesAfterElectiveEvar[1]<-setType(input$v2reinterventionRatesAfterElectiveEvar1,"reintervention rates")
      v2$reinterventionRatesAfterElectiveEvar[2]<-setType(input$v2reinterventionRatesAfterElectiveEvar2,"reintervention rates")
    }
    if(!is.null(input$v2reinterventionRatesAfterElectiveOpen1)){
      v2$reinterventionRatesAfterElectiveOpen[1]<-setType(input$v2reinterventionRatesAfterElectiveOpen1,"reintervention rates")
      v2$reinterventionRatesAfterElectiveOpen[2]<-setType(input$v2reinterventionRatesAfterElectiveOpen2,"reintervention rates")
    }
    if(!is.null(input$v2probOfEmergencySurgeryIfRupture)){
      v2$probOfEmergencySurgeryIfRupture<-setType(input$v2probOfEmergencySurgeryIfRupture,"probability")
    }
    if(!is.null(input$v2probOfEmergencySurgeryIsEvarProb)){
      v2$probOfEmergencySurgeryIsOpen<- 
        setType(c(intercept = qlogis(1-input$v2probOfEmergencySurgeryIsEvarProb), 
                  age = -log(input$v2probOfEmergencySurgeryIsEvarORage), 
                  aortaSize = -log(input$v2probOfEmergencySurgeryIsEvarORdiam)), 
                "logistic model for probability")
    }
    if(!is.null(input$v2probOfAaaDeathInInitialPeriodAfterEmergencyEvarSurgeryProb)){
      v2$probOfAaaDeathInInitialPeriodAfterEmergencyEvarSurgery<- 
        setType(c(intercept = qlogis(input$v2probOfAaaDeathInInitialPeriodAfterEmergencyEvarSurgeryProb), 
                  age = log(input$v2probOfAaaDeathInInitialPeriodAfterEmergencyEvarSurgeryORage), 
                  aortaSize = log(input$v2probOfAaaDeathInInitialPeriodAfterEmergencyEvarSurgeryORdiam)), 
                "logistic model for probability")
    }
    if(!is.null(input$v2probOfAaaDeathInInitialPeriodAfterEmergencyOpenSurgeryProb)){
      v2$probOfAaaDeathInInitialPeriodAfterEmergencyOpenSurgery<- 
        setType(c(intercept = qlogis(input$v2probOfAaaDeathInInitialPeriodAfterEmergencyOpenSurgeryProb), 
                  age = log(input$v2probOfAaaDeathInInitialPeriodAfterEmergencyOpenSurgeryORage), 
                  aortaSize = log(input$v2probOfAaaDeathInInitialPeriodAfterEmergencyOpenSurgeryORdiam)), 
                "logistic model for probability")
    }
    if(!is.null(input$v2rateOfAaaDeathAfterEmergencyEvarSurgeryAndInitialPeriod)){
      v2$rateOfAaaDeathAfterEmergencyEvarSurgeryAndInitialPeriod<-setType(input$v2rateOfAaaDeathAfterEmergencyEvarSurgeryAndInitialPeriod,"rate")
    }
    if(!is.null(input$v2rateOfAaaDeathAfterEmergencyOpenSurgeryAndInitialPeriod)){
      v2$rateOfAaaDeathAfterEmergencyOpenSurgeryAndInitialPeriod<-setType(input$v2rateOfAaaDeathAfterEmergencyOpenSurgeryAndInitialPeriod,"rate")
    }
    if(!is.null(input$v2reinterventionRatesAfterEmergencyEvar1)){
      v2$reinterventionRatesAfterEmergencyEvar[1]<-setType(input$v2reinterventionRatesAfterEmergencyEvar1,"reintervention rates")
      v2$reinterventionRatesAfterEmergencyEvar[2]<-setType(input$v2reinterventionRatesAfterEmergencyEvar2,"reintervention rates")
    }
    if(!is.null(input$v2reinterventionRatesAfterEmergencyOpen1)){
      v2$reinterventionRatesAfterEmergencyOpen[1]<-setType(input$v2reinterventionRatesAfterEmergencyOpen1,"reintervention rates")
      v2$reinterventionRatesAfterEmergencyOpen[2]<-setType(input$v2reinterventionRatesAfterEmergencyOpen2,"reintervention rates")
    }
    if(!is.null(input$v2costsInviteToScreen)){
      v2$costs<-setType(c(inviteToScreen=input$v2costsInviteToScreen,
                          requireReinvitation=input$v2costsRequireReinvitation,
                          screen=input$v2costsScreen,
                          monitor=input$v2costsMonitor,
                          monitorFollowingContraindication=input$v2costsMonitorFollowingContraindication,
                          consultation=input$v2costsConsultation,
                          electiveSurgeryEvar=input$v2costsElectiveSurgeryEvar,
                          electiveSurgeryOpen=input$v2costsElectiveSurgeryOpen,
                          emergencySurgeryEvar=input$v2costsEmergencySurgeryEvar,
                          emergencySurgeryOpen=input$v2costsEmergencySurgeryOpen,
                          reinterventionAfterElectiveEvar=input$v2costsReinterventionAfterElectiveEvar,
                          reinterventionAfterElectiveOpen=input$v2costsReinterventionAfterElectiveOpen,
                          reinterventionAfterEmergencyEvar=input$v2costsReinterventionAfterEmergencyEvar,
                          reinterventionAfterEmergencyOpen=input$v2costsReinterventionAfterEmergencyOpen,
                          monitorFollowingEvarSurgery=input$v2costsMonitorFollowingEvarSurgery,
                          monitorFollowingOpenSurgery=input$v2costsMonitorFollowingOpenSurgery),
                        type="costs"

      )
    }

    
    
    
    v2
  })
  
  
 
  
  # Outputs
  output$v0<-renderPrint({v0()})
  output$v1other<-renderPrint({v1other()})
  output$v2<-renderPrint({v2()})
  
 
  # Analyses. 
  result <- observeEvent(input$run,{
    
    
    
    # Create a callback function to update progress.
    # Each time this is called:
    # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
    #   distance. If non-NULL, it will set the progress to that value.
    # - It also accepts optional detail text.
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 5
      }
      progress$set(value = value, detail = detail)
    }
    
    
    # Create a Progress object
    progress <- shiny::Progress$new(min=0,max=1)
    progress$set(message = "Running simulation", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    res<-processPersonsControlOnly(v0(), v1other(), v2(),updateProgress)
    res.sampled<-processPersonsAboveDiagnosisThreshold(v0(), v1other(), v2(), 
                                                       threshold=v1other()$aortaDiameterThresholds[1], updateProgress)
    
    # res<-processPersonsControlOnly(v0, v1other, v2) 
    # res.sampled<-processPersonsAboveDiagnosisThreshold(v0, v1other, v2, threshold=v1other$aortaDiameterThresholds[1]) 
    
    
    output$complete<-reactive({
      1
    })
    outputOptions(output, 'complete', suspendWhenHidden=FALSE)
    
    ## Main cost-effectiveness table
    tab<-tab.ce(res,res.sampled)
    output$lifeyears<-renderTable(
      tab,digits=4,rownames=F,striped=T,hover=T,bordered=T,align="c",na=""
    )
    #output$lifeyears<-DT::renderDataTable(datatable(tab) %>% formatStyle(
    #  backgroundColor = "gray"
    #), server=FALSE)
    
    
    ## Table of events
    tab2<-tab.events(res,res.sampled,v0=v0(),v1other=v1other())
    output$events<-renderTable(
      tab2,rownames=F,striped=T,hover=T,bordered=T,align="c",na=""
    )
    
    ## Event vs. diameter 2d density plots
    ##data<-eventsPlotData(res,events=c("rupture","aaaDeath","electiveSurgeryEvar","electiveSurgeryOpen","emergencySurgeryEvar","emergencySurgeryOpen","incidentalDetection"),treatmentGroup="noScreening")
    data<-eventsPlotData(res,res.sampled,events=c("rupture","aaaDeath","electiveSurgeryEvar","electiveSurgeryOpen","emergencySurgeryEvar","emergencySurgeryOpen","incidentalDetection"),threshold=v1other()$aortaDiameterThresholds[1])
    
    ## data<-eventsPlotData(res,res.sampled,events=c("rupture","aaaDeath","electiveSurgeryEvar","electiveSurgeryOpen","emergencySurgeryEvar","emergencySurgeryOpen","incidentalDetection"),threshold=v1other$aortaDiameterThresholds[1])
    ## eventsPlot(data,event="rupture",v1other=v1other)
    ##eventsPlot(data,event="incidentalDetection",v1other=v1other)
    
    ## eventsPlot(data,event="rupture",v1other=v1other)
    output$plot.event<-renderPlot({
      eventsPlot(data,event=input$eventplot,v1other=v1other())
    })
    
    ## Convergence plot
    plot1<-conv.plot(res.sampled)
    output$plot.conv.LY<-renderPlot({
      plot1$LY
    })
    output$plot.conv.cost<-renderPlot({
      plot1$Cost
    })
    output$plot.conv.ICER<-renderPlot({
      plot1$ICER
    })
    
    plot2<-reactive({
      if(input$extraplot==2){
        other.conv.plot(res,event="incidentallyDetected",v1other=v1other())
      } else if(input$extraplot==3) {
        other.conv.plot(res,event="electiveRepair",v1other=v1other())
      } else if(input$extraplot==4) {
        other.conv.plot(res,event="contraindicated",v1other=v1other())
      } else if(input$extraplot==5) {
        other.conv.plot(res,event="rupture",v1other=v1other())
      } else if(input$extraplot==6) {
        other.conv.plot(res,event="emergencyRepair",v1other=v1other())
      } else if(input$extraplot==7) {
        other.conv.plot(res,event="aaaDeath",v1other=v1other())
      } else if(input$extraplot==8) {
        other.conv.plot(res,event="nonAaaDeath",v1other=v1other())
      }  else {
        NULL
      }
    })
    
    output$plot.conv.extra<-renderPlot({
      plot2()
    })
    
    
    
  })
  
})
