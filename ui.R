library(shinythemes)

shinyUI(
  fluidPage( theme = shinytheme("superhero") ,
    
      
    titlePanel(
      HTML('<img src="SWAN logo.jpg", align="center", height=70/> <img src="NIHR.png", align="right", height=70/> AAA Screening Simulation Model'),
      windowTitle="AAA Screening Simulation Model"
    ),
    tabsetPanel(
      tabPanel("Inputs",
               fluidRow(
                 column(3,
                        wellPanel(
                          radioButtons("inputType", label = h3("Model parameters"),
                                       choices = list(#"User defined" = 1,
                                         "Reference model for women (invite age 65, AAA 3.0cm, referral for repair 5.5cm)" = 2, 
                                         "Best alternative screening strategy for women (invite age 70, AAA 2.5cm, referral for repair 5.0cm)" = 3,
                                         "30-year contemporary model for men" = 4),
                                                      #"4-year model for men (validating to MASS)" = 5), 
                                       selected = 2)
                        ),
                          
                        wellPanel(
                          h4("Simulation parameters"),
                          uiOutput("modelInputSimulation")
                        ),
                        
                        wellPanel(
                          h4("Screening parameters"),
                          uiOutput("modelInputScreening")
                        ),
                        wellPanel(
                          h4("Surveillance parameters"),
                          uiOutput("modelInputSurveillance")
                        ),
                        wellPanel(
                          h4("Elective surgery parameters"),
                           wellPanel(
                            h4("Probability of non-intervention"),  
                            uiOutput("modelInputContraindication")
                          ),
                          wellPanel(
                            h4("Probability of receiving elective EVAR"),  
                            uiOutput("modelInputElectiveSurgeryIsEvar")
                          ),
                          wellPanel(
                            h4("Elective EVAR 30-day operative mortality"),  
                            uiOutput("modelInput30dayElectiveEVAR")
                          ),
                          wellPanel(
                            h4("Elective Open repair 30-day operative mortality"),  
                            uiOutput("modelInput30dayElectiveOpen")
                          ),
                          wellPanel(
                            h4("Long-term (>30 day) AAA-related mortality"),  
                            uiOutput("modelInputLongTermElectiveMortalityRate")
                          ),
                          wellPanel(
                            h4("Re-intervention rate after Elective EVAR"),
                            uiOutput("modelInputReintElectiveEVAR")
                          ),
                          wellPanel(
                            h4("Re-intervention rate after Elective Open repair"),
                            uiOutput("modelInputReintElectiveOpen")
                          )
                        ),
                        wellPanel(
                          h4("Emergency surgery parameters"),
                          uiOutput("modelInputEmergencySurgeryIfRupture"),
                          wellPanel(
                            h4("Probability of receiving emergency EVAR"),  
                            uiOutput("modelInputEmergencySurgeryIsEvar")
                          ),
                          wellPanel(
                            h4("Emergency EVAR 30-day operative mortality"),  
                            uiOutput("modelInput30dayEmergencyEVAR")
                          ),
                          wellPanel(
                            h4("Emergency Open repair 30-day operative mortality"),  
                            uiOutput("modelInput30dayEmergencyOpen")
                          ),
                          wellPanel(
                            h4("Long-term (>30 day) AAA-related mortality"),  
                            uiOutput("modelInputLongTermEmergencyMortalityRate")
                          ),
                          wellPanel(
                            h4("Re-intervention rate after Emergency EVAR"),
                            uiOutput("modelInputReintEmergencyEVAR")
                          ),
                          wellPanel(
                            h4("Re-intervention rate after Emergency Open repair"),
                            uiOutput("modelInputReintEmergencyOpen")
                          )
                        ),
                        wellPanel(
                          h4("Costs"),
                          uiOutput("modelInputCosts")
                        )
                        
                 ),
                 column(9,
                        #h5("Output"),
                        #h5("v0"),
                        #textOutput("v0"),
                        #h5("v1other"),
                        #textOutput("v1other"),
                        #h5("v2"),
                        #textOutput("v2"),
                        actionButton("run", label = "Run Model"),
                        conditionalPanel(
                          condition = "output.complete==1",
                          h3("Life-years, costs and incremental cost-effectiveness")
                        ),
                        conditionalPanel(
                          condition = "output.complete==1",
                          tableOutput("lifeyears")
                          #DT::dataTableOutput('lifeyears')
                        ),
                        conditionalPanel(
                          condition = "output.complete==1",
                          h3("Table of events")
                        ),
                        conditionalPanel(
                          condition = "output.complete==1",
                          tableOutput("events")  
                        ),
                        conditionalPanel(
                          condition= "output.complete==1",
                          selectInput("eventplot", label = h3('Timing and AAA size of specific events' ),
                                      choices = list("Incidental AAA detection"="incidentalDetection", 
                                                     "Elective EVAR AAA repair" = "electiveSurgeryEvar",
                                                     "Elective Open AAA repair" = "electiveSurgeryOpen",
                                                     "AAA rupture"="rupture",
                                                     "Emergency EVAR AAA repair"="emergencySurgeryEvar", 
                                                     "Emergency Open AAA repair"="emergencySurgeryOpen", 
                                                     "AAA-related death"="aaaDeath"), 
                                      selected = "rupture"),
                          plotOutput("plot.event")
                        )
                 )
               )
      ),
      
      
      tabPanel("Other model parameters" ,
               fluidRow(
                 column(12,
                        wellPanel(
                          h4("Simulation parameters"),
                          uiOutput("modelInputSimulationExtra")
                        ),
                        wellPanel(
                          h4("Growth and rupture rate model parameters"),
                          h5("Growth rate parameters"),
                          uiOutput("modelInputGrowth"),
                          h5("Rupture rate parameters"),
                          uiOutput("modelInputRupture")
                        ),
                        wellPanel(
                          h4("Surveillance parameters"),
                          uiOutput("modelInputSurveillanceExtra")
                        ),
                        wellPanel(
                          h4("Miscellaneous parameters"),
                          uiOutput("modelInputMiscellaneous")
                        )
                 )
               )
      ) , 
      tabPanel("Instructions" , 
               h3('Background'),
               h4('This web app allows users to generate events, costs and outcomes from a group of individuals invited to Abdominal Aortic
Aneurysm (AAA) screening compared to a group of individuals not invited. The app runs the discrete event simulation
                        model described in', a('Glover et al, Medical Decision Making 2018; 38(4): 491-451', href="https://dx.doi.org/10.1177/0272989X17753380"), 
                  'and implemented in', a('Sweeting et al, Lancet 2018.', href="http://dx.doi.org/10.1016/S0140-6736(18)31222-4")),
               h3('Default models'),
               h4('Users can choose to implement one of two models for women based on relevant women-specific parameters obtained from sources
including systematic literature reviews, national registry or administrative databases, major AAA surgery trials, and
   UK National Health Service reference costs', a('(Sweeting et al, Lancet 2018).', href="http://dx.doi.org/10.1016/S0140-6736(18)31222-4"),'The two models that can be chosen are the "Reference" model, which
implements a screening programme using the same screening protocol as for 65-year-old men
in the UK National AAA Screening Programme, or an "Best alternative strategy" that screens women at age 70 and lowers the diagnosis
threshold to 2.5cm, and the intervention threshold to 5.0cm. Alternatively, users can choose to implement a contemporary model for 
   men', a('Glover et al, Medical Decision Making 2018; 38(4): 491-451', href="https://dx.doi.org/10.1177/0272989X17753380")),
               h3('Running the model'),
               h4('Just press the "Run model" button in the Inputs tab. A progress bar will appear at the bottom of the screen. Please be patient if you have set the simulation to run over many thousands of pairs!'),
               h3('Number of simulations and accuracy of results'),
               h4('The default number of simulated pairs of individuals is set to 20,000, which should take around 90 seconds to run on a quad-core 2.4GHz processor. HOWEVER, convergence plots must be checked as 20,000 pairs of individuals may not provide sufficient accuracy. 
  There is more accuracy in the differences between the two groups (e.g. in incremental numbers of events, costs and the incremental cost-effectiveness ratio).
   This is because differences are calculated from seletively sampling patients over the diagnosis threshold, hence providing more accurate results for quantitities related to AAA. However, the absolute number of events in the "not invited to screening" group is
   calculated by sampling from the whole population. A sample size of 20,000 pairs of individuals may therefore not be sufficient to get accurate measurements of absolute numbers, especially if the prevalence of AAA is small.'),
               h3('Convergence checks'),
               h4('Convergence of incremental life-years, costs and the ICER are produced automatically in the "Convergence plots" tab.'),
               h3('Changing input parameters'),
               h4('Any of the input parameters can be changed and the model re-run to investigate the impact on model outputs. A 
   full description of the model parameters can be found in the two publications described above.')
      ),
      tabPanel("Convergence plots",
               conditionalPanel(
                 condition = "output.complete==1",
                 h3('Life-years'),
                 plotOutput("plot.conv.LY"),  
                 h3('Costs'),
                 plotOutput("plot.conv.cost"),  
                 h3('Incremental cost-effectiveness ratio (GBP/QALY)'),
                 plotOutput("plot.conv.ICER"),
                 selectInput("extraplot", label = h3('Convergence of specific events in "Not invited to screening" group' ),
                             choices = list(" " = 1,"Incidental AAA detection"=2, "Elective AAA repair" = 3,
                                            "Elective AAA repair contraindicated"=4,"AAA rupture"=5,
                                            "Emergency AAA repair"=6, "AAA-related death"=7, "Non AAA-related death"=8), 
                             selected = 1),
                 plotOutput("plot.conv.extra")
               )
      )
    )
  )
)