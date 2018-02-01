## ui.R
## riskyrApp | R Shiny | spds, uni.kn | 2018 02 01


#####
# Preparing the ground

## Dependencies:
library("shiny")
library("shinyBS")
library("markdown")
library("DT")
library("colourpicker")
library("vcd")

## Install the currently included version of riskyr:
# install.packages("./riskyr_0.0.0.911.tar.gz", repos = NULL, type = "source")
# detach("package:riskyr", unload = TRUE)
library("riskyr")


## Import ready-made and worked out example data:
datasets <- read.csv2("./www/examples_riskyR.csv", stringsAsFactors = FALSE)

default.colors <- c(color.hi =  rgb(128, 177,  57, max = 255),  # col.green.2
                    color.mi =  rgb(210,  52,  48, max = 255),  # col.red.2
                    color.fa =  rgb(230, 142, 140, max = 255),  # col.red.1
                    color.cr =  rgb(184, 217, 137, max = 255),  # col.green.1 
                    color.ppv = rgb(242, 100,  24, max = 255),  # col.orange.2
                    color.npv = rgb( 29, 149, 198, max = 255)   # col.blue.3
                    )

#####
## Define user interface logic:

shinyUI(
  
  navbarPage(title = "riskyrApp",
             theme = "bootstrap.sandstone.css",
             id = "tabs",
             selected = "represent", # should be changed to about in the end
             
             
             #####
             tabPanel("1: About", 
                      icon = icon("home", lib = "glyphicon"),
                      includeMarkdown("about.md")
             ),
             
             #####
             tabPanel("2: Statistics",
                      icon = icon("equalizer", lib = "glyphicon"), value = "stats",
                      
                      #####
                      sidebarLayout(
                        #####
                        # Sidebar panel for inputs:
                        sidebarPanel(
                          radioButtons("checkpop2", label = "Population", 
                                       choiceNames = list("Slider", "Field"),
                                       choiceValues = c(0, 1), inline = TRUE
                          ),
                          conditionalPanel(condition = "input.checkpop2 == 0",
                                           sliderInput("N2", label = NULL, value = 100,
                                                       min = 0, max = 10^6, step = 10
                                           )
                          ),
                          conditionalPanel(condition = "input.checkpop2 == 1",
                                           numericInput("numN2", label = NULL, value = 100,
                                                        min = 0, max = 10^6, step = 10
                                           )
                          ),
                          br(),
                          radioButtons("checkprev2", label = "Prevalence", 
                                       choiceNames = list("Slider", "Field"),
                                       choiceValues = c(0, 1), inline = TRUE
                          ),
                          conditionalPanel(condition = "input.checkprev2 == 0",
                                           sliderInput("prev2",  label = NULL, sep = "",
                                                       value = 0.15, min = 0, max = 1, step = 10^-6
                                           )
                          ),
                          conditionalPanel(condition = "input.checkprev2 == 1", 
                                           numericInput("numprev2", label = NULL, value = 0.15,
                                                        min = 0, max = 1, step = 10^-6
                                           )
                          ),
                          br(),
                          radioButtons("checksens2", label = "Sensitivity", 
                                       choiceNames = list("Slider", "Field"),
                                       choiceValues = c(0, 1), inline = TRUE
                          ),
                          conditionalPanel(condition = "input.checksens2 == 0",
                                           sliderInput("sens2", label = NULL, sep = "", value = 0.85,
                                                       min = 0, max = 1, step = 10^-6
                                           )
                          ),
                          conditionalPanel(condition = "input.checksens2 == 1", 
                                           numericInput("numsens2", label = NULL, value = 0.85,
                                                        min = 0, max = 1, step = 10^-6
                                           )
                          ),
                          radioButtons("checkspec2", label = "Specificity", 
                                       choiceNames = list("Slider", "Field"),
                                       choiceValues = c(0, 1), inline = TRUE
                          ),
                          conditionalPanel(condition = "input.checkspec2 == 0",
                                           sliderInput("spec2", label = NULL, sep = "", value = 0.75,
                                                       min = 0, max = 1, step = 10^-6
                                           )
                          ),
                          conditionalPanel(condition = "input.checkspec2 == 1", 
                                           numericInput("numspec2", label = NULL, value = 0.75,
                                                        min = 0, max = 1, step = 10^-6
                                           )
                          ),
                          br(), 
                          
                          # Provide existing data sets as drop-down list:
                          selectInput("dataselection2", label = "Or view an example:",
                                      choices = setNames(as.list(1:nrow(datasets)), # create choices from datasets
                                                         datasets$dataset),
                                      selected = 1),
                          
                          # bsButton("inputhelp", label = "Help", 
                          #          icon = icon("question-sign", lib = "glyphicon"),
                          #          style = "default", type = "action"),
                          
                          # Tooltips on inputs:
                          bsTooltip(id = "N2", title = "Number of individuals making up the population",
                                    placement = "right", trigger = "hover", options = list(container = "body")),

                          bsTooltip(id = "prev2", title = "Probability of being affected: p(true)",
                                    placement = "right", trigger = "hover", options = list(container = "body")),

                          bsTooltip(id = "sens2", title = "Probability of correctly detecting an affected individual: p(decision positive | condition true)",
                                    placement = "right", trigger = "hover", options = list(container = "body")),

                          bsTooltip(id = "spec2", title = "Probability of correctly rejecting an unaffected individual: p(decision negative | condition false) = 1 - FA",
                                    placement = "right", trigger = "hover", options = list(container = "body"))
                        ),
                        
                        #####
                        # Main panel for different statistics
                        mainPanel(
                          tabsetPanel(type = "tabs",
                                      #####
                                      # # Info
                                      # tabPanel("Info",
                                      #          br(),
                                      #          withMathJax(includeMarkdown("www/statstab_riskyr.md"))
                                      #          ),
                                      #####
                                      #  ACC
                                      tabPanel("Accuracy",
                                               br(),
                                               tableOutput("confusiontable1"),
                                               br(),
                                               withMathJax(includeMarkdown("www/statstab_riskyr_ACC.md")),
                                               uiOutput("ACC")
                                               ),
                                      #####
                                      # PPV
                                      tabPanel("Positive Predictive Value",
                                               br(),
                                               tableOutput("confusiontable2"),
                                               br(),
                                               withMathJax(includeMarkdown("www/statstab_riskyr_PPV1.md")),
                                               uiOutput("PPV1"),
                                               br(),
                                               withMathJax(includeMarkdown("www/statstab_riskyr_PPV2.md")),
                                               uiOutput("PPV2")
                                               ),
                                      #####
                                      # NPV
                                      tabPanel("Negative Predictive Value",
                                               br(),
                                               tableOutput("confusiontable3"),
                                               br(),
                                               withMathJax(includeMarkdown("www/statstab_riskyr_NPV1.md")),
                                               uiOutput("NPV1"),
                                               br(),
                                               withMathJax(includeMarkdown("www/statstab_riskyr_NPV2.md")),
                                               uiOutput("NPV2")
                                               ),
                                      # FDR
                                      tabPanel("False Discovery Rate",
                                               br(),
                                               tableOutput("confusiontable4"),
                                               br(),
                                               withMathJax(includeMarkdown("www/statstab_riskyr_FDR.md")),
                                               uiOutput("FDR")
                                               ),
                                      # FOR
                                      tabPanel("False Omission Rate",
                                               br(),
                                               tableOutput("confusiontable5"),
                                               br(),
                                               withMathJax(includeMarkdown("www/statstab_riskyr_FOR.md")),
                                               uiOutput("FOR")
                                               )
                                      )
                        )
                        
                        )
                      
                
             ),
             
             #####               
             tabPanel("3: Representations",
                      icon = icon("tree-deciduous", lib = "glyphicon"), value = "represent",
                      
                      #####
                      sidebarLayout(
                        #####
                        # Sidebar panel for inputs:
                        sidebarPanel(
                          radioButtons("checkpop", label = "Population", 
                                       choiceNames = list("Slider", "Field"),
                                       choiceValues = c(0, 1), inline = TRUE
                                       ),
                          conditionalPanel(condition = "input.checkpop == 0",
                                           sliderInput("N", label = NULL, value = 100,
                                                       min = 0, max = 10^6, step = 10
                                                       )
                                           ),
                          conditionalPanel(condition = "input.checkpop == 1",
                                           numericInput("numN", label = NULL, value = 100,
                                                        min = 0, max = 10^6, step = 10
                                                        )
                                           ),
                          br(),
                          radioButtons("checkprev", label = "Prevalence", 
                                       choiceNames = list("Slider", "Field"),
                                       choiceValues = c(0, 1), inline = TRUE
                                       ),
                          conditionalPanel(condition = "input.checkprev == 0",
                                           sliderInput("prev",  label = NULL, sep = "",
                                                       value = 0.15, min = 0, max = 1, step = 10^-6
                                                       )
                                           ),
                          conditionalPanel(condition = "input.checkprev == 1", 
                                           numericInput("numprev", label = NULL, value = 0.15,
                                                        min = 0, max = 1, step = 10^-6
                                                        )
                                           ),
                          br(),
                          radioButtons("checksens", label = "Sensitivity", 
                                       choiceNames = list("Slider", "Field"),
                                       choiceValues = c(0, 1), inline = TRUE
                                       ),
                          conditionalPanel(condition = "input.checksens == 0",
                                           sliderInput("sens", label = NULL, sep = "", value = 0.85,
                                                       min = 0, max = 1, step = 10^-6
                                                       )
                                           ),
                          conditionalPanel(condition = "input.checksens == 1", 
                                           numericInput("numsens", label = NULL, value = 0.85,
                                                        min = 0, max = 1, step = 10^-6
                                                        )
                                           ),
                          radioButtons("checkspec", label = "Specificity", 
                                       choiceNames = list("Slider", "Field"),
                                       choiceValues = c(0, 1), inline = TRUE
                                       ),
                          conditionalPanel(condition = "input.checkspec == 0",
                                           sliderInput("spec", label = NULL, sep = "", value = 0.75,
                                                       min = 0, max = 1, step = 10^-6
                                                       )
                                           ),
                          conditionalPanel(condition = "input.checkspec == 1", 
                                           numericInput("numspec", label = NULL, value = 0.75,
                                                        min = 0, max = 1, step = 10^-6
                                                        )
                                           ),
                          br(), 
                          
                          ## Provide existing data sets as drop-down list:
                          selectInput("dataselection", label = "Or view an example:", 
                                      choices = setNames(as.list(1:nrow(datasets)), # create choices from datasets
                                                         datasets$dataset), 
                                      selected = 1),
                          
                          # bsButton("inputhelp", label = "Help", 
                          #          icon = icon("question-sign", lib = "glyphicon"),
                          #          style = "default", type = "action"),
                          
                          ## Tooltips on inputs:
                          bsTooltip(id = "N", title = "Number of individuals making up the population",
                                    placement = "right", trigger = "hover", options = list(container = "body")), 
                          
                          bsTooltip(id = "prev", title = "Probability of being affected: p(true)",
                                    placement = "right", trigger = "hover", options = list(container = "body")),
                          
                          bsTooltip(id = "sens", title = "Probability of correctly detecting an affected individual: p(decision positive | condition true)",
                                    placement = "right", trigger = "hover", options = list(container = "body")), 
                          
                          bsTooltip(id = "spec", title = "Probability of correctly rejecting an unaffected individual: p(decision negative | condition false) = 1 - FA",
                                    placement = "right", trigger = "hover", options = list(container = "body"))
                        ),
                        #####
                        ## Main panel for displaying different aspects about risk:
                        mainPanel(
                          
                          ## Help modal:
                          bsModal(id = "modalinputhelp", 
                                  title = "So you want to know more about the inputs", 
                                  "Here, we will provide some theoretical background information.",
                                  trigger = "inputhelp", size = "medium"),
                          
                          ## Tabset with raw data table, icon array, nf tree, confusion table, and PV graphs: 
                          tabsetPanel(type = "tabs",
                                      #####
                                      # # Intro
                                      # tabPanel("Intro",
                                      #          br(),
                                      #          "This is just a quick page for displaying rendered text based on inputs. ",
                                      #          "Spacing doesn't work yet, but that's only formatting... ",
                                      #          br(), br(),
                                      #          "The current set of parameters are as follows:",
                                      #          br(), br(),
                                      #          textOutput("N"),
                                      #          br(), br(),
                                      #          textOutput("prev"),
                                      #          br(), br(),
                                      #          textOutput("sens"),
                                      #          br(), br(),
                                      #          textOutput("spec")
                                      #          ),
                                      #####
                                      # Overview
                                      tabPanel("Overview",
                                               br(),
                                               plotOutput("network", width = "550", height = "550"),
                                               br(),
                                               wellPanel(
                                                 fluidRow(
                                                   column(3, offset = 1,
                                                          radioButtons("netby", "Build Network by", c("Condition first" = "cddc",
                                                                                                      "Decision first" = "dccd"), inline = TRUE)),
                                                   column(6, 
                                                          radioButtons("nettype","Type of Boxes", c("Default boxes" = "no", "Squares" = "sq", 
                                                                                                     "Horizontal rectangles" = "hr", "Vertical rectangles" = "vr"), inline = TRUE)
                                                   )
                                                   ))
                                               ),
                                      # Stats
                                      #####
                                      # Stats
                                      # tabPanel("Stats",
                                      #          br(),
                                      #          withMathJax(includeMarkdown("www/statstab_riskyr.md")),
                                      #          br(),
                                      #          tableOutput("confusiontable1"),
                                      #          br(),
                                      #          withMathJax(includeMarkdown("www/statstab_riskyr_ACC.md")),
                                      #          uiOutput("ACC"),
                                      #          br(), br(),
                                      #          withMathJax(includeMarkdown("www/statstab_riskyr_PPV1.md")),
                                      #          uiOutput("PPV1"),
                                      #          withMathJax(includeMarkdown("www/statstab_riskyr_PPV2.md")),
                                      #          uiOutput("PPV2"),
                                      #          br(), br(),
                                      #          withMathJax(includeMarkdown("www/statstab_riskyr_NPV1.md")),
                                      #          uiOutput("NPV1"),
                                      #          withMathJax(includeMarkdown("www/statstab_riskyr_NPV2.md")),
                                      #          uiOutput("NPV2"),
                                      #          br(), br(),
                                      #          withMathJax(includeMarkdown("www/statstab_riskyr_FORFDR.md"))
                                      #          ),
                                      #####
                                      # Cases
                                      tabPanel("Individual Cases", 
                                               br(),
                                               "Individual cases:", 
                                               br(), br(),
                                               bsButton("sort", label = "Sort/Shuffle", value = FALSE, 
                                                        icon = icon("random", lib = "glyphicon"),
                                                        style = "default", type = "toggle"),
                                               br(), br(),
                                               conditionalPanel(condition = "input.dataselection != 1",
                                                                "Source:",
                                                                verbatimTextOutput("sourceOutput")),
                                               DT::dataTableOutput("rawdatatable"),
                                               br()
                                               ),
                                      #####
                                      # Icons
                                      tabPanel("Icon Array", 
                                               br(), 
                                               paste0("Icon array: Coming soon..."), 
                                               br(), br()
                                               ),
                                      #####
                                      # Tree
                                      tabPanel("Tree of Natural Frequencies", 
                                               br(), 
                                               paste0("Tree of natural frequencies:"), 
                                               br(), br(),  
                                               plotOutput("nftree", width = "550", height = "550"), 
                                               br(),
                                               wellPanel(
                                                 fluidRow(
                                                   column(3, offset = 1,
                                                          radioButtons("treeby", "Build Tree by", c("Condition" = "cd", "Decision" = "dc"), inline = TRUE)),
                                                   column(6, 
                                                          radioButtons("treetype","Type of Boxes", c("Default boxes" = "no", "Squares" = "sq", 
                                                                                                    "Horizontal rectangles" = "hr", "Vertical rectangles" = "vr"), inline = TRUE)
                                                          )
                                                 )
                                                )
                                               ),
                                      # Table
                                      #####
                                      tabPanel("Cross-Tabulation", 
                                               br(), 
                                               paste0("Aggregated cases:"), 
                                               br(), br(),  
                                               tableOutput("confusiontable"),
                                               br(),
                                               paste0("The following mosaic plot shows the cell frequencies as area sizes:"), 
                                               br(),  br(), 
                                               plotOutput("mosaicplot", height = "400px", width = "400px"),
                                               br()
                                               ),
                                      #####
                                      # PV curves
                                      tabPanel("Predictive Values: Curves", 
                                               br(),
                                               paste0("Predictive values (PPV/NPV) by prevalance:"), br(), br(),
                                               plotOutput("PVs"),
                                               br(),
                                               # paste0("PPV = ", data()$PPV, ", NPV = ", data()$NPV), 
                                               # print(data()$PPV),
                                               # ERROR: object of type 'closure' is not subsettable ???
                                               wellPanel(
                                                 fluidRow(
                                                   column(4, checkboxInput("boxPVprev", label = "Show current prevalence in plot", value = TRUE)),
                                                   column(4, checkboxInput("boxPVpoints1", label = "Show current PPV/NPV in plot", value = TRUE)),
                                                   column(4, checkboxInput("boxPVlog", label = "Show prevalence on logarithmic scale", value = FALSE))
                                                   )
                                                 )
                                               ),
                                      #####
                                      # PV cubes
                                      tabPanel("Predictive Values: Cubes", 
                                               br(),
                                               paste0("Predictive values (PPV/NPV) by sensitivity and specificity:"), br(), br(),
                                               fluidRow(
                                                 column(6, plotOutput("PV3dPPV")),
                                                 column(6, plotOutput("PV3dNPV"))
                                                 ),
                                               br(),
                                               br(),
                                               wellPanel(
                                                 checkboxInput("boxPVpoints2", label = "Show current PPV/NPV in plots", value = TRUE), 
                                                 br(),
                                                 fluidRow(
                                                   column(6, sliderInput("theta", "Horizontal viewing angle:", value = -45, min = -90, max = +90)),
                                                   column(6, sliderInput("phi", "Vertical viewing angle:", value = 0, min = 0, max =  90))
                                                   ),
                                                 br()
                                                 )
                                               )
                                      )
                          )
                        )
                      ),
             
             #####        
             tabPanel("4: Information",
                      icon = icon("education", lib = "glyphicon")
                      
             ),
             
             #####
             navbarMenu("5: Customize",
                        icon = icon("wrench", lib = "glyphicon"), 
                        
                        # spacer
                        "----",
                        
                        #####
                        # Customize labels:
                        tabPanel("Customize labels",
                                 icon = icon("pencil", lib = "glyphicon"),
                                 
                                 sidebarLayout(
                                   #####
                                   # Sidebar panel for inputs:
                                   sidebarPanel(
                                     # Inputs for label customization:
                                     h3("Use your own labels!"),
                                     br(),
                                     fluidRow(
                                       column(6, textInput("target.population.lbl",
                                                           label = "Description of population:",
                                                           value = "Population description")),
                                       column(6, textInput("scenario.txt",
                                                         label = "Description of scenario:",
                                                         value = "Generic Example"))
                                     ),
                                     br(),
                                     textInput("condition.lbl",
                                               label = "Condition name:",
                                               value = "Current condition"),
                                     fluidRow(
                                       column(6, textInput("cond.true.lbl",
                                                           label = "Condition true",
                                                           value = "Condition true")),
                                       column(6, textInput("cond.false.lbl",
                                                           label = "Condition false",
                                                           value = "Condition false"))
                                     ),
                                     br(),
                                     textInput("decision.lbl",
                                               label = "Decision",
                                               value = "Diagnostic decision"),
                                     fluidRow(
                                       column(6, textInput("dec.true.lbl",
                                                           label = "Decision positive",
                                                           value = "Decision positive")),
                                       column(6, textInput("dec.false.lbl",
                                                           label = "Decision negative",
                                                           value = "Decision negative"))
                                       ),
                                     br(),
                                     fluidRow(
                                       column(6, textInput("sdt.hi.lbl", label = "Hit", value = "hit")),
                                       column(6, textInput("sdt.mi.lbl", label = "Miss", value = "miss"))
                                       ),
                                     fluidRow(
                                       column(6, textInput("sdt.fa.lbl", label = "False alarm", value = "false alarm")),
                                       column(6, textInput("sdt.cr.lbl", label = "Correct rejection", value = "correct rejection"))
                                       ),
                                     br(),
                                     bsButton("applycustomlabel", label = "Customize!",
                                              icon = icon("wrench", lib = "glyphicon"),
                                              style = "default", type = "action"),
                                     bsButton("resetcustomlabel", label = "Reset default",
                                              icon = icon("refresh", lib = "glyphicon"),
                                              style = "default", type = "action")
                                   ),

                                   #####
                                   ## Main panel for displaying preview of labels:
                                   mainPanel(h3("Here is a simplified preview of your labels:"),
                                             "Click the 'Customize' button to update your color selection.",
                                             br(), br(),
                                             textOutput("labeltext"),
                                             br(),
                                             tableOutput("labeltable")
                                             )
                                 )
                        ),
                        
                        # spacer
                        "----",
                        # Customize colors:
                        #####
                        tabPanel("Customize colors",
                                 icon = icon("adjust", lib = "glyphicon"),
                                 sidebarLayout(
                                 #####
                                 sidebarPanel(
                                   # Inputs for color customization:
                                   h3("Choose your own colors!"),
                                   br(),
                                   colourInput("color.hi", label = "Choose the color for hits",
                                               value = default.colors["color.hi"], showColour = "background",
                                               palette = "square", allowedCols = NULL),
                                   colourInput("color.mi", label = "Choose the color for miss",
                                               value = default.colors["color.mi"], showColour = "background",
                                               palette = "square", allowedCols = NULL),
                                   colourInput("color.fa", label = "Choose the color for false alarm",
                                               value = default.colors["color.fa"], showColour = "background",
                                               palette = "square", allowedCols = NULL),
                                   colourInput("color.cr", label = "Choose the color for correct rejection",
                                               value = default.colors["color.cr"], showColour = "background",
                                               palette = "square", allowedCols = NULL),
                                   br(),
                                   colourInput("color.ppv", label = "Color for the positive predictive value (PPV)",
                                               value = default.colors["color.ppv"], showColour = "background",
                                               palette = "square", allowedCols = NULL),
                                   colourInput("color.npv", label = "Color for the negative predictive value (NPV)",
                                               value = default.colors["color.npv"], showColour = "background",
                                               palette = "square", allowedCols = NULL),
                                   br(),
                                   bsButton("applycustomcolor", label = "Customize!",
                                            icon = icon("wrench", lib = "glyphicon"),
                                            style = "default", type = "action"),
                                   bsButton("resetcustomcolor", label = "Reset default",
                                            icon = icon("refresh", lib = "glyphicon"),
                                            style = "default", type = "action")
                                 ),
                                 
                                 #####
                                 ## Main panel for displaying preview plots with colors:
                                 mainPanel(h3("Here are simplified preview plots of your colors:"),
                                           "Click the 'Customize' button to update your color selection.",
                                           fluidRow(column(6, plotOutput("sampleplot")),
                                                    column(6, plotOutput("sampleplotcurves"))
                                           )
                                           )
                                 )
                        ),
                        
                        # spacer
                        "----"
                        ),
             
             #####
             navbarMenu("Dropdown-Navigation",
                        
                        # spacer
                        "----",
                        
                        # 1st screen in dropdown navigation:
                        tabPanel("Further information",
                                 icon = icon("book", lib = "glyphicon"),
                                 "Text of tab panel", br() 
                        ),
                        
                        # spacer
                        "----",
                        
                        # 2nd screen in dropdown navigation: 
                        tabPanel("B Imprint",
                                 icon = icon("info-sign", lib = "glyphicon"),
                                 "Hier Text für Panel B", br(), br(),
                                 a("SPDS@uni.kn", href = "https://www.spds.uni-konstanz.de"), br(), br(),
                                 tags$code("This text will be displayed as computer code."), br() 
                        ),
                        
                        # spacer
                        "----"
             )
  )
)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
## eof. #
