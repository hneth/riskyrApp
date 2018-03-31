## ui.R
## riskyrApp | R Shiny | spds, uni.kn | 2018 03 15
## riskyr package version 0.1.0 ------

##### Preparing the ground: ------

## Dependencies: ------
library("shiny")
library("shinyBS")
library("markdown")
library("DT")
library("colourpicker")
library("vcd")

## Install the currently included version of riskyr: ------
# detach("package:riskyr", unload = TRUE)
# from CRAN: <https://CRAN.R-project.org/package=riskyr>
# devtools::install_github("hneth/riskyr")
# install.packages("./riskyr_0.1.0.tar.gz", repos = NULL, type = "source")
library("riskyr")
# sessionInfo()

## Import ready-made and worked out example data: ------
datasets <- read.csv2("./www/examples_riskyrApp_2018-02-15.csv", stringsAsFactors = FALSE)

default.colors <- c(color.hi =  rgb(128, 177,  57, max = 255),  # col.green.2
                    color.mi =  rgb(210,  52,  48, max = 255),  # col.red.2
                    color.fa =  rgb(230, 142, 140, max = 255),  # col.red.1
                    color.cr =  rgb(184, 217, 137, max = 255),  # col.green.1 
                    color.ppv = rgb(242, 100,  24, max = 255),  # col.orange.2
                    color.npv = rgb( 29, 149, 198, max = 255)   # col.blue.3
                    )

## logifySlider javascript function
JS.logify <-
  "
// function to logify a sliderInput
function logifySlider (sliderId, sci = false) {
if (sci) {
// scientific style
$('#'+sliderId).data('ionRangeSlider').update({
'prettify': function (num) { return ('10<sup>'+num+'</sup>'); }
})
} else {
// regular number style
$('#'+sliderId).data('ionRangeSlider').update({
'prettify': function (num) { return (Math.pow(10, num)); }
})
}
}"

## call logifySlider for each relevant sliderInput
JS.onload <-
  "
// execute upon document loading
$(document).ready(function() {
// wait a few ms to allow other scripts to execute
setTimeout(function() {
// include call for each slider
logifySlider('N', sci = false)
logifySlider('N2', sci = false)

}, 5)})
"



#####
## Define user interface logic: ------

shinyUI(
  
  navbarPage(title = "riskyrApp",
             theme = "bootstrap.sandstone.css",
             id = "tabs",
             selected = "welcome", # start screen
             
             
             #####
             tabPanel("Welcome!",
                      icon = icon("flag", lib = "glyphicon"),
                      value = "welcome",
                      fluidRow(column(4, offset = 0, h1("Welcome to the riskyrApp!"))),
                               "Hover over the image to find your way.",
                      br(),
                      fluidRow(column(12, offset = 0, includeHTML("www/imageMap.html")))
                      ),
             tabPanel("1: About", 
                      icon = icon("home", lib = "glyphicon"),
                      value = "about",
                      fluidRow(column(4, 
                                      h1("The riskyrApp"),
                                      
                                      h2("About"),
                                      "The ", code("riskyrApp"), " is an R Shiny application that complements the ", code("riskyr"),
                                      "toolbox for rendering risk literacy more transparent.",
                                      br()
                                      ),
                               column(6, HTML(' <a href="https://CRAN.R-project.org/package=riskyr">
                                                 <img src="riskyr_cube_s.png" align = "right" alt="riskyr@CRAN">
                                                 </a>'))),
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
                          tags$head(tags$script(HTML(JS.logify))),
                          tags$head(tags$script(HTML(JS.onload))),
                          
                          sliderInput("N2", label = "Population (logarithmic scale)",
                                      min = 1, max = 5,
                                      value = 2, round = FALSE),
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
                          
                          bsButton("help_stats", label = "Help",
                                   icon = icon("question-sign", lib = "glyphicon"),
                                   style = "default", type = "action")
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
                          tags$head(tags$script(HTML(JS.logify))),
                          tags$head(tags$script(HTML(JS.onload))),
                          
                          sliderInput("N", label = "Population (logarithmic scale)",
                                      min = 1, max = 5,
                                      value = 2, round = FALSE),
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
                          
                          bsButton("help_represent", label = "Help",
                                   icon = icon("question-sign", lib = "glyphicon"),
                                   style = "default", type = "action")
                          
                        ),
                        #####
                        ## Main panel for displaying different aspects about risk:
                        mainPanel(
                          
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
                                               fluidRow(
                                                 column(8, offset = 2, plotOutput("network", width = "550", height = "550"))),
                                               # plotOutput("network", width = "550", height = "550"),
                                               # br(),
                                               wellPanel(
                                                 fluidRow(
                                                   column(3, offset = 0,
                                                          radioButtons("netby", "Build Network by", c("Condition first" = "cddc",
                                                                                                      "Decision first" = "dccd"), inline = TRUE)),
                                                   column(6, 
                                                          radioButtons("nettype", "Type of Boxes", c("Default boxes" = "no", "Squares" = "sq", 
                                                                                                     "Horizontal rectangles" = "hr", "Vertical rectangles" = "vr"), inline = TRUE)),
                                                   column(2, downloadButton("fnetdl", label = "Save Network"))
                                                   )
                                               )
                                               ),
                                      #####
                                      # Cases
                                      tabPanel("Table", 
                                               br(),
                                               "Individual cases:", 
                                               br(), br(),
                                             
                                               conditionalPanel(condition = "input.dataselection != 1",
                                                                "Source:",
                                                                verbatimTextOutput("sourceOutput")),
                                               DT::dataTableOutput("rawdatatable"),
                                               br(), br(),
                                               wellPanel(
                                                 fluidRow(
                                                   column(2, offset = 2,
                                                          bsButton("sort", label = "Sort/Shuffle", value = TRUE, 
                                                                   icon = icon("random", lib = "glyphicon"),
                                                                   style = "default", type = "toggle")),
                                                   column(2, offset = 2,
                                                          downloadButton("rawdatadl", label = "Save Raw Data"))
                                                 )
                                               ),
                                               br()
                                               ),
                                      #####
                                      # Icons
                                      tabPanel("Icons", 
                                               br(), 
                                               fluidRow(
                                                 column(8, offset = 2, plotOutput("iconarray", width = "650", height = "500"))),
                                               # plotOutput("iconarray", width = "550", height = "550"), 
                                               # br(), 
                                               wellPanel(
                                                 fluidRow(
                                                   column(4, offset = 2,
                                                          radioButtons("arraytype", "Display:",
                                                                       choices = list("Array" = "array", "Shuffled" = "shuffledarray",
                                                                                      "Scattered" = "scatter", "Mosaic" = "mosaic"), inline = TRUE)),
                                                          column(2, downloadButton("iconarraydl", label = "Save Icon Array"))
                                                   ), 
                                                   br(),
                                                 fluidRow(
                                                   column(3,
                                                          selectInput("symbol.hi", label = "Symbol of hits (hi):", 
                                                                      choices = list("Circle" = 21, "Square" = 22, "Rhombus" = 23, "Triangle" = 24),
                                                                      selected = "22")
                                                          ),
                                                   column(3,
                                                          selectInput("symbol.mi", label = "Symbol of miss (mi):", 
                                                                      choices = list("Circle" = 21, "Square" = 22, "Rhombus" = 23, "Triangle" = 24),
                                                                      selected = "22")
                                                          ),
                                                   column(3,
                                                          selectInput("symbol.cr", label = "Symbol of correct rejections (cr):", 
                                                                      choices = list("Circle" = 21, "Square" = 22, "Rhombus" = 23, "Triangle" = 24),
                                                                      selected = "22")
                                                          ),
                                                   column(3,
                                                          selectInput("symbol.fa", label = "Symbol of false alarms (fa):", 
                                                                      choices = list("Circle" = 21, "Square" = 22, "Rhombus" = 23, "Triangle" = 24),
                                                                      selected = "22")
                                                          )
                                                  )
                                                 )
                                               ),
                                      #####
                                      # Tree
                                      tabPanel("Tree", 
                                               br(), 
                                               paste0("Tree of natural frequencies:"), 
                                               br(), br(),  
                                               fluidRow(
                                                 column(8, offset = 2, plotOutput("nftree", width = "550", height = "550"))),
                                               # br(),
                                               wellPanel(
                                                 fluidRow(
                                                   column(3, offset = 0,
                                                          radioButtons("treeby", "Build Tree by", c("Condition" = "cd", "Decision" = "dc"), inline = TRUE)),
                                                   column(6, 
                                                          radioButtons("treetype","Type of Boxes", c("Default boxes" = "no", "Squares" = "sq", 
                                                                                                    "Horizontal rectangles" = "hr", "Vertical rectangles" = "vr"), inline = TRUE)),
                                                   column(2, downloadButton("nftreedl", label = "Save Frequency Tree"))
                                                 )
                                                )
                                               ),
                                      # Table
                                      #####
                                      tabPanel("Cross-Tabulation", 
                                               br(), 
                                               # paste0("Aggregated cases:"), 
                                               # br(), br(),  
                                               # tableOutput("confusiontable"),
                                               # br(),
                                               # paste0("A mosaic plot shows cell frequencies as area sizes:"), 
                                               # br(),  br(), 
                                               # plotOutput("mosaicplot", height = "400px", width = "400px"),
                                               # br(),
                                               # wellPanel(
                                               #   fluidRow(
                                               #     column(2, downloadButton("mosaicplotdl", label = "Save Mosaic Plot")),
                                               #     column(2, downloadButton("confusiontabledl", label = "Save Confusion Table"))
                                               #   ))
                                               
                                               fluidRow(
                                                 column(6, offset = 0, paste0("Aggregated cases:"), br(), br(),br(), br()),
                                                 column(6, offset = 0, paste0("A mosaic plot shows cell frequencies as area sizes:"),
                                                        br(), br())
                                                 ), 
                                              fluidRow(
                                                 column(5, offset = 1, tableOutput("confusiontable")),
                                                 column(5, offset = 1, plotOutput("mosaicplot", height = "400px", width = "400px"))
                                               ),
                                               wellPanel(
                                                 fluidRow(
                                                   column(2, offset = 2, downloadButton("confusiontabledl", label = "Save Confusion Table")),
                                                   column(2, offset = 4, downloadButton("mosaicplotdl", label = "Save Mosaic Plot"))
                                                 ))
                                               ),
                                      #####
                                      # PV curves
                                      tabPanel("Curves", 
                                               br(),
                                               paste0("Positive Predictive Value (PPV) and Negative Predictive Value (NPV) by prevalance:"), br(), br(),
                                               fluidRow(
                                                 column(8, offset = 2, plotOutput("PVs"))),
                                               br(),
                                               wellPanel(
                                                 fluidRow(
                                                   # column(4, checkboxInput("boxPVprev", label = "Show current prevalence in plot", value = TRUE)),
                                                   column(2, offset = 2, checkboxInput("boxPVpoints1", label = "Show point values", value = TRUE)),
                                                   column(3, checkboxInput("boxPVlog", label = "Scale prevalence on logarithmic scale", value = FALSE))),
                                                 fluidRow(
                                                   column(2, offset = 2, checkboxInput("boxPVacc", label = "Show accuracy (acc)", value = FALSE)),
                                                   column(4, checkboxInput("boxPVppod", label = "Show proportion of positive decisions (ppod)", value = FALSE)),
                                                   column(1, downloadButton("PVsdl", label = "Save Curves"))
                                                   )
                                                 )
                                               ),
                                      #####
                                      # PV cubes
                                      tabPanel("Cubes", 
                                               br(),
                                               paste0("Predictive values (PPV/NPV) by sensitivity and specificity:"), br(), br(),
                                               fluidRow(
                                                 column(6, plotOutput("PV3dPPV")),
                                                 column(6, plotOutput("PV3dNPV"))
                                                 ),
                                               br(),
                                               br(),
                                               wellPanel(
                                                 fluidRow(
                                                   column(3, checkboxInput("boxPVpoints2", label = "Show current PPV/NPV in plots", value = TRUE)), 
                                                   column(2, offset = 1, downloadButton("PV3dPPVdl", label = "Save PPV Cube")),
                                                   column(2, offset = 4,
                                                          downloadButton("PV3dNPVdl", label = "Save NPV Cube"))
                                                 ),
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
             tabPanel("Under Dev: Quiz",
                      icon = icon("education", lib = "glyphicon"), value = "quiz",
                      
                      sidebarLayout(
                        #####
                        # Sidebar panel for feedback:
                        sidebarPanel(
                          h2("Question 1:"), br(),
                          bsAlert("alert_question1"),
                          h2("Question 2:"), br(),
                          bsAlert("alert_question2"),
                          h2("Question 3:"), br(),
                          bsAlert("alert_question3"),
                          h2("Question 4:"), br(),
                          bsAlert("alert_question4")
                        ),
                        #####
                        # Sidebar panel for questions:
                        mainPanel(
                          
                          tabsetPanel(type = "tabs",
                                      tabPanel("Question 1",
                                               h2("Question 1"),
                                               "This is a sample multiple choice question. Options 1 and 3 are correct.",
                                               checkboxGroupInput("question1", label = h3("Sample item"), 
                                                                  choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3, "Choice 4" = 4),
                                                                  selected = NULL),
                                               bsButton("submit_question1", label = "Submit answer", type = "action")
                                              ),
                                      tabPanel("Question 2",
                                               h2("Question 2"),
                                               "This is a sample forced choice question. Option 2 is correct.",
                                               radioButtons("question2", label = h3("Sample item"),
                                                            choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3, "Choice 4" = 4), 
                                                            selected = character(0)),
                                               bsButton("submit_question2", label = "Submit answer", type = "action")
                                               ),
                                      tabPanel("Question 3",
                                               h2("Question 3"),
                                               "This is a sample calculation question. The correct answer is '0.95'.",
                                               numericInput("question3", label = h3("Sample Item"), min = 0, max = 1, value = NULL),
                                               bsButton("submit_question3", label = "Submit answer", type = "action")
                                               
                                               ),
                                      
                                      tabPanel("Question 4",
                                               h2("Question 4"),
                                               "This is a sample item for graph literacy. The left option is correct.",
                                               h3("Which graph is a tree diagram by condition?"),
                                               fluidRow(
                                                 column(5, offset = 0, img(src = "optionA.png")),
                                                 column(5, offset = 1, img(src = "optionB.png"))
                                               ),
                                               radioButtons("question4", label = "Sample Item",
                                                            choices = list("Option A" = 1, "Option B" = 2),
                                                            inline = TRUE, selected = character(0)),
                                               bsButton("submit_question4", label = "Submit answer", type = "action")
                                               )
                                      )
                          )
                        )
                        
                      

             ),
             
             #####
             navbarMenu("4: Customize",
                        icon = icon("wrench", lib = "glyphicon"), 
                        
                        # spacer
                        "----",
                        
                        #####
                        # Customize labels:
                        tabPanel("Customize labels",
                                 icon = icon("pencil", lib = "glyphicon"), value = "custom_labels",
                                 
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
                                              style = "default", type = "action"),
                                     bsButton("help_custom_labels", label = "Help",
                                              icon = icon("question-sign", lib = "glyphicon"),
                                              style = "default", type = "action")
                                   ),

                                   #####
                                   ## Main panel for displaying preview of labels:
                                   mainPanel(h3("Here is a simplified preview of your labels:"),
                                             "Click the 'Customize' button to update your selection of labels to build your own case study.",
                                             br(),
                                             # br(),
                                             # textOutput("labeltext"),
                                             # br(),
                                             # tableOutput("labeltable"),
                                             # br(),
                                             plotOutput("previewlabels", width = "800", height = "750")
                                             )
                                 )
                        ),
                        
                        # spacer
                        "----",
                        # Customize colors:
                        #####
                        tabPanel("Customize colors",
                                 icon = icon("adjust", lib = "glyphicon"),
                                 value = "custom_colors",
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
                                            style = "default", type = "action"),
                                   bsButton("help_custom_colors", label = "Help",
                                            icon = icon("question-sign", lib = "glyphicon"),
                                            style = "default", type = "action")
                                 ),
                                 
                                 #####
                                 ## Main panel for displaying preview plots with colors:
                                 mainPanel(h3("Here are simplified preview plots of your colors:"),
                                           "Click the 'Customize' button to update your color selection.",
                                           fluidRow(column(3, plotOutput("sampleplot")),
                                                    column(3, plotOutput("sampleplotcurves"))
                                           )
                                           )
                                 )
                        ),
                        
                        # spacer
                        "----"
                        ),
             
             #####
             navbarMenu("Further information",

                        # spacer
                        "----",

                        # 1st screen in dropdown navigation:
                        tabPanel("References & Readings",
                                 icon = icon("book", lib = "glyphicon"),
                                 value = "references",
                                 h1("References and recommended readings"),
                                 fluidRow(
                                   column(5, offset = 0,
                                          includeMarkdown("www/recommended_readings.md")),
                                   column(5, offset = 1,
                                          includeMarkdown("www/references.md"))
                                 )
                                 # includeMarkdown("www/recommended_readings.md")
                        ),

                        # spacer
                        "----",

                        # 2nd screen in dropdown navigation:
                        tabPanel("Imprint",
                                 icon = icon("info-sign", lib = "glyphicon"),
                                 h2("Imprint"),
                                 fluidRow(
                                 HTML('<a href="https://CRAN.R-project.org/package=riskyr"> 
                                        <img src="riskyr_cube_s.png" alt="riskyr@CRAN"
                                      align = "left">
                                        </a>'),
                                 HTML('<a href="https://www.spds.uni-konstanz.de"> 
                                        <img src="uniKn_logo_s.png" alt="SPDS@uni.KN"
                                        align = "left">
                                      </a>')
                                 ),
                                 br(),
                                 includeMarkdown("www/imprint.md")
                        ),

                        # spacer
                        "----"
             ),
             
             ### TUTORIAL ELEMENTS (under development)
             # bsModal(id = "tutstart", 
             #         title = "So you want to take the guided tour (aka tutorial)...", 
             #         br(),
             #         "First of all, please note that the riskyrApp comprises several tabs.",
             #         br(),
             #         "The tab menu is visible at all times in the upper left corner of the screen and looks like this:",
             #         br(),
             #         img(src = "riskyr_header_menu.png"),
             #         footer = list(modalButton("OK"), bsButton("tut1", "Continue Tutorial")),
             #         trigger = "link_to_tutorial", size = "large"),
             
            
             
             ######
             ## Tooltips
             
             # On inputs
             bsTooltip(id = list("N", "numN", "N2", "numN2"), 
                       title = "Number of individuals making up the population",
                       placement = "right", trigger = "hover", options = list(container = "body")),
             
             bsTooltip(id = list("prev", "numprev", "prev2", "numprev2"),
                       title = "Probability of being affected: p(true)",
                       placement = "right", trigger = "hover", options = list(container = "body")),
             
             bsTooltip(id = list("sens", "numsens", "sens2", "numsens2"),
                       title = "Probability of correctly detecting an affected individual: p(decision positive | condition true)",
                       placement = "right", trigger = "hover", options = list(container = "body")),
             
             bsTooltip(id = list("spec", "numspec", "spec2", "numspec2"),
                       title = "Probability of correctly rejecting an unaffected individual: p(decision negative | condition false) = 1 - FA",
                       placement = "right", trigger = "hover", options = list(container = "body")),
             
             # On Download buttons
             
             bsTooltip(id = list("fnetdl", "iconarraydl", "nftreedl", "mosaicplotdl", "PVsdl", "PV3dPPVdl", "PV3dNPVdl"),
                       title = "Click this button to download and save the graph as .png file.",
                       placement = "above", trigger = "hover", options = list(container = "body")),
             
             
             bsTooltip(id = list("rawdatadl", "confusiontabledl"),
                       title = "Click this button to download and save the table as .csv file.",
                       placement = "above", trigger = "hover", options = list(container = "body"))

  )
)

## ------
## eof. #
