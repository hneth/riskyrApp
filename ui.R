# ui.R
# riskyrApp | R Shiny | spds, uni.kn | 2018 12 07

## Dependencies: ------
library("shiny")
library("shinyBS")
library("markdown")
library("DT")
library("colourpicker")


## Install the currently included version of riskyr: ------
# detach("package:riskyr", unload = TRUE)
# from CRAN: <https://CRAN.R-project.org/package=riskyr>
# devtools::install_github("hneth/riskyr")
library("riskyr")
# sessionInfo()

## Import ready-made and worked out example data: ------
datasets <- read.csv2("./www/examples_riskyrApp_2018-03-30.csv", stringsAsFactors = FALSE)

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

            #####               
            tabPanel("Visualize risks",
                     icon = icon("blackboard", lib = "glyphicon"), value = "represent",
                     
                     #####
                     sidebarLayout(
                       #####
                       # Sidebar panel for inputs:
                       sidebarPanel(
                         tags$head(tags$script(HTML(JS.logify))),
                         tags$head(tags$script(HTML(JS.onload))),
                         
                         sliderInput("N", label = "Population (logarithmic scale)",
                                     min = 1, max = 5,
                                     value = 2, round = FALSE
                         ),
                         br(),
                         radioButtons("checkprev", label = "Prevalence (in Percent)", 
                                      choiceNames = list("Slider", "Field"),
                                      choiceValues = c(0, 1), inline = TRUE
                         ),
                         conditionalPanel(condition = "input.checkprev == 0",
                                          sliderInput("prev",  label = NULL, sep = "",
                                                      value = 15, min = 0, max = 100, step = 1,
                                                      pre=NULL, post="%"
                                          )
                         ),
                         conditionalPanel(condition = "input.checkprev == 1", 
                                          numericInput("numprev", label = NULL, value = 15,
                                                       min = 0, max = 100, step = 10^-2
                                          )
                         ),
                         br(),
                         radioButtons("checksens", label = "Sensitivity (in Percent)", 
                                      choiceNames = list("Slider", "Field"),
                                      choiceValues = c(0, 1), inline = TRUE
                         ),
                         conditionalPanel(condition = "input.checksens == 0",
                                          sliderInput("sens", label = NULL, sep = "", value = 85.00,
                                                      min = 0, max = 100, step = 1,
                                                      pre=NULL, post="%"
                                          )
                         ),
                         conditionalPanel(condition = "input.checksens == 1", 
                                          numericInput("numsens", label = NULL, value = 85.00,
                                                       min = 0, max = 100, step = 10^-2
                                          )
                         ),
                         radioButtons("checkspec", label = "Specificity (in Percent)", 
                                      choiceNames = list("Slider", "Field"),
                                      choiceValues = c(0, 1), inline = TRUE
                         ),
                         conditionalPanel(condition = "input.checkspec == 0",
                                          sliderInput("spec", label = NULL, sep = "", value = 75,
                                                      min = 0, max = 100, step = 1,
                                                      pre=NULL, post="%"
                                          )
                         ),
                         conditionalPanel(condition = "input.checkspec == 1", 
                                          numericInput("numspec", label = NULL, value = 75,
                                                       min = 0, max = 100, step = 10^-2
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
                                     # Prism
                                     tabPanel("Prism",
                                              br(),
                                              fluidRow(
                                                column(8, offset = 2, plotOutput("prism", width = "550", height = "550"))),
                                              wellPanel(
                                                fluidRow(
                                                  column(3, offset = 0,
                                                         selectInput("prism.by", label = "Build prism by:", 
                                                                     choices = list("condition only" = "cd", 
                                                                                    "decision only" = "dc", 
                                                                                    "accuracy only" = "ac",
                                                                                    "condition and decision" = "cddc",
                                                                                    "condition and accuracy" = "cdac",
                                                                                    "decision and condition" = "dccd",
                                                                                    "decision and accuracy" = "dcac",
                                                                                    "accuracy and condition" = "accd",
                                                                                    "accuracy and decision" = "acdc"),
                                                                     selected = "cddc")
                                                         ),
                                                  column(3, 
                                                         selectInput("prism.area", label = "Type of Boxes", 
                                                                     choices = list("default boxes" = "no", 
                                                                                    "horizontal rectangles" = "hr",
                                                                                    "squares" = "sq"), 
                                                                      selected = "no")),
                                                  column(3, 
                                                         selectInput("prism.f_lbl", label = "Labels:", 
                                                                     choices = list("abbrev. names & values" = "def", 
                                                                                    "abbrev. names" = "abbr",
                                                                                    "names only" = "nam",
                                                                                    "values only" = "num",
                                                                                    "names & values" = "namnum",
                                                                                    "no labels" = "no"
                                                                                    ), 
                                                                      selected = "num")),
                                                  column(2, offset = 1, downloadButton("prism.dl", label = "Save Prism"))
                                                )
                                              )
                                     ),
                                     #####
                                     # Table
                                     tabPanel("Table", 
                                              br(),
                                              fluidRow(
                                                  column(8, offset = 2, plotOutput("table", width = "550", height = "550"))),
                                              wellPanel(
                                                  fluidRow(
                                                      column(3, offset = 0,
                                                             selectInput("table.by", label = "Build table by:", 
                                                                         choices = list("condition and decision" = "cddc",
                                                                                        "condition and accuracy" = "cdac",
                                                                                        "decision and condition" = "dccd",
                                                                                        "decision and accuracy" = "dcac",
                                                                                        "accuracy and condition" = "accd",
                                                                                        "accuracy and decision" = "acdc"),
                                                                         selected = "cddc")
                                                      ),
                                                      column(3, 
                                                             selectInput("table.p_split", label = "Population split", 
                                                                         choices = list("vertical" = "v", 
                                                                                        "horizontal" = "h"), 
                                                                         selected = "v")),
                                                      column(3, 
                                                             selectInput("table.f_lbl", label = "Labels:", 
                                                                         choices = list("abbrev. names & values" = "def", 
                                                                                        "abbrev. names" = "abbr",
                                                                                        "names only" = "nam",
                                                                                        "values only" = "num",
                                                                                        "names & values" = "namnum",
                                                                                        "no labels" = "no"
                                                                         ), 
                                                                         selected = "num")),
                                                      column(2, offset = 1, downloadButton("table.dl", label = "Save Table"))
                                                  )
                                              )
                                     ),
                                     # Area
                                     #####
                                     tabPanel("Area", 
                                              br(),
                                              fluidRow(
                                                  column(8, offset = 2, plotOutput("area", width = "550", height = "550"))),
                                              wellPanel(
                                                  fluidRow(
                                                      column(3, offset = 0,
                                                             selectInput("area.by", label = "Build area by:", 
                                                                         choices = list("condition and decision" = "cddc",
                                                                                        "condition and accuracy" = "cdac",
                                                                                        "decision and condition" = "dccd",
                                                                                        "decision and accuracy" = "dcac",
                                                                                        "accuracy and condition" = "accd",
                                                                                        "accuracy and decision" = "acdc"),
                                                                         selected = "cddc")
                                                      ),
                                                      column(3, 
                                                             selectInput("area.p_split", label = "Population split", 
                                                                         choices = list("vertical" = "v", 
                                                                                        "horizontal" = "h"), 
                                                                         selected = "v")),
                                                      column(3, 
                                                             selectInput("area.f_lbl", label = "Labels:", 
                                                                         choices = list("abbrev. names & values" = "def", 
                                                                                        "abbrev. names" = "abbr",
                                                                                        "names only" = "nam",
                                                                                        "values only" = "num",
                                                                                        "names & values" = "namnum",
                                                                                        "no labels" = "no"
                                                                         ), 
                                                                         selected = "num")),
                                                      column(2, offset = 1, downloadButton("area.dl", label = "Save Area"))
                                                  )
                                              )
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
                                     # Bars
                                     tabPanel("Bars", 
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
                                   
                                     #####
                                     # Curves
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
                                     # Planes
                                     tabPanel("Planes", 
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
                                     ),
                                     #####
                                     # contrast representations
                                     tabPanel("Compare", 
                                              br(),
                                              paste0("Compare two representations:"), br(), br(),
                                              fluidRow(
                                                column(6, plotOutput("represent1", width = "550", height = "550")),
                                                column(6, plotOutput("represent2", width = "550", height = "550"))
                                              ),
                                              br(),
                                              br(),
                                              wellPanel(
                                                fluidRow(
                                                  column(3,
                                                         selectInput("represent1", label = "Selection representation 1:", 
                                                                     choices = list("Network" = "fnet", "Icon array" = "iconarray",
                                                                                    "Frequency tree" = "tree", "Mosaic plot" = "mosaic"))
                                                  ),
                                                  column(3, offset = 3,
                                                         selectInput("represent2", label = "Selection representation 2:", 
                                                                     choices = list("Network" = "fnet", "Icon array" = "iconarray",
                                                                                    "Frequency tree" = "tree", "Mosaic plot" = "mosaic"))
                                                         # maybe a download button here
                                                  )
                                                ),
                                                fluidRow(
                                                  column(2, offset = 0, downloadButton("represent1dl", label = "Save representation")),
                                                  column(2, offset = 4, downloadButton("represent2dl", label = "Save representation"))
                                                )
                                              ),
                                              br()
                                     )
                                     
                                     
                         )
                       )
                     )
            ),

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
            
            # Customize colors:
            #####
            tabPanel("Customize colors",
                     icon = icon("wrench", lib = "glyphicon"),
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

             #####
             navbarMenu("About",  icon = icon("info-sign", lib = "glyphicon"),
                        
                        # spacer
                        "----",
                        
                        # 1st screen in dropdown navigation:
                        tabPanel("References & Readings",
                                 icon = icon("book", lib = "glyphicon"),
                                 value = "references",
                                 h1("References and recommended readings"),
                                 fluidRow(
                                   column(5, offset = 0, includeMarkdown("www/recommended_readings.md")),
                                   column(5, offset = 1, includeMarkdown("www/references.md"))
                                 )
                        ),
                        
                        # spacer
                        "----",
                        
                        # 2nd screen in dropdown navigation:
                        tabPanel("Imprint", value = "about",
                                 icon = icon("pushpin", lib = "glyphicon"),
                                 h1("Imprint"),
                                 br(),
                                 includeMarkdown("www/imprint.md")
                                 ),
                        
                        # spacer
                        "----"
             ),

             
             ######
             ## Tooltips
             
             # On inputs
             bsTooltip(id = list("N", "numN"), 
                       title = "Number of individuals making up the population",
                       placement = "right", trigger = "hover", options = list(container = "body")),
             
             bsTooltip(id = list("prev", "numprev"),
                       title = "Probability of being affected: p(true)",
                       placement = "right", trigger = "hover", options = list(container = "body")),
             
             bsTooltip(id = list("sens", "numsens"),
                       title = "Probability of correctly detecting an affected individual: p(decision positive | condition true)",
                       placement = "right", trigger = "hover", options = list(container = "body")),
             
             bsTooltip(id = list("spec", "numspec"),
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

