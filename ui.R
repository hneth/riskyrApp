## ui.R
## riskyrApp | R Shiny | spds, uni.kn | 2018 01 10
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 

# rm(list=ls()) # clean all.

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
## Dependencies:

library("shiny")
library("shinyBS")
library("markdown")
library("DT")
library("diagram")
library("shape")
library("tidyr")
library("dplyr")
library("ggplot2")
library("vcd")
library("colourpicker")



## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
## Initial environment:

e1 <- list("name" = "Demo",  # name (e.g., HIV, mammography, ...)
           "N" = 100,        # N in population
           "prev" = .15,     # prevalence in population = p(true positive)
           "sens" = .85,     # sensitivity = p(positive decision | true positive)
           "spec" = .75,     # specificity = p(negative decision | true negative)
           "source" = "source information" # information source (e.g., citation)
)

env <- e1 # from current environment

## Import ready-made and worked out example data:
datasets <- read.csv2("./www/examples_riskyR.csv", stringsAsFactors = FALSE)
            # WAS: read.csv2("./www/datasets_riskyr.csv", stringsAsFactors = FALSE)
            # WAS: read.csv("./www/riskyR_datasets.csv", stringsAsFactors = FALSE)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
## JavaScript:

{
  ## Source: https://stackoverflow.com/questions/30502870/shiny-slider-on-logarithmic-scale
  
  ## logifySlider javascript function: 
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

## call logifySlider for each relevant sliderInput: 
JS.onload <-
  "
// execute upon document loading: 
$(document).ready(function() {
// wait a few ms to allow other scripts to execute
setTimeout(function() {
// include call for each slider
logifySlider('log_slider', sci = false)
logifySlider('log_slider2', sci = true)
}, 5)})
"

}

## colors


seeblau <- rgb(0, 169, 224, max = 255) # seeblau.4 (non-transparent)

## from https://bootswatch.com/sandstone/ 

col.sand.light = rgb(248, 245, 240, max = 255)
col.sand.mid   = rgb(142, 140, 132, max = 255)
col.sand.dark  = rgb(62, 63, 58, max = 255)


col.grey.1 <- rgb(181, 179, 174, max = 255)
col.grey.2 <- rgb(123, 121, 113, max = 255)
col.grey.3 <- "grey25"
col.grey.4 <- "grey10"

col.green.1 <- rgb(184, 217, 137, max = 255)
col.green.2 <- rgb(128, 177, 57, max = 255)

col.red.1 <- rgb(230, 142, 140, max = 255)
col.red.2 <- rgb(210, 52, 48, max = 255)

col.blue.1 <- rgb(115, 200, 234, max = 255)
col.blue.2 <- rgb(121, 149, 177, max = 255)
col.blue.3 <- rgb(29, 149, 198, max = 255)
col.blue.4 <- rgb(40, 74, 108, max = 255)

col.orange.1 <- rgb(247, 169, 127, max = 255)
col.orange.2 <- rgb(242, 100, 24, max = 255)



## Define named colors for app display:
col.ppv <- col.orange.2 # "orange3" # "firebrick" "red3"
col.npv <- col.blue.3 # seeblau "steelblue3" # "green4" "gray50" "brown4" "chartreuse4"  
sdt.colors <- setNames(c(col.green.2, col.red.2, col.green.1, col.red.1), 
                       c("hi", "mi", "cr", "fa"))




## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
## Define user interface logic:

shinyUI(
  
  # tags$head(tags$script(HTML(JS.logify))),
  # tags$head(tags$script(HTML(JS.onload))),
  
  navbarPage(title = "riskyrApp",
             theme = "bootstrap.sandstone.css",
             ## or another bootsstrap theme https://bootswatch.com/3/, e.g., 
             # theme = "bootstrap.yeti.css",
             # theme = "bootstrap.simplex.css",
             # theme = "bootstrap.lumen.css", 
             # theme = "bootstrap.paper.css",
             # theme = "bootstrap.cosmo.css",
             # theme = "bootstrap.spacelab.css",
             # theme = "bootstrap.flatly.css",
             # theme = "bootstrap.slate.css",
             
             #####               
             tabPanel("1: Representations",
                      icon = icon("tree-deciduous", lib = "glyphicon"),
                      
                      #####
                      sidebarLayout(
                        #####
                        # Sidebar panel for inputs:
                        sidebarPanel(
                          
                          # Input: Select all input values:

                          h3("Please select inputs:"),
                          helpText("(Use slider or enter number)"),
                          br(),
                          fluidRow(h5(tags$b("Population")),
                            column(7,
                          sliderInput("N",
                                      # label = "Population size:",
                                      label = NULL,
                                      value = 100,
                                      min = 0,
                                      max = 10^6,
                                      step = 10)
                                      ), # use log-scale from 1 to 10^9
                          column(5,
                          numericInput("numN", 
                                       label = NULL, 
                                       value = 100,
                                       min = 0,
                                       max = 10^6,
                                       step = 10)
                          )
                          ),
                          br(),
                          fluidRow(h5(tags$b("Prevalence")),
                            column(7, 
                          sliderInput("prev", 
                                      # label = "Prevalence:", 
                                      label = NULL,
                                      sep = "",
                                      value = 0.15, 
                                      min = 0,
                                      max = 1,
                                      step = 10^-6)
                                      ),
                          column(5, 
                          numericInput("numprev", 
                                       label = NULL, 
                                       value = 0.15,
                                       min = 0,
                                       max = 1,
                                       step = 10^-6)
                          )),
                          br(),
                          sliderInput("sens", 
                                      label = "Sensitivity", sep = "",
                                      value = 0.85,
                                      min = 0,
                                      max = 1,
                                      step = 10^-6),
                          numericInput("numsens", 
                                       label = NULL, 
                                       value = 0.85,
                                       min = 0,
                                       max = 1,
                                       step = 10^-6),
                          br(),
                          sliderInput("spec", 
                                      label = "Specificity", sep = "",
                                      value = 0.75,
                                      min = 0,
                                      max = 1, 
                                      step = 10^-6),
                          numericInput("numspec", 
                                       label = NULL, 
                                       value = 0.75,
                                       min = 0,
                                       max = 1,
                                       step = 10^-6),
                          
                          br(), 
                          
                          ## Provide existing data sets as drop-down list:
                          selectInput("dataselection", label = "Or view an example:", 
                                      choices = setNames(as.list(1:nrow(datasets)), # create choices from datasets
                                                         datasets$dataset), 
                                      selected = 1),
                          
                          bsButton("inputhelp", label = "Help", 
                                   icon = icon("question-sign", lib = "glyphicon"),
                                   style = "default", type = "action"),
                          
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
                                      # Intro
                                      #####
                                      tabPanel("Intro",
                                               br(),
                                               "This is just a quick page for displaying rendered text based on inputs. ",
                                               "Spacing doesn't work yet, but that's only formatting... ",
                                               br(), br(), 
                                               "The current set of parameters are as follows:", 
                                               br(), br(), 
                                               textOutput("N"),
                                               textOutput("prev"),
                                               textOutput("sens"),
                                               textOutput("spec")
                                               ),
                                      # Stats
                                      #####
                                      tabPanel("Stats",
                                               br(),
                                               withMathJax(includeMarkdown("www/statstab_riskyr.md")),
                                               br(),
                                               tableOutput("confusiontable1"),
                                               br(),
                                               withMathJax(includeMarkdown("www/statstab_riskyr_ACC.md")),
                                               uiOutput("ACC"),
                                               br(), br(),
                                               withMathJax(includeMarkdown("www/statstab_riskyr_PPV1.md")),
                                               uiOutput("PPV1"),
                                               withMathJax(includeMarkdown("www/statstab_riskyr_PPV2.md")),
                                               uiOutput("PPV2"),
                                               br(), br(),
                                               withMathJax(includeMarkdown("www/statstab_riskyr_NPV1.md")),
                                               uiOutput("NPV1"),
                                               withMathJax(includeMarkdown("www/statstab_riskyr_NPV2.md")),
                                               uiOutput("NPV2"),
                                               br(), br(),
                                               withMathJax(includeMarkdown("www/statstab_riskyr_FORFDR.md"))
                                               ),
                                      # Cases
                                      #####
                                      tabPanel("Cases", 
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
                                      # Icons
                                      #####
                                      tabPanel("Icons", 
                                               br(), 
                                               paste0("Icon array:"), 
                                               br(), br()
                                               ),
                                      # Tree
                                      #####
                                      tabPanel("Tree", 
                                               br(), 
                                               paste0("Tree of natural frequencies:"), 
                                               br(), br(),  
                                               plotOutput("nftree"), 
                                               br()
                                               ),
                                      # Table
                                      #####
                                      tabPanel("Table", 
                                               br(), 
                                               paste0("Aggregated cases:"), 
                                               br(), br(),  
                                               tableOutput("confusiontable2"),
                                               br(),
                                               paste0("The following mosaic plot shows the cell frequencies as area sizes:"), 
                                               br(),  br(), 
                                               plotOutput("mosaicplot"),
                                               br()
                                               ),
                                      # PV curves
                                      #####
                                      tabPanel("PV curves", 
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
                                      # PV cubes
                                      #####
                                      tabPanel("PV cubes", 
                                               br(),
                                               paste0("Predictive values (PPV/NPV) by sensitivity and specificity:"), br(), br(),
                                               fluidRow(
                                                 column(6, # plotOutput("PVplanes"), DEPRECATED 
                                                        plotOutput("PV3dPPV")),
                                                 column(6, 
                                                        plotOutput("PV3dNPV"))
                                                 ),
                                               br(),
                                               # paste0("PPV = ", data$PPV, ", NPV = ", data$NPV), 
                                               # ERROR: object of type 'closure' is not subsettable ???
                                               br(),
                                               wellPanel(
                                                 # br(),
                                                 checkboxInput("boxPVpoints2", label = "Show current PPV/NPV in plots", value = TRUE), 
                                                 br(),
                                                 fluidRow(
                                                   column(6, sliderInput("theta", "Horizontal viewing angle:", 
                                                                         value = -45, min = -90, max = +90)
                                                          ),
                                                   column(6, sliderInput("phi", "Vertical viewing angle:",
                                                                      value = 0, min = 0, max =  90))
                                                   )
                                               ),
                                               # br(), 
                                               # "Perspective effects:",
                                               # br(),
                                               # sliderInput("d", 
                                               #             "D (in-/decrease perspective effect):",
                                               #             value = 1.2,
                                               #             min = 0.1,
                                               #             max = 2), 
                                               # # br(),
                                               # sliderInput("expand", 
                                               #             "Expansion (in z-direction):",
                                               #             value = 0.9,
                                               #             min = 0.1,
                                               #             max = 2), 
                                               # br(),
                                               # "Color effects:",
                                               # br(),
                                               # sliderInput("ltheta", 
                                               #             "Ltheta (...):",
                                               #             value = 200,
                                               #             min = 0,
                                               #             max = 1000), 
                                               # # br(),
                                               # sliderInput("shade", 
                                               #             "Shade (...):",
                                               #             value = 0.10,
                                               #             min = 0,
                                               #             max = 1), 
                                               # br(),
                                               br()
                                               )

                              )
                        )
                      )
             ),
             
             #####        
             tabPanel("2: Information",
                      icon = icon("education", lib = "glyphicon")
                      
             ),
             
             #####
             tabPanel("3: About", 
                      icon = icon("home", lib = "glyphicon"),
                      includeMarkdown("about.md")
             ),
             
             #####
             navbarMenu("4: Customize",
                        icon = icon("wrench", lib = "glyphicon"), # or icon for "adjust
                        
                        # spacer
                        "----",
                        
                        # Customize labels:
                        #####
                        tabPanel("Customize labels",
                                 icon = icon("pencil", lib = "glyphicon"),
                                 sidebarLayout(

                                   # Sidebar panel for inputs:
                                   sidebarPanel(
                                     # Inputs for label customization:
                                     h3("Use your own labels!"),
                                     helpText("(Just enter values below)"),
                                     br(),
                                     textInput("target.population.lbl",
                                               label = "Description of population:",
                                               value = "Population description"),
                                     textInput("scenario.txt",
                                               label = "Description of scenario:",
                                               value = "Describe the scenario in a paragraph here."),
                                     br(),
                                     textInput("condition.lbl",
                                               label = "Condition name:",
                                               value = "Current condition"),
                                     textInput("cond.true.lbl",
                                               label = "Condition true",
                                               value = "Condition true"),
                                     textInput("cond.false.lbl",
                                               label = "Condition false",
                                               value = "Condition false"),
                                     br(),
                                     textInput("decision.lbl",
                                               label = "Decision",
                                               value = "Diagnostic decision"),
                                     textInput("dec.true.lbl",
                                               label = "Decision positive",
                                               value = "Decision positive"),
                                     textInput("dec.false.lbl",
                                               label = "Decision negative",
                                               value = "Decision negative"),
                                     br(),
                                     textInput("sdt.hi.lbl",
                                               label = "Hit",
                                               value = "hit"),
                                     textInput("sdt.mi.lbl",
                                               label = "Miss",
                                               value = "miss"),
                                     textInput("sdt.fa.lbl",
                                               label = "False alarm",
                                               value = "false alarm"),
                                     textInput("sdt.cr.lbl",
                                               label = "Correct rejection",
                                               value = "correct rejection"),
                                     br(),
                                     bsButton("applycustomlabel", label = "Customize!",
                                              icon = icon("wrench", lib = "glyphicon"),
                                              style = "default", type = "action"),
                                     bsButton("resetcustomlabel", label = "Reset default",
                                              icon = icon("refresh", lib = "glyphicon"),
                                              style = "default", type = "action")
                                   ),

                                   #####
                                   ## Main panel for displaying different aspects about risk:
                                   mainPanel("Would be cool to have a sample here, e.g. a 
                                             scenario generated from the inputs.")
                                 )
                        ),
                        
                        # spacer
                        "----",
                        # Customize colors:
                        #####
                        tabPanel("Customize colors",
                                 icon = icon("adjust", lib = "glyphicon"),
                                 #####
                                 sidebarPanel(
                                   # Inputs for color customization:
                                   h3("Choose your own colors!"),
                                   helpText("(Just select colors below)"),
                                   br(),
                                   colourInput("color.hi", label = "Choose the color for hits",
                                               value = sdt.colors["hi"], showColour = "background",
                                               palette = "square", allowedCols = NULL),
                                   colourInput("color.mi", label = "Choose the color for miss",
                                               value = sdt.colors["mi"], showColour = "background",
                                               palette = "square", allowedCols = NULL),
                                   colourInput("color.fa", label = "Choose the color for false alarm",
                                               value = sdt.colors["fa"], showColour = "background",
                                               palette = "square", allowedCols = NULL),
                                   colourInput("color.cr", label = "Choose the color for correct rejection",
                                               value = sdt.colors["cr"], showColour = "background",
                                               palette = "square", allowedCols = NULL),
                                   br(),
                                   colourInput("color.ppv", label = "Color for the positive predictive value (PPV)",
                                               value = col.ppv , showColour = "background",
                                               palette = "square", allowedCols = NULL),
                                   colourInput("color.npv", label = "Color for the negative predictive value (NPV)",
                                               value = col.npv, showColour = "background",
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
                                 ## Main panel for displaying different aspects about risk:
                                 mainPanel(h3("Here are simplified preview plots of your colors:"),
                                           "Click the 'Customize' button to update your color selection.",
                                           plotOutput("sampleplot"),
                                           plotOutput("sampleplotcurves")
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
