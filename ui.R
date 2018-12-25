# ui.R
# riskyrApp | R Shiny | spds, uni.kn | 2018 12 25

## Dependencies: ------

library("shiny")
library("shinyBS")
library("markdown")
library("colourpicker")

## Install current version of riskyr: ------
# detach("package:riskyr", unload = TRUE)
# from CRAN: <https://CRAN.R-project.org/package=riskyr>
# devtools::install_github("hneth/riskyr")
library("riskyr")
# sessionInfo()

## Import data (example scenarios) and default colors and labels: ------

datasets <- read.csv2("./www/df_scenarios_riskyrApp_2018-12-14.csv", stringsAsFactors = FALSE)

# Default colors and text labels: 
default.colors <- init_pal()
default.labels <- init_txt()

## logifySlider javascript function: ------ 
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

## Call logifySlider for each relevant sliderInput: ------ 
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

## Define user interface logic: ------

shinyUI(
  
  navbarPage(title = "riskyrApp",
             theme = "bootstrap.sandstone.css",
             id = "tabs",
             
             # Tab panel: ------ 
             
             tabPanel("Visualize risks",
                      icon = icon("blackboard", lib = "glyphicon"), value = "represent",
                      
                      # Sidebar: ------ 
                      
                      sidebarLayout(
                        
                        # Sidebar panel for inputs: ---- 
                        sidebarPanel(
                          tags$head(tags$script(HTML(JS.logify))),
                          tags$head(tags$script(HTML(JS.onload))),
                          
                          sliderInput("N", label = "Population (logarithmic scale)",
                                      min = 1, max = 5,
                                      value = 3, round = FALSE
                          ),
                          br(),
                          
                          radioButtons("checkprev", label = "Prevalence (in percent)", 
                                       choiceNames = list("Slider", "Field"),
                                       choiceValues = c(0, 1), inline = TRUE
                          ),
                          
                          conditionalPanel(condition = "input.checkprev == 0",
                                           sliderInput("prev",  label = NULL, sep = "",
                                                       value = 15, min = 0, max = 100, step = 1,
                                                       pre = NULL, post = "%"
                                           )
                          ),
                          
                          conditionalPanel(condition = "input.checkprev == 1", 
                                           numericInput("numprev", label = NULL, value = 15,
                                                        min = 0, max = 100, step = 10^-2
                                           )
                          ),
                          br(),
                          
                          radioButtons("checksens", label = "Sensitivity (in percent)", 
                                       choiceNames = list("Slider", "Field"),
                                       choiceValues = c(0, 1), inline = TRUE
                          ),
                          
                          conditionalPanel(condition = "input.checksens == 0",
                                           sliderInput("sens", label = NULL, sep = "", value = 85.00,
                                                       min = 0, max = 100, step = 1,
                                                       pre = NULL, post = "%"
                                           )
                          ),
                          
                          conditionalPanel(condition = "input.checksens == 1", 
                                           numericInput("numsens", label = NULL, value = 85.00,
                                                        min = 0, max = 100, step = 10^-2
                                           )
                          ),
                          
                          radioButtons("checkspec", label = "Specificity (in percent)", 
                                       choiceNames = list("Slider", "Field"),
                                       choiceValues = c(0, 1), inline = TRUE
                          ),
                          
                          conditionalPanel(condition = "input.checkspec == 0",
                                           sliderInput("spec", label = NULL, sep = "", value = 75,
                                                       min = 0, max = 100, step = 1,
                                                       pre = NULL, post = "%"
                                           )
                          ),
                          
                          conditionalPanel(condition = "input.checkspec == 1", 
                                           numericInput("numspec", label = NULL, value = 75,
                                                        min = 0, max = 100, step = 10^-2
                                           )
                          ),
                          br(), 
                          
                          # Provide existing data sets as drop-down list: ------ 
                          
                          selectInput("dataselection", label = "Load example:", 
                                      choices = setNames(as.list(1:nrow(datasets)), # create choices from datasets
                                                         datasets$scen_lbl), 
                                      selected = 1),
                          
                          bsButton("help_inputs", label = "Help",
                                   icon = icon("question-sign", lib = "glyphicon"),
                                   style = "default", type = "action")
                          
                        ),
                        
                        # Main panel for displaying different visualizations: ------ 
                        
                        mainPanel(
                          
                          ## Tabset with prism/table/area/etc.: 
                          tabsetPanel(type = "tabs",
                                      
                                      # Prism: ---- 
                                      
                                      tabPanel("Prism",
                                               br(),
                                               fluidRow(
                                                 column(8, offset = 0, plotOutput("prism", width = "600", height = "450"))),
                                               wellPanel(
                                                 fluidRow(
                                                   column(4, offset = 0,
                                                          selectInput("prism.by", label = "Perspective by:", 
                                                                      choices = list("condition tree" = "cd", 
                                                                                     "decision tree"  = "dc", 
                                                                                     "accuracy tree"  = "ac",
                                                                                     "condition + decision" = "cddc",
                                                                                     "condition + accuracy" = "cdac",
                                                                                     "decision + condition" = "dccd",
                                                                                     "decision + accuracy"  = "dcac",
                                                                                     "accuracy + condition" = "accd",
                                                                                     "accuracy + decision"  = "acdc"),
                                                                      selected = "cddc")
                                                   ),
                                                   column(4, 
                                                          selectInput("prism.area", label = "Box area:", 
                                                                      choices = list("default" = "no", 
                                                                                     "squares" = "sq", 
                                                                                     "horizontal rectangles" = "hr"), 
                                                                      selected = "no"))),
                                                 fluidRow(
                                                   column(4, 
                                                          selectInput("prism.f_lbl", label = "Frequency labels:", 
                                                                      choices = list("default" = "def", 
                                                                                     "names only" = "nam",
                                                                                     "values only" = "num",
                                                                                     "names and values" = "namnum",
                                                                                     "none" = "no"
                                                                      ), 
                                                                      selected = "num")),
                                                   column(4, 
                                                          selectInput("prism.p_lbl", label = "Probability labels:", 
                                                                      choices = list("default" = "def",
                                                                                     "minimum" = "min",
                                                                                     "mix names + values" = "mix", 
                                                                                     "names only" = "nam",
                                                                                     "values only" = "num",
                                                                                     "names + values" = "namnum",
                                                                                     "none" = "no"
                                                                      ), 
                                                                      selected = "no")),
                                                   column(2, offset = 1, downloadButton("prism.dl", label = "Save prism"))
                                                 )
                                               )
                                      ),
                                      
                                      # Table: ---- 
                                      
                                      tabPanel("Table", 
                                               br(),
                                               fluidRow(
                                                 column(8, offset = 0, plotOutput("table", width = "600", height = "450"))),
                                               wellPanel(
                                                 fluidRow(
                                                   column(3, offset = 0,
                                                          selectInput("table.by", label = "Perspective by:", 
                                                                      choices = list("condition + decision" = "cddc",
                                                                                     "condition + accuracy" = "cdac",
                                                                                     "decision + condition" = "dccd",
                                                                                     "decision + accuracy"  = "dcac",
                                                                                     "accuracy + condition" = "accd",
                                                                                     "accuracy + decision"  = "acdc"),
                                                                      selected = "cddc")
                                                   ),
                                                   column(3, 
                                                          selectInput("table.p_split", label = "Population split:", 
                                                                      choices = list("vertical" = "v", 
                                                                                     "horizontal" = "h"), 
                                                                      selected = "v")),
                                                   column(3, 
                                                          selectInput("table.f_lbl", label = "Frequency labels:", 
                                                                      choices = list("default" = "def", 
                                                                                     "abbr. names" = "abbr",
                                                                                     "names only" = "nam",
                                                                                     "values only" = "num",
                                                                                     "names and values" = "namnum",
                                                                                     "no labels" = "no"
                                                                      ), 
                                                                      selected = "num")),
                                                   column(2, offset = 1, downloadButton("table.dl", label = "Save table"))
                                                 )
                                               )
                                      ),
                                      
                                      # Area: ---- 
                                      
                                      tabPanel("Area", 
                                               br(),
                                               fluidRow(
                                                 column(8, offset = 0, plotOutput("area", width = "600", height = "450"))),
                                               wellPanel(
                                                 fluidRow(
                                                   column(3, offset = 0,
                                                          selectInput("area.by", label = "Perspective by:", 
                                                                      choices = list("condition + decision" = "cddc",
                                                                                     "condition + accuracy" = "cdac",
                                                                                     "decision + condition" = "dccd",
                                                                                     "decision + accuracy"  = "dcac",
                                                                                     "accuracy + condition" = "accd",
                                                                                     "accuracy + decision"  = "acdc"),
                                                                      selected = "cddc")
                                                   ),
                                                   column(3, 
                                                          selectInput("area.p_split", label = "Split by:", 
                                                                      choices = list("vertical" = "v", 
                                                                                     "horizontal" = "h"), 
                                                                      selected = "v")),
                                                   column(3, 
                                                          selectInput("area.f_lbl", label = "Labels:", 
                                                                      choices = list("default" = "def", 
                                                                                     "abbr. names" = "abbr",
                                                                                     "names only" = "nam",
                                                                                     "values only" = "num",
                                                                                     "names and values" = "namnum",
                                                                                     "no labels" = "no"
                                                                      ), 
                                                                      selected = "num")),
                                                   column(2, offset = 1, downloadButton("area.dl", label = "Save area"))
                                                 )
                                               )
                                      ),
                                      
                                      # Icons: ---- 
                                      
                                      tabPanel("Icons", 
                                               br(), 
                                               fluidRow(
                                                 column(8, offset = 0, plotOutput("icons", width = "600", height = "450"))),
                                               wellPanel(
                                                 fluidRow(
                                                   column(4, offset = 0,
                                                          selectInput("icons.arr_type", "Array type:",
                                                                      choices = list("array" = "array", 
                                                                                     "shuffled" = "shuffledarray",
                                                                                     "mosaic" = "mosaic",
                                                                                     "fill equal" = "fillequal",
                                                                                     "fill left" = "fillleft",
                                                                                     "fill top" = "filltop",
                                                                                     "scattered" = "scatter"), 
                                                                      selected = "array")),
                                                   column(2, offset = 6, downloadButton("icons.dl", label = "Save icons"))
                                                 ), 
                                                 br(),
                                                 fluidRow(
                                                   column(3,
                                                          selectInput("symbol.hi", label = "hi (TP) symbol:", 
                                                                      choices = list("Circle" = 21, "Square" = 22, "Rhombus" = 23, "Triangle" = 24),
                                                                      selected = "22")
                                                   ),
                                                   column(3,
                                                          selectInput("symbol.mi", label = "mi (FN) symbol:", 
                                                                      choices = list("Circle" = 21, "Square" = 22, "Rhombus" = 23, "Triangle" = 24),
                                                                      selected = "22")
                                                   ),
                                                   column(3,
                                                          selectInput("symbol.cr", label = "cr (TN) symbol:", 
                                                                      choices = list("Circle" = 21, "Square" = 22, "Rhombus" = 23, "Triangle" = 24),
                                                                      selected = "22")
                                                   ),
                                                   column(3,
                                                          selectInput("symbol.fa", label = "fa (FP) symbol:", 
                                                                      choices = list("Circle" = 21, "Square" = 22, "Rhombus" = 23, "Triangle" = 24),
                                                                      selected = "22")
                                                   )
                                                 )
                                               )
                                      ),
                                      
                                      # Bars: ----  
                                      
                                      tabPanel("Bars", 
                                               br(),
                                               fluidRow(
                                                 column(8, offset = 0, plotOutput("bar", width = "600", height = "450"))),
                                               wellPanel(
                                                 fluidRow(
                                                   column(3, offset = 0,
                                                          selectInput("bar.by", label = "Perspective by:", 
                                                                      choices = list("condition" = "cd",
                                                                                     "decision"  = "dc",
                                                                                     "accuracy"  = "ac",
                                                                                     "all" = "all"),
                                                                      selected = "all")
                                                   ),
                                                   column(3, 
                                                          selectInput("bar.dir", label = "Directions:", 
                                                                      choices = list("uni-directional" = 1, 
                                                                                     "bi-directional"  = 2), 
                                                                      selected = 1)),
                                                   column(3, 
                                                          selectInput("bar.f_lbl", label = "Labels:", 
                                                                      choices = list("default" = "def", 
                                                                                     "abbr. names" = "abbr",
                                                                                     "names only" = "nam",
                                                                                     "values only" = "num",
                                                                                     "names and values" = "namnum",
                                                                                     "no labels" = "no"
                                                                      ), 
                                                                      selected = "num")),
                                                   column(2, offset = 1, downloadButton("bar.dl", label = "Save bars"))
                                                 )
                                               )
                                      ),
                                      
                                      # Curves: ---- 
                                      
                                      tabPanel("Curves", 
                                               br(),
                                               fluidRow(
                                                 column(12, offset = 0, plotOutput("curve", width = "600", height = "450"))),
                                               wellPanel(
                                                 fluidRow(
                                                   column(2, offset = 2, checkboxInput("curve.show_points", label = "Show point values", value = TRUE)),
                                                   column(3, checkboxInput("curve.log_scale", label = "Scale prevalence on logarithmic scale", value = FALSE))),
                                                 fluidRow(
                                                   column(2, offset = 2, checkboxInput("curve.show_acc", label = "Accuracy (acc)", value = FALSE)),
                                                   column(4, checkboxInput("curve.show_ppod", label = "Proportion of positive decisions (ppod)", value = FALSE)),
                                                   column(1, downloadButton("curve.dl", label = "Save curves"))
                                                 )
                                               )
                                               
                                      ),
                                      
                                      # Planes: ---- 
                                      
                                      tabPanel("Planes", 
                                               br(),
                                               fluidRow(
                                                 column(6, plotOutput("plane.ppv")),
                                                 column(6, plotOutput("plane.npv"))
                                               ),
                                               br(),
                                               br(),
                                               wellPanel(
                                                 fluidRow(
                                                   column(3, checkboxInput("plane.show_point", label = "Show current PPV/NPV in plots", value = TRUE)), 
                                                   column(2, offset = 1, downloadButton("plane.ppv.dl", label = "Save PPV plane")),
                                                   column(2, offset = 4,
                                                          downloadButton("plane.npv.dl", label = "Save NPV plane"))
                                                 ),
                                                 br(),
                                                 fluidRow(
                                                   column(6, sliderInput("theta", "Horizontal viewing angle:", value = -45, min = -90, max = +90)),
                                                   column(6, sliderInput("phi", "Vertical viewing angle:", value = 0, min = 0, max =  90))
                                                 ),
                                                 br()
                                               )
                                      ),
                                      
                                      # Contrast 4 representations: ------
                                      
                                      tabPanel("Compare", 
                                               br(),
                                               fluidRow(
                                                 column(6, plotOutput("represent1", width = "500", height = "350")),
                                                 column(6, plotOutput("represent2", width = "500", height = "350")),
                                                 column(6, plotOutput("represent3", width = "500", height = "350")),
                                                 column(6, plotOutput("represent4", width = "500", height = "350"))
                                               ),
                                               br(),
                                               br(),
                                               wellPanel(
                                                 fluidRow(
                                                   column(3,
                                                          selectInput("represent1", label = "Selection 1:", 
                                                                      choices = list("Prism" = "prism", 
                                                                                     "Table" = "table",
                                                                                     "Area" = "area",
                                                                                     "Icons" = "icons",
                                                                                     "Bars" = "bar",
                                                                                     "Curves" = "curve",
                                                                                     "Plane PPV" = "plane.ppv",
                                                                                     "Plane NPV" = "plane.npv"),
                                                                      selected = "prism")
                                                   ),
                                                   column(3,
                                                          selectInput("represent2", label = "Selection 2:", 
                                                                      choices = list("Prism" = "prism", 
                                                                                     "Table" = "table",
                                                                                     "Area" = "area",
                                                                                     "Icons" = "icons",
                                                                                     "Bars" = "bar",
                                                                                     "Curves" = "curve",
                                                                                     "Plane PPV" = "plane.ppv",
                                                                                     "Plane NPV" = "plane.npv"),
                                                                      selected = "table")
                                                   ),
                                                   column(3,
                                                          selectInput("represent3", label = "Selection 3:", 
                                                                      choices = list("Prism" = "prism", 
                                                                                     "Table" = "table",
                                                                                     "Area" = "area",
                                                                                     "Icons" = "icons",
                                                                                     "Bars" = "bar",
                                                                                     "Curves" = "curve",
                                                                                     "Plane PPV" = "plane.ppv",
                                                                                     "Plane NPV" = "plane.npv"),
                                                                      selected = "icons")
                                                   ),
                                                   column(3,
                                                          selectInput("represent4", label = "Selection 4:", 
                                                                      choices = list("Prism" = "prism", 
                                                                                     "Table" = "table",
                                                                                     "Area" = "area",
                                                                                     "Icons" = "icons",
                                                                                     "Bars" = "bar",
                                                                                     "Curves" = "curve",
                                                                                     "Plane PPV" = "plane.ppv",
                                                                                     "Plane NPV" = "plane.npv"),
                                                                      selected = "bar")
                                                   )
                                                 )
                                               )
                                      )
                                      
                                      
                          )
                        )
                      )
             ),
             
             # Customize labels: ------ 
             
             tabPanel("Customize labels",
                      icon = icon("pencil", lib = "glyphicon"), value = "custom_labels",
                      
                      sidebarLayout(
                        
                        # Sidebar panel for inputs: ------ 
                        sidebarPanel(
                          # Inputs for label customization:
                          h3("Use your own labels"),
                          br(),
                          fluidRow(
                            column(6, textInput("scen_lbl",
                                                label = "Scenario label:",
                                                value = default.labels$scen_lbl))
                            # ,
                            # column(6, textInput("scen_txt",
                            # label = "Description of scenario:",
                            # value = default.labels$scen_txt))
                          ),
                          br(),
                          fluidRow(
                            # column(6, textInput("N_lbl",
                            #                     label = "Label for population:",
                            #                     value = default.labels$N_lbl))
                            # ,
                            column(6, textInput("popu_lbl",
                                                label = "Population label:",
                                                value = default.labels$popu_lbl))
                          ),
                          br(),
                          fluidRow(
                            column(4, textInput("cond_lbl",
                                                label = "Condition label:",
                                                value = default.labels$cond_lbl)),
                            column(4, textInput("cond.true_lbl",
                                                label = "Condition true:",
                                                value = default.labels$cond.true_lbl)),
                            column(4, textInput("cond.false_lbl",
                                                label = "Condition false:",
                                                value = default.labels$cond.false_lbl))
                          ),
                          br(),
                          fluidRow(
                            column(3, textInput("hi_lbl", label = "hi (TP):", value = default.labels$hi_lbl)),
                            column(3, textInput("mi_lbl", label = "mi (FN):", value = default.labels$mi_lbl)),
                            column(3, textInput("fa_lbl", label = "fa (FP):", value = default.labels$fa_lbl)),
                            column(3, textInput("cr_lbl", label = "cr (TN):", value = default.labels$cr_lbl))
                          ),
                          br(),
                          fluidRow(
                            column(4, textInput("dec_lbl",
                                                label = "Decision label:",
                                                value = default.labels$dec_lbl)),
                            column(4, textInput("dec.pos_lbl",
                                                label = "Decision positive:",
                                                value = default.labels$dec.pos_lbl)),
                            column(4, textInput("dec.neg_lbl",
                                                label = "Decision negative:",
                                                value = default.labels$dec.neg_lbl))
                          ),
                          br(), br(),
                          br(), br(),
                          bsButton("resetcustomlabel", label = "Reset defaults",
                                   icon = icon("refresh", lib = "glyphicon"),
                                   style = "default", type = "action"),
                          bsButton("help_custom_labels", label = "Help",
                                   icon = icon("question-sign", lib = "glyphicon"),
                                   style = "default", type = "action")
                        ),
                        
                        # Main panel for displaying preview of labels: ------ 
                        mainPanel(
                          br(),
                          h3("Preview of current text labels"),
                          br(), br(),
                          fluidRow(offset = 1,
                                   column(6, plotOutput("previewlabels", width = "650", height = "500")
                                   )
                          )
                          
                        )
                      )
             ),
             
             # Customize colors: ------- 
             
             tabPanel("Customize colors",
                      icon = icon("wrench", lib = "glyphicon"),
                      value = "custom_colors",
                      sidebarLayout(
                        
                        sidebarPanel(
                          # Inputs for color customization:
                          h3("Use your own colors"),
                          br(),
                          fluidRow(
                            column(4, colourInput("color.hi", label = "hi (TP):",
                                                  value = default.colors["hi"], showColour = "background",
                                                  palette = "square", allowedCols = NULL)),
                            column(4, colourInput("color.fa", label = "fa (FP):",
                                                  value = default.colors["fa"], showColour = "background",
                                                  palette = "square", allowedCols = NULL)),
                            
                            column(4, colourInput("color.pos", label = "Positive decisions:",
                                                  value = default.colors["pos"], showColour = "background",
                                                  palette = "square", allowedCols = NULL))
                          ),
                          fluidRow(
                            column(4, colourInput("color.mi", label = "mi (FN):",
                                                  value = default.colors["mi"], showColour = "background",
                                                  palette = "square", allowedCols = NULL)),
                            column(4, colourInput("color.cr", label = "cr (TN):",
                                                  value = default.colors["cr"], showColour = "background",
                                                  palette = "square", allowedCols = NULL)),
                            column(4, colourInput("color.neg", label = "Negative decisions:",
                                                  value = default.colors["neg"], showColour = "background",
                                                  palette = "square", allowedCols = NULL))
                          ),
                          fluidRow(
                            column(4, colourInput("color.true", label = "Condition true:",
                                                  value = default.colors["true"], showColour = "background",
                                                  palette = "square", allowedCols = NULL)),
                            column(4, colourInput("color.false", label = "Condition false:",
                                                  value = default.colors["false"], showColour = "background",
                                                  palette = "square", allowedCols = NULL)),
                            column(4, colourInput("color.N", label = "Population:",
                                                  value = default.colors["N"], showColour = "background",
                                                  palette = "square", allowedCols = NULL))
                          ),
                          br(), br(),
                          br(), br(),
                          br(), br(),
                          fluidRow(
                            column(6, colourInput("color.ppv", label = "Positive predictive value (PPV):",
                                                  value = default.colors["ppv"], showColour = "background",
                                                  palette = "square", allowedCols = NULL)),
                            column(6, colourInput("color.npv", label = "Negative predictive value (NPV):",
                                                  value = default.colors["npv"], showColour = "background",
                                                  palette = "square", allowedCols = NULL))
                          ),
                          fluidRow(
                            column(6, colourInput("color.txt", label = "Text:",
                                                  value = default.colors["txt"], showColour = "background",
                                                  palette = "square", allowedCols = NULL)),
                            column(6, colourInput("color.brd", label = "Lines:",
                                                  value = default.colors["brd"], showColour = "background",
                                                  palette = "square", allowedCols = NULL))
                          ),
                          br(), br(),
                          fluidRow(
                            column(6,
                                   selectInput("alt.palette", label = "Load color palette:",
                                               choices = list("---" = "default",
                                                              "4 colours" = "pal_4c",
                                                              "black & white" = "pal_bw",
                                                              "green, blue, sand" = "pal_gbs",
                                                              "uni.kn" = "pal_kn",
                                                              "viridis" = "pal_vir"), selected = 1)
                            )
                          ),
                          br(),
                          fluidRow(
                            column(3,                          
                                   bsButton("resetcustomcolor", label = "Reset default",
                                            icon = icon("refresh", lib = "glyphicon"),
                                            style = "default", type = "action")
                            ),
                            column(3,
                                   bsButton("help_custom_colors", label = "Help",
                                            icon = icon("question-sign", lib = "glyphicon"),
                                            style = "default", type = "action")
                            )
                            
                          )
                        ),
                        
                        
                        # Main panel for displaying preview plots with colors: ---- 
                        mainPanel(
                          br(),
                          h3("Preview of current colors"),
                          fluidRow(offset = 1,
                                   column(6, plotOutput("sample.table", width = "450", height = "350"))
                          ),
                          br(),
                          fluidRow(
                            column(6, plotOutput("sample.curves", width = "450", height = "350"))
                          )
                        )
                      )
             ),
             
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
             
             
             # Tooltips: ------ 
             
             # On inputs
             bsTooltip(id = list("N", "numN"), 
                       title = "N of individuals in population",
                       placement = "right", trigger = "hover", options = list(container = "body")),
             
             bsTooltip(id = list("prev", "numprev"),
                       title = "Probability of condition: p(true)",
                       placement = "right", trigger = "hover", options = list(container = "body")),
             
             bsTooltip(id = list("sens", "numsens"),
                       title = "Probability of correctly detecting true condition: p(decision positive | condition true)",
                       placement = "right", trigger = "hover", options = list(container = "body")),
             
             bsTooltip(id = list("spec", "numspec"),
                       title = "Probability of correctly detecting false condition: p(decision negative | condition false)",
                       placement = "right", trigger = "hover", options = list(container = "body")),
             
             # Download buttons: ------ 
             
             bsTooltip(id = list("prism.dl", "table.dl", "area.dl", "icons.dl", "bar.dl", "curve.dl", 
                                 "plane.ppv.dl", "plane.npv.dl"),
                       title = "Save current graph as .png file.",
                       placement = "left", trigger = "hover", options = list(container = "body"))
             
             
  )
)

## eof. ---------- 