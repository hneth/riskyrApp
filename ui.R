# ui.R
# riskyrApp | R Shiny | spds, uni.kn | 2019 01 06

## Dependencies: ------

library("shiny")
library("shinyBS")
library("markdown")
library("colourpicker")
library("shinyWidgets")

## Install/load current version of riskyr: ------
# detach("package:riskyr", unload = TRUE)
# from CRAN: <https://CRAN.R-project.org/package=riskyr>
# devtools::install_github("hneth/riskyr")
library("riskyr")
# sessionInfo()

## Import data (example scenarios): ------

datasets <- read.csv2("./www/df_scenarios.csv", stringsAsFactors = FALSE)  # 2018 12 30

# Default color palette and text labels: ------ 
default.colors <- pal_mod  # init_pal() 
default.labels <- txt_TF   # init_txt()


## Define user interface logic: ------

shinyUI(
  navbarPage(title = "riskyrApp",
             theme = "bootstrap.sandstone.css",
             id = "tabs",
             
             # 1. Tab panel: Visualizations ------ 
             
             tabPanel("Visualize risks",
                      icon = icon("blackboard", lib = "glyphicon"), value = "represent",
                      
                      # Sidebar: ------ 
                      
                      sidebarLayout(
                        
                        # A. Sidebar panel for inputs: ---- 
                        sidebarPanel(
                          radioButtons("checkN", label = "Population", 
                                       choiceNames = list("Slider", "Field"),
                                       choiceValues = c(0, 1), inline = TRUE),
                          conditionalPanel(condition = "input.checkN == 0",
                                         sliderTextInput(inputId = "N", label = NULL,
                                                        choices = c(10, 100, 1000, 10000, 100000),
                                                        grid = TRUE, selected = 1000,
                                                        animate = FALSE)),
                                                        # animate = animationOptions(interval = 1500,
                                                                                   # loop = TRUE))),
                          
                          
                          conditionalPanel(condition = "input.checkN == 1", 
                                           numericInput("numN", label = NULL, value = 1000,
                                                        min = 10, max = 100000, step = 1)),
                          br(),
                          
                          radioButtons("checkprev", label = "Prevalence (in percent)", 
                                       choiceNames = list("Slider", "Field"),
                                       choiceValues = c(0, 1), inline = TRUE),
                          
                          conditionalPanel(condition = "input.checkprev == 0",
                                           sliderInput("prev",  label = NULL, sep = "",
                                                       value = 15, min = 0, max = 100, step = 1,
                                                       pre = NULL, post = "%")),
                          
                          conditionalPanel(condition = "input.checkprev == 1", 
                                           numericInput("numprev", label = NULL, value = 15,
                                                        min = 0, max = 100, step = 10^-2)),
                          br(),
                          
                          radioButtons("checksens", label = "Sensitivity (in percent)", 
                                       choiceNames = list("Slider", "Field"),
                                       choiceValues = c(0, 1), inline = TRUE),
                          
                          conditionalPanel(condition = "input.checksens == 0",
                                           sliderInput("sens", label = NULL, sep = "", value = 85.00,
                                                       min = 0, max = 100, step = 1,
                                                       pre = NULL, post = "%")),
                          
                          conditionalPanel(condition = "input.checksens == 1", 
                                           numericInput("numsens", label = NULL, value = 85.00,
                                                        min = 0, max = 100, step = 10^-2)),
                          
                          radioButtons("checkspec", label = "Specificity (in percent)", 
                                       choiceNames = list("Slider", "Field"),
                                       choiceValues = c(0, 1), inline = TRUE),
                          
                          conditionalPanel(condition = "input.checkspec == 0",
                                           sliderInput("spec", label = NULL, sep = "", value = 75,
                                                       min = 0, max = 100, step = 1,
                                                       pre = NULL, post = "%")),
                          
                          conditionalPanel(condition = "input.checkspec == 1", 
                                           numericInput("numspec", label = NULL, value = 75,
                                                        min = 0, max = 100, step = 10^-2)),
                          br(), 
                          
                          # Provide example data (as drop-down list): ------ 
                          
                          selectInput("dataselection", label = "Load example:", 
                                      choices = setNames(as.list(1:nrow(datasets)), # create choices from datasets
                                                         datasets$scen_lbl), 
                                      selected = NA),
                          
                          bsButton("help_inputs", label = "Help",
                                   icon = icon("question-sign", lib = "glyphicon"),
                                   style = "default", type = "action")
                        ),
                        
                        # B. Main panel for displaying different visualizations: ------ 
                        
                        mainPanel(
                          
                          # Tabset with prism/table/area/etc.: 
                          tabsetPanel(type = "tabs",
                                      
                                      # Prism: ------ 
                                      
                                      tabPanel("Prism",
                                               br(),
                                               
                                               fluidRow(
                                                 column(8, offset = 0, plotOutput("prism", width = "600", height = "450"))),
                                               br(),
                                               
                                               wellPanel(
                                                 fluidRow(
                                                   column(4, offset = 0,
                                                          selectInput("prism.by", label = "Perspective by", 
                                                                      choices = list("condition tree" = "cd", 
                                                                                     "decision tree"  = "dc", 
                                                                                     "accuracy tree"  = "ac",
                                                                                     "condition + decision" = "cddc",
                                                                                     "condition + accuracy" = "cdac",
                                                                                     "decision + condition" = "dccd",
                                                                                     "decision + accuracy"  = "dcac",
                                                                                     "accuracy + condition" = "accd",
                                                                                     "accuracy + decision"  = "acdc"),
                                                                      selected = "cddc")),
                                                   column(4, 
                                                          selectInput("prism.area", label = "Box area", 
                                                                      choices = list("default" = "no", 
                                                                                     "squares" = "sq", 
                                                                                     "horizontal rectangles" = "hr"), 
                                                                      selected = "no"))),
                                                 
                                                 fluidRow(
                                                   column(4, 
                                                          selectInput("prism.f_lbl", label = "Frequency labels", 
                                                                      choices = list("default" = "def", 
                                                                                     "names only"     = "nam",
                                                                                     "values only"    = "num",
                                                                                     "names + values" = "namnum",
                                                                                     "abbr. names"          = "abb",
                                                                                     "abbr. names + values" = "abbnum",
                                                                                     "none" = "no"
                                                                      ), 
                                                                      selected = "num")),
                                                   column(4, 
                                                          selectInput("prism.p_lbl", label = "Probability labels", 
                                                                      choices = list("default" = "def",
                                                                                     "names only"     = "nam",
                                                                                     "values only"    = "num",
                                                                                     "names + values" = "namnum",
                                                                                     "abbr. names only"     = "abb",
                                                                                     "abbr. names + values" = "abbnum",
                                                                                     "main names only"      = "min",
                                                                                     "main names + values"  = "mix", 
                                                                                     "none" = "no"
                                                                      ), 
                                                                      selected = "num"))),
                                                 
                                                 fluidRow(
                                                   column(4, checkboxInput("prism.show_foot", label = "Show margin notes", value = FALSE)),
                                                   column(2, offset = 5, downloadButton("prism.dl", label = "Save prism"))) 
                                               ) # wellPanel. 
                                      ), # tabPanel.
                                      
                                      # Table: ------ 
                                      
                                      tabPanel("Table", 
                                               br(),
                                               
                                               fluidRow(
                                                 column(8, offset = 0, plotOutput("table", width = "600", height = "450"))),
                                               br(),
                                               
                                               wellPanel(
                                                 fluidRow(
                                                   column(4, offset = 0,
                                                          selectInput("table.by", label = "Perspective by", 
                                                                      choices = list("condition x decision" = "cddc",
                                                                                     "condition x accuracy" = "cdac",
                                                                                     "decision x condition" = "dccd",
                                                                                     "decision x accuracy"  = "dcac",
                                                                                     "accuracy x condition" = "accd",
                                                                                     "accuracy x decision"  = "acdc"),
                                                                      selected = "cddc")
                                                   ),
                                                   column(4, 
                                                          selectInput("table.p_split", label = "Population split", 
                                                                      choices = list("vertical" = "v", 
                                                                                     "horizontal" = "h"), 
                                                                      selected = "v"))),
                                                 
                                                 fluidRow(
                                                   column(4, 
                                                          selectInput("table.f_lbl", label = "Frequency labels", 
                                                                      choices = list("default" = "def", 
                                                                                     "names only"     = "nam",
                                                                                     "values only"    = "num",
                                                                                     "names + values" = "namnum",
                                                                                     "abbr. names only"     = "abb",
                                                                                     "abbr. names + values" = "abbnum",
                                                                                     "none" = "no"), 
                                                                      selected = "num")),
                                                   column(4, 
                                                          selectInput("table.p_lbl", label = "Probability labels", 
                                                                      choices = list("none" = "NA",
                                                                                     "names only"     = "nam",
                                                                                     "values only"    = "num",
                                                                                     "names + values" = "namnum",
                                                                                     "abbr. names only"     = "abb",
                                                                                     "abbr. names + values" = "abbnum",
                                                                                     "no labels" = "no"), 
                                                                      selected = "none"))),
                                                 
                                                 fluidRow(
                                                   column(4, checkboxInput("table.show_foot", label = "Show margin notes", value = FALSE)),
                                                   column(2, offset = 5, downloadButton("table.dl", label = "Save table")))
                                               ) # wellPanel. 
                                      ), # tabPanel.
                                      
                                      # Area: ------  
                                      
                                      tabPanel("Area", 
                                               br(),
                                               
                                               fluidRow(
                                                 column(8, offset = 0, plotOutput("area", width = "600", height = "450"))),
                                               br(),
                                               
                                               wellPanel(
                                                 fluidRow(
                                                   column(4, offset = 0,
                                                          selectInput("area.by", label = "Perspective by", 
                                                                      choices = list("condition x decision" = "cddc",
                                                                                     "condition x accuracy" = "cdac",
                                                                                     "decision x condition" = "dccd",
                                                                                     "decision x accuracy"  = "dcac",
                                                                                     "accuracy x condition" = "accd",
                                                                                     "accuracy x decision"  = "acdc"),
                                                                      selected = "cddc")),
                                                   column(4, 
                                                          selectInput("area.p_split", label = "Split by", 
                                                                      choices = list("vertical"   = "v", 
                                                                                     "horizontal" = "h"), 
                                                                      selected = "v"))),
                                                 
                                                 fluidRow(
                                                   column(4, 
                                                          selectInput("area.f_lbl", label = "Frequency labels", 
                                                                      choices = list("default" = "def", 
                                                                                     "names only"     = "nam",
                                                                                     "values only"    = "num",
                                                                                     "names + values" = "namnum",
                                                                                     "abbr. names only"     = "abb",
                                                                                     "abbr. names + values" = "abbnum",
                                                                                     "none" = "no"), 
                                                                      selected = "num")),
                                                   column(4, 
                                                          selectInput("area.p_lbl", label = "Probability labels", 
                                                                      choices = list("none" = "NA",
                                                                                     "names only"     = "nam",
                                                                                     "values only"    = "num",
                                                                                     "names + values" = "namnum",
                                                                                     "abbr. names only"     = "abb",
                                                                                     "abbr. names + values" = "abbnum",
                                                                                     "no labels" = "no"), 
                                                                      selected = "none"))),
                                                 
                                                 fluidRow(
                                                   column(6, sliderInput("area.sum_w", "Marginal sum width", value = 0, min = 0, max = 100, step = 5, pre = NULL, post = "%")),
                                                   column(3, checkboxInput("area.show_foot", label = "Show margin notes", value = FALSE)),
                                                   column(2, offset = 0, downloadButton("area.dl", label = "Save area")))
                                               ) # wellPanel. 
                                      ), # tabPanel.
                                      
                                      # Icons: ---- 
                                      
                                      tabPanel("Icons", 
                                               br(), 
                                               
                                               fluidRow(
                                                 column(8, offset = 0, plotOutput("icons", width = "600", height = "450"))),
                                               br(),
                                               
                                               wellPanel(
                                                 fluidRow(
                                                   # column(4, offset = 0,
                                                   #        selectInput("icons.by", label = "Perspective by", 
                                                   #                    choices = list("condition x decision" = "cddc",
                                                   #                                   "condition only" = "cd",
                                                   #                                   "decision only"  = "dc",
                                                   #                                   "accuracy only"  = "ac"),
                                                   #                    selected = "cddc")), 
                                                   column(3, offset = 0,
                                                          selectInput("icons.arr_type", "Array type",
                                                                      choices = list("array" = "array", 
                                                                                     "shuffled" = "shuffledarray",
                                                                                     "mosaic" = "mosaic",
                                                                                     "fill equal" = "fillequal",
                                                                                     "fill left" = "fillleft",
                                                                                     "fill top" = "filltop",
                                                                                     "scattered" = "scatter"), 
                                                                      selected = "array"))),
                                                 
                                                 fluidRow(
                                                   column(3,
                                                          selectInput("symbol.hi", label = "hi (TP) symbol", 
                                                                      choices = list("Circle" = 21, "Square" = 22, "Rhombus" = 23, "Triangle" = 24),
                                                                      selected = "22")),
                                                   column(3,
                                                          selectInput("symbol.mi", label = "mi (FN) symbol", 
                                                                      choices = list("Circle" = 21, "Square" = 22, "Rhombus" = 23, "Triangle" = 24),
                                                                      selected = "22")),
                                                   column(3,
                                                          selectInput("symbol.fa", label = "fa (FP) symbol", 
                                                                      choices = list("Circle" = 21, "Square" = 22, "Rhombus" = 23, "Triangle" = 24),
                                                                      selected = "22")),
                                                   column(3,
                                                          selectInput("symbol.cr", label = "cr (TN) symbol", 
                                                                      choices = list("Circle" = 21, "Square" = 22, "Rhombus" = 23, "Triangle" = 24),
                                                                      selected = "22"))),
                                                 
                                                 fluidRow(
                                                   column(4, checkboxInput("icons.show_foot", label = "Show margin notes", value = FALSE)),
                                                   column(2, offset = 5, downloadButton("icons.dl", label = "Save icons")))
                                               )
                                      ),
                                      
                                      # Bars: ------  
                                      
                                      tabPanel("Bars", 
                                               br(),
                                               
                                               fluidRow(
                                                 column(8, offset = 0, plotOutput("bar", width = "600", height = "450"))),
                                               br(),
                                               
                                               wellPanel(
                                                 fluidRow(
                                                   column(4, offset = 0,
                                                          selectInput("bar.by", label = "Perspective by", 
                                                                      choices = list("condition" = "cd",
                                                                                     "decision"  = "dc",
                                                                                     "accuracy"  = "ac",
                                                                                     "all" = "all"),
                                                                      selected = "all")),
                                                   column(4, 
                                                          selectInput("bar.dir", label = "Directions", 
                                                                      choices = list("uni-directional" = 1, 
                                                                                     "bi-directional"  = 2), 
                                                                      selected = 1)),
                                                   column(3, 
                                                          selectInput("bar.f_lbl", label = "Labels", 
                                                                      choices = list("default" = "def", 
                                                                                     "abbr. names" = "abb",
                                                                                     "names only" = "nam",
                                                                                     "values only" = "num", 
                                                                                     "names + values" = "namnum", 
                                                                                     "no labels" = "no"), 
                                                                      selected = "num"))),
                                                 
                                                 fluidRow(
                                                   column(4, offset = 0, 
                                                          checkboxInput("bar.scale_f", label = "Scale by frequencies", value = TRUE)),
                                                   column(4, offset = 0, 
                                                          checkboxInput("bar.show_foot", label = "Show margin notes", value = FALSE)),
                                                   column(2, offset = 1, 
                                                          downloadButton("bar.dl", label = "Save bars")))
                                               ) # wellPanel. 
                                      ), # tabPanel.
                                      
                                      # Curves: ------ 
                                      
                                      tabPanel("Curves", 
                                               br(),
                                               
                                               fluidRow(
                                                 column(12, offset = 0, plotOutput("curve", width = "600", height = "450"))),
                                               br(),
                                               
                                               wellPanel(
                                                 fluidRow(
                                                   column(3, offset = 0, checkboxInput("curve.show_PPV", label = "Positive predictive value (PPV)", value = TRUE)),
                                                   column(3, checkboxInput("curve.show_NPV", label = "Negative predictive value (NPV)", value = TRUE)),
                                                   column(3, checkboxInput("curve.show_acc", label = "Accuracy (acc)", value = FALSE)),
                                                   column(3, checkboxInput("curve.show_ppod", label = "Proportion of positive decisions (ppod)", value = FALSE))),
                                                 
                                                 fluidRow(
                                                   column(3, offset = 0, checkboxInput("curve.show_points", label = "Show point values", value = TRUE)),
                                                   column(3, checkboxInput("curve.show_foot", label = "Show margin notes", value = FALSE)), 
                                                   column(6, checkboxInput("curve.log_scale", label = "Prevalence on logarithmic scale", value = FALSE))),
                                                 
                                                 fluidRow(
                                                   column(8, sliderInput("curve.uc", "Uncertainty", value = 0, min = 0, max = 50, step = 1, pre = NULL, post = "%")),
                                                   column(2, offset = 1, downloadButton("curve.dl", label = "Save curves")))
                                               ) # wellPanel. 
                                      ), # tabPanel.
                                      
                                      # Planes: ------ 
                                      
                                      tabPanel("Planes", 
                                               br(),
                                               
                                               fluidRow(
                                                 column(6, plotOutput("plane.ppv")),
                                                 column(6, plotOutput("plane.npv"))),
                                               br(),
                                               
                                               wellPanel(
                                                 fluidRow(
                                                   column(6, sliderInput("theta", "Horizontal viewing angle", value = -45, min = -90, max = +90)),
                                                   column(6, sliderInput("phi", "Vertical viewing angle", value = 0, min = 0, max =  90))),
                                                 
                                                 fluidRow(
                                                   column(6, checkboxInput("plane.show_point", label = "Show current PPV/NPV in plots", value = TRUE)),
                                                   column(6, checkboxInput("plane.show_foot",  label = "Show margin notes", value = FALSE))),
                                                 
                                                 fluidRow(
                                                   column(2, offset = 3, downloadButton("plane.ppv.dl", label = "Save PPV plane")),
                                                   column(2, offset = 4, downloadButton("plane.npv.dl", label = "Save NPV plane")))
                                               ) # wellPanel. 
                                      ), # tabPanel.
                                      
                                      # Contrast 4 representations: ------
                                      
                                      tabPanel("Compare", 
                                               br(),
                                               
                                               fluidRow(
                                                 column(6, plotOutput("represent1", width = "515", height = "350")),
                                                 column(6, plotOutput("represent2", width = "515", height = "350")),
                                                 column(6, plotOutput("represent3", width = "515", height = "350")),
                                                 column(6, plotOutput("represent4", width = "515", height = "350"))),
                                               br(),
                                               
                                               wellPanel(
                                                 fluidRow(
                                                   column(3, offset = 0, 
                                                          selectInput("represent1", label = "Plot 1", 
                                                                      choices = list("Prism" = "prism", 
                                                                                     "Table" = "table",
                                                                                     "Area" = "area",
                                                                                     "Icons" = "icons",
                                                                                     "Bars" = "bar",
                                                                                     "Curves" = "curve",
                                                                                     "Plane PPV" = "plane.ppv",
                                                                                     "Plane NPV" = "plane.npv"),
                                                                      selected = "table")),
                                                   column(3, offset = 3, 
                                                          selectInput("represent2", label = "Plot 2", 
                                                                      choices = list("Prism" = "prism", 
                                                                                     "Table" = "table",
                                                                                     "Area" = "area",
                                                                                     "Icons" = "icons",
                                                                                     "Bars" = "bar",
                                                                                     "Curves" = "curve",
                                                                                     "Plane PPV" = "plane.ppv",
                                                                                     "Plane NPV" = "plane.npv"),
                                                                      selected = "prism"))),
                                                 
                                                 fluidRow(
                                                   column(3, offset = 0, 
                                                          selectInput("represent3", label = "Plot 3", 
                                                                      choices = list("Prism" = "prism", 
                                                                                     "Table" = "table",
                                                                                     "Area" = "area",
                                                                                     "Icons" = "icons",
                                                                                     "Bars" = "bar",
                                                                                     "Curves" = "curve",
                                                                                     "Plane PPV" = "plane.ppv",
                                                                                     "Plane NPV" = "plane.npv"),
                                                                      selected = "area")),
                                                   column(3, offset = 3, 
                                                          selectInput("represent4", label = "Plot 4", 
                                                                      choices = list("Prism" = "prism", 
                                                                                     "Table" = "table",
                                                                                     "Area" = "area",
                                                                                     "Icons" = "icons",
                                                                                     "Bars" = "bar",
                                                                                     "Curves" = "curve",
                                                                                     "Plane PPV" = "plane.ppv",
                                                                                     "Plane NPV" = "plane.npv"),
                                                                      selected = "icons")))
                                               ) # wellPanel. 
                                      ) # tabPanel.
                          )
                        )
                      )
             ),
             
             # 2. Customize text labels: ------ 
             
             tabPanel("Customize labels",
                      icon = icon("pencil", lib = "glyphicon"), value = "custom_labels",
                      
                      sidebarLayout(
                        
                        # A. Sidebar panel for text inputs: ------ 
                        sidebarPanel(
                          # Inputs for label customization:
                          h3("Use your own labels"),
                          br(),
                          
                          fluidRow(
                            column(7, textInput("scen_lbl",
                                                label = "Scenario title",
                                                value = default.labels$scen_lbl))#,
                            
                            # column(7, textInput("scen_txt",
                            #                    label = "Description of scenario:",
                            #                    value = default.labels$scen_txt))
                          ),
                          # br(),
                          
                          fluidRow(
                            column(4, textInput("popu_lbl",
                                                label = "Population",
                                                value = default.labels$popu_lbl)),
                            
                            column(3, textInput("N_lbl",
                                                label = "N",
                                                value = default.labels$N_lbl))
                          ), 
                          br(),
                          
                          fluidRow(
                            column(4, textInput("cond_lbl",
                                                label = "Condition",
                                                value = default.labels$cond_lbl)),
                            column(3, textInput("cond_true_lbl",
                                                label = "cond_true",
                                                value = default.labels$cond_true_lbl)),
                            column(3, textInput("cond_false_lbl",
                                                label = "cond_false",
                                                value = default.labels$cond_false_lbl))),
                          # br(),
                          
                          fluidRow(
                            column(4, textInput("dec_lbl",
                                                label = "Decision",
                                                value = default.labels$dec_lbl)),
                            column(3, textInput("dec_pos_lbl",
                                                label = "dec_pos",
                                                value = default.labels$dec_pos_lbl)),
                            column(3, textInput("dec_neg_lbl",
                                                label = "dec_neg",
                                                value = default.labels$dec_neg_lbl))),
                          # br(),
                          
                          fluidRow(
                            column(4, textInput("acc_lbl",
                                                label = "Accuracy",
                                                value = default.labels$acc_lbl)),
                            column(3, textInput("dec_cor_lbl",
                                                label = "dec_cor",
                                                value = default.labels$dec_cor_lbl)),
                            column(3, textInput("dec_err_lbl",
                                                label = "dec_err",
                                                value = default.labels$dec_err_lbl))), 
                          br(), 
                          
                          fluidRow(
                            column(4, textInput("sdt_lbl", label = "Cases",value = default.labels$sdt_lbl)),
                            column(3, textInput("hi_lbl", label = "hi (TP)", value = default.labels$hi_lbl)),
                            column(3, textInput("fa_lbl", label = "fa (FP)", value = default.labels$fa_lbl))), 
                          
                          fluidRow(
                            column(offset = 4,
                                   3, textInput("mi_lbl", label = "mi (FN)", value = default.labels$mi_lbl)),
                            column(3, textInput("cr_lbl", label = "cr (TN)", value = default.labels$cr_lbl))), 
                          
                          br(), 
                          br(),
                          
                          fluidRow(
                            column(5,     
                                   bsButton("resetcustomlabel", label = "Reset default",
                                            icon = icon("refresh", lib = "glyphicon"),
                                            style = "default", type = "action")),
                            column(3, offset = 1,     
                                   bsButton("help_custom_labels", label = "Help",
                                            icon = icon("question-sign", lib = "glyphicon"),
                                            style = "default", type = "action")))
                        ), # sidebarPanel.
                        
                        # B. Main panel: Preview plots for current text labels: ------ 
                        mainPanel(
                          br(),
                          
                          h3("Preview of current labels"),
                          
                          fluidRow(
                            column(6, plotOutput("preview_labels_prism", width = "500", height = "400"))),  # prism by cddc
                          
                          fluidRow(
                            column(6, plotOutput("preview_labels_table", width = "450", height = "350")))  # table by ac
                          
                        ) # mainPanel. 
                      )),
             
             # 3. Customize colors: ------- 
             
             tabPanel("Customize colors",
                      icon = icon("wrench", lib = "glyphicon"),
                      value = "custom_colors",
                      sidebarLayout(
                        
                        # A. Sidebar panel for color inputs: ------
                        sidebarPanel(
                          # Inputs for color customization:
                          h3("Use your own colors"),
                          br(), br(),
                          
                          fluidRow(
                            column(4, colourInput("color.hi", label = "hi (TP)",
                                                  value = default.colors["hi"], showColour = "background",
                                                  palette = "square", allowedCols = NULL)),
                            
                            column(4, colourInput("color.fa", label = "fa (FP)",
                                                  value = default.colors["fa"], showColour = "background",
                                                  palette = "square", allowedCols = NULL)),
                            
                            column(4, colourInput("color.pos", label = "dec_pos",
                                                  value = default.colors["dec_pos"], showColour = "background",
                                                  palette = "square", allowedCols = NULL))),
                          
                          fluidRow(
                            column(4, colourInput("color.mi", label = "mi (FN)",
                                                  value = default.colors["mi"], showColour = "background",
                                                  palette = "square", allowedCols = NULL)),
                            column(4, colourInput("color.cr", label = "cr (TN)",
                                                  value = default.colors["cr"], showColour = "background",
                                                  palette = "square", allowedCols = NULL)),
                            column(4, colourInput("color.neg", label = "dec_neg",
                                                  value = default.colors["dec_neg"], showColour = "background",
                                                  palette = "square", allowedCols = NULL))),
                          
                          fluidRow(
                            column(4, colourInput("color.true", label = "cond_true",
                                                  value = default.colors["cond_true"], showColour = "background",
                                                  palette = "square", allowedCols = NULL)),
                            column(4, colourInput("color.false", label = "cond_false",
                                                  value = default.colors["cond_false"], showColour = "background",
                                                  palette = "square", allowedCols = NULL)),
                            column(4, colourInput("color.N", label = "population",
                                                  value = default.colors["N"], showColour = "background",
                                                  palette = "square", allowedCols = NULL))),
                          br(), br(),
                          
                          fluidRow(
                            column(4, colourInput("color.cor", label = "dec_cor",
                                                  value = default.colors["dec_cor"], showColour = "background",
                                                  palette = "square", allowedCols = NULL)),
                            column(4, colourInput("color.err", label = "dec_err",
                                                  value = default.colors["dec_err"], showColour = "background",
                                                  palette = "square", allowedCols = NULL))),
                          br(), br(),
                          
                          fluidRow(
                            column(6, colourInput("color.ppv", label = "PPV",
                                                  value = default.colors["ppv"], showColour = "background",
                                                  palette = "square", allowedCols = NULL)),
                            column(6, colourInput("color.npv", label = "NPV",
                                                  value = default.colors["npv"], showColour = "background",
                                                  palette = "square", allowedCols = NULL))),
                          
                          fluidRow(
                            column(6, colourInput("color.txt", label = "Text",
                                                  value = default.colors["txt"], showColour = "background",
                                                  palette = "square", allowedCols = NULL)),
                            column(6, colourInput("color.brd", label = "Lines",
                                                  value = default.colors["brd"], showColour = "background",
                                                  palette = "square", allowedCols = NULL))),
                          br(), br(), br(), 
                          
                          fluidRow(
                            column(7,
                                   selectInput("alt.palette", label = "Color palette",
                                               choices = list("---" = "default",
                                                              "modern colors"   = "pal_mod",
                                                              "mod/bw colors"   = "pal_mbw",
                                                              "original colors" = "pal_org",
                                                              "RGB colors"      = "pal_rgb",
                                                              "uni.kn colors"   = "pal_kn",
                                                              "viridis colors"  = "pal_vir",
                                                              "black + white"   = "pal_bw"#,
                                                              #"b + w printing"  = "pal_bwp"  # [new in v0.2.0.9002] 
                                               ),
                                               selected = 1))),
                          
                          br(), br(), br(),
                          
                          fluidRow(
                            column(5,                          
                                   bsButton("resetcustomcolor", label = "Reset default",
                                            icon = icon("refresh", lib = "glyphicon"),
                                            style = "default", type = "action")),
                            column(3, offset = 1,  
                                   bsButton("help_custom_colors", label = "Help",
                                            icon = icon("question-sign", lib = "glyphicon"),
                                            style = "default", type = "action")))
                          
                        ), # sidebarPanel.
                        
                        # B. Main panel: Preview plots for current colors: ---- 
                        mainPanel(
                          br(),
                          
                          h3("Preview of current colors"),
                          
                          fluidRow(
                            column(6, plotOutput("preview_colors_table", width = "450", height = "280"))), # table by cddc
                          
                          fluidRow(
                            column(6, plotOutput("preview_colors_tree", width = "500", height = "280"))), # tree by ac 
                          
                          fluidRow(
                            column(6, plotOutput("preview_colors_curves", width = "450", height = "350")))
                          
                        ) # mainPanel.
                        
                      )),
             
             navbarMenu("About",  icon = icon("info-sign", lib = "glyphicon"),
                        
                        # spacer
                        "----",
                        
                        # 1st screen in dropdown navigation:
                        tabPanel("Readings & References",
                                 icon = icon("book", lib = "glyphicon"),
                                 value = "references",
                                 h1("Readings and references"),
                                 
                                 fluidRow(
                                   column(5, offset = 0, includeMarkdown("www/recommended_readings.md")),
                                   column(5, offset = 1, includeMarkdown("www/references.md")))),
                        
                        # spacer
                        "----",
                        
                        # 2nd screen in dropdown navigation:
                        tabPanel("Imprint", value = "about",
                                 icon = icon("pushpin", lib = "glyphicon"),
                                 h1("Imprint"),
                                 
                                 br(),
                                 includeMarkdown("www/imprint.md")),
                        
                        # spacer
                        "----"
             ),
             
             # 4. Tooltips: ------ 
             
             # On inputs:
             
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
             
             # On download buttons:
             
             bsTooltip(id = list("prism.dl", "table.dl", "area.dl", "icons.dl", "bar.dl", "curve.dl", 
                                 "plane.ppv.dl", "plane.npv.dl"),
                       title = "Save current graph as .png file.",
                       placement = "left", trigger = "hover", options = list(container = "body"))
  )
)

## eof. ---------- 