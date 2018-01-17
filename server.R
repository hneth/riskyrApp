## server.R
## riskyrApp | R Shiny | spds, uni.kn | 2018 01 17

#####

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
## Clean up:

rm(list=ls()) # clean all.

## Dependencies:

# get packages for app
library("shiny")
library("shinyBS")
library("markdown")
library("DT")
library("colourpicker")
library("vcd")

# get riskyr
# install.packages("../riskyr_0.0.0.904.tar.gz", repos = NULL, type="source")
library("riskyr")

## The honorable mentions that have been outsourced along the way...
# library("diagram")
# library("shape")
# library("tidyr")
# library("dplyr")
# library("ggplot2")



## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
## Import ready-made and worked out example data 
## (in both ui.R and server.R):

datasets <- read.csv2("./www/examples_riskyR.csv", stringsAsFactors = FALSE)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
## Customizations

## Sensible set of defaults

# take defaults from example datasets stored in www folder
default.parameters <- setNames(datasets[1, 2:5], names(datasets)[2:5])
default.terminology <- setNames(datasets[1, 7:18], names(datasets)[7:18])
# define default colors
default.colors <- c(color.hi = rgb(128, 177, 57, max = 255), # col.green.2
                    color.mi = rgb(210, 52, 48, max = 255), # col.red.2
                    color.fa = rgb(184, 217, 137, max = 255), # col.green.1
                    color.cr = rgb(230, 142, 140, max = 255), # col.red.1
                    color.ppv = rgb(242, 100, 24, max = 255), # col.orange.2
                    color.npv = rgb(29, 149, 198, max = 255) # col.blue.3
                    )
  
  

{

  ## (and make user-customizable later):
  # cus$target.population.lbl = "population description"
  # cus$scenario.txt = "Describe the scenario in a paragraph here."
  # (a) True condition:
  condition.lbl <- "Current condition"
  cond.true.lbl <- "Condition true"   # "has condition", "is affected"
  cond.false.lbl <- "Condition false" # "does not have condition", "is unaffected"
  # (b) Decisions:
  decision.lbl <- "Diagnostic decision"
  dec.true.lbl <- "Decision positive"  # "has condition", "is affected"
  dec.false.lbl <- "Decision negative" # "does not have condition", "is unaffected"
  # (c) sdt cases (combinations):
  sdt.hi.lbl <- "hit"   # "has condition and is detected as such",     p(dec true  | cond true)
  sdt.mi.lbl <- "miss"  # "has condition and is NOT detected as such", p(dec false | cond true)
  sdt.fa.lbl <- "false alarm" # ...
  sdt.cr.lbl <- "correct rejection" # ... 
  # (d) PPV/NPV:
  # (...)
  
}

## B. Graphical settings: 


  ## Color names:
  ## (make user-customizable later):
  {

    ## from uni.kn: 
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
  
  }
  
  ## Define six default colors for app display:
  
  # THESE COLORS ARE STILL USED BY SEVERAL REPRESENATIONS!!!
  # CHANGE FUNCTION TO TAKE REACTIVE VALUES FROM cus list
  col.ppv <- col.orange.2 # "orange3" # "firebrick" "red3"
  col.npv <- col.blue.3 # seeblau "steelblue3" # "green4" "gray50" "brown4" "chartreuse4"
  sdt.colors <- setNames(c(col.green.2, col.red.2, col.green.1, col.red.1),
                         c("hi", "mi", "cr", "fa"))


#####

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
## Define server logic:

shinyServer(function(input, output, session){
  
  ## Collect all customizable values in reactive list
  # TO DO: Change on UI side as well
  cus <- reactiveValues(
    # initalize with defaults defined above
    # headlines
    target.population.lbl = default.terminology[names(default.terminology) == "target.population.lbl"],
    scenario.txt = default.terminology[names(default.terminology) == "scenario.txt"],
    # (a) Condition
    condition.lbl = default.terminology[names(default.terminology) == "condition.lbl"],
    cond.true.lbl = default.terminology[names(default.terminology) == "cond.true.lbl"],
    cond.false.lbl = default.terminology[names(default.terminology) == "cond.false.lbl"],
    # (b) Decisions:
    decision.lbl = default.terminology[names(default.terminology) == "decision.lbl"],
    dec.true.lbl = default.terminology[names(default.terminology) == "dec.true.lbl"],
    dec.false.lbl = default.terminology[names(default.terminology) == "dec.false.lbl"],
    # (c) sdt cases (combinations):
    sdt.hi.lbl = default.terminology[names(default.terminology) == "sdt.hi.lbl"],
    sdt.mi.lbl = default.terminology[names(default.terminology) == "sdt.mi.lbl"],
    sdt.fa.lbl = default.terminology[names(default.terminology) == "sdt.fa.lbl"],
    sdt.cr.lbl = default.terminology[names(default.terminology) == "sdt.cr.lbl"],    
    # colors
    color.hi = default.colors["color.hi"],
    color.mi = default.colors["color.mi"],
    color.fa = default.colors["color.fa"],
    color.cr = default.colors["color.cr"],
    color.ppv = default.colors["color.ppv"],
    color.npv = default.colors["color.npv"]
  )

  ## Define a common data structure:
  ## Generate 3 data structures as lists of reactive elements:
  env <- reactiveValues(env = NULL)   # 1. a list of current environment parameters (inputs)
  data <- reactiveValues(data = NULL) # 2. list of derived parameters (list of only scalars/atomic vectors)
  # population <- reactiveValues(population = NULL) # 3. current population (as df of 3 vectors)
  
  ##### 
  ## Couple numeric and slider inputs:
  observeEvent(input$numN,
               {updateSliderInput(session, "N", value = input$numN)})
  observeEvent(input$N,
               {updateNumericInput(session, "numN", value = input$N)})
  
  observeEvent(input$numprev,
               {updateSliderInput(session, "prev", value = input$numprev)})
  observeEvent(input$prev,
               {updateNumericInput(session, "numprev", value = input$prev)})
  
  observeEvent(input$numsens,
               {updateSliderInput(session, "sens", value = input$numsens)})
  observeEvent(input$sens,
               {updateNumericInput(session, "numsens", value = input$sens)})
  
  observeEvent(input$numspec,
               {updateSliderInput(session, "spec", value = input$numspec)})
  observeEvent(input$spec,
               {updateNumericInput(session, "numspec", value = input$spec)})
  
  #####
  ## Observe inputs and generate data used in outputs:
  observeEvent({
    input$name   # name of current environment 
    input$N      # N in population
    input$prev   # prevalence in population = p(true positive)
    input$sens   # sensitivity = p(decision positive | condition positive)
    input$spec   # specificity = p(decision negative | condition negative)
  }, {
    
    ## (A) Environment parameters:  
    ## Set parameters of current environment:
    env$name <- input$name
    env$N <- input$N
    env$prev <- input$prev
    env$sens <- input$sens
    env$spec <- input$spec
    env$source <- "source information"
    
    ## (B) Derive data:
    ## (1) Determine the truth:
    data$n.true <- round((env$prev * env$N), 0) # n.true cases
    data$n.false <- (env$N - data$n.true)      # n.false cases
    
    ## Vector of true states:
    data$truth <- c(rep(TRUE, data$n.true), rep(FALSE, data$n.false)) 
    
    ## (2) Determine decisions:
    data$n.hi <- round((env$sens * data$n.true), 0)  # a. hits
    data$n.mi <- (data$n.true - data$n.hi)           # b. misses
    data$n.cr <- round((env$spec * data$n.false), 0) # d. correct rejections
    data$n.fa <- (data$n.false - data$n.cr)          # c. false alarms
    
    data$dec.pos <- data$n.hi + data$n.fa # 1. decision positives (true & false)
    data$dec.neg <- data$n.mi + data$n.cr # 2. decision negatives (true & false)
    
    ## Vector of decisions (ordered by truth values):
    data$decision <- c(rep(TRUE, data$n.hi), rep(FALSE, data$n.mi), 
                       rep(TRUE, data$n.fa), rep(FALSE, data$n.cr))
    
    ## (3) SDT (status decision/truth):
    data$sdt <- c(rep("hi", data$n.hi), rep("mi", data$n.mi), 
                  rep("fa", data$n.fa), rep("cr", data$n.cr))
    
    ## (4) Coerce 3 vectors into ordered factors:
    data$truth <- factor(data$truth, 
                         levels = c(TRUE, FALSE),
                         labels = c(cus$cond.true.lbl, cus$cond.false.lbl), # explicit labels
                         ordered = TRUE)
    
    
    data$decision <- factor(data$decision, 
                            levels = c(TRUE, FALSE),
                            labels = c(dec.true.lbl, dec.false.lbl), # explicit labels
                            ordered = TRUE)
    
    data$sdt <- factor(data$sdt, 
                          levels = c("hi", "mi", "fa", "cr"),
                          labels = c(sdt.hi.lbl, sdt.mi.lbl, sdt.fa.lbl, sdt.cr.lbl), # explicit labels
                          ordered = TRUE)
    
    ## (5) Combine vectors of length N in population data frame:
    data$population <- data.frame(tru = data$truth,
                                  dec = data$decision,
                                  sdt = data$sdt)
    names(data$population) <- c("Truth", "Decision", "sdt")
    
    ## (6) Compute and store current PPV and NPV values (to use in graphs and labels):
    data$PPV <- riskyr::comp_PPV(env$prev, env$sens, env$spec)
    data$NPV <- riskyr::comp_NPV(env$prev, env$sens, env$spec)
  })
  
  ## Integrate worked out examples:
  observeEvent(input$dataselection, {
    if (input$dataselection != 1) { # if 1st option is not ("---")
      updateSliderInput(session, "N", value = datasets[input$dataselection, "N" ]) 
      updateSliderInput(session, "sens", value = datasets[input$dataselection, "sens" ])
      updateSliderInput(session, "prev", value = datasets[input$dataselection, "prev" ])
      updateSliderInput(session, "spec", value = datasets[input$dataselection, "spec" ]) 
      output$sourceOutput <- renderText(datasets[input$dataselection, "source"]) }
  }, ignoreInit = TRUE)
  

  #####
  ## Outputs:
  ## (1) Intro tab:
  ## get all current inputs within text statements as outputs
  output$N <- renderText({ paste0("1. Population size: We are considering a population of ", input$N, " individuals. ") })
  output$prev <- renderText({ paste0("2. Prevalence describes the probability of being affected: p(true).  The current prevalence is ", input$prev, ". ")})
  output$sens <- renderText({ paste0("3. Sensitivity describes the probability of correctly detecting an affected individual: p(decision positive | condition true).  The current sensitivity is ", input$sens, ". ") })
  output$spec <- renderText({ paste0("4. Specificity describes the probability of correctly rejecting an unaffected individual: p(decision negative | condition false) = 1 - FA.  The current specificity is ", input$spec, ". ") })
  
  
  ## (2) Stats tab:
  output$ACC <- renderUI({
    withMathJax(paste0("$$ ACC = \\frac{", data$n.hi, " + ", data$n.mi, "}{", env$N, "} = ", round((data$n.hi + data$n.mi)/env$N, 4), "$$"))
  })
  
  output$PPV1 <- renderUI({
    withMathJax(paste0("$$ PPV = \\frac{", data$n.hi, "}{", data$dec.pos, "} = \\frac{", data$n.hi, "}{", data$n.hi, " + ", data$n.fa, "}= ", 
                       round(data$PPV, 4), "$$"))
  })

  output$PPV2 <- renderUI({
    withMathJax(paste0("$$PPV = \\frac{", input$sens, " \\times ", input$prev,"}{", input$sens, " \\times ", 
                       input$prev," + (1 - ", input$spec,") \\times (1 - ", input$prev,")} = ", round(data$PPV, 4), "$$"))
  })
  
  output$NPV1 <- renderUI({
    withMathJax(paste0("$$ NPV = \\frac{", data$n.cr, "}{", data$dec.neg, "} = \\frac{", data$n.cr, "}{", data$n.cr, " + ", data$n.mi, "}= ", 
                       round(data$NPV, 4), "$$"))
  })
  
  output$NPV2 <- renderUI({
    withMathJax(paste0("$$NPV = \\frac{", input$spec, " \\times (1 - ", input$prev,")}{(1 - ", input$sens, ") \\times ", 
                       input$prev," + ", input$spec," \\times (1 - ", input$prev,")} = ", round(data$NPV, 4), "$$"))
  })
  
  
  ## (a) Raw data table: 
  output$rawdatatable <- DT::renderDataTable(DT::datatable({
    if(input$sort == FALSE) {display <- data$population[sample(rownames(data$population)), ]}
    else display <- data$population[sort(as.numeric(rownames(data$population))), ]
    display
    }) %>%
    formatStyle("sdt", target = "row", backgroundColor = styleEqual(levels = c("hit", "miss", "false alarm", "correct rejection"),
                                                                    values = c(cus$color.hi, cus$color.mi, cus$color.fa, cus$color.cr)))
  )
  
  ## (b) Icon array:
  ## ... 
  
  ## (c) 2x2 confusion table (ordered by rows/decisions):
  # To be precise, we need two confusion tables (at table and stats tab)
  # We create a reactive table and then render it twice (shiny doesn't allow identical output elements)
  
  confusiontable <- reactive({matrix(data = c(data$n.hi, data$n.fa, data$dec.pos, 
                                              data$n.mi, data$n.cr, data$dec.neg, 
                                              data$n.true, data$n.false, env$N),
                                     nrow = 3, byrow = TRUE,
                                     dimnames = list(c(dec.true.lbl,  # "Decision positive:", 
                                                       dec.false.lbl, # "Decision negative:", 
                                                       "Truth sums:"), 
                                                     c(cond.true.lbl,  # "Condition true:", 
                                                       cond.false.lbl, # "Condition false:", 
                                                       "Decision sums:")
                                                     )
                                     )
  })
  
  output$confusiontable1 <- renderTable(confusiontable(), bordered = TRUE, hover = TRUE,  
                                        align = 'r', digits = 0, rownames = TRUE, na = 'missing')
  
  output$confusiontable2 <- renderTable(confusiontable(), bordered = TRUE, hover = TRUE,  
                                        align = 'r', digits = 0, rownames = TRUE, na = 'missing')
  
  
  ## (d) Mosaic plot:
  output$mosaicplot <- renderPlot({ 
    mosaic(Truth ~ Decision, data = data$population,
           shade=TRUE, colorize = TRUE, 
           gp = gpar(fill = matrix(data = c(cus$color.hi, cus$color.mi, cus$color.fa, cus$color.cr),
                                   nrow = 2, ncol = 2, byrow = FALSE)),
           main_gp = gpar(fontsize = 12, fontface = "bold"),
           main = paste0(env$name, "\n(N = ", env$N, ")"))
  })
  
  ## (e) Tree with natural frequencies:
  output$nftree <- renderPlot({
    plot_nftree(prev = env$prev, sens = env$sens, spec = env$spec, fart = (1-env$spec), N = env$N,
                box.area = input$treetype,
                title.lbl = cus$scenario.txt,    # custom labels
                popu.lbl = cus$target.population.lbl,
                cond.lbl = cus$condition.lbl,     # condition
                cond.true.lbl = cus$cond.true.lbl,
                cond.false.lbl = cus$cond.false.lbl,
                dec.lbl = cus$decision.lbl,       # decision
                dec.true.lbl = cus$dec.true.lbl,
                dec.false.lbl = cus$dec.false.lbl,
                sdt.hi.lbl = cus$sdt.hi.lbl, # SDT combinations
                sdt.mi.lbl = cus$sdt.mi.lbl,
                sdt.fa.lbl = cus$sdt.fa.lbl,
                sdt.cr.lbl = cus$sdt.cr.lbl,
                col.boxes = c("#F2F2F2FC", "lightgoldenrod1", "lightskyblue2", 
                              cus$color.hi, cus$color.mi, cus$color.fa, cus$color.cr),
                col.txt = grey(0.01, alpha = 0.99), 
                col.border = "grey10",
                col.shadow = col.sand.dark, cex.shadow = 0)}
  )
  

  # output$nftree <- renderPlot(
  #   plot_nftree(prev = env$prev, sens = env$sens, spec = env$spec, fart = (1-env$spec),
  #                                         N = env$N, n.true = data$n.true, n.false = data$n.false,
  #                                         n.hi = data$n.hi, n.mi = data$n.mi, n.fa = data$n.fa, n.cr = data$n.cr,
  #                                         title.lbl = cus$scenario.txt,    # custom labels
  #                                         popu.lbl = cus$target.population.lbl,
  #                                         cond.lbl = cus$condition.lbl,     # condition
  #                                         cond.true.lbl = cus$cond.true.lbl,
  #                                         cond.false.lbl = cus$cond.false.lbl,
  #                                         dec.lbl = cus$decision.lbl,       # decision
  #                                         dec.true.lbl = cus$dec.true.lbl,
  #                                         dec.false.lbl = cus$dec.false.lbl,
  #                                         sdt.hi.lbl = cus$sdt.hi.lbl, # SDT combinations
  #                                         sdt.mi.lbl = cus$sdt.mi.lbl,
  #                                         sdt.fa.lbl = cus$sdt.fa.lbl,
  #                                         sdt.cr.lbl = cus$sdt.cr.lbl,
  #                                         col.txt = "black", #grey(.01, alpha = .99), # black
  #                                         col.border = grey(.01, alpha = .99), #col.grey.4,
  #                                         col.N = col.sand.light, # col.sand.light,
  #                                         col.true = col.sand.light, col.false = col.sand.light, # both col.N, previously
  #                                         col.hi = cus$color.hi, col.mi = cus$color.mi, col.fa = cus$color.fa, col.cr = cus$color.cr
  #                                         )
  # )
                                          
                                                     
                                                   
                                                      
  
  ## (f) 2D plot of PPV and NPV as a function of prev.range:
  output$PVs <- renderPlot(plot_PV(prev = env$prev, sens = env$sens, spec = env$spec,
                                   show.PVprev = input$boxPVprev, # mark current prevalence in plot                
                                   show.PVpoints = input$boxPVpoints1, # mark cur.PPV/cur.NPV in plot
                                   log.scale = input$boxPVlog,
                                   title.lbl = cus$scenario.txt,
                                   col.ppv = cus$color.ppv,
                                   col.npv = cus$color.npv)
    )
  

  ## (g) 3D plots of PPV and NPV planes as functions of sens and spec:
  output$PV3dPPV <- renderPlot(plot_PV3d(prev = env$prev, sens = env$sens, spec = env$spec,
                                         is.ppv = TRUE,
                                         show.PVpoints = input$boxPVpoints2,
                                         cur.theta = input$theta,
                                         cur.phi = input$phi,
                                         title.lbl = cus$scenario.txt,
                                         col.pv = cus$color.ppv
                                         )
                               )
  
  output$PV3dNPV <- renderPlot(plot_PV3d(prev = env$prev, sens = env$sens, spec = env$spec,
                                         is.ppv = FALSE,
                                         show.PVpoints = input$boxPVpoints2,
                                         cur.theta = input$theta,
                                         cur.phi = input$phi,
                                         title.lbl = cus$scenario.txt,
                                         col.pv = cus$color.npv
  )
  )
  
  
  
  
  
  #####
  ## Customization tab
  ## Customize labels
  
  # Apply label selection
  observeEvent(input$applycustomlabel, {
    cus$target.population.lbl <- input$target.population.lbl
    cus$scenario.txt <- input$scenario.txt
    # (a) Condition
    cus$condition.lbl <- input$condition.lbl 
    cus$cond.true.lbl <- input$cond.true.lbl 
    cus$cond.false.lbl <- input$cond.false.lbl 
    # (b) Decisions:
    cus$decision.lbl <- input$decision.lbl 
    cus$dec.true.lbl <- input$dec.true.lbl 
    cus$dec.false.lbl <- input$dec.false.lbl 
    # (c) sdt cases (combinations):
    cus$sdt.hi.lbl <- input$sdt.hi.lbl 
    cus$sdt.mi.lbl <- input$sdt.mi.lbl 
    cus$sdt.fa.lbl <- input$sdt.fa.lbl 
    cus$sdt.cr.lbl <- input$sdt.cr.lbl 
  })
  
  # Reset labels to default
  observeEvent(input$resetcustomlabel, {
    cus$target.population.lbl <- default.terminology[names(default.terminology) == "target.population.lbl"]
    cus$scenario.txt <- default.terminology[names(default.terminology) == "scenario.txt"]
    # (a) Condition
    cus$condition.lbl <- default.terminology[names(default.terminology) == "condition.lbl"]
    cus$cond.true.lbl <- default.terminology[names(default.terminology) == "cond.true.lbl"]
    cus$cond.false.lbl <- default.terminology[names(default.terminology) == "cond.false.lbl"]
    # (b) Decisions:
    cus$decision.lbl <- default.terminology[names(default.terminology) == "decision.lbl"]
    cus$dec.true.lbl <- default.terminology[names(default.terminology) == "dec.true.lbl"]
    cus$dec.false.lbl <- default.terminology[names(default.terminology) == "dec.false.lbl"]
    # (c) sdt cases (combinations):
    cus$sdt.hi.lbl <- default.terminology[names(default.terminology) == "sdt.hi.lbl"]
    cus$sdt.mi.lbl <- default.terminology[names(default.terminology) == "sdt.mi.lbl"]
    cus$sdt.fa.lbl <- default.terminology[names(default.terminology) == "sdt.fa.lbl"]
    cus$sdt.cr.lbl <- default.terminology[names(default.terminology) == "sdt.cr.lbl"]
    updateTextInput(session, "target.population.lbl", value = default.terminology[["target.population.lbl"]])
    updateTextInput(session, "scenario.txt", value = default.terminology[["scenario.txt"]])
    # (a) Condition
    updateTextInput(session, "condition.lbl", value = default.terminology[["condition.lbl"]])
    updateTextInput(session, "cond.true.lbl", value = default.terminology[["cond.true.lbl"]])
    updateTextInput(session, "cond.false.lbl", value = default.terminology[["cond.false.lbl"]])
    # (b) Decisions:
    updateTextInput(session, "decision.lbl", value = default.terminology[["decision.lbl"]])
    updateTextInput(session, "dec.true.lbl", value = default.terminology[["dec.true.lbl"]])
    updateTextInput(session, "dec.false.lbl", value = default.terminology[["dec.false.lbl"]])
    # (c) sdt cases (combinations):
    updateTextInput(session, "sdt.hi.lbl", value = default.terminology[["sdt.hi.lbl"]])
    updateTextInput(session, "sdt.mi.lbl", value = default.terminology[["sdt.mi.lbl"]])
    updateTextInput(session, "sdt.fa.lbl", value = default.terminology[["sdt.fa.lbl"]])
    updateTextInput(session, "sdt.cr.lbl", value = default.terminology[["sdt.cr.lbl"]])
    
  })
  
  
  ## Customize colors

  # Apply color selection
  observeEvent(input$applycustomcolor, {
    cus$color.hi <- input$color.hi
    cus$color.mi <- input$color.mi
    cus$color.fa <- input$color.fa
    cus$color.cr <- input$color.cr
    cus$color.ppv <- input$color.ppv
    cus$color.npv <- input$color.npv
  })

  # Simplified display of sdt states
  output$sampleplot <- renderPlot({
    par(pty="s")
    plot(c(0,2), c(0,2), type="n", xaxt='n', yaxt='n', ann=FALSE)
    rect(xleft = c(0, 1, 0, 1), ybottom = c(0, 0, 1, 1), 
         xright = c(1, 2, 1, 2), ytop = c(1, 1, 2, 2),
         col = c(cus$color.mi, cus$color.cr, cus$color.hi, cus$color.fa))
    text(x = c(0.5, 1.5, 0.5, 1.5), y = c(1.5, 1.5, 0.5, 0.5), col = "black",
         labels = c("hit", "false alarm", "miss", "correct rejection"), cex = 1.1)
  })
  
  # Simplified plot with PPV and NPV curves
  output$sampleplotcurves <- renderPlot({
    par(pty="s")
    plot(c(0,1), c(0,1), type= "n", xaxt='n', yaxt='n', ann=FALSE)
    # plot  simplified PPV curve
    curve((x * 0.85)/((x * 0.85) +  (1-x)*0.25), from = 0, to = 1, n = 1000,
          lwd = 3, col = cus$color.ppv, add = TRUE)
    # plot simplified NPV curve
    curve(((1-x)*0.75)/(((1-x)*0.75) + (x * 0.15)), from = 0, to = 1, n = 1000, 
          add = TRUE, lwd = 3, col = cus$color.npv)
    # legend
    legend("bottom", lwd = c(3, 3), bty = "n", legend = c("PPV", "NPV"),
           col = c(cus$color.ppv, cus$color.npv))
  })

  
  # Reset colors to default
  observeEvent(input$resetcustomcolor, {
    # reset colors in background
    cus$color.hi <- default.colors["color.hi"]
    cus$color.mi <- default.colors["color.mi"]
    cus$color.fa <- default.colors["color.fa"]
    cus$color.cr <- default.colors["color.cr"]
    cus$color.ppv <- default.colors["color.ppv"]
    cus$color.npv <- default.colors["color.npv"]
    # reset colors on colorpickers
    updateColourInput(session, "color.hi", value = as.character(default.colors["color.hi"]))
    updateColourInput(session, "color.mi", value = as.character(default.colors["color.mi"]))
    updateColourInput(session, "color.fa", value = as.character(default.colors["color.fa"]))
    updateColourInput(session, "color.cr", value = as.character(default.colors["color.cr"]))
    updateColourInput(session, "color.ppv", value = as.character(default.colors["color.ppv"]))
    updateColourInput(session, "color.npv", value = as.character(default.colors["color.npv"]))
  })
  
  
}
)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
## eof. #