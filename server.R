# server.R
# riskyrApp | R Shiny | spds, uni.kn | 2018 12 07


## Clean up: ------

rm(list=ls()) # clean all.

## Dependencies: ------
library("shiny")
library("shinyBS")
library("markdown")
library("DT")
library("colourpicker")

## Install the currently included version of riskyr: ------
# detach("package:riskyr", unload = TRUE)
# devtools::install_github("hneth/riskyr")
library("riskyr")
# sessionInfo()

## Import ready-made and worked out example data: ------
datasets <- read.csv2("./www/examples_riskyrApp_2018-03-30.csv", stringsAsFactors = FALSE)

##### 
## Define defaults: ------

# take defaults from example datasets stored in www folder
default.parameters <- setNames(datasets[1, 2:5], names(datasets)[2:5])
default.terminology <- setNames(datasets[1, 9:20], names(datasets)[9:20])
# define default colors: 
default.colors <- c(color.hi  = rgb(128, 177,  57, max = 255),  # col.green.2
                    color.mi  = rgb(210,  52,  48, max = 255),  # col.red.2
                    color.fa  = rgb(230, 142, 140, max = 255),  # col.red.1 
                    color.cr  = rgb(184, 217, 137, max = 255),  # col.green.1 
                    color.ppv = rgb(242, 100,  24, max = 255),  # col.orange.2
                    color.npv = rgb( 29, 149, 198, max = 255)   # col.blue.3
                    )

riskyr.colors <- reactive({ init_pal() })

#####
## Define server logic: ------

shinyServer(function(input, output, session){
  
  #####
  # define common data scructure
  ## Generate 4 data structures as lists of reactive elements:
  env <- reactiveValues(env = NULL) # Current ENVironment parameters
  freq <- reactiveValues(freq = NULL) # Calculated FREQuencies, based on parameters
  prob <- reactiveValues(prob = NULL) # calculated PROBabilities, based on parameters
  cus <- reactiveValues(  # CUStomizable labels and colors and their defaults
    # headlines
    target.population.lbl = as.character(default.terminology[names(default.terminology) == "target.population.lbl"]),
    scenario.txt = as.character(default.terminology[names(default.terminology) == "scenario.txt"]),
    # (a) Condition
    condition.lbl = as.character(default.terminology[names(default.terminology) == "condition.lbl"]),
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
  
  


  
#####
  # Tutorial elements (under development)
  
  ######
  # Add popovers for diagrams
  
  addPopover(session, id = "network", title = "Network diagram", 
             content = paste0("<p>The network diagram plots all different frequencies as nodes and depicts all probabilities as edges between these nodes.</p>
                              <p>The network diagram is thus a generalization of the tree diagram.</p>"),
             placement = "right", trigger = "hover", options = NULL)
  
  addPopover(session, id = "rawdatatable", title = "Raw data", 
             content = paste0("<p>The raw data table provides the arguably most simple and least informative representation:</p>
                              <p>Every row represents an individual case and every column contains an attribute for that case.</p>"),
             placement = "bottom",
             trigger = "hover", options = NULL)
  
  addPopover(session, id = "iconarray", title = "Icon array", 
             content = paste0("<p>The icon array plots an entire population of individuals.</p>
                              <p>Each individual is represented as a color-coded symbol.</p>"),
             placement = "right", trigger = "hover", options = NULL)
  
  addPopover(session, id = "nftree", title = "Tree diagram", 
             content = paste0("<p>The tree diagram visualizes the frequency of population 
                              subgroups as nodes and the probabilities as edges.</p>"),
             placement = "right", trigger = "hover", options = NULL)
  
  addPopover(session, id = "mosaicplot", title = "Mosaic plot", 
             content = paste0("<p>The mosaic plot depicts the population as a square and dissects it 
                              into various subgroups that represent parts of the population.</p> 
                              <p>The relative proportions of rectangle sizes represent the relative frequencies of the corresponding subgroups.</p>"),
             placement = "bottom", trigger = "hover", options = NULL)
  
  addPopover(session, id = "PVs", title = "Curves", 
             content = paste0("<p>The curves (or lines) show selected parameters as a function of the prevalence for a given decision process or diagnostic test (i.e., given values of sensitivity and specificity).</p>
                              <p>Rather than just computing a single value, we could ask: How do values of the <b>Positive Predictive Value (PPV)</b> develop as a function of prevalence?</p> 
                              <p>The curves illustrate this relationship (and some more).</p>"),
             placement = "bottom", trigger = "hover", options = NULL)
  
  addPopover(session, id = "PV3dPPV", title = "Planes", 
             content = paste0("<p>This plane shows the <b>Positive Predictive Value (PPV)</b> as a function of 
                               sensitivity and specificity for a given prevalence.</p>"),
             placement = "bottom", trigger = "hover", options = NULL)
  
  addPopover(session, id = "PV3dNPV", title = "Planes", 
             content = paste0("<p>This plane shows the <b>Negative Predictive Value (NPV)</b> as a function of 
                              sensitivity and specificity for a given prevalence.</p>"),
             placement = "bottom", trigger = "hover", options = NULL)
  
  
  #####
  # create modals with tutorial/help contents
  
  tut_start_modal <- modalDialog(
    title = "So you want to take the guided tour (aka tutorial)...", 
    img(src = "bulb_64.png"),
    "First of all, please note that the riskyrApp comprises several tabs.",
    br(),
    "The tab menu is visible at all times in the upper left corner of the screen and looks like this:",
    br(),
    img(src = "riskyr_header_menu.png"),
    br(),
    "Let's check out the tab for statistics...",
    easyClose = FALSE, size = "l",
    footer = list(modalButton("Close Tutorial"), bsButton("tut_welcome", "Continue to statistics"))
  )
  
  stats_modal <-  modalDialog(
    title = "Statistics", 
    br(),
    "Coming soon...",
    easyClose = FALSE, size = "l",
    footer = list(modalButton("Close Tutorial"), bsButton("tut_back_welcome", "Go back to overview"), bsButton("tut_represent", "Continue to representations"))
  )
  
  represent_modal <- modalDialog(
    title = "Representations", 
    br(),
    "Coming soon...",
    easyClose = FALSE, size = "l",
    footer = list(modalButton("Close Tutorial"), bsButton("tut_welcome", "Go back to statistics"), bsButton("tut_custom_labels", "Continue to customizing labels"))
  )
  
  custom_labels_modal <- modalDialog(
    title = "Customize Labels", 
    br(),
    "Coming soon...",
    easyClose = FALSE, size = "l",
    footer = list(modalButton("Close Tutorial"), bsButton("tut_represent", "Go back to representations"), bsButton("tut_custom_colors", "Continue to customizing colors"))
  )
  
  custom_colors_modal <- modalDialog(
    title = "Customize Colors", 
    br(),
    "Coming soon...",
    easyClose = FALSE, size = "l",
    footer = list(modalButton("Close Tutorial"),  bsButton("tut_custom_labels", "Go back to customizing labels"), bsButton("tut_references", "Continue to references"))
  )
  
  references_modal <- modalDialog(
    title = "References and recommended Readings", 
    br(),
    "Coming soon...",
    easyClose = FALSE, size = "l",
    footer = list(modalButton("Close Tutorial"), bsButton("tut_custom_colors", "Go back to customizing colors"), bsButton("tut_overview", "Continue to overview"))
  )
  
  tut_end_modal <- modalDialog(
    title = "You have finished the tutorial", 
    br(),
    "Coming soon...",
    "Now that you have an overview of the riskyrApp, it is time to play around with it.",
    "That what it is meant for.",
    "Enjoy your path to risk literacy.",
    easyClose = FALSE, size = "l",
    footer = modalButton("End Tutorial")
  )
  
  
  #####
  # Help buttons deliever tutorial/help modals
  
  observeEvent(input$help_stats, { showModal(stats_modal) })
  observeEvent(input$help_represent, { showModal(represent_modal) })
  observeEvent(input$help_custom_labels, { showModal(custom_labels_modal) })
  observeEvent(input$help_custom_colors, { showModal(custom_colors_modal) })
  
  
  #####
  # Create tutorial with modals
  
  observeEvent(input$link_to_tutorial, {  showModal(tut_start_modal) })
  observeEvent(input$tut_back_welcome, { updateTabsetPanel(session, "tabs", "welcome"); showModal(tut_start_modal) })
  observeEvent(input$tut_welcome, { updateTabsetPanel(session, "tabs", "stats"); showModal(stats_modal) })
  observeEvent(input$tut_represent, { updateTabsetPanel(session, "tabs", "represent"); showModal(represent_modal) })
  observeEvent(input$tut_custom_labels, { updateTabsetPanel(session, "tabs", "custom_labels"); showModal(custom_labels_modal) })
  observeEvent(input$tut_custom_colors, { updateTabsetPanel(session, "tabs", "custom_colors"); showModal(custom_colors_modal) })
  observeEvent(input$tut_references, { updateTabsetPanel(session, "tabs", "references"); showModal(references_modal) })
  observeEvent(input$tut_overview, { updateTabsetPanel(session, "tabs", "welcome"); showModal(tut_end_modal) })
  
  
  
  ##### 
  # Couple numeric and slider inputs:
  
  #####
  ## population (logified version)
  
  observeEvent({
    input$N
  }, {
    env$N <- 10**input$N
    env$recalc.N <- input$N
  })
  

  #####
  ## prevalence
  
  observeEvent({
    input$prev }, {
      env$prev <- input$prev/100
      env$recalc.prev <- input$prev
      updateNumericInput(session, "numprev", value = env$recalc.prev)
    })
  

  observeEvent({
    input$numprev }, {
      env$prev <- input$numprev/100
      env$recalc.prev <- input$numprev
      updateSliderInput(session, "prev", value = env$recalc.prev)
    })
  

  #####
  ## sensitivity
  
  observeEvent({
    input$sens }, {
      env$sens <- input$sens/100
      env$recalc.sens <- input$sens
      updateNumericInput(session, "numsens", value = env$recalc.sens)
    })
  

  observeEvent({
    input$numsens }, {
      env$sens <- input$numsens/100
      env$recalc.sens <- input$numsens
      updateSliderInput(session, "sens", value = env$recalc.sens)
    })
  
  #####
  ## specificity
  
  observeEvent({
    input$spec }, {
      env$spec <- input$spec/100
      env$recalc.spec <- input$spec
      updateNumericInput(session, "numspec", value = env$recalc.spec)
    })
  
  observeEvent({
    input$numspec }, {
      env$spec <- input$numspec/100
      env$recalc.spec <- input$numspec
      updateSliderInput(session, "spec", value = env$recalc.spec)
    })

  
  ##### 
  # Create reactive riskyr.scenario object from inputs
  riskyr.scenario <- reactive({
      riskyr(scen_lbl = "Example",
             cond_lbl = "Hustosis",
             dec_lbl = "Screening test",
             popu_lbl = "Sample",
             N = env$N,  # population size (log scale)
             prev = env$prev,
             sens = env$sens,
             spec = env$spec
          )
      })

  
  
  #####
  # Calculate freq and prob objects
  observeEvent({
    env$name   # name of current environment
    env$N      # N in population
    env$prev   # prevalence in population = p(true positive)
    env$sens   # sensitivity = p(decision positive | condition positive)
    env$spec   # specificity = p(decision negative | condition negative)
  }, {
    freq$cond.true <- round((env$N * env$prev), 0)
    freq$cond.false <- (env$N - freq$cond.true)
    freq$hi <- round((env$sens * freq$cond.true), 0)
    freq$mi <- freq$cond.true - freq$hi
    freq$cr <- round((env$spec * freq$cond.false), 0)
    freq$fa <- freq$cond.false - freq$cr
    freq$dec.pos <- freq$hi + freq$fa
    freq$dec.neg <- freq$mi + freq$cr 
    prob$PPV <- riskyr::comp_PPV(prev = env$prev, sens = env$sens, spec = env$spec)
    prob$FDR <- riskyr::comp_FDR(prev = env$prev, sens = env$sens, spec = env$spec)
    prob$NPV <- riskyr::comp_NPV(prev = env$prev, sens = env$sens, spec = env$spec)
    prob$FOR <- riskyr::comp_FOR(prev = env$prev, sens = env$sens, spec = env$spec)
  })
  
  
  #####
  # Create reactive population object
  popu <- reactive({ riskyr::comp_popu(
    hi = freq$hi, mi = freq$mi, 
    fa = freq$fa, cr = freq$cr, 
    cond.true.lbl = cus$cond.true.lbl, cond.false.lbl = cus$cond.false.lbl,
    dec.pos.lbl = cus$dec.true.lbl, dec.neg.lbl = cus$dec.false.lbl,
    hi.lbl = cus$sdt.hi.lbl, mi.lbl = cus$sdt.mi.lbl,
    fa.lbl = cus$sdt.fa.lbl, cr.lbl = cus$sdt.cr.lbl)
    })
  
  
  #####
  # Integrate worked out examples:
  # NEW:
  observeEvent({
    input$dataselection
    input$dataselection2 }, {
      if(input$tabs == "stats") {
        updateSelectInput(session, "dataselection", selected = input$dataselection2)
      }
      if(input$tabs == "represent") {
        updateSelectInput(session, "dataselection2", selected = input$dataselection)
      }
      if (input$dataselection != 1 | input$dataselection2 != 1) { # if 1st option is not ("---")
        # update all sliders
        updateSliderInput(session, "N", value = round(log10(datasets[input$dataselection, "N" ]), 0))
        updateSliderInput(session, "sens", value = datasets[input$dataselection, "sens" ])
        updateNumericInput(session, "numsens", value = datasets[input$dataselection, "sens"])
        updateSliderInput(session, "prev", value = datasets[input$dataselection, "prev"])
        updateNumericInput(session, "numprev",value = datasets[input$dataselection, "prev"])
        updateSliderInput(session, "spec", value = datasets[input$dataselection, "spec" ])
        updateNumericInput(session, "numspec", value = datasets[input$dataselection, "spec" ])
        # display source
        output$sourceOutput <- renderText(datasets[input$dataselection, "source"]) 
        # set labels
        updateTextInput(session, "target.population.lbl", value = datasets[input$dataselection, "target.population.lbl"])
        updateTextInput(session, "scenario.txt", value = datasets[input$dataselection, "scenario.txt"])
        
        cus$target.population.lbl <- datasets[input$dataselection, "target.population.lbl"]
        cus$scenario.txt <- datasets[input$dataselection, "scenario.txt"]
        # (a) Condition
        updateTextInput(session, "condition.lbl", value = datasets[input$dataselection, "condition.lbl"])
        updateTextInput(session, "cond.true.lbl", value = datasets[input$dataselection, "cond.true.lbl"])
        updateTextInput(session, "cond.false.lbl", value = datasets[input$dataselection, "cond.false.lbl"])
        
        cus$condition.lbl <- datasets[input$dataselection, "condition.lbl"]
        cus$cond.true.lbl <- datasets[input$dataselection, "cond.true.lbl"]
        cus$cond.false.lbl <- datasets[input$dataselection, "cond.false.lbl"]
        # (b) Decisions:
        updateTextInput(session, "decision.lbl", value = datasets[input$dataselection, "decision.lbl"])
        updateTextInput(session, "dec.true.lbl", value = datasets[input$dataselection, "dec.true.lbl"] )
        updateTextInput(session, "dec.false.lbl", value = datasets[input$dataselection, "dec.false.lbl"] )
        
        
        cus$decision.lbl <- datasets[input$dataselection, "decision.lbl"]
        cus$dec.true.lbl <- datasets[input$dataselection, "dec.true.lbl"]
        cus$dec.false.lbl <- datasets[input$dataselection, "dec.false.lbl"]
        # (c) sdt cases (combinations):
        updateTextInput(session, "sdt.hi.lbl", value = datasets[input$dataselection, "sdt.hi.lbl"])
        updateTextInput(session, "sdt.mi.lbl", value = datasets[input$dataselection, "sdt.mi.lbl"])
        updateTextInput(session, "sdt.fa.lbl", value = datasets[input$dataselection, "sdt.fa.lbl"])
        updateTextInput(session, "sdt.cr.lbl", value = datasets[input$dataselection, "sdt.cr.lbl"])

        cus$sdt.hi.lbl <- datasets[input$dataselection, "sdt.hi.lbl"]
        cus$sdt.mi.lbl <- datasets[input$dataselection, "sdt.mi.lbl"]
        cus$sdt.fa.lbl <- datasets[input$dataselection, "sdt.fa.lbl"]
        cus$sdt.cr.lbl <- datasets[input$dataselection, "sdt.cr.lbl"]
        }
  }, ignoreInit = TRUE)


  #####
  ## Outputs:
 
  ## (1) Prism:
  
  prism <- function(){
      plot(riskyr.scenario(), 
           type  = "prism",
           col_pal = riskyr.colors(),
           by = input$prism.by,
           area = input$prism.area,
           f_lbl = input$prism.f_lbl
           )
  }

  output$prism <- renderPlot({ prism() }) 
  
  output$prism.dl <- downloadHandler(
    filename = function() {paste0("riskyrApp_prism_", gsub(":", "-", Sys.time()), ".png")},
    content =  function(file){
      png(file, width = 550, height = 550)
      prism()
      dev.off()}
    )


  ## (2) Table: 
  
  table <- function(){
      plot(riskyr.scenario(), 
           type  = "table",
           col_pal = riskyr.colors(),
           by = input$table.by,
           area = input$table.p_split,
           f_lbl = input$table.f_lbl
      )
  }
  
  output$table <- renderPlot({ table() }) 
  
  output$table.dl <- downloadHandler(
      filename = function() {paste0("riskyrApp_table_", gsub(":", "-", Sys.time()), ".png")},
      content =  function(file){
          png(file, width = 550, height = 550)
          table()
          dev.off()}
  )
  
  
  ## (3) Area: 
  
  area <- function(){
      plot(riskyr.scenario(), 
           type  = "area",
           col_pal = riskyr.colors(),
           by = input$area.by,
           p_split = input$area.p_split,
           f_lbl = input$area.f_lbl
      )
  }
  
  output$area <- renderPlot({ area() }) 
  
  output$area.dl <- downloadHandler(
      filename = function() {paste0("riskyrApp_area_", gsub(":", "-", Sys.time()), ".png")},
      content =  function(file){
          png(file, width = 550, height = 550)
          area()
          dev.off()}
  )
  
  
  ## (4) Icons: 
  
  icons <- function(){
      plot(riskyr.scenario(), 
           type  = "icons",
           col_pal = riskyr.colors(),
           arr_type = input$icons.arr_type,
           icon_types = c(as.integer(input$symbol.hi), as.integer(input$symbol.mi), 
                          as.integer(input$symbol.cr), as.integer(input$symbol.fa)) 
      )
  }
  
  output$icons <- renderPlot({ icons() }) 
  
  output$icons.dl <- downloadHandler(
      filename = function() {paste0("riskyrApp_icons_", gsub(":", "-", Sys.time()), ".png")},
      content =  function(file){
          png(file, width = 550, height = 550)
          icons()
          dev.off()}
  )
  
  
  ## (5) Bars: 
  
  bar <- function(){
      plot(riskyr.scenario(), 
           type  = "bar",
           col_pal = riskyr.colors(),
           by = input$bar.by,
           dir = input$bar.dir,
           f_lbl = input$bar.f_lbl
      )
  }
  
  output$bar <- renderPlot({ bar() }) 
  
  output$bar.dl <- downloadHandler(
      filename = function() {paste0("riskyrApp_bar_", gsub(":", "-", Sys.time()), ".png")},
      content =  function(file){
          png(file, width = 550, height = 550)
          bar()
          dev.off()}
  )
  
  
  ## (6): Curves
  
  curve <- function(){
      plot(riskyr.scenario(), 
           type  = "curve",
           col_pal = riskyr.colors(),
           what = c("prev", "PPV", "NPV", "acc", "ppod")[c(TRUE, TRUE, TRUE, input$curve.show_acc, input$curve.show_ppod)],
           show_points = input$curve.show_points,
           log_scale = input$curve.log_scale
           # ,
           # p_lbl = input$bar.f_lbl
      )
  }
  
  output$curve <- renderPlot({ curve() })
  
  output$curve.dl <- downloadHandler(
    filename = function() {paste0("riskyrApp_curves_", gsub(":", "-", Sys.time()), ".png")},
    content =  function(file){
      png(file, width = 1250, height = 400)
        curve()
      dev.off()}
  )
  

  ## (g) 3D plots of PPV and NPV planes as functions of sens and spec:
  
  PV3dPPV <- function(){    
    riskyr::plot_plane(prev = env$prev, sens = env$sens, spec = env$spec,
                                               what = "PPV",
                                               what.col = cus$color.ppv,
                                               show.point = input$boxPVpoints2,
                                               theta = input$theta, phi = input$phi,
                                               title.lbl = cus$scenario.txt) 
    }
  
  output$PV3dPPV <- renderPlot({ PV3dPPV() })
  
  output$PV3dPPVdl <- downloadHandler(
    filename = function() {paste0("riskyrApp_PPV-cube_", gsub(":", "-", Sys.time()), ".png")},
    content =  function(file){
      png(file, width = 610, height = 400)
      PV3dPPV()
      dev.off()}
  )
  
  PV3dNPV <- function(){
    riskyr::plot_plane(prev = env$prev, sens = env$sens, spec = env$spec,
                       what = "NPV",
                       what.col = cus$color.npv,
                       show.point = input$boxPVpoints2,
                       theta = input$theta, phi = input$phi,
                       title.lbl = cus$scenario.txt)
    
  }
  
  output$PV3dNPV <- renderPlot({ PV3dNPV() })
  
  output$PV3dNPVdl <- downloadHandler(
    filename = function() {paste0("riskyrApp_NPV-cube_", gsub(":", "-", Sys.time()), ".png")},
    content =  function(file){
      png(file, width = 610, height = 400)
      PV3dNPV()
      dev.off()}
  )
  
  ## (h) Contrasting two freely selectable representations
  
  output$represent1 <- renderPlot({
    switch(input$represent1,
           fnet = fnet(),
           # rawdatatable
           iconarray = icons(),
           tree = nftree(),
           mosaic = mosaicplot()
           
    )
  })
  
  output$represent1dl <- downloadHandler(
    filename = function() {paste0("riskyrApp_representation1_", gsub(":", "-", Sys.time()), ".png")},
    content =  function(file){
      png(file, width = 550, height = 550)
      switch(input$represent1,
             fnet = fnet(),
             # rawdatatable
             iconarray = icons(),
             tree = nftree(),
             mosaic = mosaicplot()
             
      )
      dev.off()}
  )
  
  
  output$represent2 <- renderPlot({
    switch(input$represent2,
           fnet = fnet(),
           # rawdatatable
           iconarray = icons(),
           tree = nftree(),
           mosaic = mosaicplot()
           
    )
  })
  
  
  output$represent2dl <- downloadHandler(
    filename = function() {paste0("riskyrApp_representation2_", gsub(":", "-", Sys.time()), ".png")},
    content =  function(file){
      png(file, width = 550, height = 550)
      switch(input$represent2,
             fnet = fnet(),
             # rawdatatable
             iconarray = icons(),
             tree = nftree(),
             mosaic = mosaicplot()
             
      )
      dev.off()}
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
    cus$target.population.lbl <- as.character(default.terminology[names(default.terminology) == "target.population.lbl"])
    cus$scenario.txt <- as.character(default.terminology[names(default.terminology) == "scenario.txt"])
    # (a) Condition
    cus$condition.lbl <- as.character(default.terminology[names(default.terminology) == "condition.lbl"])
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

  output$previewlabels <- renderPlot({
    
    M <- matrix(nrow = 10, ncol = 11, byrow = TRUE, data = 0)
    M[2:3, 1] <- M[4:5, 2] <- M[6:7, 3] <- M[4, 8] <- ""
    M[6, 8] <- M[5, 9] <-  M[7, 9] <-  M[8:9, 10] <- ""
    
    diagram::plotmat(M, pos = c(1, 2, 4, 2, 1), curve = 0, lwd = 1,
                     box.type = "rect", relsize = 0.98, arr.pos = 0.5, 
                     box.prop = 1/2,
                     arr.length = 0.2, arr.type = "triangle", arr.width = 0.15,
                     box.col = "lightgrey", shadow.size = 0,
                     lcol = rgb(62, 63, 58, max = 255),
                     name = c(cus$target.population.lbl,
                              paste0(cus$condition.lbl, ":\n",
                                     cus$cond.true.lbl),
                              paste0(cus$condition.lbl, ":\n",
                                     cus$cond.false.lbl),
                              cus$sdt.hi.lbl,
                              cus$sdt.mi.lbl,
                              cus$sdt.fa.lbl,  
                              cus$sdt.cr.lbl,
                              paste0(cus$decision.lbl, ":\n",
                                     cus$dec.true.lbl),
                              paste0(cus$decision.lbl, ":\n",
                                     cus$dec.false.lbl),
                             cus$target.population.lbl
                              )
    )
    
    
  })
  

  ## Customize colors: ------

  # Apply color selection
  observeEvent(input$applycustomcolor, {
    cus$color.hi <- input$color.hi
    cus$color.mi <- input$color.mi
    cus$color.fa <- input$color.fa
    cus$color.cr <- input$color.cr
    cus$color.ppv <- input$color.ppv
    cus$color.npv <- input$color.npv
    print(input$color.hi)
  })

  # Simplified display of sdt states
  output$sampleplot <- renderPlot({
    par(pty="s")
    plot(c(0,2), c(0,2), type="n", xaxt='n', yaxt='n', ann=FALSE)
    rect(xleft = c(0, 1, 0, 1), ybottom = c(0, 0, 1, 1), 
         xright = c(1, 2, 1, 2), ytop = c(1, 1, 2, 2),
         col = c(cus$color.mi, cus$color.cr, cus$color.hi, cus$color.fa))
    text(x = c(0.5, 1.5, 0.5, 1.5), y = c(1.5, 1.5, 0.5, 0.5), col = "black",
         labels = c("hit", "false alarm", "miss", "correct\nrejection"), cex = 1.1)
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

  
  # Reset colors to default: ------
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
