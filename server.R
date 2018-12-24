# server.R
# riskyrApp | R Shiny | spds, uni.kn | 2018 12 24

## Clean up: ------

rm(list=ls()) # clean all.

## Dependencies: ------

library("shiny")
library("shinyBS")
library("markdown")
library("colourpicker")

## Install the currently included version of riskyr: ------
# detach("package:riskyr", unload = TRUE)
# devtools::install_github("hneth/riskyr")
library("riskyr")
# sessionInfo()

## Import data (example scenarios) and default colors and labels: ------

datasets <- read.csv2("./www/df_scenarios_riskyrApp_2018-12-14.csv", stringsAsFactors = FALSE)

# Default colors and text labels: 
default.colors <- init_pal()
default.labels <- init_txt()

# reactive color palette: 
riskyr.colors <- reactive({ init_pal() })

# reactive labels: 
riskyr.labels <- reactive({ init_txt() })


## Define server logic: ------ 

shinyServer(function(input, output, session){
  
  # save environment parameters N, prev, sens, spec as reactive values: 
  env <- reactiveValues(env = NULL) 
  
  # create modals with tutorial/help contents: ---- 
  
  inputs_modal <- modalDialog(
    title = "Inputs", 
    br(),
    "Coming soon...",
    easyClose = FALSE, size = "l",
    footer = modalButton("Close")
  )
  
  custom_labels_modal <- modalDialog(
    title = "Customize Labels", 
    br(),
    "Coming soon...",
    easyClose = FALSE, size = "l",
    footer = modalButton("Close")
  )
  
  custom_colors_modal <- modalDialog(
    title = "Customize Colors", 
    br(),
    "Coming soon...",
    easyClose = FALSE, size = "l",
    footer = modalButton("Close")
  )
  
  # Help buttons deliever tutorial/help modals: 
  observeEvent(input$help_inputs, { showModal(inputs_modal) })
  observeEvent(input$help_custom_labels, { showModal(custom_labels_modal) })
  observeEvent(input$help_custom_colors, { showModal(custom_colors_modal) })
  
  
  ## Couple numeric and slider inputs: ------ 
  
  ## population (logified version): ---- 
  
  observeEvent({
    input$N
  }, {
    env$N <- 10**input$N
    env$recalc.N <- input$N
  })
  
  ## prevalence: ---- 
  
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
  
  ## sensitivity: ---- 
  
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
  
  ## specificity: ---- 
  
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
  
  
  ## Create reactive riskyr.scenario object from inputs: ---- 
  riskyr.scenario <- reactive({
    riskyr(
      scen_lbl = input$scen_lbl,
      scen_txt = input$scenario_txt,
      # scen_src = input$scen_src,
      # scen_apa = input$scen_apa,
      # scen_lng = input$scen_lng,
      popu_lbl = input$popu_lbl,
      # N_lbl = input$N_lbl,
      cond_lbl = input$cond_lbl,
      cond.true_lbl = input$cond.true_lbl,
      cond.false_lbl = input$cond.false_lbl,
      dec_lbl = input$dec_lbl,
      dec.pos_lbl = input$dec.pos_lbl,
      dec.neg_lbl = input$dec.neg_lbl,
      # acc_lbl = input$acc_lbl,
      # dec.cor_lbl = input$dec.cor_lbl,
      # dec.err_lbl= input$dec.err_lbl,
      # sdt_lbl = input$sdt_lbl,
      hi_lbl = input$hi_lbl,
      mi_lbl = input$mi_lbl,
      fa_lbl = input$fa_lbl,
      cr_lbl = input$cr_lbl,
      N = env$N,
      prev = env$prev,
      sens = env$sens,
      spec = env$spec
    )
  })
  
  ## Integrate worked out examples: ------ 
  observeEvent(
    input$dataselection, {
      if (input$dataselection != 1) { # if 1st option is not ("---")
        # update all sliders
        updateSliderInput(session, "N", value = round(log10(datasets[input$dataselection, "N" ]), 0))
        updateSliderInput(session, "sens", value = datasets[input$dataselection, "sens" ])
        updateNumericInput(session, "numsens", value = datasets[input$dataselection, "sens"])
        updateSliderInput(session, "prev", value = datasets[input$dataselection, "prev"])
        updateNumericInput(session, "numprev",value = datasets[input$dataselection, "prev"])
        updateSliderInput(session, "spec", value = datasets[input$dataselection, "spec" ])
        updateNumericInput(session, "numspec", value = datasets[input$dataselection, "spec" ])
        # set labels
        updateTextInput(session, "scen_lbl", value = datasets[input$dataselection, "scen_lbl"])
        updateTextInput(session, "popu_lbl", value = datasets[input$dataselection, "popu_lbl"])
        updateTextInput(session, "cond_lbl", value = datasets[input$dataselection, "cond_lbl"])
        updateTextInput(session, "cond.true_lbl", value = datasets[input$dataselection, "cond.true_lbl"])
        updateTextInput(session, "cond.false_lbl", value = datasets[input$dataselection, "cond.false_lbl"])
        updateTextInput(session, "dec_lbl", value = datasets[input$dataselection, "dec_lbl"])
        updateTextInput(session, "dec.pos_lbl", value = datasets[input$dataselection, "dec.pos_lbl"])
        updateTextInput(session, "dec.neg_lbl", value = datasets[input$dataselection, "dec.neg_lbl"])
        updateTextInput(session, "hi_lbl", value = datasets[input$dataselection, "hi_lbl"])
        updateTextInput(session, "mi_lbl", value = datasets[input$dataselection, "mi_lbl"])
        updateTextInput(session, "fa_lbl", value = datasets[input$dataselection, "fa_lbl"])
        updateTextInput(session, "cr_lbl", value = datasets[input$dataselection, "cr_lbl"])
        updateTextInput(session, "scenario.txt", value = datasets[input$dataselection, "scenario.txt"])
      }
    }, ignoreInit = TRUE)
  
  
  ## Outputs: ---------- 
  
  ## (1) Prism: ----
  
  prism <- function(){
    plot(riskyr.scenario(), 
         type  = "prism",
         col_pal = riskyr.colors(),
         # lbl_txt = riskyr.labels(),
         by = input$prism.by,
         area = input$prism.area,
         f_lbl = input$prism.f_lbl
    )
  }
  
  output$prism <- renderPlot({ prism() }) 
  
  output$prism.dl <- downloadHandler(
    filename = function() {paste0("riskyrApp_prism_", gsub(":", "-", Sys.time()), ".png")},
    content =  function(file){
      png(file, width = 750, height = 550)
      prism()
      dev.off()}
  )
  
  ## (2) Table: ----
  
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
      png(file, width = 650, height = 550)
      table()
      dev.off()}
  )
  
  ## (3) Area: ----
  
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
      png(file, width = 650, height = 550)
      area()
      dev.off()}
  )
  
  ## (4) Icons: ---- 
  
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
      png(file, width = 650, height = 550)
      icons()
      dev.off()}
  )
  
  ## (5) Bars: ---- 
  
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
      png(file, width = 650, height = 550)
      bar()
      dev.off()}
  )
  
  ## (6) Curves: ---- 
  
  curve <- function(){
    plot(riskyr.scenario(), 
         type  = "curve",
         col_pal = riskyr.colors(),
         what = c("prev", "PPV", "NPV", "acc", "ppod")[c(TRUE, TRUE, TRUE, input$curve.show_acc, input$curve.show_ppod)],
         show_points = input$curve.show_points,
         log_scale = input$curve.log_scale
    )
  }
  
  output$curve <- renderPlot({ curve() })
  
  output$curve.dl <- downloadHandler(
    filename = function() {paste0("riskyrApp_curves_", gsub(":", "-", Sys.time()), ".png")},
    content =  function(file){
      png(file, width = 750, height = 550)
      curve()
      dev.off()}
  )
  
  ## (7) Planes: ---- 
  
  plane.ppv <- function(){
    plot(riskyr.scenario(),
         type = "plane",
         what = "PPV",
         col_pal = riskyr.colors(),
         show_point = input$plane.show_point,
         theta = input$theta, 
         phi = input$phi
    ) 
  }
  
  output$plane.ppv <- renderPlot({ plane.ppv() })
  
  output$plane.ppv.dl <- downloadHandler(
    filename = function() {paste0("riskyrApp_PPV-plane_", gsub(":", "-", Sys.time()), ".png")},
    content =  function(file){
      png(file, width = 610, height = 400)
      plane.ppv()
      dev.off()}
  )
  
  plane.npv <- function(){
    plot(riskyr.scenario(),
         type = "plane",
         what = "NPV",
         col_pal = riskyr.colors(),
         show_point = input$plane.show_point,
         theta = input$theta, 
         phi = input$phi
    ) 
  }
  
  output$plane.npv <- renderPlot({ plane.npv() })
  
  output$plane.npv.dl <- downloadHandler(
    filename = function() {paste0("riskyrApp_NPV-plane_", gsub(":", "-", Sys.time()), ".png")},
    content =  function(file){
      png(file, width = 610, height = 400)
      plane.npv()
      dev.off()}
  )
  
  ## (8) Contrasting representations: ------ 
  
  output$represent1 <- renderPlot({
    switch(input$represent1,
           prism = prism(),
           table = table(),
           area = area(),
           icons = icons(),
           bar = bar(),
           curve = curve(),
           plane.ppv = plane.ppv(),
           plane.npv = plane.npv()
    )
  })
  
  output$represent2 <- renderPlot({
    switch(input$represent2,
           prism = prism(),
           table = table(),
           area = area(),
           icons = icons(),
           bar = bar(),
           curve = curve(),
           plane.ppv = plane.ppv(),
           plane.npv = plane.npv()
    )
  })
  
  output$represent3 <- renderPlot({
    switch(input$represent3,
           prism = prism(),
           table = table(),
           area = area(),
           icons = icons(),
           bar = bar(),
           curve = curve(),
           plane.ppv = plane.ppv(),
           plane.npv = plane.npv()
    )
  })
  
  output$represent4 <- renderPlot({
    switch(input$represent4,
           prism = prism(),
           table = table(),
           area = area(),
           icons = icons(),
           bar = bar(),
           curve = curve(),
           plane.ppv = plane.ppv(),
           plane.npv = plane.npv()
    )
  })
  
  ## Customize labels: ------ 
  
  # Reset labels to default
  observeEvent(input$resetcustomlabel, {
    updateTextInput(session, "scen_lbl", value = default.labels$scen_lbl)
    updateTextInput(session, "scen_txt", value = default.labels$scen_txt)
    updateTextInput(session, "scen_src", value = default.labels$scen_src)
    updateTextInput(session, "scen_apa", value = default.labels$scen_apa)
    updateTextInput(session, "scen_lng", value = default.labels$scen_lng)
    updateTextInput(session, "popu_lbl", value = default.labels$popu_lbl)
    updateTextInput(session, "N_lbl", value = default.labels$N_lbl)
    updateTextInput(session, "cond_lbl", value = default.labels$cond_lbl)
    updateTextInput(session, "cond.true_lbl", value = default.labels$cond.true_lbl)
    updateTextInput(session, "cond.false_lbl", value = default.labels$cond.false_lbl)
    updateTextInput(session, "dec_lbl", value = default.labels$dec_lbl)
    updateTextInput(session, "dec.pos_lbl", value = default.labels$dec.pos_lbl)
    updateTextInput(session, "dec.neg_lbl", value = default.labels$dec.neg_lbl)
    updateTextInput(session, "acc_lbl", value = default.labels$acc_lbl)
    updateTextInput(session, "dec.cor_lbl", value = default.labels$dec.cor_lbl)
    updateTextInput(session, "dec.err_lbl", value = default.labels$dec.err_lbl)
    updateTextInput(session, "sdt_lbl", value = default.labels$sdt_lbl)
    updateTextInput(session, "hi_lbl", value = default.labels$hi_lbl)
    updateTextInput(session, "mi_lbl", value = default.labels$mi_lbl)
    updateTextInput(session, "fa_lbl", value = default.labels$fa_lbl)
    updateTextInput(session, "cr_lbl", value = default.labels$cr_lbl)
  })
  
  # Prism for previewing labels: 
  output$previewlabels <- renderPlot({
    plot(riskyr.scenario(),
         type = "prism",
         col_pal = riskyr.colors(),
         f_lbl = "nam",
         mar_notes = FALSE,
         p_lbl = NULL,
         arr_c = 0
    ) 
  })
  
  ## Customize colors: ------ 
  
  riskyr.colors <- reactive({
    init_pal(
      N_col = input$color.N,
      cond.true_col = input$color.true,
      cond.false_col = input$color.false,
      dec.pos_col = input$color.pos,
      dec.neg_col = input$color.neg,
      # dec.cor_col = input$colo,
      # dec.err_col = ,
      hi_col = input$color.hi,
      mi_col = input$color.mi,
      fa_col = input$color.fa,
      cr_col = input$color.cr,
      PPV_col = input$color.ppv,
      NPV_col = input$color.npv,
      txt_col = input$color.txt,
      brd_col = input$color.brd
    )
  })
  
  # Simplified display of table: 
  output$sample.table <- renderPlot({
    plot(riskyr.scenario(),
         col_pal = riskyr.colors(),
         type = "table",
         f_lbl = "nam",
         # title_lbl = NA,
         mar_notes = FALSE)
  })
  
  # Simplified display of prism: 
  output$sample.prism <- renderPlot({
    plot(riskyr.scenario(),
         col_pal = riskyr.colors(),
         type = "prism",
         f_lbl = "nam",
         # title_lbl = NA,
         mar_notes = FALSE)
  })
  
  # Simplified plot with PPV and NPV curves: 
  output$sample.curves <- renderPlot({
    plot(riskyr.scenario(),
         col_pal = riskyr.colors(),
         type = "curve",
         f_lbl = "nam",
         show_points = FALSE,
         # title_lbl = NA,
         mar_notes = FALSE)
  })
  
  # Reset colors to default: 
  observeEvent(input$resetcustomcolor, {
    updateSelectInput(session, "alt.palette", selected = "default")
  })
  
  # Integrated alternative palettes: 
  observeEvent(input$alt.palette,{
    new.colors <- switch(input$alt.palette,
                         default = init_pal(),
                         pal_4c = pal_4c,
                         pal_bw = pal_bw,
                         pal_gbs = pal_gbs,
                         pal_kn = pal_kn,
                         pal_org = pal_org,
                         pal_vir = pal_vir
    )
    updateColourInput(session, "color.N", value = as.character(new.colors["N"]))
    updateColourInput(session, "color.true", value = as.character(new.colors["true"]))
    updateColourInput(session, "color.false", value = as.character(new.colors["false"]))
    updateColourInput(session, "color.pos", value = as.character(new.colors["pos"]))
    updateColourInput(session, "color.neg", value = as.character(new.colors["neg"]))
    updateColourInput(session, "color.cor", value = as.character(new.colors["corr"]))
    updateColourInput(session, "color.err", value = as.character(new.colors["err"]))
    updateColourInput(session, "color.hi", value = as.character(new.colors["hi"]))
    updateColourInput(session, "color.mi", value = as.character(new.colors["mi"]))
    updateColourInput(session, "color.fa", value = as.character(new.colors["fa"]))
    updateColourInput(session, "color.cr", value = as.character(new.colors["cr"]))
    updateColourInput(session, "color.ppv", value = as.character(new.colors["ppv"]))
    updateColourInput(session, "color.npv", value = as.character(new.colors["npv"]))
    updateColourInput(session, "color.txt", value = as.character(new.colors["txt"]))
    updateColourInput(session, "color.brd", value = as.character(new.colors["brd"]))
  })
  
}
)

## eof. ---------- 