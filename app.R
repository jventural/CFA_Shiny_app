# app.R

# ——————————————————————————————————————————————————————————————
# 1) Activar renv para que Posit Connect use las librerías de renv/library
if (file.exists("renv/activate.R")) {
  source("renv/activate.R")
}

# 2) Establecer espejo CRAN por defecto
options(repos = c(CRAN = "https://cran.rstudio.com"))

# 3) Cargar todas las bibliotecas necesarias (sin ggpubr, wesanderson ni patchwork)
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(lavaan)
  library(semPlot)
  library(semTools)
  library(dplyr)
  library(readxl)
  library(sessioninfo)
  library(bibtex)
  library(PsyMetricTools)
  library(ggplot2)
  library(tidyr)
  library(gridExtra)   # para grid.arrange
  library(gtable)
  library(purrr)
  library(reshape2)
})

# ——————————————————————————————————————————————————————————————
# 4) Función boot_cfa_plot SIN llamar a library(wesanderson) ni patchwork
boot_cfa_plot <- function(df,
                          save = TRUE,
                          path = "Plot_boot_cfa.jpg",
                          dpi = 600,
                          omega_ymin_annot = NULL,
                          omega_ymax_annot = NULL,
                          comp_ymin_annot = NULL,
                          comp_ymax_annot = NULL,
                          abs_ymin_annot = NULL,
                          abs_ymax_annot = NULL,
                          palette = "grey",
                          ...) {
  suppressWarnings({
    #============================================================
    # 0. Cargamos únicamente lo imprescindible (¡NO hay wesanderson ni patchwork!)
    library(ggplot2)
    library(tidyr)
    library(dplyr)
    library(purrr)
    library(reshape2)
    library(gridExtra)
    library(gtable)
    
    #------------------------------------------------------------
    # 1. Paleta de colores: usa wesanderson SOLO si está instalado
    #------------------------------------------------------------
    get_palette <- function(pal, n) {
      if (requireNamespace("wesanderson", quietly = TRUE) &&
          pal %in% names(wesanderson::wes_palettes)) {
        wesanderson::wes_palette(pal, n, type = "discrete")
      } else if (identical(pal, "grey")) {
        gray.colors(n, start = 0.5, end = 0.9)
      } else {
        rep(pal, n)
      }
    }
    
    #------------------------------------------------------------
    # 2. Color encabezado de tablas
    #------------------------------------------------------------
    get_header_color <- function(pal) {
      if (identical(pal, "grey")) {
        "grey85"
      } else if (requireNamespace("wesanderson", quietly = TRUE) &&
                 pal %in% names(wesanderson::wes_palettes)) {
        wesanderson::wes_palette(pal, 1, type = "discrete")
      } else {
        pal
      }
    }
    
    #------------------------------------------------------------
    # 3. Bordes horizontales en tablas
    #------------------------------------------------------------
    add_horizontal_borders <- function(tbl) {
      tbl <- gtable::gtable_add_grob(tbl,
                                     grobs = grid::segmentsGrob(
                                       x0 = unit(0, "npc"), x1 = unit(1, "npc"),
                                       y0 = unit(1, "npc"), y1 = unit(1, "npc"),
                                       gp = grid::gpar(lwd = 2)
                                     ),
                                     t = 1, l = 1, r = ncol(tbl))
      tbl <- gtable::gtable_add_grob(tbl,
                                     grobs = grid::segmentsGrob(
                                       x0 = unit(0, "npc"), x1 = unit(1, "npc"),
                                       y0 = unit(0, "npc"), y1 = unit(0, "npc"),
                                       gp = grid::gpar(lwd = 2)
                                     ),
                                     t = nrow(tbl), l = 1, r = ncol(tbl))
      if (nrow(tbl) > 1) {
        tbl <- gtable::gtable_add_grob(tbl,
                                       grobs = grid::segmentsGrob(
                                         x0 = unit(0, "npc"), x1 = unit(1, "npc"),
                                         y0 = unit(1, "npc") - unit(1, "pt"),
                                         y1 = unit(1, "npc") - unit(1, "pt"),
                                         gp = grid::gpar(lwd = 1)
                                       ),
                                       t = 2, l = 1, r = ncol(tbl))
      }
      tbl
    }
    
    #------------------------------------------------------------
    # 4. Tema de tabla
    #------------------------------------------------------------
    make_table_theme <- function(pal) {
      gridExtra::ttheme_default(
        core = list(bg_params = list(fill = "white", col = NA),
                    fg_params = list(fontface = 1)),
        colhead = list(bg_params = list(fill = get_header_color(pal), col = NA),
                       fg_params = list(col = "black", fontface = c(1,1,3,1,1))),
        rowhead = list(fg_params = list(col = "black", fontface = 1)),
        base_size = 8
      )
    }
    
    #------------------------------------------------------------
    # 5. Panel A: Omega (fiabilidad)
    #------------------------------------------------------------
    plot_and_table_omega <- function(df_repli, ymin_ann, ymax_ann, pal) {
      if ("fit_measures1" %in% names(df_repli)) {
        idx <- which(names(df_repli) == "fit_measures1")
        dat <- df_repli %>% select(-(1:idx))
      } else {
        dat <- df_repli %>% select(where(is.numeric))
      }
      
      res_tbl <- dat %>%
        pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
        mutate(Variable = substr(Variable, 1, 3)) %>%
        group_by(Variable) %>%
        summarise(
          mean = round(mean(Value, na.rm = TRUE), 2),
          sd   = round(sd(Value,   na.rm = TRUE), 2),
          min  = round(min(Value,  na.rm = TRUE), 2),
          max  = round(max(Value,  na.rm = TRUE), 2)
        ) %>%
        ungroup()
      
      ymin <- if (is.null(ymin_ann)) max(res_tbl$mean) else ymin_ann
      ymax <- if (is.null(ymax_ann)) 0.92 else ymax_ann
      
      dat_long <- dat %>%
        pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
        mutate(Variable = substr(Variable, 1, 3))
      
      tbl_grob <- gridExtra::tableGrob(res_tbl, rows = NULL,
                                       theme = make_table_theme(pal)) %>%
        add_horizontal_borders()
      
      p <- ggplot(dat_long, aes(x = Variable, y = Value, fill = Variable)) +
        geom_boxplot(outlier.shape = 16) +
        theme_bw() +
        scale_fill_manual(values = get_palette(pal, length(unique(dat_long$Variable)))) +
        coord_cartesian(ylim = c(min(res_tbl$min) - 0.1, 1)) +
        labs(y = "\u03C9 values") +
        theme(legend.position = "none") +
        annotation_custom(tbl_grob,
                          xmin = 1, xmax = length(res_tbl$Variable),
                          ymin = ymin, ymax = ymax)
      
      list(table = res_tbl, plot = p)
    }
    
    #------------------------------------------------------------
    # 6. Panel B: CFI / TLI
    #------------------------------------------------------------
    plot_and_table_comp <- function(df_repli, ymin_ann, ymax_ann, pal) {
      dfm <- map_dfr(df_repli$fit_measures1, as_tibble)
      
      res_tbl <- dfm %>%
        select(CFI, TLI) %>%
        pivot_longer(everything(), names_to = "Fit", values_to = "Value") %>%
        mutate(Value = round(Value, 3)) %>%
        group_by(Fit) %>%
        summarise(
          mean = round(mean(Value, na.rm = TRUE), 2),
          sd   = round(sd(Value,   na.rm = TRUE), 2),
          min  = round(min(Value,  na.rm = TRUE), 2),
          max  = round(max(Value,  na.rm = TRUE), 2)
        ) %>%
        ungroup()
      
      ymin <- if (is.null(ymin_ann)) min(res_tbl$min) - 0.05 else ymin_ann
      ymax <- if (is.null(ymax_ann)) ymin + 0.05 else ymax_ann
      y0   <- if (min(res_tbl$min) > 0.95) 0.90 else min(res_tbl$min)
      
      dfm_long <- dfm %>%
        select(CFI, TLI) %>%
        pivot_longer(everything(), names_to = "Fit", values_to = "Value") %>%
        mutate(Value = round(Value, 3))
      
      tbl_grob <- gridExtra::tableGrob(res_tbl, rows = NULL,
                                       theme = make_table_theme(pal)) %>%
        add_horizontal_borders()
      
      p <- ggplot(dfm_long, aes(x = Fit, y = Value, fill = Fit)) +
        geom_boxplot(outlier.shape = 16, outlier.size = 1) +
        theme_bw() +
        scale_fill_manual(values = get_palette(pal, length(unique(dfm_long$Fit)))) +
        coord_cartesian(ylim = c(y0, 1)) +
        labs(y = "values") +
        theme(legend.position = "none") +
        annotation_custom(tbl_grob,
                          xmin = 0, xmax = 3,
                          ymin = ymin, ymax = ymax)
      
      list(table = res_tbl, plot = p)
    }
    
    #------------------------------------------------------------
    # 7. Panel C: RMSEA / SRMR / CRMR
    #------------------------------------------------------------
    plot_and_table_abs <- function(df_repli, ymin_ann, ymax_ann, pal) {
      dfm <- map_dfr(df_repli$fit_measures1, as_tibble)
      
      res_tbl <- dfm %>%
        select(RMSEA, SRMR, CRMR) %>%
        pivot_longer(everything(), names_to = "Fit", values_to = "Value") %>%
        mutate(Value = round(Value, 3)) %>%
        group_by(Fit) %>%
        summarise(
          mean = round(mean(Value, na.rm = TRUE), 2),
          sd   = round(sd(Value,   na.rm = TRUE), 2),
          min  = round(min(Value,  na.rm = TRUE), 2),
          max  = round(max(Value,  na.rm = TRUE), 2)
        ) %>%
        ungroup()
      
      ymin <- if (is.null(ymin_ann)) 0 else ymin_ann
      ymax <- if (is.null(ymax_ann)) max(res_tbl$max) else ymax_ann
      
      dfm_long <- dfm %>%
        select(RMSEA, SRMR, CRMR) %>%
        pivot_longer(everything(), names_to = "Fit", values_to = "Value") %>%
        mutate(Value = round(Value, 3))
      
      tbl_grob <- gridExtra::tableGrob(res_tbl, rows = NULL,
                                       theme = make_table_theme(pal)) %>%
        add_horizontal_borders()
      
      p <- ggplot(dfm_long, aes(x = Fit, y = Value, fill = Fit)) +
        geom_boxplot(outlier.shape = 16, outlier.size = 1) +
        theme_bw() +
        scale_fill_manual(values = get_palette(pal, length(unique(dfm_long$Fit)))) +
        coord_cartesian(ylim = c(0, max(res_tbl$max))) +
        labs(y = "values") +
        theme(legend.position = "none") +
        annotation_custom(tbl_grob,
                          xmin = 0, xmax = 4,
                          ymin = ymin, ymax = ymax)
      
      list(table = res_tbl, plot = p)
    }
    
    #------------------------------------------------------------
    # 8. Ensamblar y guardar usando grid.arrange (NO patchwork)
    #------------------------------------------------------------
    o <- plot_and_table_omega(df, omega_ymin_annot, omega_ymax_annot, palette)
    c <- plot_and_table_comp(df, comp_ymin_annot, comp_ymax_annot, palette)
    a <- plot_and_table_abs(df, abs_ymin_annot, abs_ymax_annot, palette)
    
    combined <- gridExtra::grid.arrange(
      o$plot, c$plot, a$plot,
      ncol = 3,
      top = grid::textGrob("Bootstrap CFA Results",
                           gp = grid::gpar(fontsize = 16, fontface = "bold"))
    )
    
    if (save) {
      ggsave(
        filename = path,
        plot     = combined,
        height   = 16,
        width    = 22,
        dpi      = dpi,
        units    = "cm",
        ...
      )
    }
    
    combined
  })
}

# ——————————————————————————————————————————————————————————————
# 5) Generar archivo de referencias en BibTeX
si <- sessioninfo::session_info()
attached_pkgs <- si$packages$package[si$packages$attached]
bibtex::write.bib(attached_pkgs, file = "references.bib")
bibs <- bibtex::read.bib("references.bib")

convert_bib_to_html <- function(bibs) {
  bib_char <- sapply(bibs, function(x) paste(format(x), collapse = " "))
  bib_html <- gsub("_(.*?)_", "<em>\\1</em>", bib_char)
  bib_html <- gsub("\\\\texttt\\{(.*?)\\}", "<code>\\1</code>", bib_html)
  bib_html <- paste0("<p>", bib_html, "</p>")
  paste(bib_html, collapse = "\n")
}

# ——————————————————————————————————————————————————————————————
# 6) Definir la interfaz de usuario (UI)
ui <- dashboardPage(
  dashboardHeader(title = "CFA Shiny"),
  dashboardSidebar(
    width = 300,
    collapsed = FALSE,
    fileInput("datafile", "Load Excel File", accept = c(".xlsx")),
    textAreaInput(
      "modelInput", "CFA Model Specification",
      value = "",
      placeholder = "Enter your CFA model specification",
      rows = 5
    ),
    textInput("invertItems", "Items to Invert (comma-separated)", value = ""),
    numericInput("numRespuestas", "Number of Responses", value = 4),
    checkboxInput("comienzaConCero", "Starts with Zero", value = FALSE),
    actionButton("run", "Run Analysis", class = "run-analysis"),
    hr(),
    sidebarMenu(
      menuItem("Model Summary",       tabName = "summary",    icon = icon("clipboard")),
      menuItem("Fit Measures",        tabName = "fitMeasures",icon = icon("chart-line")),
      menuItem("Model Plot",          tabName = "modelPlot",  icon = icon("project-diagram")),
      menuItem("Modification Indices",tabName = "modIndices", icon = icon("wrench")),
      menuItem("Bootstrap CFA",       tabName = "bootstrap",  icon = icon("seedling")),
      menuItem("Cómo citar",          tabName = "citation",   icon = icon("book"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* =======================
           COLORES DE LA CABECERA
           ======================= */
        .skin-blue .main-header .logo {
          background-color: #446455;
        }
        .skin-blue .main-header .logo:hover {
          background-color: #446455 !important;
        }
        .skin-blue .main-header .navbar {
          background-color: #446455;
        }
        .skin-blue .main-header .navbar:hover {
          background-color: #446455 !important;
        }
        /* ==========================
           ICONO DE HAMBURGUESA
           ========================== */
        .skin-blue .main-header .navbar .sidebar-toggle {
          background-color: #446455 !important;
          border: none !important;
        }
        .skin-blue .main-header .navbar .sidebar-toggle:hover {
          background-color: #446455 !important;
        }
        .skin-blue .main-header .navbar .sidebar-toggle .icon-bar {
          background-color: #FFFFFF !important;
        }
        .skin-blue .main-header .navbar .sidebar-toggle:hover .icon-bar {
          background-color: #FFFFFF !important;
        }
        /* ==========================
           COLORES DEL SIDEBAR
           ========================== */
        .skin-blue .main-sidebar {
          background-color: #5b5b5b;
        }
        /* ===========================================
           BOTÓN 'Run Analysis' EN COLOR BLANCO
           =========================================== */
        .run-analysis {
          background-color: #FFFFFF !important;
          border-color:    #FFFFFF !important;
          color:           #000000 !important;
        }
        .run-analysis:hover,
        .run-analysis:focus,
        .run-analysis:active {
          background-color: #FFFFFF !important;
          border-color:    #FFFFFF !important;
          color:           #000000 !important;
        }
      "))
    ),
    
    tabItems(
      # Pestaña: Model Summary
      tabItem(
        tabName = "summary",
        h2("Model Summary"),
        uiOutput("modelWarning"),
        verbatimTextOutput("summaryOutput")
      ),
      
      # Pestaña: Fit Measures
      tabItem(
        tabName = "fitMeasures",
        h2("Fit Measures"),
        tableOutput("fitMeasures")
      ),
      
      # Pestaña: Model Plot
      tabItem(
        tabName = "modelPlot",
        h2("Model Plot"),
        fluidRow(
          column(4,
                 numericInput("rotation",     "Rotation",                value = 2,   step = 0.1),
                 numericInput("curve",        "Curve",                   value = 2.5, step = 0.1),
                 numericInput("sizeMan",      "Manifest Variable Size",  value = 5,   step = 0.1),
                 numericInput("sizeMan2",     "Manifest Variable Size 2",value = 3,   step = 0.1),
                 numericInput("sizeLat",      "Latent Variable Size",    value = 6,   step = 0.1),
                 numericInput("esize",        "Label Size",              value = 2,   step = 0.1),
                 numericInput("asize",        "Arrow Size",              value = 3,   step = 0.1),
                 numericInput("labelCex",     "Text Size",               value = 1,   step = 0.1),
                 numericInput("edgeLabelCex", "Edge Label Size",         value = 0.8, step = 0.1),
                 numericInput("edgeWidth",    "Edge Width",              value = 0.5, step = 0.1)
          ),
          column(8,
                 downloadButton("downloadPlot","Download High-Quality Plot", class = "btn-warning"),
                 br(), br(),
                 plotOutput("modelPlot")
          )
        )
      ),
      
      # Pestaña: Modification Indices
      tabItem(
        tabName = "modIndices",
        h2("Modification Indices"),
        tableOutput("modIndices")
      ),
      
      # Pestaña: Bootstrap CFA
      tabItem(
        tabName = "bootstrap",
        h2("Bootstrap CFA Analysis"),
        fluidRow(
          column(4,
                 textInput("itemPrefix",       "Item Prefix",                  value = ""),
                 numericInput("bootstrapSeed", "Seed",                         value = 2023),
                 numericInput("nReplications", "Number of Replications",       value = 1000),
                 numericInput("bootstrapDpi",  "DPI",                          value = 600),
                 textInput("bootstrapPalette","Palette",                       value = "grey"),
                 numericInput("omega_ymin_annot","Omega Y-min Annotation",    value = 0.67),
                 numericInput("omega_ymax_annot","Omega Y-max Annotation",    value = 0.70),
                 numericInput("comp_ymin_annot", "Composite Y-min Annotation", value = 0.90),
                 numericInput("comp_ymax_annot", "Composite Y-max Annotation", value = 0.95),
                 numericInput("abs_ymin_annot",  "Absolute Y-min Annotation",  value = 0.08),
                 numericInput("abs_ymax_annot",  "Absolute Y-max Annotation",  value = 0.10),
                 br(),
                 actionButton("runBootstrap", "Run Bootstrap", class = "btn-primary")
          ),
          column(8,
                 downloadButton("downloadBootstrapPlot", "Guardar Gráfico Bootstrap", class = "btn-warning"),
                 br(), br(),
                 plotOutput("bootstrapPlot")
          )
        )
      ),
      
      # Pestaña: Cómo citar
      tabItem(
        tabName = "citation",
        h2("Cómo citar este Shiny"),
        tags$blockquote(
          "Ventura-León, J. (2025). ",
          em("CFA Shiny"),
          " [Aplicación Shiny]. https://github.com/jventural/CFA_Shiny_app"
        ),
        h3("Referencias utilizadas para construir la aplicación:"),
        htmlOutput("bibReferences")
      )
    )
  )
)

# ——————————————————————————————————————————————————————————————
# 7) Definir servidor (server)
server <- function(input, output, session) {
  
  # Reactiva para guardar avisos del modelo CFA
  modelWarnings <- reactiveVal(character(0))
  
  # Leer, limpiar datos y aplicar inversión de ítems si corresponde
  data_processed <- reactive({
    req(input$datafile)
    raw_data <- read_excel(input$datafile$datapath) %>% na.omit()
    
    items_to_invert <- if (nzchar(input$invertItems)) {
      trimws(unlist(strsplit(input$invertItems, split = ",")))
    } else {
      character(0)
    }
    
    if (length(items_to_invert) > 0) {
      invertir_items(
        raw_data,
        items = items_to_invert,
        num_respuestas = input$numRespuestas,
        comienza_con_cero = input$comienzaConCero
      )
    } else {
      raw_data
    }
  })
  
  # Ajustar modelo CFA capturando warnings
  fit_initial <- eventReactive(input$run, {
    req(input$modelInput)
    warn_msgs <- character(0)
    fit <- withCallingHandlers(
      cfa(
        input$modelInput,
        data = data_processed(),
        estimator = "WLSMV",
        mimic = "Mplus",
        ordered = TRUE
      ),
      warning = function(w) {
        warn_msgs <<- c(warn_msgs, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    modelWarnings(warn_msgs)
    fit
  })
  
  # Renderizar resumen del modelo
  output$summaryOutput <- renderPrint({
    req(fit_initial())
    summary(
      fit_initial(),
      rsquare       = TRUE,
      standardized  = TRUE,
      fit.measures  = TRUE
    )
  })
  
  # Renderizar avisos del modelo
  output$modelWarning <- renderUI({
    msgs <- modelWarnings()
    if (length(msgs) > 0) {
      HTML(paste("<strong>Avisos del modelo:</strong><br>", paste(msgs, collapse = "<br>")))
    }
  })
  
  # Fit Measures
  output$fitMeasures <- renderTable({
    req(fit_initial())
    fm <- fitMeasures(
      fit_initial(),
      c("chisq", "df", "pvalue",
        "cfi.scaled", "tli.scaled",
        "rmsea.scaled", "srmr")
    )
    df_fm <- data.frame(index = names(fm), value = fm)
    row.names(df_fm) <- NULL
    df_fm
  }, rownames = FALSE)
  
  # Model Plot
  output$modelPlot <- renderPlot({
    req(fit_initial())
    semPaths(
      fit_initial(),
      whatLabels      = "std",
      rotation        = input$rotation,
      edge.color      = "grey32",
      curve           = input$curve,
      residuals       = FALSE,
      sizeMan         = input$sizeMan,
      sizeLat         = input$sizeLat,
      esize           = input$esize,
      asize           = input$asize,
      intercepts      = FALSE,
      thresholds      = FALSE,
      label.cex       = input$labelCex,
      edge.label.cex  = input$edgeLabelCex,
      sizeMan2        = input$sizeMan2,
      edge.width      = input$edgeWidth
    )
  })
  
  # Modification Indices
  output$modIndices <- renderTable({
    req(fit_initial())
    modificationindices(fit_initial(), sort. = TRUE, power = TRUE)
  }, rownames = TRUE)
  
  # Descargar Model Plot
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0("ModelPlot_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(fit_initial())
      png(file, width = 8, height = 6, units = "in", res = 600)
      semPaths(
        fit_initial(),
        whatLabels      = "std",
        rotation        = input$rotation,
        edge.color      = "grey32",
        curve           = input$curve,
        residuals       = FALSE,
        sizeMan         = input$sizeMan,
        sizeLat         = input$sizeLat,
        esize           = input$esize,
        asize           = input$asize,
        intercepts      = FALSE,
        thresholds      = FALSE,
        label.cex       = input$labelCex,
        edge.label.cex  = input$edgeLabelCex,
        sizeMan2        = input$sizeMan2,
        edge.width      = input$edgeWidth
      )
      dev.off()
    }
  )
  
  # ReactiveValues para resultados de Bootstrap
  bootResults <- reactiveValues(plot = NULL, results = NULL)
  
  # Ejecutar Bootstrap CFA
  observeEvent(input$runBootstrap, {
    req(input$datafile, input$modelInput)
    withProgress(message = "Ejecutando Bootstrap CFA...", value = 0, {
      incProgress(0.2, detail = "Preparando datos")
      results <- boot_cfa(
        new_df         = data_processed(),
        model_string   = input$modelInput,
        item_prefix    = input$itemPrefix,
        seed           = input$bootstrapSeed,
        n_replications = input$nReplications
      )
      
      incProgress(0.5, detail = "Generando gráfico")
      # Llamar a la versión actualizada de boot_cfa_plot
      bootPlot <- boot_cfa_plot(
        results,
        save              = FALSE,
        dpi               = input$bootstrapDpi,
        palette           = input$bootstrapPalette,
        omega_ymin_annot  = input$omega_ymin_annot,
        omega_ymax_annot  = input$omega_ymax_annot,
        comp_ymin_annot   = input$comp_ymin_annot,
        comp_ymax_annot   = input$comp_ymax_annot,
        abs_ymin_annot    = input$abs_ymin_annot,
        abs_ymax_annot    = input$abs_ymax_annot
      )
      
      incProgress(0.3, detail = "Finalizando")
      bootResults$plot    <- bootPlot
      bootResults$results <- results
    })
  })
  
  # Renderizar gráfico de Bootstrap
  output$bootstrapPlot <- renderPlot({
    req(bootResults$plot)
    bootResults$plot
  })
  
  # Descargar gráfico de Bootstrap
  output$downloadBootstrapPlot <- downloadHandler(
    filename = function() {
      paste0("BootstrapPlot_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(bootResults$plot)
      ggsave(
        filename = file,
        plot     = bootResults$plot,
        device   = "png",
        height   = 16,
        width    = 22,
        units    = "cm"
      )
    }
  )
  
  # Referencias en HTML para “Cómo citar”
  output$bibReferences <- renderUI({
    HTML(convert_bib_to_html(bibs))
  })
}

# ——————————————————————————————————————————————————————————————
# 8) Ejecutar la aplicación Shiny
shinyApp(ui, server)
