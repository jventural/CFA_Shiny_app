# app.R
# Cargar paquetes requeridos para el shiny
library(shiny)
library(shinydashboard)
library(lavaan)          # Structural equation modeling
library(semPlot)         # SEM visualization
library(semTools)        # SEM analysis tools and reliability
library(dplyr)           # Data manipulation
library(PsyMetricTools)  # Funciones psicométricas (incluye invertir_items, boot_cfa, boot_cfa_plot)
library(readxl)          # Lectura de archivos Excel
library(ggplot2)         # For ggsave when downloading plots
library(sessioninfo)     # Para extraer información de la sesión
library(bibtex)          # Para gestionar referencias BibTeX
# Generar el archivo de referencias con los paquetes adjuntos
si <- sessioninfo::session_info()
attached_pkgs <- si$packages$package[ si$packages$attached ]
bibtex::write.bib(attached_pkgs, file = "references.bib")
bibs <- bibtex::read.bib("references.bib")

# Función para convertir las referencias a HTML
convert_bib_to_html <- function(bibs) {
  # Convertir cada entrada a texto y unir las líneas
  bib_char <- sapply(bibs, function(x) paste(format(x), collapse = " "))
  # Reemplazar _texto_ por cursivas (<em>texto</em>)
  bib_html <- gsub("_(.*?)_", "<em>\\1</em>", bib_char)
  # Reemplazar \texttt{texto} por código (<code>texto</code>)
  bib_html <- gsub("\\\\texttt\\{(.*?)\\}", "<code>\\1</code>", bib_html)
  # Envolver cada referencia en un párrafo
  bib_html <- paste0("<p>", bib_html, "</p>")
  # Unir todas las entradas
  paste(bib_html, collapse = "\n")
}

# Definir la interfaz de usuario
ui <- dashboardPage(
  dashboardHeader(title = "CFA Shiny"),
  dashboardSidebar(
    width = 300,
    collapsed = FALSE,
    # Controles del sidebar
    fileInput("datafile", "Load Excel File", accept = c(".xlsx")),
    textAreaInput("modelInput", "CFA Model Specification",
                  value = "",
                  placeholder = "Enter your CFA model specification",
                  rows = 5),
    # Controles para invertir ítems
    textInput("invertItems", "Items to Invert (comma-separated)", value = ""),
    numericInput("numRespuestas", "Number of Responses", value = 4),
    checkboxInput("comienzaConCero", "Starts with Zero", value = FALSE),
    # Botón para correr el análisis CFA
    actionButton("run", "Run Analysis", class = "run-analysis"),
    hr(),
    # Menú de navegación
    sidebarMenu(
      menuItem("Model Summary", tabName = "summary", icon = icon("clipboard")),
      menuItem("Fit Measures", tabName = "fitMeasures", icon = icon("chart-line")),
      menuItem("Model Plot", tabName = "modelPlot", icon = icon("project-diagram")),
      menuItem("Modification Indices", tabName = "modIndices", icon = icon("wrench")),
      menuItem("Bootstrap CFA", tabName = "bootstrap", icon = icon("seedling")),
      menuItem("Cómo citar", tabName = "citation", icon = icon("book"))
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
          border-color: #FFFFFF !important;
          color: #000000 !important;
        }
        .run-analysis:hover,
        .run-analysis:focus,
        .run-analysis:active {
          background-color: #FFFFFF !important;
          border-color: #FFFFFF !important;
          color: #000000 !important;
        }
      "))
    ),
    
    tabItems(
      # Pestaña: Model Summary (aviso primero, resumen después)
      tabItem(tabName = "summary",
              h2("Model Summary"),
              # Mostrar los avisos del modelo (si existen) al inicio
              uiOutput("modelWarning"),
              verbatimTextOutput("summaryOutput")
      ),
      
      # Pestaña: Fit Measures
      tabItem(tabName = "fitMeasures",
              h2("Fit Measures"),
              tableOutput("fitMeasures")
      ),
      
      # Pestaña: Model Plot
      tabItem(tabName = "modelPlot",
              h2("Model Plot"),
              fluidRow(
                column(4,
                       numericInput("rotation", "Rotation", value = 2, step = 0.1),
                       numericInput("curve", "Curve", value = 2.5, step = 0.1),
                       numericInput("sizeMan", "Manifest Variable Size", value = 5, step = 0.1),
                       numericInput("sizeMan2", "Manifest Variable Size 2", value = 3, step = 0.1),
                       numericInput("sizeLat", "Latent Variable Size", value = 6, step = 0.1),
                       numericInput("esize", "Label Size", value = 2, step = 0.1),
                       numericInput("asize", "Arrow Size", value = 3, step = 0.1),
                       numericInput("labelCex", "Text Size", value = 1, step = 0.1),
                       numericInput("edgeLabelCex", "Edge Label Size", value = 0.8, step = 0.1),
                       numericInput("edgeWidth", "Edge Width", value = 0.5, step = 0.1)
                ),
                column(8,
                       downloadButton("downloadPlot", "Download High-Quality Plot", class = "btn-warning"),
                       br(), br(),
                       plotOutput("modelPlot")
                )
              )
      ),
      
      # Pestaña: Modification Indices
      tabItem(tabName = "modIndices",
              h2("Modification Indices"),
              tableOutput("modIndices")
      ),
      
      # Pestaña: Bootstrap CFA
      tabItem(tabName = "bootstrap",
              h2("Bootstrap CFA Analysis"),
              fluidRow(
                column(4,
                       textInput("itemPrefix", "Item Prefix", value = ""),
                       numericInput("bootstrapSeed", "Seed", value = 2023),
                       numericInput("nReplications", "Number of Replications", value = 1000),
                       numericInput("bootstrapDpi", "DPI", value = 600),
                       textInput("bootstrapPalette", "Palette", value = "grey"),
                       numericInput("omega_ymin_annot", "Omega Y-min Annotation", value = 0.67),
                       numericInput("omega_ymax_annot", "Omega Y-max Annotation", value = 0.70),
                       numericInput("comp_ymin_annot", "Composite Y-min Annotation", value = 0.90),
                       numericInput("comp_ymax_annot", "Composite Y-max Annotation", value = 0.95),
                       numericInput("abs_ymin_annot", "Absolute Y-min Annotation", value = 0.08),
                       numericInput("abs_ymax_annot", "Absolute Y-max Annotation", value = 0.10),
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
      tabItem(tabName = "citation",
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

# Definir el servidor
server <- function(input, output, session) {
  
  # Variable reactiva para almacenar avisos del modelo
  modelWarnings <- reactiveVal(character(0))
  
  # Leer, limpiar datos y aplicar inversión de ítems (si se especifica)
  data_processed <- reactive({
    req(input$datafile)
    raw_data <- read_excel(input$datafile$datapath) %>% na.omit()
    # Procesar ítems a invertir
    items_to_invert <- if(nchar(input$invertItems) > 0) {
      trimws(unlist(strsplit(input$invertItems, split = ",")))
    } else {
      character(0)
    }
    # Si se especifican ítems para invertir, se procesan; de lo contrario se usan los datos originales
    if(length(items_to_invert) > 0) {
      invertir_items(raw_data,
                     items = items_to_invert,
                     num_respuestas = input$numRespuestas,
                     comienza_con_cero = input$comienzaConCero)
    } else {
      raw_data
    }
  })
  
  # Ajustar modelo CFA usando los datos procesados, capturando warnings
  fit_initial <- eventReactive(input$run, {
    req(input$modelInput)
    warn_msgs <- character(0)
    fit <- withCallingHandlers(
      cfa(input$modelInput, data = data_processed(), estimator = "WLSMV", mimic = "Mplus", ordered = TRUE),
      warning = function(w) {
        warn_msgs <<- c(warn_msgs, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    modelWarnings(warn_msgs)  # Guardamos los avisos
    fit
  })
  
  # Renderizar resumen del modelo
  output$summaryOutput <- renderPrint({
    req(fit_initial())
    summary(fit_initial(), rsquare = TRUE, standardized = TRUE, fit.measures = TRUE)
  })
  
  # Renderizar los avisos del modelo (si existen) al inicio
  output$modelWarning <- renderUI({
    msgs <- modelWarnings()
    if (length(msgs) > 0) {
      HTML(paste("<strong>Avisos del modelo:</strong><br>", paste(msgs, collapse = "<br>")))
    }
  })
  
  # Fit Measures
  output$fitMeasures <- renderTable({
    req(fit_initial())
    fm <- fitMeasures(fit_initial(), c("chisq", "df", "pvalue", 
                                       "cfi.scaled", "tli.scaled", 
                                       "rmsea.scaled", "srmr"))
    df_fm <- data.frame(index = names(fm), value = fm)
    row.names(df_fm) <- NULL
    df_fm
  }, rownames = FALSE)
  
  # Model Plot
  output$modelPlot <- renderPlot({
    req(fit_initial())
    semPaths(fit_initial(), whatLabels = "std",
             rotation = input$rotation,
             edge.color = "grey32",
             curve = input$curve,
             residuals = FALSE,
             sizeMan = input$sizeMan,
             sizeLat = input$sizeLat,
             esize = input$esize,
             asize = input$asize,
             intercepts = FALSE,
             thresholds = FALSE,
             label.cex = input$labelCex,
             edge.label.cex = input$edgeLabelCex,
             sizeMan2 = input$sizeMan2,
             edge.width = input$edgeWidth)
  })
  
  # Modification Indices
  output$modIndices <- renderTable({
    req(fit_initial())
    modindices(fit_initial(), sort. = TRUE, power = TRUE)
  }, rownames = TRUE)
  
  # Descargar el Model Plot en alta calidad (600 dpi)
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0("ModelPlot_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(fit_initial())
      png(file, width = 8, height = 6, units = "in", res = 600)
      semPaths(fit_initial(), whatLabels = "std",
               rotation = input$rotation,
               edge.color = "grey32",
               curve = input$curve,
               residuals = FALSE,
               sizeMan = input$sizeMan,
               sizeLat = input$sizeLat,
               esize = input$esize,
               asize = input$asize,
               intercepts = FALSE,
               thresholds = FALSE,
               label.cex = input$labelCex,
               edge.label.cex = input$edgeLabelCex,
               sizeMan2 = input$sizeMan2,
               edge.width = input$edgeWidth)
      dev.off()
    }
  )
  
  # ReactiveValues para almacenar resultados del bootstrap
  bootResults <- reactiveValues(plot = NULL, results = NULL)
  
  # Ejecutar el bootstrap CFA cuando se presiona el botón, con barra de progreso
  observeEvent(input$runBootstrap, {
    req(input$datafile, input$modelInput)
    withProgress(message = "Ejecutando Bootstrap CFA...", value = 0, {
      
      incProgress(0.2, detail = "Preparando datos")
      # Ejecutar boot_cfa utilizando los datos procesados
      results <- boot_cfa(
        new_df = data_processed(),
        model_string = input$modelInput,
        item_prefix = input$itemPrefix,
        seed = input$bootstrapSeed,
        n_replications = input$nReplications
      )
      
      incProgress(0.5, detail = "Generando gráfico")
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
      
      bootResults$plot <- bootPlot
      bootResults$results <- results
    })
  })
  
  # Renderizar el gráfico del Bootstrap CFA
  output$bootstrapPlot <- renderPlot({
    req(bootResults$plot)
    bootResults$plot
  })
  
  # Botón para descargar el Gráfico de Bootstrap usando ggsave con dimensiones específicas
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
  
  # Mostrar las referencias en HTML en la pestaña "Cómo citar"
  output$bibReferences <- renderUI({
    HTML(convert_bib_to_html(bibs))
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui, server)
