# app.R

# Cargar paquetes requeridos
library(shiny)
library(shinydashboard)
library(lavaan)          # Structural equation modeling
library(semPlot)         # SEM visualization
library(semTools)        # SEM analysis tools and reliability
library(dplyr)           # Data manipulation
library(PsyMetricTools)  # Psychometric functions (includes invertir_items)
library(readxl)          # Excel file reading
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
    # Botón para correr el análisis
    actionButton("run", "Run Analysis", class = "run-analysis"),
    hr(),
    # Menú de navegación
    sidebarMenu(
      menuItem("Model Summary", tabName = "summary", icon = icon("clipboard")),
      menuItem("Fit Measures", tabName = "fitMeasures", icon = icon("chart-line")),
      menuItem("Model Plot", tabName = "modelPlot", icon = icon("project-diagram")),
      menuItem("Modification Indices", tabName = "modIndices", icon = icon("wrench")),
      menuItem("Omega Reliability", tabName = "reliability", icon = icon("balance-scale")),
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
      
      # Pestaña: Omega Reliability
      tabItem(tabName = "reliability",
              h2("Omega Reliability"),
              fluidRow(
                column(4,
                       textInput("invertItems", "Items to Invert (comma-separated)", value = ""),
                       numericInput("numRespuestas", "Number of Responses", value = 4),
                       checkboxInput("comienzaConCero", "Starts with Zero", value = FALSE)
                ),
                column(8,
                       tableOutput("reliabilityOutput")
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
  
  # Leer y limpiar datos originales
  data_full <- reactive({
    req(input$datafile)
    read_excel(input$datafile$datapath) %>% na.omit()
  })
  
  # Ajustar modelo CFA usando los datos completos, capturando warnings
  fit_initial <- eventReactive(input$run, {
    req(input$modelInput)
    warn_msgs <- character(0)
    fit <- withCallingHandlers(
      cfa(input$modelInput, data = data_full(), estimator = "WLSMV", mimic = "Mplus", ordered = TRUE),
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
    modificationindices(fit_initial(), sort. = TRUE, power = TRUE)
  }, rownames = TRUE)
  
  # Omega Reliability
  output$reliabilityOutput <- renderTable({
    req(input$datafile)
    raw_data <- read_excel(input$datafile$datapath) %>% na.omit()
    
    # Procesar ítems a invertir
    items_to_invert <- strsplit(input$invertItems, split = ",")[[1]]
    items_to_invert <- trimws(items_to_invert)
    items_to_invert <- items_to_invert[items_to_invert != ""]
    
    # Invertir ítems según la configuración del usuario
    df_survey_modified <- invertir_items(raw_data, 
                                         items = items_to_invert, 
                                         num_respuestas = input$numRespuestas, 
                                         comienza_con_cero = input$comienzaConCero)
    
    # Ajustar modelo con datos modificados
    fit_modified <- cfa(input$modelInput, data = df_survey_modified, 
                        estimator = "WLSMV", mimic = "Mplus", ordered = TRUE)
    
    # Calcular fiabilidad compuesta (Omega)
    reliability_res <- compRelSEM(fit_modified, tau.eq = FALSE, ord.scale = TRUE)
    as.data.frame(reliability_res)
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
  
  # Mostrar las referencias en HTML en la pestaña "Cómo citar"
  output$bibReferences <- renderUI({
    HTML(convert_bib_to_html(bibs))
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui, server)
