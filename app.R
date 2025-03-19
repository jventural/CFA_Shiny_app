# app.R
library(shiny)
library(shinydashboard)  # Para usar dashboardPage, dashboardHeader, dashboardSidebar, dashboardBody
library(lavaan)          # Structural equation modeling
library(semPlot)         # SEM visualization
library(semTools)        # SEM analysis tools and reliability
library(dplyr)           # Data manipulation
library(PsyMetricTools)  # Psychometric functions (includes invertir_items)
library(readxl)          # Excel file reading

ui <- dashboardPage(
  dashboardHeader(title = "CFA Shiny"),
  
  dashboardSidebar(
    width = 300,   # Por ejemplo 300 px de ancho
    collapsed = FALSE,
    
    # Controles del sidebar
    fileInput("datafile", "Load Excel File", accept = c(".xlsx")),
    textAreaInput("modelInput", "CFA Model Specification",
                  value = "",
                  placeholder = "Enter your CFA model specification",
                  rows = 5),
    # Usamos una clase personalizada "run-analysis" para el botón
    actionButton("run", "Run Analysis", class = "run-analysis"),
    hr(),
    
    # Menú de navegación
    sidebarMenu(
      menuItem("Model Summary", tabName = "summary", icon = icon("clipboard")),
      menuItem("Fit Measures", tabName = "fitMeasures", icon = icon("chart-line")),
      menuItem("Model Plot", tabName = "modelPlot", icon = icon("project-diagram")),
      menuItem("Modification Indices", tabName = "modIndices", icon = icon("wrench")),
      menuItem("Omega Reliability", tabName = "reliability", icon = icon("balance-scale"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* =======================
           COLORES DE LA CABECERA
           ======================= */
        .skin-blue .main-header .logo {
          background-color: #446455; /* Color para el header (logo) */
        }
        .skin-blue .main-header .logo:hover {
          background-color: #446455 !important; /* Mantén el mismo color al hover */
        }
        
        .skin-blue .main-header .navbar {
          background-color: #446455; /* Color para la navbar */
        }
        .skin-blue .main-header .navbar:hover {
          background-color: #446455 !important; /* Mantén el mismo color al hover */
        }

        /* ==========================
           ICONO DE HAMBURGUESA
           ========================== */
        /* Fondo del botón */
        .skin-blue .main-header .navbar .sidebar-toggle {
          background-color: #446455 !important; /* Mismo color que la navbar */
          border: none !important;
        }
        .skin-blue .main-header .navbar .sidebar-toggle:hover {
          background-color: #446455 !important; /* Sin cambio al hover */
        }
        /* Color de las 3 barras del ícono */
        .skin-blue .main-header .navbar .sidebar-toggle .icon-bar {
          background-color: #FFFFFF !important; /* Barras en blanco */
        }
        .skin-blue .main-header .navbar .sidebar-toggle:hover .icon-bar {
          background-color: #FFFFFF !important; /* Sin cambio al hover */
        }

        /* ==========================
           COLORES DEL SIDEBAR
           ========================== */
        .skin-blue .main-sidebar {
          background-color: #5b5b5b; /* Cambia aquí para el sidebar */
        }
        
        /* ===========================================
           BOTÓN 'Run Analysis' EN COLOR BLANCO
           =========================================== */
        .run-analysis {
          background-color: #FFFFFF !important;
          border-color: #FFFFFF !important;
          color: #000000 !important; /* Texto en negro */
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
      tabItem(tabName = "summary",
              h2("Model Summary"),
              verbatimTextOutput("summaryOutput")
      ),
      
      tabItem(tabName = "fitMeasures",
              h2("Fit Measures"),
              tableOutput("fitMeasures")
      ),
      
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
      
      tabItem(tabName = "modIndices",
              h2("Modification Indices"),
              tableOutput("modIndices")
      ),
      
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
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Leer y limpiar datos originales
  data_full <- reactive({
    req(input$datafile)
    read_excel(input$datafile$datapath) %>% na.omit()
  })
  
  # Ajustar modelo CFA usando los datos completos
  fit_initial <- eventReactive(input$run, {
    req(input$modelInput)
    cfa(input$modelInput, data = data_full(), estimator = "WLSMV", mimic = "Mplus", ordered = TRUE)
  })
  
  # Model Summary
  output$summaryOutput <- renderPrint({
    req(fit_initial())
    summary(fit_initial(), rsquare = TRUE, standardized = TRUE, fit.measures = TRUE)
  })
  
  # Fit Measures
  output$fitMeasures <- renderTable({
    req(fit_initial())
    # fitMeasures() devuelve un vector con nombres (chisq, df, etc.)
    fm <- fitMeasures(fit_initial(), c("chisq", "df", "pvalue", 
                                       "cfi.scaled", "tli.scaled", 
                                       "rmsea.scaled", "srmr"))
    # Convertimos ese vector en un data frame con columnas "index" y "value"
    df_fm <- data.frame(index = names(fm), value = fm)
    df_fm
  }, rownames = F)
  
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
  
  # Omega Reliability (mostrado en formato de tabla)
  output$reliabilityOutput <- renderTable({
    req(input$datafile)
    raw_data <- read_excel(input$datafile$datapath) %>% na.omit()
    items_to_invert <- strsplit(input$invertItems, split = ",")[[1]]
    items_to_invert <- trimws(items_to_invert)
    items_to_invert <- items_to_invert[items_to_invert != ""]
    df_survey_modified <- invertir_items(raw_data, items = items_to_invert, 
                                         num_respuestas = input$numRespuestas, 
                                         comienza_con_cero = input$comienzaConCero)
    fit_modified <- cfa(input$modelInput, data = df_survey_modified, 
                        estimator = "WLSMV", mimic = "Mplus", ordered = TRUE)
    reliability_res <- compRelSEM(fit_modified, tau.eq = FALSE, ord.scale = TRUE)
    as.data.frame(reliability_res)
  }, rownames = TRUE)
  
  # Download handler para el Model Plot en alta calidad (600 dpi)
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
}

shinyApp(ui, server)
