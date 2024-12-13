library(shiny)
library(readxl)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(DT)
library(dplyr)
library(highcharter)
library(lattice)
library(lme4)
library(lmerTest)
library(emmeans)
library(agricolae)

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Table", 
             fileInput('file1', 'Select an Excel file'),
             dataTableOutput("miTable")),
    tabPanel("PCA",
             fluidRow(
               dataTableOutput("Correlation Matrix")
             ),
             fluidRow(
               dataTableOutput("TableEigenvalues")
             ),
             fluidRow(
               dataTableOutput("TableEigenvectors")
             )),
    tabPanel("Graphs", 
             fluidRow(
               column(12, plotOutput("Variance"))
             ),
             fluidRow(
               column(6, plotOutput("Biplot")),
               column(6, plotOutput("EPM"))
             )
    ),
    tabPanel("ANOVA",
             selectInput("SelectedColumn",
                         "Select a Column:",
                         choices = NULL),
             verbatimTextOutput("ANOVA_result")
    ),
    tabPanel("Mean Comparation",
             selectInput("SelectedColumnComparation",
                         "Select an atribute:",
                         choices = NULL),
             verbatimTextOutput("lsdResults")
    ),
    tabPanel("Reproducibility Graphs",
             selectInput("SelectedColumnReproducibility",
                         "Select an atribute:",
                         choices = NULL),
             plotOutput("Reproducibility")
    ),
    tabPanel("Interaction Graphs",
             selectInput("SelectedColumnInteraction",
                         "Select an atribute:",
                         choices = NULL),
             plotOutput("Interaction")),
    
    tabPanel("Radial Graph",
             highchartOutput("radialPlot"))
  )
)


server <- function(input, output, session){
  datos <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) {
      # Retorna NULL si no se ha seleccionado ningún archivo
      return(NULL)
    }
    QDA <- read_excel(inFile$datapath)
    QDA$Repetición <- as.character(QDA$Repetición)
    QDA$Panelista <- as.character(QDA$Panelista)
    return(QDA)
  })
  
  output$miTable <- renderDataTable({
    datos() 
  })
  
  Correlacion <- reactive({
    QDA <- datos()
    if (is.null(QDA)) {
      return()
    }
    num_data <- QDA[, sapply(QDA, is.numeric)]
    cor_matrix <- cor(num_data)
    return(cor_matrix)
  })
  
  output$'Correlation Matrix' <- renderDataTable({
    Correlacion()
  })
  
  PCAresults <- reactive({
    QDA <- datos()
    if (is.null(QDA)) {
      return(NULL)
    }
    res.pca <- PCA(QDA[, sapply(QDA, is.numeric)], graph = FALSE)
    return(res.pca)
  })
  
  output$TableEigenvalues <- renderDataTable({
    res.pca <- PCAresults()
    if (is.null(res.pca)) {
      return()
    }
    eigenvalues <- get_eigenvalue(res.pca)
    return(eigenvalues)
  })
  
  output$TableEigenvectors <- renderDataTable({
    res.pca <- PCAresults()
    if (is.null(res.pca)) {
      return()
    }
    eigenvectors <- res.pca$var$coord
    return(eigenvectors)
  })
  
  res.pca <- reactive({
    QDA <- datos()
    if (is.null(QDA)) {
      return(NULL)
    }
    
    # Realizar el PCA, excluyendo las primeras 3 Columnas
    PCA <- PCA(QDA[, -c(1:3)], graph = TRUE)
  })
  output$Variance <- renderPlot({
    pca_result <- res.pca()
    if (is.null(pca_result)) {
      return()
    }
    fviz_eig(pca_result,title = "EigbarPlot")
  })
  output$Biplot <- renderPlot({
    QDA <- datos()
    pca_result <- res.pca()
    if (is.null(pca_result) || is.null(QDA)) {
      return()
    }
    QDA$Panelista <- as.factor(QDA$Panelista)
    fviz_pca_biplot(pca_result, label = "var", habillage = QDA$Panelista, addEllipses = TRUE)
  })
  output$EPM <- renderPlot({
    QDA <- datos()
    pca_result <- res.pca()
    if (is.null(pca_result) || is.null(QDA)) {
      return()
    }
    QDA$Tratamiento <- as.factor(QDA$Tratamiento)
    fviz_pca_biplot(pca_result, label = "var", habillage = QDA$Tratamiento, addEllipses = FALSE, title = "External Preference Map")
  })


  observe({
    # Verifica si 'datos()' no es NULL
    if (!is.null(datos())) {
      # Actualiza las opciones del selectInput con los nombres de las Columnas de 'datos()'
      updateSelectInput(session, "SelectedColumn",
                        choices = colnames(datos()))
    }
  })
  datos_reactivos <- reactive({
    QDA <- datos()
    if (is.null(QDA)) return()
    QDA$Panelista <- as.factor(QDA$Panelista)
    QDA$Tratamiento <- as.factor(QDA$Tratamiento)
    return(QDA)
  })
  observe({
    QDA <- datos_reactivos()
    if (is.null(QDA) || input$SelectedColumn == "") {
      return()  # Sal si los datos son nulos o no se ha seleccionado ninguna Columna
    }
    
    # Asegúrate de que las Columnas necesarias existan en QDA
    if (!all(c("Repetición", "Tratamiento", "Panelista", input$SelectedColumn) %in% colnames(QDA))) {
      return()  # Sal si alguna Columna necesaria no es válida
    }
    
    # Intenta el modelo con sp.plot()
    try({
      modelo <- sp.plot(QDA$Repetición, QDA$Tratamiento, QDA$Panelista, QDA[[input$SelectedColumn]])
      print(summary(modelo))  # Muestra el resumen en la consola
    }, silent = TRUE)  # Usa try() para manejar errores potenciales en la construcción del modelo
  })
  
  output$ANOVA_result <- renderPrint({
    QDA <- datos_reactivos()
    if (is.null(QDA) || input$SelectedColumn == "") {
      return("No hay datos cargados o no se ha seleccionado una Columna.")
    }
    
    if (!all(c("Repetición", "Tratamiento", "Panelista", input$SelectedColumn) %in% colnames(QDA))) {
      return("La Columna seleccionada no es válida.")
    }
    
    tryCatch({
      modelo <- sp.plot(QDA$Repetición, QDA$Tratamiento, QDA$Panelista, QDA[[input$SelectedColumn]])
      summary(modelo)
    }, error = function(e) {
      paste("Error al ajustar el modelo: ", e$message)
    })
  })


  observe({
    if (!is.null(datos())) {
      updateSelectInput(session, "SelectedColumnComparation",
                        choices = colnames(datos()))
    }
  })
  
  # Renderiza los resultados del test LSD
  output$lsdResults <- renderPrint({
    QDA <- datos_reactivos()
    if (is.null(QDA) || input$SelectedColumnComparation == "") {
      return("No hay datos cargados o no se ha seleccionado una Columna.")
    }
    
    if (!input$SelectedColumnComparation %in% colnames(QDA)) {
      return("La Columna seleccionada no es válida.")
    }
    
    variable_dependiente <- input$SelectedColumnComparation
    
    # Ajusta el modelo de parcela dividida usando sp.plot
    tryCatch({
      # Asegúrate de tener las Columnas correctas para 'Tratamiento', 'Repetición' y 'Panelista'
      if (!all(c("Repetición", "Tratamiento", "Panelista") %in% colnames(QDA))) {
        stop("Las Columnas necesarias no están presentes en el data frame.")
      }
      
      # Aplica sp.plot para obtener el modelo
      modelo_sp_plot <- sp.plot(QDA$Repetición, QDA$Tratamiento, QDA$Panelista, QDA[[variable_dependiente]])
      
      # Extrae los componentes necesarios para LSD.test
      DFerror <- modelo_sp_plot$gl.a
      MSerror <- modelo_sp_plot$Ea
      
      # Aplica la prueba de LSD
      lsd_result <- LSD.test(QDA[[variable_dependiente]], QDA$Tratamiento, DFerror, MSerror, group = TRUE)
      
      # Muestra los resultados de la prueba de LSD
      print(lsd_result)
      
    }, error = function(e) {
      print(paste("Error al ajustar el modelo o al realizar la comparación de medias:", e$message))
    })
  })
  observe({
    req(datos_reactivos())  # Asegúrate de que los datos están cargados
    updateSelectInput(session, "SelectedColumnReproducibility", 
                      choices = colnames(datos_reactivos()))
  })

      
  
  output$Reproducibility <- renderPlot({
    req(datos_reactivos())  # Asegura que los datos están cargados y que el input está seleccionado
    if (input$SelectedColumnReproducibility == "") {
      return(NULL)
    }
    
    if (!input$SelectedColumnReproducibility %in% colnames(datos_reactivos())) {
      return(NULL)
    }
    
    
    QDA <- datos_reactivos()
    QDA$Tratamiento <- as.factor(QDA$Tratamiento)
    QDA$Panelista <- as.factor(QDA$Panelista)
    QDA$Repetición <- as.numeric(QDA$Repetición)
    QDA <- QDA[order(QDA$Panelista, QDA$Tratamiento, QDA$Repetición),]
    
    # Gráfico de reproducibilidad
    xyplot(QDA[[input$SelectedColumnReproducibility]] ~ Tratamiento | Panelista, groups = Repetición, data = QDA, aspect = "xy", type = "o", auto.key = TRUE)
  })
  
  
  observe({
    req(datos_reactivos())  # Asegúrate de que los datos están cargados
    updateSelectInput(session, "SelectedColumnInteraction", 
                      choices = colnames(datos_reactivos()))
  })
  
  
  
  output$Interaction <- renderPlot({
    req(datos_reactivos())
    QDA <- datos_reactivos()
    if (!input$SelectedColumnInteraction %in% colnames(QDA)) {
      return(NULL)
    }
    
    # Asegura que la variable y los factores estén correctamente definidos
    QDA$Tratamiento <- as.factor(QDA$Tratamiento)
    QDA$Panelista <- as.factor(QDA$Panelista)
    QDA$Repetición <- as.numeric(QDA$Repetición)
    QDA <- QDA[order(QDA$Panelista, QDA$Tratamiento, QDA$Repetición),]
    
    # Calcula la media por panelista y tratamiento usando la variable seleccionada
    media_panelista <- QDA %>%
      group_by(Panelista, Tratamiento) %>%
      summarise(MediaPanelista = mean(.data[[input$SelectedColumnInteraction]], na.rm = TRUE), .groups = 'drop')
    media_panelista$Calificación <- 'Juez'
    
    # Calcula la media global para cada tratamiento y replica las medias por panelista
    medias <- media_panelista %>%
      group_by(Tratamiento) %>%
      summarise(MediaPanelista = mean(MediaPanelista)) %>%
      slice(rep(1:n(), each = n_distinct(media_panelista$Panelista)))
    medias$Calificación <- 'Media'
    medias$Panelista <- rep(unique(media_panelista$Panelista), times = nrow(medias) / length(unique(media_panelista$Panelista)))
    
    # Crea el dataframe final
    df_final <- bind_rows(media_panelista, medias) %>%
      arrange(Panelista, Tratamiento, desc(Calificación))
    
    # Genera el gráfico
    with(df_final, xyplot(MediaPanelista ~ Tratamiento | Panelista, groups = Calificación, aspect = "xy", type = "o", auto.key = TRUE))
  })
  
  
  # Crear el gráfico radial
  output$radialPlot <- renderHighchart({
    req(datos())  # Asegurarse de que los datos están cargados
    QDA <- datos()
    
    QDA_means <- QDA %>%
      select(-Panelista, -Repetición) %>%
      group_by(Tratamiento) %>%
      summarise_all(mean, na.rm = TRUE) %>%
      ungroup()
    
    QDA_means <- as.data.frame(QDA_means)
    
    QDA_transposed <- as.data.frame(t(QDA_means[-1]))
    colnames(QDA_transposed) <- QDA_means$Tratamiento
    
    QDA_for_chart <- data.frame(Attribute = rownames(QDA_transposed), QDA_transposed)
    rownames(QDA_for_chart) <- NULL
    
    hc <- hchart(QDA_for_chart, type = "line", hcaes(x = Attribute)) %>%
      hc_chart(polar = TRUE) %>%
      hc_title(text = "Radial plot") %>%
      hc_xAxis(categories = QDA_for_chart$Attribute, tickmarkPlacement = 'on', lineWidth = 0) %>%
      hc_yAxis(gridLineInterpolation = 'polygon', lineWidth = 0, min = 0)
    
    # Añadir dinámicamente las series de tratamientos
    for(treatment in colnames(QDA_transposed)) {
      hc <- hc %>% hc_add_series(data = QDA_transposed[[treatment]], name = treatment)
    }
    
    hc %>% hc_tooltip(shared = TRUE, pointFormat = '<span style="color:{series.color}">{series.name}: <b>{point.y:.2f}</b><br/>')
    
    return(hc)
  })
}

shinyApp(ui = ui, server = server)


