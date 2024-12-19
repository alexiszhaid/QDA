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
             checkboxGroupInput("ExcludePanelists",
                                "Exclude Panelists:",
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
      # Retorn NULL if there is no file selected
      return(NULL)
    }
    QDA <- read_excel(inFile$datapath)
    QDA$Repetition <- as.character(QDA$Repetition)
    QDA$Panelist <- as.character(QDA$Panelist)
    return(QDA)
  })
  
  datos_reactivos <- reactive({
    QDA <- datos()
    if (is.null(QDA)) return(NULL)
    QDA$Panelist <- as.factor(QDA$Panelist)
    QDA$Treatment <- as.factor(QDA$Treatment)
    if (!is.null(input$ExcludePanelists)) {
      QDA <- QDA[!QDA$Panelist %in% input$ExcludePanelists, ]
    }
    return(QDA)
  })
  
  observe({
    QDA <- datos_reactivos()
    if (!is.null(QDA)) {
      updateSelectInput(session, "SelectedColumn",
                        choices = colnames(QDA))
      updateSelectInput(session, "SelectedColumnInteraction",
                        choices = colnames(QDA))
      updateSelectInput(session, "SelectedColumnComparison",
                        choices = colnames(QDA))
    }
  })
  
  observe({
    QDA <- datos()
    if (!is.null(QDA)) {
      updateCheckboxGroupInput(session, "ExcludePanelists",
                               choices = unique(QDA$Panelist),
                               selected = NULL)
    }
  })
  
  output$miTable <- renderDataTable({
    datos_reactivos() 
  })
  
  Correlation <- reactive({
    QDA <- datos_reactivos()
    if (is.null(QDA)) {
      return()
    }
    num_data <- QDA[, sapply(QDA, is.numeric)]
    cor_matrix <- cor(num_data)
    return(cor_matrix)
  })
  
  output$'Correlation Matrix' <- renderDataTable({
    Correlation()
  })
  
  PCAresults <- reactive({
    QDA <- datos_reactivos()
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
    QDA <- datos_reactivos()
    if (is.null(QDA)) {
      return(NULL)
    }
    
    # Launch PCA, excluding the first 3 Columns
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
    QDA <- datos_reactivos()
    pca_result <- res.pca()
    if (is.null(pca_result) || is.null(QDA)) {
      return()
    }
    QDA$Panelist <- as.factor(QDA$Panelist)
    fviz_pca_biplot(pca_result, label = "var", habillage = QDA$Panelist, addEllipses = TRUE)
  })
  output$EPM <- renderPlot({
    QDA <- datos_reactivos()
    pca_result <- res.pca()
    if (is.null(pca_result) || is.null(QDA)) {
      return()
    }
    QDA$Treatment <- as.factor(QDA$Treatment)
    fviz_pca_biplot(pca_result, label = "var", habillage = QDA$Treatment, addEllipses = FALSE, title = "External Preference Map")
  })


  observe({
    # Verify if 'datos_reactivos()' is NULL
    if (!is.null(datos_reactivos())) {
      # Updates the options in selectInput with the names of the columns in 'datos_reactivos()'
      updateSelectInput(session, "SelectedColumn",
                        choices = colnames(datos_reactivos()))
    }
  })
  datos_reactivos2 <- reactive({
    QDA <- datos_reactivos()
    if (is.null(QDA)) return()
    QDA$Panelist <- as.factor(QDA$Panelist)
    QDA$Treatment <- as.factor(QDA$Treatment)
    return(QDA)
  })
  observe({
    QDA <- datos_reactivos2()
    if (is.null(QDA) || input$SelectedColumn == "") {
      return()
    }
    
    # Ensures that the needed columns exist
    if (!all(c("Repetition", "Treatment", "Panelist", input$SelectedColumn) %in% colnames(QDA))) {
      return()
    }
    
    # Try the modelwith sp.plot()
    try({
      modelo <- sp.plot(QDA$Repetition, QDA$Treatment, QDA$Panelist, QDA[[input$SelectedColumn]])
      print(summary(modelo))
    }, silent = TRUE)
  })
  
  output$ANOVA_result <- renderPrint({
    QDA <- datos_reactivos2()
    if (is.null(QDA) || input$SelectedColumn == "") {
      return("There are no data or a valid column has not been selected.")
    }
    
    if (!all(c("Repetition", "Treatment", "Panelist", input$SelectedColumn) %in% colnames(QDA))) {
      return("The selected column is not valid")
    }
    
    tryCatch({
      modelo <- sp.plot(QDA$Repetition, QDA$Treatment, QDA$Panelist, QDA[[input$SelectedColumn]])
      summary(modelo)
    }, error = function(e) {
      paste("Error fitting the model: ", e$message)
    })
  })


  observe({
    if (!is.null(datos_reactivos())) {
      updateSelectInput(session, "SelectedColumnComparation",
                        choices = colnames(datos_reactivos()))
    }
  })
  
  # Shows the results of the LSD test
  output$lsdResults <- renderPrint({
    QDA <- datos_reactivos2()
    if (is.null(QDA) || input$SelectedColumnComparation == "") {
      return("There are no data or a valid column has not been selected.")
    }
    
    if (!input$SelectedColumnComparation %in% colnames(QDA)) {
      return("The selected column is not valid")
    }
    
    dependent_variable <- input$SelectedColumnComparation
    
    # Ajust the split plot model using sp.plot()
    tryCatch({
      if (!all(c("Repetition", "Treatment", "Panelist") %in% colnames(QDA))) {
        stop("Needed columns arer not present in the data frame.")
      }
      
      # Applies sp.plot()
      model_sp_plot <- sp.plot(QDA$Repetition, QDA$Treatment, QDA$Panelist, QDA[[dependent_variable]])
      
      # Substract needed coponents to apply LSD.test
      DFerror <- model_sp_plot$gl.a
      MSerror <- model_sp_plot$Ea
      
      # Applies LSD test
      lsd_result <- LSD.test(QDA[[dependent_variable]], QDA$Treatment, DFerror, MSerror, group = TRUE)
      
      # Muestra los resultados de la prueba de LSD
      print(lsd_result)
      
    }, error = function(e) {
      print(paste("Error to fit the model or doing mean comparation:", e$message))
    })
  })
  observe({
    req(datos_reactivos2())
    updateSelectInput(session, "SelectedColumnReproducibility", 
                      choices = colnames(datos_reactivos2()))
  })

      
  
  output$Reproducibility <- renderPlot({
    req(datos_reactivos2())
    if (input$SelectedColumnReproducibility == "") {
      return(NULL)
    }
    
    if (!input$SelectedColumnReproducibility %in% colnames(datos_reactivos2())) {
      return(NULL)
    }
    
    
    QDA <- datos_reactivos2()
    QDA$Treatment <- as.factor(QDA$Treatment)
    QDA$Panelist <- as.factor(QDA$Panelist)
    QDA$Repetition <- as.numeric(QDA$Repetition)
    QDA <- QDA[order(QDA$Panelist, QDA$Treatment, QDA$Repetition),]
    
    # Reproducibility plot
    xyplot(QDA[[input$SelectedColumnReproducibility]] ~ Treatment | Panelist, groups = Repetition, data = QDA, aspect = "xy", type = "o", auto.key = TRUE)
  })
  
  
  observe({
    req(datos_reactivos2())
    updateSelectInput(session, "SelectedColumnInteraction", 
                      choices = colnames(datos_reactivos2()))
  })
  
  
  
  output$Interaction <- renderPlot({
    req(datos_reactivos2())
    QDA <- datos_reactivos2()
    if (!input$SelectedColumnInteraction %in% colnames(QDA)) {
      return(NULL)
    }
    
    QDA$Treatment <- as.factor(QDA$Treatment)
    QDA$Panelist <- as.factor(QDA$Panelist)
    QDA$Repetition <- as.numeric(QDA$Repetition)
    QDA <- QDA[order(QDA$Panelist, QDA$Treatment, QDA$Repetition),]
    
    # Calculate mean by Panelist and Treatment using the selected column
    mean_Panelist <- QDA %>%
      group_by(Panelist, Treatment) %>%
      summarise(meanPanelist = mean(.data[[input$SelectedColumnInteraction]], na.rm = TRUE), .groups = 'drop')
    mean_Panelist$AsignValue <- 'Panelist'
    
    # Calculate global mean for each Treatment y duplicates means for each Panelist
    means <- mean_Panelist %>%
      group_by(Treatment) %>%
      summarise(meanPanelist = mean(meanPanelist)) %>%
      slice(rep(1:n(), each = n_distinct(mean_Panelist$Panelist)))
    means$AsignValue <- 'mean'
    means$Panelist <- rep(unique(mean_Panelist$Panelist), times = nrow(means) / length(unique(mean_Panelist$Panelist)))
    
    # Create the final dataframe
    df_final <- bind_rows(mean_Panelist, means) %>%
      arrange(Panelist, Treatment, desc(AsignValue))
    
    # Create the plot
    with(df_final, xyplot(meanPanelist ~ Treatment | Panelist, groups = AsignValue, aspect = "xy", type = "o", auto.key = TRUE))
  })
  
  
  # Create the radial plot
  output$radialPlot <- renderHighchart({
    req(datos_reactivos())
    QDA <- datos_reactivos()
    
    QDA_means <- QDA %>%
      select(-Panelist, -Repetition) %>%
      group_by(Treatment) %>%
      summarise_all(mean, na.rm = TRUE) %>%
      ungroup()
    
    QDA_means <- as.data.frame(QDA_means)
    
    QDA_transposed <- as.data.frame(t(QDA_means[-1]))
    colnames(QDA_transposed) <- QDA_means$Treatment
    
    QDA_for_chart <- data.frame(Attribute = rownames(QDA_transposed), QDA_transposed)
    rownames(QDA_for_chart) <- NULL
    
    hc <- hchart(QDA_for_chart, type = "line", hcaes(x = Attribute)) %>%
      hc_chart(polar = TRUE) %>%
      hc_title(text = "Radial plot") %>%
      hc_xAxis(categories = QDA_for_chart$Attribute, tickmarkPlacement = 'on', lineWidth = 0) %>%
      hc_yAxis(gridLineInterpolation = 'polygon', lineWidth = 0, min = 0)
    
    for(treatment in colnames(QDA_transposed)) {
      hc <- hc %>% hc_add_series(data = QDA_transposed[[treatment]], name = treatment)
    }
    
    hc %>% hc_tooltip(shared = TRUE, pointFormat = '<span style="color:{series.color}">{series.name}: <b>{point.y:.2f}</b><br/>')
    
    return(hc)
  })
}

shinyApp(ui = ui, server = server)


