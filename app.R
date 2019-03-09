library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(skimr)
library(ggplot2)
library(stringr)
library(plotly)
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/
# install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")
library("corrplot")#;install.packages("corrplot")
# library("grid") 
# library(devtools)
# install_github("vqv/ggbiplot")
library(ggbiplot)#;install.packages("ggbiplot")

library(shiny)
library(shinydashboard)


# load data ---------------------------------------------------------------
pokemon_data <- readr::read_csv("pokemon.csv")

# functions ---------------------------------------------------------------
contribution_julio <- function(data){
    pl <- data %>% as.data.frame() %>% 
        tibble::rownames_to_column(var="variable") %>% 
        gather(PC,valor,-variable) %>% 
        ggplot(aes(x=PC,y=valor,fill=variable))+
        geom_col() +
        geom_hline(yintercept=0, linetype="dashed", color = "black") +
        theme_light() +
        labs(title="Contribucion de las variables a las Componentes Principales")
    
    pl
}


selected_cols_are_valid <- function(select_cols, data){
    # this should be changed to a real validation , 
    # if selected cols, are all numeric, PCA can be performed.
    ret_value <- TRUE

    ret_value
    
}


# UI ----------------------------------------------------------------------
header <- dashboardHeader(
    title="Tidy tuesday challenge: Week 2019-02-26 french train delays",
    titleWidth = 770#,
    #dropdownMenu(dropdownMenuOutput("msg_menu"))
)
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Raw Data",
                 tabName = "dataset"
        ),
        menuItem("PCA Data",
                 tabName = "pca_data"
        ),
        menuItem("PC ScreePlot",
                 tabName = "pca_scree_plot"
        ),
        menuItem("PC Contribution",
                 tabName = "pca_contribution"
        ),
        menuItem("PC Biplot",
                 tabName = "pca_biplot"
        ),
        menuItem("Contact", 
                 href="https://www.juliospairani.com" )
    )
)

body <- dashboardBody(
    tabItems(
        # TAB graph pov --------------------------------------------------------------------------
        tabItem(tabName = "dataset",DT::dataTableOutput("data_raw_out")),
        tabItem(tabName = "pca_data",verbatimTextOutput("pca_data_out")),
        tabItem(tabName = "pca_scree_plot",plotOutput("pca_scree_plot_out")),
        
        tabItem(tabName = "pca_contribution",
                selectizeInput(inputId = "select_pc_contribution", 
                               label = "Component", choices = '',
                               options = list(
                                   placeholder = 'Select PC',
                                   onInitialize = I('function() { this.setValue(""); }')),
                               multiple = FALSE),
                plotOutput("pca_contribution_out")),
        
        tabItem(tabName = "pca_biplot",
                selectizeInput(inputId = "select_pc_biplot_x", 
                               label = "Component X axis", choices = '',
                               options = list(
                                   placeholder = 'Select PC',
                                   onInitialize = I('function() { this.setValue(""); }')),
                               multiple = FALSE),
                selectizeInput(inputId = "select_pc_biplot_y", 
                               label = "Component Y axis", choices = '',
                               options = list(
                                   placeholder = 'Select PC',
                                   onInitialize = I('function() { this.setValue(""); }')),
                               multiple = FALSE),
                plotOutput("pca_biplot_out"))

        ) # tab items end
)# body end

ui <- dashboardPage(header, sidebar, body)


# SERVER ------------------------------------------------------------------
server <- function(input, output,session) {
    
    dataset_reactive <- reactive({
        # adapt this for input data
        pokemon_data
    })
    
    dataset_filtered_for_pca <- reactive({
        # adapt this to current dataset select columns data.
        select_columns <- c("Total","HitPoints","Attack","Defense","SpecialAttack","SpecialDefense","Speed")
        pokemon_data_pca<- dataset_reactive() %>% tibble::column_to_rownames(var="Name") %>% select(select_columns)
        pokemon_data_pca
    })
    
    dataset_reactive_pca <- reactive({
        ret_value <- NA
        
        # validate selected columns then , execute pca if valid
        if(selected_cols_are_valid(selected_cols, dataset_filtered_for_pca)){
            ret_value <- PCA(dataset_filtered_for_pca(), graph = FALSE)    
        }
        # data_pca <- prcomp(pokemon_data_pca,scale=TRUE)
        ret_value
    })
    
    output$data_raw_out <- DT::renderDataTable({
        DT::datatable(dataset_reactive(),
            escape = FALSE,
            rownames = FALSE,
            selection = 'none') 
    })
    
    output$pca_data_out <- renderPrint({ 
        summary(dataset_reactive_pca())
    })
    
    
    
    # Affects all PC's, no previous select or whatever.
    output$pca_scree_plot_out <- renderPlot({
        ret_p <- NA
        # if(is.na(dataset_reactive_pca()) ){
        if(length(dataset_reactive_pca()) > 1 ) {
            ret_p <- fviz_eig(dataset_reactive_pca(), addlabels = TRUE, ylim = c(0, 100))    
        }
        ret_p
    })
    
    pc_seleccion_options <- reactive({
        ret_opt <- NA
        # if(is.na(dataset_reactive_pca()) ){
        if(length(dataset_reactive_pca()) > 1 ) {
            ret_opt <- colnames(dataset_reactive_pca()$var$coord)
            # ret_opt <- paste("PC",seq(nrow(dataset_reactive_pca()$eig)))
        }
        ret_opt
    })
    
    # Here we gotta select some vars to update the select of pca_contribution
    # and the select of 
    observe({
        dataset_reactive_pca()
        updateSelectizeInput(session, "select_pc_contribution", 
                             choices = pc_seleccion_options())
        
        updateSelectizeInput(session, "select_pc_biplot_x", 
                             choices = pc_seleccion_options())
        
        updateSelectizeInput(session, "select_pc_biplot_y", 
                             choices = pc_seleccion_options())

    })

    
    output$pca_contribution_out <- renderPlot({
        ret_p <- NA
        req(input$select_pc_contribution)
        # if(is.na(dataset_reactive_pca()) ){
        if(length(dataset_reactive_pca()) > 1 ) {
            
            current_pc_axis <- input$select_pc_contribution %>%
                str_replace("\\.","") %>% 
                parse_number() 

            #ret_p <- fviz_contrib(data_pca_PCA, choice = "var", axes = current_pc_axis, top = 10)
            ret_p <- fviz_contrib(data_pca_PCA, choice = "var", axes = current_pc_axis)
        }
        ret_p
    })
    
    output$pca_biplot_out <- renderPlot({
        ret_p <- NA
        req(input$select_pc_biplot_x)
        req(input$select_pc_biplot_y)
        if(length(dataset_reactive_pca()) > 1 ) {
            
            current_pc_x <- input$select_pc_biplot_x %>%
                str_replace("\\.","") %>% 
                parse_number() 
            current_pc_y <- input$select_pc_biplot_y %>%
                str_replace("\\.","") %>% 
                parse_number() 
            
            #ret_p <- fviz_contrib(data_pca_PCA, choice = "var", axes = current_pc_axis, top = 10)
            ret_p <- fviz_pca_biplot(data_pca_PCA, repel = FALSE,
                                     axes=c(current_pc_x,current_pc_y),
                                     col.var = "#2E9FDF", # Variables color
                                     col.ind = "#696969"  # Individuals color
            )
        }
        ret_p
    })
    
    
    
    
}



# shiny app ---------------------------------------------------------------
shinyApp(ui = ui, server = server)