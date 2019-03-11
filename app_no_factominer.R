library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(skimr)
library(ggplot2)
library(stringr)
library(plotly)
# https://github.com/fawda123/ggord/
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/
# install.packages(c("FactoMineR", "factoextra"))

# library("grid") 
# library(devtools)
# install_github("vqv/ggbiplot")
# library(ggbiplot)#;install.packages("ggbiplot")

# library(devtools)
# install_github('fawda123/ggord')
library(ggord)

library(shiny)
library(shinydashboard)


# load data ---------------------------------------------------------------

# loaded from application

# functions ---------------------------------------------------------------

opcion_todos <- "Todos"

screeplot_julio <- function(data,
                            par_title="Gráfico de sedimentación de Componentes Principales",
                            par_subtitle="rojo: proporcion acumulada",
                            par_y="Proporcion de Variancia Explicada"){
    
    # data_pca$sdev
    
    pr.var <- data$sdev^2
    pve <- pr.var / sum(pr.var)
    pr_explained <- tibble(pc=colnames(data$rotation),
                           pve,
                           cumulative_prop=cumsum(pve))
    
    
    plot_out <- pr_explained %>% 
        ggplot(aes(y=pve,x=pc,group=""))+
        geom_line()+
        geom_point()+
        geom_text(label = paste(round(pve,3)*100,' %'), vjust = -0.4, hjust = 0.5)+
        geom_line(aes(y=cumulative_prop),color="red")+
        geom_point(aes(y=cumulative_prop),color="red")+
        geom_text(aes(y=cumulative_prop),color="red",label = paste(round(pr_explained$cumulative_prop,3)*100,' %'), vjust = -0.4, hjust = 0.5)+
        scale_y_continuous(limits = c(0,1),labels = scales::percent) +
        theme_light()+
        labs(title=par_title,
             subtitle=par_subtitle,
             x="", y=par_y)
    
    plot_out
    
}
# data_pca$rotation

# contribution_julio(data_pca$rotation)
# contribution_julio(data_pca$rotation,pc_filter=c("PC1"))
# contribution_julio(data_pca$rotation,pc_filter=c("PC2"))
contribution_julio <- function(data,pc_filter=c(),par_title="Contribucion de las variables a las Componentes Principales"){
    
    data_tmp <-  data %>% as.data.frame() %>% 
        tibble::rownames_to_column(var="variable") %>% 
        gather(PC,valor,-variable)
    
    pl <- ""
    if(length(pc_filter) > 0 ){
        data_tmp <- data_tmp %>% filter(PC %in% pc_filter)
        pl <- data_tmp %>% 
            ggplot(aes(x=variable,y=valor,fill=variable))+
            geom_col() +
            geom_hline(yintercept=0, linetype="dashed", color = "black") +
            theme_light() +
            labs(title=par_title,
                 subtitle = paste0("para componente ", pc_filter))+
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }else{
        pl <- data_tmp %>% 
            ggplot(aes(x=PC,y=valor,fill=variable))+
            geom_col() +
            geom_hline(yintercept=0, linetype="dashed", color = "black") +
            theme_light() +
            labs(title=par_title)
    }
    

    
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
    title="PCA - Utility App",
    titleWidth = 770#,
    #dropdownMenu(dropdownMenuOutput("msg_menu"))
)
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Disclaimer",
                 tabName = "disclaimer_app"
        ),
        menuItem("Upload Data",
                 tabName = "upload_data"
        ),
        menuItem("Raw Data",
                 tabName = "dataset"
        ),
        menuItem("Select Columns For PCA",
                 tabName = "select_columns"
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
        tabItem(tabName = "disclaimer_app",div(
            p("this app uses the package: ggbiplot"),
            p("A complete tutorial on to interpret the results can be found on: "),
            a(href="http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/",
              "Link to article.")
        )),
        tabItem(tabName = "upload_data",fileInput("data_file_upload","Select a file")),
        tabItem(tabName = "select_columns",
                selectizeInput(inputId = "select_column_for_row_name",
                               label = "Select Column for row name",
                               choices = NULL, 
                               selected = NULL, 
                               multiple = FALSE),
                selectizeInput(inputId = "select_columns_for_pca",
                               label = "Select Columns For PCA",
                               choices = NULL, 
                               selected = NULL, 
                               multiple = TRUE)
                ),
        
        
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
                # plotOutput("pca_biplot_out"))
                plotlyOutput("pca_biplot_out"))

        ) # tab items end
)# body end

ui <- dashboardPage(header, sidebar, body)


# SERVER ------------------------------------------------------------------
server <- function(input, output,session) {
    
    input_file <- reactive({
        # print(class(input$data_file_upload))
        
        req(input$data_file_upload)
        if (is.null(input$data_file_upload)) {
            return("")
        }
        
        # print(class(input$data_file_upload))
        # skimr::skim(input$data_file_upload)
        
        # Read the text in the uploaded file
        # print(class(readLines(input$data_file_upload$datapath)))
        readr::read_csv(input$data_file_upload$datapath)
    })

    dataset_reactive <- reactive({
        req(input$data_file_upload)
        # adapt this for input data
        input_file()
    })
    
    dataset_filtered_for_pca <- reactive({
        # adapt this to current dataset select columns data.
        # print(skimr::skim(dataset_reactive()))
        req(input$select_column_for_row_name)
        req(input$select_columns_for_pca)
        select_columns <- input$select_columns_for_pca
        data_ret<- dataset_reactive() %>% 
            tibble::column_to_rownames(var=input$select_column_for_row_name) %>% 
            select(select_columns)
        data_ret
    })
    
    dataset_reactive_pca <- reactive({
        ret_value <- NA
        
        # validate selected columns then , execute pca if valid
        if(selected_cols_are_valid(selected_cols, dataset_filtered_for_pca)){
            ret_value <- prcomp(dataset_filtered_for_pca(),scale=TRUE)
            # ret_value <- PCA(dataset_filtered_for_pca(), graph = FALSE)    
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
            # ret_p <- fviz_eig(dataset_reactive_pca(), addlabels = TRUE, ylim = c(0, 100))
            ret_p <- screeplot_julio(dataset_reactive_pca())
        }
        ret_p
    })
 
    
    pc_seleccion_options_all <- reactive({
        req(input$data_file_upload)
        ret_opt <- NA
        # if(is.na(dataset_reactive_pca()) ){
        if(length(dataset_reactive_pca()) > 1 ) {
            # ret_opt <- colnames(dataset_reactive_pca()$var$coord)
            ret_opt <- colnames(dataset_reactive_pca()$rotation)
            # ret_opt <- paste("PC",seq(nrow(dataset_reactive_pca()$eig)))
            ret_opt <- c(opcion_todos,ret_opt)
        }
        ret_opt
    })
       
    pc_seleccion_options <- reactive({
        req(input$data_file_upload)
        ret_opt <- NA
        # if(is.na(dataset_reactive_pca()) ){
        if(length(dataset_reactive_pca()) > 1 ) {
            # ret_opt <- colnames(dataset_reactive_pca()$var$coord)
            ret_opt <- colnames(dataset_reactive_pca()$rotation)
            # ret_opt <- paste("PC",seq(nrow(dataset_reactive_pca()$eig)))
        }
        ret_opt
    })
    
    columns_selection_options <- reactive({
        req(input$data_file_upload)
        ret_opt <- colnames(dataset_reactive())
        ret_opt
    })
    
    columns_selection_options_pca <- reactive({
        req(input$data_file_upload)
        ret_opt <- colnames(dataset_reactive() %>% dplyr::select_if( is.numeric))
        ret_opt
    })
    
    # Here we gotta select some vars to update the select of pca_contribution
    # and the select of 
    observe({
        dataset_reactive_pca()
        
        
        updateSelectizeInput(session, "select_pc_contribution", 
                             choices = pc_seleccion_options_all())
        
        updateSelectizeInput(session, "select_pc_biplot_x", 
                             choices = pc_seleccion_options())
        
        updateSelectizeInput(session, "select_pc_biplot_y", 
                             choices = pc_seleccion_options())

    })
    observe({
        dataset_reactive()
        updateSelectizeInput(session, 'select_column_for_row_name',
                             choices = columns_selection_options())
        updateSelectizeInput(session, 'select_columns_for_pca',
                             choices = columns_selection_options_pca())
    })
    output$pca_contribution_out <- renderPlot({
        ret_p <- NA
        req(input$select_pc_contribution)
        # if(is.na(dataset_reactive_pca()) ){
        if(length(dataset_reactive_pca()) > 1 ) {
            
            if(input$select_pc_contribution == opcion_todos){
                ret_p <-  contribution_julio(dataset_reactive_pca()$rotation)    
            }else{
                current_pc_axis <- input$select_pc_contribution %>%
                    str_replace("\\.","") %>% 
                    parse_number() 
                
                #ret_p <- fviz_contrib(data_pca_PCA, choice = "var", axes = current_pc_axis, top = 10)
                # ret_p <- fviz_contrib(dataset_reactive_pca(), choice = "var", axes = current_pc_axis)
                
                ret_p <-  contribution_julio(dataset_reactive_pca()$rotation,pc_filter = paste0("PC",current_pc_axis))    
            }
            
            
            
        }
        ret_p
    })
    
    output$pca_biplot_out <- renderPlotly({
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
            # ret_p <- fviz_pca_biplot(dataset_reactive_pca(), repel = FALSE,
            #                          axes=c(current_pc_x,current_pc_y),
            #                          col.var = "#2E9FDF", # Variables color
            #                          col.ind = "#696969"  # Individuals color
            # )
            # ret_p <- ggbiplot(dataset_reactive_pca(),choices = c(current_pc_x,current_pc_y))
            ret_p <- ggord(dataset_reactive_pca(),
                           axes = c(current_pc_x,current_pc_y),
                           size = 0.5,
                           vec_ext = 3, veccol = 'red', veclsz = 1,labcol = 'red')
        }
        # ret_p
        ggplotly(ret_p)
    })
 
}

# shiny app ---------------------------------------------------------------
shinyApp(ui = ui, server = server)