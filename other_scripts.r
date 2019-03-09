

screeplot_julio <- function(data){
    
    # data_pca$sdev
    
    pr.var <- data_pca$sdev^2
    pve <- pr.var / sum(pr.var)
    pr_explained <- tibble(pc=colnames(data_pca$rotation),
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
        labs(title="Gráfico de sedimentación de Componentes Principales",
             subtitle="rojo: proporcion acumulada",
             x="", y="Proporcion de Variancia Explicada")
    
    plot_out
    
}
# data_pca$rotation
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



# https://stackoverflow.com/questions/6578355/plotting-pca-biplot-with-ggplot2


pokemon_data <- readr::read_csv("pokemon.csv")
select_columns <- c("Total","HitPoints","Attack","Defense","SpecialAttack","SpecialDefense","Speed")
pokemon_data_pca<- pokemon_data %>% tibble::column_to_rownames(var="Name") %>% select(select_columns)


# skimr::skim(pokemon_data_pca)
data_pca <- prcomp(pokemon_data_pca,scale=TRUE)
data_pca_PCA <- PCA(pokemon_data_pca, graph = FALSE)
summary(data_pca_PCA)
nrow(data_pca_PCA$eig)

data_pca_PCA
fviz_eig(data_pca_PCA, addlabels = TRUE, ylim = c(0, 100))
fviz_pca_var(data_pca_PCA, col.var = "red")
corrplot(data_pca_PCA$var$cos2, is.corr=FALSE)
fviz_contrib(data_pca_PCA, choice = "var", axes = 1, top = 10)
fviz_contrib(data_pca_PCA, choice = "var", axes = 2, top = 10)
fviz_contrib(data_pca_PCA, choice = "var", axes = 3, top = 10)
corrplot(data_pca_PCA$var$contrib, is.corr=FALSE)    
fviz_pca_var(data_pca_PCA, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

data_pca_PCA$eig
colnames(data_pca_PCA$var$coord)
contribution_julio(data_pca_PCA$ind$contrib)
fviz_pca_var(data_pca_PCA, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

ggplotly(
fviz_pca_biplot(data_pca_PCA, repel = FALSE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
))
data_pca$rotation
data_pca_PCA$var$contrib 


summary(data_pca)


# Plot cumulative proportion of variance explained
plot(cumsum(pve), xlab = "Principal Component", 
     ylab = "Cumulative Proportion of Variance Explained", 
     ylim = c(0, 1), type = "b")
