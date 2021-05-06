library(tidyverse)
library(factoextra)
library(FactoMineR)
library(fmsb)
library(hrbrthemes)
library(GGally)
library(viridis)
setwd("G:/My Drive/ML Methodology/Clustering")



# 1 Load sample dataset & scaling -----------------------------------------------------

  data("USArrests")
  data <- as_tibble(scale(USArrests)) %>% 
    rownames_to_column("ID")
  rm(USArrests)


# 2 exploring Principle Components (PCA) -----------------------------------------------------------

  # Computing PCs 
  pca <- PCA(data %>% 
               select(-ID), scale.unit = TRUE, ncp = 15, graph = FALSE)
  
  # Visualizing variance explained
  fviz_eig(pca, addlabels = TRUE, ncp = 15)
  
  # Visualizing quality of representation of the variables on first 2 PC (cos2 metric)
  fviz_pca_var(pca, col.var = "cos2", alpha.var = "cos2",
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
               repel = TRUE # Avoid text overlapping
  )
  


# 3 Visualizing Kmeans clustering metrics (wss\gap) to choose # clusters  ------------------------

  # Visualizing wss - CHOOSE # clusters manually
  fviz_nbclust(data %>% 
                 select(-ID), kmeans, method = "wss") +
    geom_vline(xintercept = 4, linetype = 2) +
    labs(subtitle = "Elbow method")
  
  # Visualizing gap_stat
  km <- eclust(data %>% 
                 select(-ID), nboot = 500, "kmeans", nstart = 25, graph = FALSE)
  fviz_gap_stat(km$gap_stat)

# 4 Binding cluster $ PCs columns to data -------------------------------------------------------------------------

  data_clustered <- data %>% 
    mutate(Cluster = km$cluster) %>% 
    bind_cols(as_tibble(pca$ind$coord))


# 5 Visualize PC with obs. & predictors (biplot) -----------------------------------------------------------


  fviz_pca_biplot(
    pca,
    # obs
    geom.ind = "point",
    fill.ind = factor(data_clustered$Cluster), col.ind = "black",
    pointshape = 21, pointsize = 2,
    addEllipses = TRUE, ellipse.type = "convex",
    # predictors
    alpha.var ="cos2", col.var = "black",
    legend.title = list(fill = "Cluster", col = "Cluster", alpha = "Predictor \nRepresentation")) +
    
    # Adding cluster label from KMeans    
    geom_text(data = data_clustered,
              aes(x = Dim.1, y = Dim.2, label = ID),
              nudge_x = 0.1)
  
  # 6  Visualizing Clusters on Predictors - Parallel coordinates Plot --------------------------------------
  


  # Data Prep - means and SDs
  means_data <- data_clustered %>%
    select(Murder:Cluster) %>% 
    group_by(Cluster) %>% 
    summarise_all(.funs = c("mean" = mean, "sd" = sd)) %>% 

    pivot_longer(Murder_mean:Rape_sd, names_to = "Predictor", values_to = "Value") %>%
    separate(Predictor, into = c("Predictor", "Stat")) %>% 
    mutate(Predictor = factor(Predictor, levels = c("UrbanPop", "Murder", "Assault", "Rape", "Cluster")),
           Cluster = factor(Cluster)) %>% 
    
    pivot_wider(names_from = Stat, values_from = Value) %>% 
    
    mutate(MinusSD = mean - sd,
           PlusSD = mean + sd)
    
  
  # Data Prep - all obs. as data-points
  plot_data <- data_clustered %>%
    select(Murder:Cluster) %>%
    
    pivot_longer(Murder:Rape, names_to = "Predictor", values_to = "Value") %>%
    mutate(Predictor = factor(Predictor, levels = c("UrbanPop", "Murder", "Assault", "Rape", "Cluster")),
           Cluster = factor(Cluster))
  
  # ggplotting
  ggplot() +
    geom_point(data = plot_data,
               aes(x = Predictor,
                   y = Value,
                   col = Cluster),
               alpha = 0.3, size = 2) +
    geom_line(data = means_data,
              aes(x = Predictor,
                  y = mean,
                  group = Cluster,
                  col = Cluster),
              size = 1.5, alpha = 0.5) +
    geom_pointrange(data = means_data,
                  aes(x = Predictor,
                      y = mean,
                      ymin = MinusSD,
                      ymax = PlusSD,
                      col = Cluster),
                  position = position_dodge(width = 0.15),
                  size = 1.1, alpha = 0.7) +
  
  theme_bw()
  
  
