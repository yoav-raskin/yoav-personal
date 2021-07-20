library(tidyverse)
library(factoextra)
library(FactoMineR)
library(fmsb)
library(hrbrthemes)
library(GGally)
library(viridis)
library(cluster)
library(NbClust)

setwd("C:/Users/yoavr/Documents/GitHub/yoav-personal/ML generic code/Clustering")



# 1 Load sample dataset & scaling -----------------------------------------------------

  data("USArrests")
  data <- as_tibble(scale(USArrests)) %>% 
    rownames_to_column("ID")
  rm(USArrests)


# 2 Exploring Principle Components (PCA) -----------------------------------------------------------

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
  


# 3 Visualizing Kmeans\KMediods\hclust clustering metrics (wss\gap) to choose # clusters  ------------------------
  

  metrics_plots <- list()
  metrics_gap_data <- tibble()
  metrics_wss_data <- tibble()
  
  
  for (m in c("kmeans", "pam", "hcut")) {
    # m <- "kmeans"
    
  

  # Computing wss 
  wss <- fviz_nbclust(data %>% 
                 select(-ID), eval(parse(text = m)), nstart = 25, method = "wss") +
    labs(subtitle = paste0("Elbow method - with ", m))
  
  # Computing gap_stat
  gap_stat <- fviz_nbclust(data %>% 
                 select(-ID), nboot = 500, eval(parse(text = m)), nstart = 25, method = "gap_stat") +
    labs(subtitle = paste0("Gap Statistic - with ", m))
  
  # Collecting metric data
  metrics_plots[[m]][["wss"]] <- wss
  metrics_plots[[m]][["gap_stat"]] <- gap_stat 
  
  metrics_gap_data_i <- metrics_plots[[m]][["gap_stat"]][["data"]] %>% 
    mutate(Model = paste0(m))
  
  metrics_gap_data <- bind_rows(metrics_gap_data_i, metrics_gap_data)
  
  metrics_wss_data_i <- metrics_plots[[m]][["wss"]][["data"]] %>% 
    mutate(Model = paste0(m))
  
  metrics_wss_data <- bind_rows(metrics_wss_data_i, metrics_wss_data)
  
  
  
  print(paste0(m, " Collected"))
  
  }
  
  rm(metrics_gap_data_i, metrics_wss_data_i)
  
  # Plotting gap_stat
  metrics_gap_data %>% 
    ggplot(aes(y = gap, x = factor(clusters), col = Model)) +
    geom_pointrange(aes(ymin = ymin, ymax = ymax), position = position_dodge(width = 0.5)) +
    
    theme_bw() +
    xlab("# of Clusters") +
    ylab("Gap Statistic")
  
  ggsave("gap_stat.png")
  
  # Plotting wss
  metrics_wss_data %>% 
    ggplot(aes(y = y, x = factor(clusters), col = Model)) +
    geom_point() +
    geom_line(aes(group = Model)) +
    
    theme_bw() +
    xlab("# of Clusters") +
    ylab("Within Sum of Squares")
  
  ggsave("wss.png")
  
  # choose manually
  
  cluster_assignment <- eclust(data %>% select(-ID),
                               k = 4, nboot = 500, FUNcluster = "pam", nstart = 25, graph = FALSE) %>% 
    pluck("clustering")
  
  
  
  

# 4 Binding cluster $ PCs columns to data -------------------------------------------------------------------------

  data_clustered <- data %>% 
    # Add appropriate cluster element 
    mutate(Cluster = cluster_assignment) %>% 
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
    geom_jitter(data = plot_data,
               aes(x = Predictor,
                   y = Value,
                   col = Cluster),
               alpha = 0.3, size = 2, width = 0.2, height = 0.2) +
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
                  size = 0.9, alpha = 0.7) +
  
  theme_bw()
  
  
