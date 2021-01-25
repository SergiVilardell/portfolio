
# PCA ---------------------------------------------------------------------

library(factoextra)

logr_df <- readRDS(file = "logr_data.RDS")

res.pca <- prcomp(logr_df, scale = TRUE)
fviz_eig(res.pca)
fviz_pca_var(res.pca,
             col.var = "cos2", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)