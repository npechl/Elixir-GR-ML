



rm(list = ls())
gc()


# load libraries ------------------

library(data.table)
library(stringr)

library(ggplot2)

library(ggnewscale)
library(colorspace)

library(paletteer)

source("R/helpers.R")

# load data ----------------


d0 <- "data/clean-data.tsv" |> fread(sep = "\t", fill = TRUE, quote = "")

# filtering ---------------

d1 <- d0[which(!is.na(`MeSH IDs`) & `MeSH IDs` != "")]
d1 <- d1[which(year >= 2000)]

d1$year <- d1$year |> as.numeric()

rm(d0)
gc()

index <- d1$countries |> str_detect("GR") |> which()

greece <- d1[index]
world  <- d1[-index]

rm(index)

# 1 -----------------------

t1      <- list()
t1_plot <- list()

t1[["world"]]  <- analyse_concepts(world)
t1[["greece"]] <- analyse_concepts(greece)

t1_plot[["world"]]  <- plot_concepts(t1$world$original)
t1_plot[["greece"]] <- plot_concepts(t1$greece$original)

ggsave(plot = t1_plot$world, filename = "output/figures/npmid.concepts.world.png", width = 8, height = 8, units = "in", dpi = 600)
ggsave(plot = t1_plot$greece, filename = "output/figures/npmid.concepts.greece.png", width = 8, height = 8, units = "in", dpi = 600)

writexl::write_xlsx(t1$world$tables, "output/concepts.world.xlsx")
writexl::write_xlsx(t1$greece$tables, "output/concepts.greece.xlsx")

# 2 ---------------------

t2      <- list()
t2_plot <- list()

t2[["world"]]  <- analyse_concepts_children(world)
t2[["greece"]] <- analyse_concepts_children(greece)

t2_plot[["world"]]  <- plot_concepts_children(t2$world$filtered, my_palette = "ggthemes::Red-Blue Diverging", my_direction = -1)
t2_plot[["greece"]] <- plot_concepts_children(t2$greece$filtered, my_palette = "grDevices::Greens 3", my_direction = -1)

ggsave(plot = t2_plot$world, filename = "output/figures/npmid.concepts_children.world.png", width = 5, height = 12, units = "in", dpi = 600)
ggsave(plot = t2_plot$greece, filename = "output/figures/npmid.concepts_children.children.png", width = 4.5, height = 10.5, units = "in", dpi = 600)

writexl::write_xlsx(t2$world$tables, "output/concepts_children.world.xlsx")
writexl::write_xlsx(t2$greece$tables, "output/concepts_children.greece.xlsx")


# 3 ----------------------

t3      <- list()
t3_plot <- list()

t3[["world"]]  <- analyse_mesh_terms(world)
t3[["greece"]] <- analyse_mesh_terms(greece)

t3_plot[["world"]]  <- plot_mesh_terms(t3$world$filtered, my_palette = "ggthemes::Red-Blue Diverging", my_direction = -1)
t3_plot[["greece"]] <- plot_mesh_terms(t3$greece$filtered, my_palette = "grDevices::Greens 3", my_direction = -1)

ggsave(plot = t3_plot$world, filename = "output/figures/npmid.mesh_terms.world.png", width = 6, height = 12, units = "in", dpi = 600)
ggsave(plot = t3_plot$greece, filename = "output/figures/npmid.mesh_terms.greece.png", width = 5, height = 11, units = "in", dpi = 600)

writexl::write_xlsx(t3$world$tables, "output/mesh_terms.world.xlsx")
writexl::write_xlsx(t3$greece$tables, "output/mesh_terms.greece.xlsx")

# multi-plot -----------

t3$world$filtered$condition <- "world"
t3$greece$filtered$condition <- "greece"


df <- rbind(t3$world$filtered, t3$greece$filtered)

df$fsample <- paste0(df$year, "_", df$condition)

fmeta <- df[, c("condition", "fsample"), with = FALSE] |> unique()


df <- df |> dcast(`MeSH term` ~ fsample, value.var = "freq_pmid", fill = 0)

df$`MeSH term` <- df$`MeSH term` |> as.character()

mm <- df[, fmeta$fsample, with = FALSE] |> setDF(rownames = df$`MeSH term`) |> as.matrix()

zm <- mm |> t() |> scale(center = TRUE, scale = TRUE) |> t()

library(ComplexHeatmap)
library(circlize)




ht <- Heatmap(
    zm, name = "z-score", 
    
    clustering_distance_rows = "pearson",
    clustering_method_rows = "ward.D2",
    
    rect_gp = gpar(col = "grey", lwd = .25),
    border = TRUE,
    
    cluster_columns = FALSE,
    
    column_split = fmeta$condition,
    
    row_names_gp = gpar(fontsize = 5),
    column_names_gp = gpar(fontsize = 6)
)

grob = grid.grabExpr(draw(ht)) |> as.ggplot()

library(ggplotify)

ggsave(plot = grob, filename = "output/figures/comparison.png", width = 7, height = 12, units = "in", dpi = 600)


