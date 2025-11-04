# load libraries ------------------

library(data.table)
library(stringr)

library(ggplot2)

library(ggnewscale)
library(colorspace)

library(paletteer)

source("./R/helpers.R")
source("./R/mesh_terms.R")

# load data ----------------


# d0 <- "data/clean-data.tsv" |> fread(sep = "\t", fill = TRUE, quote = "")

d1 <- fread("./input/clean/clean-concepts.tsv.gz")
d2 <- fread("./input/clean/clean-concepts-childrean.tsv.gz")
d3 <- fread("./input/clean/clean-mesh.tsv.gz", sep = "\t", quote = "")

d3 <- d3[which(!is.na(`MeSH term`))]

mesh <- read_mesh("./input/mesh/desc2024_TN.csv.gz")

# output folders ----------------------

dir.create("output/", showWarnings = FALSE)


dir.create("output/tables", showWarnings = FALSE)
dir.create("output/tables/openalex", showWarnings = FALSE)
dir.create("output/tables/mesh", showWarnings = FALSE)

dir.create("output/figures", showWarnings = FALSE)
dir.create("output/figures/openalex", showWarnings = FALSE)
dir.create("output/figures/mesh", showWarnings = FALSE)

# filtering ---------------

# d1 <- d0[which(!is.na(`MeSH terms`) & `MeSH terms` != "")]
# d1 <- d1[which(year >= 2000)]
#
# d1$year <- d1$year |> as.numeric()
#
# rm(d0)
# gc()


index_1 <- d1$countries |>
  str_detect("GR") |>
  which()
index_2 <- d2$countries |>
  str_detect("GR") |>
  which()
index_3 <- d3$countries |>
  str_detect("GR") |>
  which()

# Open Alex - Concepts analysis -----------------------


world <- d1[-index_1]
greece <- d1[index_1]

t1 <- list()
t1_plot <- list()

t1[["world"]] <- analyse_concepts(world)
t1[["greece"]] <- analyse_concepts(greece)

t1_plot[["world"]] <- plot_concepts(t1$world$original)
t1_plot[["greece"]] <- plot_concepts(t1$greece$original)

ggsave(plot = t1_plot$world, filename = "output/figures/openalex/npmid-concepts-world.png", width = 8, height = 8, units = "in", dpi = 600)
ggsave(plot = t1_plot$greece, filename = "output/figures/openalex/npmid-concepts-greece.png", width = 8, height = 8, units = "in", dpi = 600)

writexl::write_xlsx(t1$world$tables, "output/tables/openalex/concepts-world.xlsx")
writexl::write_xlsx(t1$greece$tables, "output/tables/openalex/concepts-greece.xlsx")

# Open Alex - Concepts Children analysis ---------------------

world <- d2[-index_2]
greece <- d2[index_2]

t2 <- list()
t2_plot <- list()

t2[["world"]] <- analyse_concepts_children(world, my_filter = 0)
t2[["greece"]] <- analyse_concepts_children(greece, my_filter = 0)

# t2_plot[["world"]]  <- plot_concepts_children(t2$world$filtered, my_palette = "ggthemes::Red-Blue Diverging", my_direction = -1)
# t2_plot[["greece"]] <- plot_concepts_children(t2$greece$filtered, my_palette = "grDevices::Greens 3", my_direction = -1)
#
# ggsave(plot = t2_plot$world, filename = "output/figures/openalex/npmid-concepts_children-world.png", width = 5, height = 12, units = "in", dpi = 600)
# ggsave(plot = t2_plot$greece, filename = "output/figures/openalex/npmid-concepts_children-greece.png", width = 4.5, height = 10.5, units = "in", dpi = 600)

writexl::write_xlsx(t2$world$tables, "output/tables/openalex/concepts_children-world.xlsx")
writexl::write_xlsx(t2$greece$tables, "output/tables/openalex/concepts_children-greece.xlsx")


# MeSH term analysis ----------------------

index <- match(d3$`MeSH term`, mesh$`Descriptor Name`)

d3$`MeSH ID` <- mesh[index]$`Descriptor UI`

world <- d3[-index_3]
greece <- d3[index_3]

t3 <- list()
t3_plot <- list()


for (l in c(0, seq_len(2))) {
  t3[["world"]] <- analyse_mesh_terms(world, mesh, level = l, my_filter = 0)
  t3[["greece"]] <- analyse_mesh_terms(greece, mesh, level = l, my_filter = 0)

  # t3_plot[["world"]]  <- plot_mesh_terms(t3$world$filtered, my_palette = "ggthemes::Red-Blue Diverging", my_direction = -1)
  # t3_plot[["greece"]] <- plot_mesh_terms(t3$greece$filtered, my_palette = "grDevices::Greens 3", my_direction = -1)
  #
  # ggsave(plot = t3_plot$world, filename = "output/figures/png/npmid-mesh_terms-world.png", width = 8, height = 12, units = "in", dpi = 600)
  # ggsave(plot = t3_plot$greece, filename = "output/figures/png/npmid-mesh_terms-greece.png", width = 8, height = 12, units = "in", dpi = 600)

  writexl::write_xlsx(t3$world$tables, paste0("output/tables/mesh/mesh-world-level_", l, ".xlsx"))
  writexl::write_xlsx(t3$greece$tables, paste0("output/tables/mesh/mesh-greece-level_", l, ".xlsx"))

  ## Staistics + multi-plot -----------------------------------

  t3$world$filtered$condition <- "world"
  t3$greece$filtered$condition <- "greece"

  df <- rbind(t3$world$filtered, t3$greece$filtered)

  df <- df[which(year <= 2019)]

  library(rstatix)

  stats <- df |>
    dcast(year + result_name ~ condition, value.var = "freq_pmid", fill = 0) |>
    melt(id.vars = c("year", "result_name"), value.factor = FALSE, variable.factor = FALSE)

  stats <- stats[order(result_name, variable, year)]

  stats <- stats |>
    group_by(result_name) |>
    pairwise_wilcox_test(value ~ variable, paired = TRUE, detailed = TRUE) |>
    setDT()

  writexl::write_xlsx(stats, paste0("output/tables/mesh/stats-level_", l, ".xlsx"))

  # df$fsample <- paste0(df$year, "_", df$condition)
  #
  # df <- df[str_order(fsample, numeric = TRUE)]
  #
  # fmeta <- df[, c("condition", "fsample"), with = FALSE] |> unique()
  #
  #
  # df <- df |> dcast(result_name ~ fsample, value.var = "freq_pmid", fill = 0)
  #
  # df$result_name <- df$result_name |> as.character()
  #
  # mm <- df[, fmeta$fsample, with = FALSE] |> setDF(rownames = df$result_name) |> as.matrix()
  #
  # zm <- mm |> t() |> scale(center = TRUE, scale = TRUE) |> t()
  #
  # library(ComplexHeatmap)
  # library(circlize)
  #
  #
  #
  #
  # ht <- Heatmap(
  #     zm, name = "z-score",
  #
  #     clustering_distance_rows = "euclidean",
  #     clustering_method_rows = "ward.D2",
  #
  #     rect_gp = gpar(col = "grey", lwd = .25),
  #     border = TRUE,
  #
  #     cluster_columns = FALSE,
  #
  #     column_split = fmeta$condition,
  #
  #     row_names_gp = gpar(fontsize = 6),
  #     column_names_gp = gpar(fontsize = 6)
  # )
  #
  # library(ggplotify)
  # grob = grid.grabExpr(draw(ht)) |> as.ggplot()
  #
  #
  #
  # ggsave(plot = grob, filename = "output/figures/png/comparison.png", width = 8, height = 16, units = "in", dpi = 600)
}
