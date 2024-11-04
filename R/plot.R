



rm(list = ls())
gc()


# load libraries ------------------

library(data.table)
library(stringr)

library(ggplot2)

library(ggnewscale)
library(colorspace)

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

t1 <- analyse_concepts(world)
t2 <- analyse_concepts(greece)

ggsave(plot = t1$plot, filename = "output/figures/n_pmid.concepts.world.png", width = 8, height = 8, units = "in", dpi = 600)
ggsave(plot = t2$plot, filename = "output/figures/n_pmid.concepts.greece.png", width = 8, height = 8, units = "in", dpi = 600)

writexl::write_xlsx(t1$tables, "output/concepts.world.xlsx")
writexl::write_xlsx(t2$tables, "output/concepts.greece.xlsx")

# 2 ---------------------

t1 <- analyse_concepts_children(world)
t2 <- analyse_concepts_children(greece)

ggsave(plot = t1$plot, filename = "output/figures/n_pmid.concepts_children.world.png", width = 8, height = 8, units = "in", dpi = 600)
ggsave(plot = t2$plot, filename = "output/figures/n_pmid.concepts_children.children.png", width = 8, height = 8, units = "in", dpi = 600)

writexl::write_xlsx(t1$tables, "output/concepts_children.world.xlsx")
writexl::write_xlsx(t2$tables, "output/concepts_children.greece.xlsx")


# 3 ----------------------

d3 <- d1[, c("doi", "pmid", "year", "MeSH terms"), with = FALSE] |> unique()


t1 <- d3$`MeSH terms` |> 
    str_split("\\;") |>
    lapply(str_squish) |>
    lapply(function(x) data.table("MeSH term" = x)) |>
    rbindlist(idcol = "id")


d3 <- cbind(d3[t1$id, -c("MeSH terms")], t1[, -1])

d3 <- d3[, by = .(year, `MeSH term`), .(
    n_doi = doi |> unique() |> length(),
    n_pmid = pmid |> unique() |> length()
)]

writexl::write_xlsx(
    list(
        "No. of pmid" = d3 |> dcast(`MeSH term` ~ year, value.var = "n_pmid", fill = 0),
        "No. of doi"  = d3 |> dcast(`MeSH term` ~ year, value.var = "n_doi", fill = 0)
    ), 
    "output/MeSH_terms.xlsx"
)

t2 <- d3[, by = `MeSH term`, .(
    cumn_doi = n_doi |> sum(),
    cumn_pmid = n_pmid |> sum()
)]

t2 <- t2[which(cumn_pmid >= 20)]

d3 <- d3[which(`MeSH term` %in% t2$`MeSH term`)]

mm <- d3 |> dcast(`MeSH term` ~ year, value.var = "n_pmid", fill = 0)

ht <- mm[, -1] |> 
    setDF(rownames = mm$`MeSH term`) |> 
    as.matrix() |> 
    dist(method = "euclidean") |> 
    hclust(method = "ward.D2")

d3$term <- d3$`MeSH term` |> factor(levels = ht$labels[ht$order])

gr3 <- d3 |>
    ggplot(aes(year, term)) +
    
    geom_vline(xintercept = seq(1999.5, 2019.5, by = 1), color = "grey85", linewidth = .15) +
    geom_hline(yintercept = seq(.5, nrow(t2) + .5, by = 1), color = "grey85", linewidth = .15) +
    
    geom_tile(aes(fill = n_pmid), color = "grey75", linewidth = .15) +
    
    scale_fill_stepsn(
        colors = c('#00429d', '#5681b9', '#93c4d2', '#ffa59e', '#dd4c65', '#93003a'),
        transform = "log2", breaks = c(4, 8, 16, 32, 64),
        
        guide = guide_colorsteps(
            barheight = unit(16, "lines"),
            barwidth = unit(.5, "lines")
        )
    ) +
    
    scale_x_continuous(expand = c(0, 0), breaks = seq(2002, 2024, by = 4)) +
    scale_y_discrete(expand = c(0, 0)) +
    
    theme_minimal() +
    
    theme(
        axis.text.y = element_text(size = 6),
        
        panel.grid = element_blank(),
        
        axis.title = element_blank()
    )


ggsave(
    plot = gr3, filename = "output/figures/n_pmid.mesh_terms.png",
    width = 6, height = 12, units = "in", dpi = 600
)


# patchwork -----------

multi <- (gr2 | gr3) &
    theme(
        plot.margin = margin(10, 10, 10, 10)
    )


ggsave(
    plot = multi, filename = "output/figures/multi-plot.png",
    width = 11, height = 11, units = "in", dpi = 600
)









