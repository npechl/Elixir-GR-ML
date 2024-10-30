



rm(list = ls())
gc()


# load libraries ------------------

library(data.table)
library(stringr)

library(ggplot2)

library(ggnewscale)
library(colorspace)

# load data ----------------


d0 <- "data/clean-data.xlsx" |> readxl::read_xlsx() |> setDT()

# filtering ---------------

d1 <- d0[which(!is.na(`MeSH IDs`))]
d1 <- d1[which(year >= 2000)]

# 1 -----------------------

d2 = d1[, by = .(year, concepts_name), .(
    n_doi  = doi |> unique() |> length(),
    n_pmid = pmid |> unique() |> length()
)]


gr1 <- d2 |>
    ggplot(aes(year, n_pmid)) +
    
    geom_smooth(aes(color = concepts_name, fill = concepts_name), alpha = .1, lineend = "round") +
    
    geom_point(aes(fill = concepts_name, color = concepts_name), shape = 21, size = 2, stroke = .25) +
    
    scale_color_manual(values = c("#CD534C", "#4A6990", "#79AF97") |> darken(.25)) +
    scale_fill_manual(values = c("#CD534C", "#4A6990", "#79AF97") |> lighten(.25)) +
    
    scale_x_continuous(breaks = seq(2000, 2024, by = 3)) +
    scale_y_continuous(transform = "log2", breaks = c(1, 2, 4, 8, 16, 32, 64, 128)) +
    
    theme_minimal() +
    
    theme(
        legend.position = "top",
        legend.title.position = "top",
        
        panel.grid.major = element_line(linetype = "dashed", lineend = "round", color = "grey85"),
        panel.grid.minor = element_line(linetype = "dashed", lineend = "round", color = "grey85"),
        
        plot.margin = margin(20, 20, 20, 20)
    )

ggsave(
    plot = gr1,
    filename = "output/figures/n_pmid.concepts.png",
    width = 8, height = 8, units = "in", dpi = 600
)

# 2 ---------------------

d2 <- d1[, by = .(year, concepts_children_name), .(
    n_doi = doi |> unique() |> length(),
    n_pmid = pmid |> unique() |> length()
)]


d2 <- d2[which(!is.na(concepts_children_name))]

t1 <- d2[, by = concepts_children_name, .(
    cumn_pmids = n_pmid |> sum(),
    cumn_dois  = n_doi |> sum()
)]

t1 <- t1[which(cumn_pmids >= 5)]


d3 <- d2[which(concepts_children_name %in% t1$concepts_children_name)]
mm <- d3 |> dcast(concepts_children_name ~ year, value.var = "n_pmid", fill = 0)

ht <- mm[, -1] |> 
    setDF(rownames = mm$concepts_children_name) |> 
    as.matrix() |> 
    dist(method = "euclidean") |> 
    hclust(method = "ward.D2")

d3$term <- d3$concepts_children_name |>
    factor(levels = ht$labels[ht$order])

gr2 <- d3 |>
    ggplot(aes(year, term)) +
    
    geom_vline(xintercept = seq(1999.5, 2019.5, by = 1), color = "grey85", linewidth = .15) +
    geom_hline(yintercept = seq(.5, nrow(t1) + .5, by = 1), color = "grey85", linewidth = .15) +
    
    geom_tile(aes(fill = n_pmid), color = "grey75", linewidth = .15) +
    
    scale_x_continuous(expand = c(0, 0), breaks = seq(2002, 2024, by = 4)) +
    scale_y_discrete(expand = c(0, 0)) +
    
    scale_fill_stepsn(
        colors = c("#00429d","#73a2c6","#ffffe0","#f4777f","#93003a"),
        transform = "log2", breaks = c(2, 4, 8, 16),
        
        guide = guide_colorsteps(
            barheight = unit(16, "lines"),
            barwidth = unit(.5, "lines")
        )
    ) +
    
    theme_minimal() +
    
    theme(
        # legend.position = "top",
        # legend.title.position = "top",
        
        axis.text.y = element_text(size = 6),
        
        axis.title = element_blank(),
        
        panel.grid = element_blank()
    )

ggsave(
    plot = gr2, filename = "output/figures/n_pmid.concepts_children.png",
    width = 6, height = 12, units = "in", dpi = 600
)


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









