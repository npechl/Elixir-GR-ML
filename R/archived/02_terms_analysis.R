rm(list = ls())
gc()

# load libraries ------------------------

library(data.table)
library(stringr)

# read input table -------------------

df <- "data/terms/terms.csv" |> fread()

df$V1 <- NULL

# filter dates -------------

df <- df[which(year >= 2000)]

# split terms -----------------------

x <- df$terms |>
  str_split("\\,") |>
  lapply(function(q) data.table("term" = q)) |>
  rbindlist(idcol = "id")

x$doi <- df[x$id]$doi
x$year <- df[x$id]$year

rm(df)
gc()

# clean terms ---------------------------

x$term <- x$term |>
  str_squish() |>
  str_remove_all("\\[|\\]") |>
  str_remove_all("\\{|\\}") |>
  str_split_i("\\:", 1) |>
  str_sub(2, -2)


x$term_clean <- x$term |> str_to_lower()

# life science glossary --------------------

source("R/life_science_glossary.R")

w <- life_science_glossary("data/glossary/")

y <- x[which(term_clean %in% w)]
y <- x[which(doi %in% y$doi)]

y$tag <- ifelse(y$term_clean %in% w, "life_sciences", "other")

rm(w, life_science_glossary)

# find trends --------------------------------

z <- y[, by = .(year, term_clean, tag), .(N = id |> unique() |> length())]

z <- z[order(year, -N)]

z <- z |> split(z$tag)

plot_trends <- function(q) {
  library(ggplot2)
  library(paletteer)

  tag <- q$tag |> unique()

  q1 <- q[, by = term_clean, .(N = N |> sum())]

  q1 <- q1[order(-N)]

  q1 <- q1 |> head(10)

  q2 <- q[which(term_clean %in% q1$term_clean)]
  q3 <- q[which(!(term_clean %in% q1$term_clean))]

  gr <- ggplot() +
    geom_line(data = q3, aes(year, N, group = term_clean), color = "grey") +
    geom_line(data = q2, aes(year, N, group = term_clean, color = term_clean)) +
    scale_y_continuous(labels = scales::comma) +
    scale_color_manual(values = paletteer_d("ggsci::hallmarks_light_cosmic")) +
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.title = element_blank()
    ) +
    labs(
      y = "Number of research papers",
      title = tag
    )

  return(gr)
}


gr1 <- plot_trends(z$life_sciences)
gr2 <- plot_trends(z$other)

library(patchwork)


multi <- gr1 / gr2

ggsave(
  plot = multi, filename = "Rplot.pdf", device = cairo_pdf,
  width = 10, height = 10, units = "in"
)


# x$value = 1
#
#
# y = x[which(year == 2024)] |> dcast(term_clean ~ id, value.var = "value", fill = 0)
#
# m = y[, 2:ncol(y)] |> setDF(rownames = y$term_clean)
#
# t = m |> prcomp(center = TRUE, scale. = TRUE)
#
#
# library(ggplot2)
#
# t$x |>
#     ggplot(aes(PC4, PC5)) +
#     geom_point() +
#     scale_x_continuous(transform = scales::pseudo_log_trans(base = 10)) +
#     scale_y_continuous(transform = scales::pseudo_log_trans(base = 10))


# y = x[, by = term, .(N = id |> unique() |> length())]

# bio_terms = paste(
#     sep = "|",
#     "sequence",
#     "genomics|biology|bioinformatics",
#     "immuno|microbiome|virus|genome|genomic",
#     "pharma"
# )
#
# y = x[which(str_detect(term_clean, bio_terms))]
#
# 100 * (y$id |> unique() |> length()) / (x$id |> unique() |> length())
#
# y$term_clean |> unique()
