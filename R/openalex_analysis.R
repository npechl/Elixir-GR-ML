
rm(list = ls()) 

# load libraries ----------------------------


library(data.table)
library(stringr)

# load data -------------------------

d0 <- fread("data/openalex/output_openalex-affil")

# filter `GR` ------------------

index <- d0$countries |> str_detect("GR")

d1 <- d0[index]

# split concepts ---------------
 
t1 <- d1$`concepts_AI-ML-NLP_L1(id|level|name|score)` |> 
    str_split("\\,") |>
    lapply(function(x) data.table("concepts" = x)) |>
    rbindlist(idcol = "id")

t1 <- cbind(t1, d1[t1$id])

t1$id                                           <- NULL
t1$`concepts_AI-ML-NLP_L1(id|level|name|score)` <- NULL


# split concepts_children --------------------

t2 <- t1$`concepts_children_L2(id|level|name|score)` |> 
    str_split("\\,") |>
    lapply(function(x) data.table("concepts_children" = x)) |>
    rbindlist(idcol = "id")

t2 <- cbind(t2, t1[t2$id])

t2$id                                          <- NULL
t2$`concepts_children_L2(id|level|name|score)` <- NULL

# clean concepts --------------------

t2$concepts_id    <- t2$concepts |> str_split_i("\\|", 1)
t2$concepts_level <- t2$concepts |> str_split_i("\\|", 2) |> as.numeric()
t2$concepts_name  <- t2$concepts |> str_split_i("\\|", 3)
t2$concepts_score <- t2$concepts |> str_split_i("\\|", 4) |> as.numeric()

t2$concepts_children_id    <- t2$concepts_children |> str_split_i("\\|", 1)
t2$concepts_children_level <- t2$concepts_children |> str_split_i("\\|", 2) |> as.numeric()
t2$concepts_children_name  <- t2$concepts_children |> str_split_i("\\|", 3)
t2$concepts_children_score <- t2$concepts_children |> str_split_i("\\|", 4) |> as.numeric()

t2$concepts          <- NULL
t2$concepts_children <- NULL

# t2$concepts_level |> unique()
# t2$concepts_children_level |> unique()
# 
# t2$concepts_name |> unique()
# t2$concepts_children_name |> unique()


# load mesh terms ------------------

m0 <- fread("data/mesh/MeSH from PMID result.csv")

# merge mesh terms -------------------------------

d2 = cbind(t2, m0[match(t2$pmid, m0$pmid), -1])

# clean environment ---------

rm(d1, m0, t1, t2, index)
gc()

# print clean document ------------

d2 <- d2[order(year, pmid, doi, -concepts_score, -concepts_children_score)]

writexl::write_xlsx(d2, "data/clean-data.xlsx")







