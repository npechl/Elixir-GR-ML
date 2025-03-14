

# load libraries ----------------------------

library(data.table)
library(stringr)

# load OPEN ALEX -------------------------

d0 <- fread("./input/output_openalex-affil")

# filter entries above 2000 ------------------

d1 <- d0[order(year, pmid)]
d1 <- d1[which(year >= 2000)]

# split concepts --------------------------------
 
t1 <- d1$`concepts_AI-ML-NLP_L1(id|level|name|score)` |> str_split("\\,") |> lapply(function(x) data.table("concepts" = x)) |> rbindlist(idcol = "id")
t1 <- cbind(t1, d1[t1$id])
t1 <- t1[ , c("doi", "pmid", "year", "countries", "concepts"), with = FALSE] |> unique()

# t1$id                                           <- NULL
# t1$`concepts_AI-ML-NLP_L1(id|level|name|score)` <- NULL

# split concepts_children ------------------------------------

t2 <- d1$`concepts_children_L2(id|level|name|score)` |> str_split("\\,") |> lapply(function(x) data.table("concepts_children" = x)) |> rbindlist(idcol = "id")
t2 <- cbind(t2, d1[t2$id])
t2 <- t2[, c("doi", "pmid", "year", "countries", "concepts_children"), with = FALSE] |> unique()

# t2$id                                          <- NULL
# t2$`concepts_children_L2(id|level|name|score)` <- NULL

# clean concepts --------------------

t1$concepts_id    <- t1$concepts |> str_split_i("\\|", 1)
t1$concepts_level <- t1$concepts |> str_split_i("\\|", 2) |> as.numeric()
t1$concepts_name  <- t1$concepts |> str_split_i("\\|", 3)
t1$concepts_score <- t1$concepts |> str_split_i("\\|", 4) |> as.numeric()

t2$concepts_children_id    <- t2$concepts_children |> str_split_i("\\|", 1)
t2$concepts_children_level <- t2$concepts_children |> str_split_i("\\|", 2) |> as.numeric()
t2$concepts_children_name  <- t2$concepts_children |> str_split_i("\\|", 3)
t2$concepts_children_score <- t2$concepts_children |> str_split_i("\\|", 4) |> as.numeric()

t1$concepts          <- NULL
t2$concepts_children <- NULL

# clean RAM -----------------

gc()

# load MESH terms ------------------

m0 <- fread("./input/MeSH from PMID result_v2.csv")

# merge mesh terms -------------------------------

d1 <- cbind(d1, m0[match(d1$pmid, m0$pmid), -1])
d1 <- d1[, c("doi", "pmid", "year", "countries", "title", "MeSH terms", "MeSH IDs"), with = FALSE] |> unique()

ta <- d1$`MeSH terms` |> str_split("\\;") |> lapply(str_squish) |> lapply(function(x) data.table("MeSH term" = x)) |> rbindlist(idcol = "id")
# tb <- d1$`MeSH IDs` |> str_split("\\,") |> lapply(str_squish) |> lapply(function(x) data.table("MeSH ID" = x)) |> rbindlist(idcol = "id")

t3 <- cbind(d1[ta$id, 1:5], ta[, -1])

t3$title <- t3$title |> str_replace_all("\\n", " ") |> str_squish()

# dp = cbind(d1, m0[match(d1$pmid, m0$pmid), -1])
# 
# dp <- dp[which(`MeSH terms` == "")]
# dp <- dp[which(countries != "")]

# clean environment ---------

rm(d1, m0, ta, tb)
gc()

# print clean document ------------

t1 <- t1[order(year, pmid, doi)]
t2 <- t2[order(year, pmid, doi)]
t3 <- t3[order(year, pmid, doi)]

# writexl::write_xlsx(d2, "data/clean-data.xlsx")

# fwrite(d2, "data/clean-data.tsv", row.names = FALSE, quote = FALSE, sep = "\t")
# fwrite(dp, "data/missingMESH.tsv", row.names = FALSE, quote = FALSE, sep = "\t")

fwrite(t1, "./input/clean-concepts.tsv", row.names = FALSE, quote = FALSE, sep = "\t")
fwrite(t2, "./input/clean-concepts-childrean.tsv", row.names = FALSE, quote = FALSE, sep = "\t")
fwrite(t3, "./input/clean-mesh.tsv", row.names = FALSE, quote = FALSE, sep = "\t")






