read_mesh <- function(path) {
  dt <- path |> fread()

  t1 <- dt$`Tree Numbers` |>
    str_squish() |>
    str_sub(2, -2) |>
    str_split("\\,") |>
    lapply(str_squish) |>
    lapply(function(x) {
      data.table("tree_id" = x)
    }) |>
    rbindlist(idcol = "id")

  dt <- cbind(dt[t1$id, 1:2], t1[, -1])

  dt$tree_id <- fifelse(dt$tree_id == "", dt$`Descriptor UI`, dt$tree_id)

  return(dt)
}

search_mesh <- function(mesh_dt, mesh_ids, level = 1) {
  t1 <- mesh_dt[which(`Descriptor UI` %in% mesh_ids)]

  if (level == 0) {
    t1$tag <- fifelse(
      t1$`Descriptor Name` %in% c("Female", "Male"),
      t1$`Descriptor Name`, t1$tree_id |> str_sub(1, 1)
    )

    t1$tag_name <- fcase(
      t1$tag == "A", "Anatomy",
      t1$tag == "B", "Organisms",
      t1$tag == "C", "Diseases",
      t1$tag == "D", "Chemicals and Drugs",
      t1$tag == "E", "Analytical, Diagnostic and Therapeutic Techniques, and Equipment",
      t1$tag == "F", "Psychiatry and Psychology",
      t1$tag == "G", "Phenomena and Processes",
      t1$tag == "H", "Disciplines and Occupations",
      t1$tag == "I", "Anthropology, Education, Sociology, and Social Phenomena",
      t1$tag == "J", "Technology, Industry, and Agriculture",
      t1$tag == "K", "Humanities",
      t1$tag == "L", "Information Science",
      t1$tag == "M", "Named Groups",
      t1$tag == "N", "Health Care",
      t1$tag == "V", "Publication Characteristics",
      t1$tag == "Z", "Geographicals",
      t1$tag == "Female", "Female",
      t1$tag == "Male", "Male"
    )

    t1 <- t1[, c("Descriptor UI", "Descriptor Name", "tag", "tag_name"), with = FALSE] |> unique()

    colnames(t1) <- c("query_id", "query_name", "result_id", "result_name")
  } else {
    t1$tag <- t1$tree_id |>
      str_split("\\.") |>
      lapply(function(x) x[seq_len(level)] |> paste(collapse = ".")) |>
      unlist()

    t1 <- t1 |> merge(mesh_dt, by.x = "tag", by.y = "tree_id")

    t1 <- t1[, c("Descriptor UI.x", "Descriptor Name.x", "Descriptor UI.y", "Descriptor Name.y"), with = FALSE] |> unique()

    colnames(t1) <- c("query_id", "query_name", "result_id", "result_name")
  }

  return(t1)
}
