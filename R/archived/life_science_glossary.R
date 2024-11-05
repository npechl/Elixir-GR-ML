


life_science_glossary <- function(path) {
    
    fls <- path |> list.files(full.names = TRUE)
    
    glossary <- list()
    
    for(i in seq_along(fls)) glossary[[i]] = fls[i] |> readLines()
    
    glossary <- glossary |> unlist() |> str_to_lower()
    
    return(glossary)
}