
# concepts ---------------------

analyse_concepts <- function(x) {
    
    y = x[, by = .(year, concepts_name), .(
        n_doi  = doi |> unique() |> length(),
        n_pmid = pmid |> unique() |> length()
    )]
    
    o1 <- list(
        "No. of pmid" = y |> dcast(concepts_name ~ year, value.var = "n_pmid", fill = 0),
        "No. of doi"  = y |> dcast(concepts_name ~ year, value.var = "n_doi", fill = 0)
    )
    
    return(list("tables" = o1, "original" = y))
}

plot_concepts <- function(x) {
    
    x |>
        ggplot(aes(year, n_pmid)) +
        
        geom_smooth(aes(color = concepts_name, fill = concepts_name), alpha = .1, lineend = "round") +
        
        geom_point(aes(fill = concepts_name, color = concepts_name), shape = 21, size = 2, stroke = .25) +
        
        scale_color_manual(values = c("#CD534C", "#4A6990", "#79AF97") |> darken(.25)) +
        scale_fill_manual(values = c("#CD534C", "#4A6990", "#79AF97") |> lighten(.25)) +
        
        scale_x_continuous(breaks = seq(2000, 2024, by = 3)) +
        scale_y_continuous(transform = "log2", labels = scales::comma, limits = c(1, NA)) +
        
        theme_minimal() +
        
        theme(
            legend.position = "top",
            legend.title.position = "top",
            
            panel.grid.major = element_line(linetype = "dashed", lineend = "round", color = "grey85"),
            panel.grid.minor = element_line(linetype = "dashed", lineend = "round", color = "grey85"),
            
            plot.margin = margin(20, 20, 20, 20)
        )
    
}

# concepts children -----------------------

analyse_concepts_children <- function(x, my_filter = .001) {
    
    d2 <- x[, by = .(year, concepts_children_name), .(
        n_doi = doi |> unique() |> length(),
        n_pmid = pmid |> unique() |> length()
    )]
    
    
    d2 <- d2[which(!is.na(concepts_children_name) & concepts_children_name != "")]
    
    t2 <- d2[, by = concepts_children_name, .(cumn_pmid = n_pmid |> sum()) ]
    t2 <- t2[order(-cumn_pmid)]
    
    d2$concepts_children_name <- d2$concepts_children_name |> factor(levels = t2$concepts_children_name)
    
    o1 <- list(
        "No. of pmid" = d2 |> dcast(concepts_children_name ~ year, value.var = "n_pmid", fill = 0),
        "No. of doi"  = d2 |> dcast(concepts_children_name ~ year, value.var = "n_doi", fill = 0)
    )
    
    
    d2$freq_doi  <- d2$n_doi / sum(d2$n_doi)
    d2$freq_pmid <- d2$n_pmid / sum(d2$n_pmid)
    
    t1 <- d2[, by = concepts_children_name, .(
        cumn_pmids = freq_pmid |> sum(),
        cumn_dois  = freq_doi |> sum()
    )]
    
    t1 <- t1[which(cumn_pmids >= my_filter)]
    
    
    d3 <- d2[which(concepts_children_name %in% t1$concepts_children_name)]
    
    
    return(list("tables" = o1, "filtered" = d3))
}

plot_concepts_children <- function(x, my_palette, my_direction = 1) {
    
    x$concepts_children_name <- x$concepts_children_name |> as.character()
    
    mm <- x |> dcast(concepts_children_name ~ year, value.var = "n_pmid", fill = 0)
    
    ht <- mm[, -1] |> 
        setDF(rownames = mm$concepts_children_name) |> 
        as.matrix() |> 
        dist(method = "euclidean") |> 
        hclust(method = "ward.D2")
    
    x$term <- x$concepts_children_name |> factor(levels = ht$labels[ht$order])
    
    ploto <- x |>
        ggplot(aes(year, term)) +
        
        geom_vline(xintercept = seq(1999.5, max(x$year) + .5, by = 1), color = "grey", linewidth = .15) +
        geom_hline(yintercept = seq(.5, x$term |> unique() |> length() + .5, by = 1), color = "grey", linewidth = .15) +
        
        geom_tile(aes(fill = n_pmid), color = "grey", linewidth = .15) +
        
        scale_x_continuous(expand = c(0, 0), breaks = seq(2002, max(x$year), by = 4)) +
        scale_y_discrete(expand = c(0, 0)) +
        
        scale_fill_paletteer_c(
            palette = my_palette, direction = my_direction, transform = "log2",
            guide = guide_colorbar(
                barheight = unit(16, "lines"),
                barwidth = unit(.5, "lines")
            )
        ) +
        
        theme_minimal() +
        
        theme(
            axis.ticks.x = element_line(color = "grey", linewidth = .15),
            
            axis.text.y = element_text(size = 6),
            axis.text.x = element_text(size = 8),
            
            axis.title = element_blank(),
            
            panel.grid = element_blank()
        )
    
    return(ploto)
}

# MeSH terms -----------------------

analyse_mesh_terms <- function(x, my_filter = .001) {
    
    d3 <- x[, c("doi", "pmid", "year", "MeSH terms"), with = FALSE] |> unique()
    
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
    
    
    t2 <- d3[, by = `MeSH term`, .(cumn_pmid = n_pmid |> sum())]
    t2 <- t2[order(-cumn_pmid)]
    
    d3$`MeSH term` <- d3$`MeSH term` |> factor(levels = t2$`MeSH term`)
    
    o1 <- list(
        "No. of pmid" = d3 |> dcast(`MeSH term` ~ year, value.var = "n_pmid", fill = 0),
        "No. of doi"  = d3 |> dcast(`MeSH term` ~ year, value.var = "n_doi", fill = 0)
    )
    
    
    d3$freq_doi <- d3$n_doi / sum(d3$n_doi)
    d3$freq_pmid <- d3$n_pmid / sum(d3$n_pmid)
    
    t2 <- d3[, by = `MeSH term`, .(
        cumfreq_doi = freq_doi |> sum(),
        cumfreq_pmid = freq_pmid |> sum()
    )]
    
    t2 <- t2[which(cumfreq_pmid >= my_filter)]
    
    d3 <- d3[which(`MeSH term` %in% t2$`MeSH term`)]
    
    return(list("tables" = o1, "filtered" = d3))
    
}

plot_mesh_terms <- function(x, my_palette, my_direction = 1) {
    
    x$`MeSH term` <- x$`MeSH term` |> as.character()
    
    mm <- x |> dcast(`MeSH term` ~ year, value.var = "n_pmid", fill = 0)
    
    ht <- mm[, -1] |> 
        setDF(rownames = mm$`MeSH term`) |> 
        as.matrix() |> 
        dist(method = "euclidean") |> 
        hclust(method = "ward.D2")
    
    x$term <- x$`MeSH term` |> factor(levels = ht$labels[ht$order])
    
    o2 <- x |>
        ggplot(aes(year, term)) +
        
        geom_vline(xintercept = seq(1999.5, max(x$year) + .5, by = 1), color = "grey85", linewidth = .15) +
        geom_hline(yintercept = seq(.5, x$term |> unique() |> length() + .5, by = 1), color = "grey85", linewidth = .15) +
        
        geom_tile(aes(fill = n_pmid), color = "grey75", linewidth = .15) +
        
        scale_fill_paletteer_c(
            my_palette, direction = my_direction, transform = "log2",
            guide = guide_colorbar(
                barheight = unit(16, "lines"),
                barwidth = unit(.5, "lines")
            )
        ) +
        
        scale_x_continuous(expand = c(0, 0), breaks = seq(2002, max(x$year), by = 4)) +
        
        scale_y_discrete(expand = c(0, 0)) +
        
        theme_minimal() +
        
        theme(
            axis.text.y = element_text(size = 6),
            
            axis.ticks.x = element_line(color = "grey", linewidth = .15),
            
            panel.grid = element_blank(),
            
            axis.title = element_blank()
        )
    
    return(o2)
}