

analyse_concepts <- function(x) {
    
    y = x[, by = .(year, concepts_name), .(
        n_doi  = doi |> unique() |> length(),
        n_pmid = pmid |> unique() |> length()
    )]
    
    o1 <- list(
        "No. of pmid" = y |> dcast(concepts_name ~ year, value.var = "n_pmid", fill = 0),
        "No. of doi"  = y |> dcast(concepts_name ~ year, value.var = "n_doi", fill = 0)
    )
    
    o2 <- y |>
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
    
    return(list("tables" = o1, "plot" = o2))
}

analyse_concepts_children <- function(x) {
    
    d2 <- x[, by = .(year, concepts_children_name), .(
        n_doi = doi |> unique() |> length(),
        n_pmid = pmid |> unique() |> length()
    )]
    
    
    d2 <- d2[which(!is.na(concepts_children_name) & concepts_children_name != "")]
    
    d2$freq_doi  <- d2$n_doi / sum(d2$n_doi)
    d2$freq_pmid <- d2$n_pmid / sum(d2$n_pmid)
    
    t1 <- d2[, by = concepts_children_name, .(
        cumn_pmids = freq_pmid |> sum(),
        cumn_dois  = freq_doi |> sum()
    )]
    
    t1 <- t1[which(cumn_pmids >= .001)]
    
    
    d3 <- d2[which(concepts_children_name %in% t1$concepts_children_name)]
    mm <- d3 |> dcast(concepts_children_name ~ year, value.var = "n_pmid", fill = 0)
    
    ht <- mm[, -1] |> 
        setDF(rownames = mm$concepts_children_name) |> 
        as.matrix() |> 
        dist(method = "euclidean") |> 
        hclust(method = "ward.D2")
    
    d3$term <- d3$concepts_children_name |> factor(levels = ht$labels[ht$order])
    
    
    o1 <- list(
        "No. of pmid" = d2 |> dcast(concepts_children_name ~ year, value.var = "n_pmid", fill = 0),
        "No. of doi"  = d2 |> dcast(concepts_children_name ~ year, value.var = "n_doi", fill = 0)
    )
    
    
    o2 <- d3 |>
        ggplot(aes(year, term)) +
        
        geom_vline(xintercept = seq(1999.5, max(d3$year) + .5, by = 1), color = "grey", linewidth = .15) +
        geom_hline(yintercept = seq(.5, nrow(t1) + .5, by = 1), color = "grey", linewidth = .15) +
        
        geom_tile(aes(fill = n_pmid), color = "grey", linewidth = .15) +
        
        scale_x_continuous(expand = c(0, 0), breaks = seq(2002, max(d3$year), by = 4)) +
        scale_y_discrete(expand = c(0, 0)) +
        
        # scale_fill_viridis_c(
        #     transform = "log2", option = "magma", direction = 1,
        #     guide = guide_colorbar(
        #         barheight = unit(16, "lines"),
        #         barwidth = unit(.5, "lines")
        #     )
        # ) +
        
        scale_fill_paletteer_c(
            "grDevices::Greens 3", direction = -1, transform = "log2",
            guide = guide_colorbar(
                barheight = unit(16, "lines"),
                barwidth = unit(.5, "lines")
            )

        ) +
        
        # scale_fill_continuous(values = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 30)) +
        
        # scale_fill_stepsn(
        #     colors = c("#00429d","#73a2c6","#ffffe0","#f4777f","#93003a"),
        #     transform = "log2", # breaks = seq(min(d3$n_pmid), max(d3$n_pmid), length.out = 4),
        #     
        #     guide = guide_colorsteps(
        #         barheight = unit(16, "lines"),
        #         barwidth = unit(.5, "lines")
        #     )
        # ) +
        
        theme_minimal() +
        
        theme(
            # legend.position = "top",
            # legend.title.position = "top",
            
            axis.text.y = element_text(size = 6),
            axis.text.x = element_text(size = 8),
            
            axis.title = element_blank(),
            
            panel.grid = element_blank()
        )
    
    return(list("tables" = o1, "plot" = o2))
}