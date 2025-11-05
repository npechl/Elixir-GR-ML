

# load libraries -----------------------------------

library(data.table)
library(stringr)

library(ggplot2)
library(tidytext)

library(colorspace)

library(extrafont)

# input data ------------------------------------

dir.create("input/survey")

download.file(
  url = "https://zenodo.org/records/17414392/files/A%20survey%20on%20Machine%20Learning%20for%20Life%20Sciences%20in%20Greece%20(Responses)%20-%202023.csv?download=1",
  destfile = "input/survey/survey.csv"
)

# google sheet with the survey responses
SheetURL <- "./input/survey/survey.csv"

data <- SheetURL |> fread()

# drop out --------------------------------------------

# Process Q4
# text <- paste(data$`4. What do you consider to be your broad area(s) of specialist knowledge? (select all that apply)`, collapse = ", ")
# 
# # Split the string into individual words
# words <- unlist(strsplit(text, ","))
# words <- trimws(words) # remove leading/trailing spaces
# 
# # Count word frequencies
# word_freq <- table(words)
# 
# phrase_freq <- as.data.frame(table(words))
# 
# # Assign colors from Dark2 palette randomly
# colors <- brewer.pal(8, "Dark2")
# phrase_freq$Color <- sample(colors, nrow(phrase_freq), replace = TRUE)
# 
# phrase_freq$Size <- phrase_freq$Freq / min(phrase_freq$Freq) # simple scaling

# phrase_freq$words <- phrase_freq$words |> str_wrap(width = 20) |> str_replace_all("\\n", "<br>")


# setEPS()
# Qnum <- 4
# FNAME <- paste("wordcloud", Qnum, sep = "")
# postscript(paste(FNAME, ".eps", sep = ""))

# Create word cloud
# gr <- phrase_freq |>
#   ggplot() +
#   geom_text_wordcloud(aes(label = words, size = Size, color = Color), use_richtext = TRUE, shape = "square") +
#   scale_size_area(max_size = 10) + # larger max size
#   scale_color_identity() +
#   theme_void()
# 
# 
# dev.off()
# Sys.sleep(3)
# system(paste("epstopdf ", FNAME, ".eps", sep = ""))
# system(paste("pdfcrop ", FNAME, ".pdf", sep = ""))
# 
# com <- paste("mv ", FNAME, "-crop.pdf question", Qnum, "_wordcloud", ".pdf", sep = "")
# system(com)
# com <- paste("rm ", FNAME, ".pdf", sep = "")
# system(com)
# com <- paste("rm ", FNAME, ".eps", sep = "")
# system(com)
# 
# 
# # Order phrases by frequency
# phrase_freq <- phrase_freq[order(phrase_freq$Freq, decreasing = TRUE), ]
# 
# 
# setEPS()
# Qnum <- 4
# FNAME <- paste("wordcloud", Qnum, sep = "")
# postscript(paste(FNAME, ".eps", sep = ""))


# Figure 1A -----------------------------------------

plot_df <- data[["4. What do you consider to be your broad area(s) of specialist knowledge? (select all that apply)"]] |>
    str_split("\\,") |>
    lapply(str_squish) |>
    lapply(function(x) data.table("label" = x)) |>
    rbindlist(idcol = "id")

plot_df <- plot_df[, by = label, .N]
plot_df$Freq <- plot_df$N / 109

plot_df <- plot_df[order(-N, label)]

my_col <- c('#00429d', '#73a2c6', '#ffffe0', '#f4777f', '#93003a')

gr1 <- plot_df |>
    ggplot(aes(Freq, reorder(label, N))) +
    geom_col(aes(fill = Freq), color = "grey10", linewidth = .25) +
    geom_text(aes(label = N), position = position_nudge(x = .01), family = "Calibri", fontface = "bold") +
    scale_x_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, .21)) +
    
    scale_fill_stepsn(breaks = c(.04, .08, .12, .16), colors = my_col, labels = scales::percent,
                      guide = guide_colorsteps(barheight = unit(10, "line"), barwidth = unit(.5, "line"))) +
    
    theme_minimal(base_family = "Calibri") +
    theme(
        legend.position = "inside",
        legend.position.inside = c(.85, .35),
        legend.background = element_rect(fill = "grey90", color = NA),
        
        axis.title.y = element_blank(),
        axis.text.y = element_text(face = "bold", size = 11),
        
        panel.grid = element_line(lineend = "round"),
        
        plot.title = element_text(face = "bold", family = "Calibri", hjust = 1),
        plot.title.position = "plot"
    ) +
    
    labs(title = "What do you consider to be your broad area(s) of specialist knowledge?")




# ggplot(phrase_freq, aes(x = reorder(words, Freq), y = Freq, fill = Freq)) +
#   geom_bar(stat = "identity") +
#   coord_flip() + # flip axes for better readability
#   scale_fill_gradient(low = "skyblue", high = "darkblue") +
#   labs(title = "Phrase Frequency", x = "Phrase", y = "Frequency") +
#   theme_minimal()
# 
# 
# dev.off()
# 
# system(paste("epstopdf ", FNAME, ".eps", sep = ""))
# system(paste("pdfcrop ", FNAME, ".pdf", sep = ""))
# 
# com <- paste("mv ", FNAME, "-crop.pdf question", Qnum, "_barplot", ".pdf", sep = "")
# system(com)
# com <- paste("rm ", FNAME, ".pdf", sep = "")
# system(com)
# com <- paste("rm ", FNAME, ".eps", sep = "")
# system(com)

# Figure 1B -------------------------------------------------

plot_df <- data[["13. What current or future AI technologies from the following list (selected from within the ACM CCS ontology) below do you consider to be more impactful in changing the current biosciences research capabilities, or deliver fundamentally new ones?"]] |>
    str_split("\\,") |>
    lapply(str_squish) |>
    lapply(function(x) data.table("label" = x)) |>
    rbindlist(idcol = "id")

plot_df <- plot_df[, by = label, .N]
plot_df$Freq <- plot_df$N / 109

plot_df <- plot_df[order(-N, label)]

my_col <- c('#00429d', '#73a2c6', '#ffffe0', '#f4777f', '#93003a')

plot_df$label <- plot_df$label |> str_to_title()

gr2 <- plot_df |>
    ggplot(aes(Freq, reorder(label, N))) +
    geom_col(aes(fill = Freq), color = "grey10", linewidth = .25) +
    geom_text(aes(label = N), position = position_nudge(x = .01), family = "Calibri", fontface = "bold") +
    scale_x_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, .25)) +
    
    scale_fill_stepsn(breaks = c(.04, .08, .12, .16), colors = my_col, labels = scales::percent,
                      guide = guide_colorsteps(barheight = unit(10, "line"), barwidth = unit(.5, "line"))) +
    
    theme_minimal(base_family = "Calibri") +
    theme(
        legend.position = "inside",
        legend.position.inside = c(.85, .35),
        legend.background = element_rect(fill = "grey90", color = NA),
        
        axis.title.y = element_blank(),
        axis.text.y = element_text(face = "bold", size = 11),
        
        panel.grid = element_line(lineend = "round"),
        
        plot.title = element_text(face = "bold", family = "Calibri", hjust = 1),
        plot.title.position = "plot"
    ) +
    
    labs(title = "What current or future AI technologies from the following list (selected from within the ACM CCS\nontology) below do you consider to be more impactful in changing the current biosciences research\ncapabilities, or deliver fundamentally new ones?")

# text_var <- paste(data$`13. What current or future AI technologies from the following list (selected from within the ACM CCS ontology) below do you consider to be more impactful in changing the current biosciences research capabilities, or deliver fundamentally new ones?`, collapse = ", ")
# phrases <- trimws(unlist(strsplit(text_var, ",")))

# # Count phrase frequencies
# phrase_freq <- as.data.frame(table(phrases))
# colnames(phrase_freq) <- c("Phrase", "Freq")
# 
# # Order phrases by frequency
# phrase_freq <- phrase_freq[order(phrase_freq$Freq, decreasing = TRUE), ]
# 
# # Assign colors from Dark2 palette (recycling if needed)
# colors <- brewer.pal(8, "Dark2")
# phrase_freq$Color <- rep(colors, length.out = nrow(phrase_freq))
# 
# 
# setEPS()
# Qnum <- 13
# FNAME <- paste("wordcloud", Qnum, sep = "")
# postscript(paste(FNAME, ".eps", sep = ""))


# Create bar chart
# ggplot(phrase_freq, aes(x = reorder(Phrase, Freq), y = Freq, fill = Color)) +
#   geom_bar(stat = "identity") +
#   coord_flip() + # flip axes so long phrases fit nicely
#   scale_fill_identity() + # use the assigned colors directly
#   labs(title = "Phrase Frequency Bar Chart", x = "Phrase", y = "Frequency") +
#   theme_minimal() +
#   theme(axis.text.y = element_text(size = 12)) # bigger y-axis labels
# 
# dev.off()
# system(paste("epstopdf ", FNAME, ".eps", sep = ""))
# system(paste("pdfcrop ", FNAME, ".pdf", sep = ""))
# 
# com <- paste("mv ", FNAME, "-crop.pdf question", Qnum, "_chart", ".pdf", sep = "")
# system(com)
# com <- paste("rm ", FNAME, ".pdf", sep = "")
# system(com)
# com <- paste("rm ", FNAME, ".eps", sep = "")
# system(com)


# Save plots -----------------------

library(patchwork)

multi <- (gr1 / gr2) + plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(face = "bold", family = "Calibri", size = 18))

save_plot <- function(gr, name.out, w, h) {
    
    ggsave(gr, filename = paste0(name.out, ".pdf"), width = w, height = h, units = "in", device = cairo_pdf)
    ggsave(gr, filename = paste0(name.out, ".png"), width = w, height = h, units = "in", dpi = 600)
    
}

save_plot(multi, "./output/figures/figure_1", 9, 12)
