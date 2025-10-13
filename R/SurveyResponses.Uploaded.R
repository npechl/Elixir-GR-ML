library(googlesheets4)
library(ggplot2)
library(ggwordcloud)
library(RColorBrewer)

# google sheet with the survey responses
SheetURL="https://XXXXXXX"

data=as.data.frame(read_sheet(SheetURL))

# Process Q4
text=paste(data$`4. What do you consider to be your broad area(s) of specialist knowledge? (select all that apply)`,collapse = ", ")

# Split the string into individual words
words <- unlist(strsplit(text, ","))
words <- trimws(words)  # remove leading/trailing spaces

# Count word frequencies
word_freq <- table(words)

phrase_freq <- as.data.frame(table(words))

# Assign colors from Dark2 palette randomly
colors <- brewer.pal(8, "Dark2")
phrase_freq$Color <- sample(colors, nrow(phrase_freq), replace = TRUE)

phrase_freq$Size <- phrase_freq$Freq/ min(phrase_freq$Freq)  # simple scaling


setEPS()
Qnum=4
FNAME=paste("wordcloud",Qnum,sep="")
postscript(paste(FNAME,".eps",sep = ""))

# Create word cloud
ggplot(phrase_freq, aes(label = words, size = Size, color = Color)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 30, guide = "none") +  # larger max size
  scale_color_identity() +
  theme_minimal()


dev.off()
Sys.sleep(3)
system(paste("epstopdf ",FNAME,".eps",sep=""))
system(paste("pdfcrop ",FNAME,".pdf",sep = ""))

com=paste("mv ",FNAME,"-crop.pdf question",Qnum,"_wordcloud",".pdf",sep = "")
system(com)
com=paste("rm ",FNAME,".pdf",sep = "")
system(com)
com=paste("rm ",FNAME,".eps",sep = "")
system(com)


# Order phrases by frequency
phrase_freq <- phrase_freq[order(phrase_freq$Freq, decreasing = TRUE), ]


setEPS()
Qnum=4
FNAME=paste("wordcloud",Qnum,sep="")
postscript(paste(FNAME,".eps",sep = ""))


ggplot(phrase_freq, aes(x = reorder(words, Freq), y = Freq, fill = Freq)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # flip axes for better readability
  scale_fill_gradient(low = "skyblue", high = "darkblue") +
  labs(title = "Phrase Frequency", x = "Phrase", y = "Frequency") +
  theme_minimal()


dev.off()

system(paste("epstopdf ",FNAME,".eps",sep=""))
system(paste("pdfcrop ",FNAME,".pdf",sep = ""))

com=paste("mv ",FNAME,"-crop.pdf question",Qnum,"_barplot",".pdf",sep = "")
system(com)
com=paste("rm ",FNAME,".pdf",sep = "")
system(com)
com=paste("rm ",FNAME,".eps",sep = "")
system(com)


# Process Q13

text_var=paste(data$`13. What current or future AI technologies from the following list (selected from within the ACM CCS ontology) below do you consider to be more impactful in changing the current biosciences research capabilities, or deliver fundamentally new ones?`,collapse = ", ")
phrases <- trimws(unlist(strsplit(text_var, ",")))

# Count phrase frequencies
phrase_freq <- as.data.frame(table(phrases))
colnames(phrase_freq) <- c("Phrase", "Freq")

# Order phrases by frequency
phrase_freq <- phrase_freq[order(phrase_freq$Freq, decreasing = TRUE), ]

# Assign colors from Dark2 palette (recycling if needed)
colors <- brewer.pal(8, "Dark2")
phrase_freq$Color <- rep(colors, length.out = nrow(phrase_freq))



setEPS()
Qnum=13
FNAME=paste("wordcloud",Qnum,sep="")
postscript(paste(FNAME,".eps",sep = ""))


# Create bar chart
ggplot(phrase_freq, aes(x = reorder(Phrase, Freq), y = Freq, fill = Color)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # flip axes so long phrases fit nicely
  scale_fill_identity() +  # use the assigned colors directly
  labs(title = "Phrase Frequency Bar Chart", x = "Phrase", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12))  # bigger y-axis labels

dev.off()
system(paste("epstopdf ",FNAME,".eps",sep=""))
system(paste("pdfcrop ",FNAME,".pdf",sep = ""))

com=paste("mv ",FNAME,"-crop.pdf question",Qnum,"_chart",".pdf",sep = "")
system(com)
com=paste("rm ",FNAME,".pdf",sep = "")
system(com)
com=paste("rm ",FNAME,".eps",sep = "")
system(com)

