# Load
library("tm")
library("corpus")
library("quanteda")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")
# Read the text file from local machine , choose file interactively
text <- read.csv("D:\\Work\\Git\\Text Analytics\\SecDefDetailed.txt",
        header = TRUE, sep = "\t")
# Load the data as a corpus
text_doc <- Corpus(VectorSource(text))
#Replacing "/", "@" and "|" with space
to_space <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
text_doc <- tm_map(text_doc, to_space, "/")
text_doc <- tm_map(text_doc, to_space, "@")
# Convert the text to lower case
text_doc <- tm_map(text_doc, content_transformer(tolower))
# Remove numbers
text_doc <- tm_map(text_doc, removeNumbers)
# Remove Punctuations
text_doc <- tm_map(text_doc, removePunctuation)
# Remove english common stopwords
text_doc <- tm_map(text_doc, removeWords, stopwords("english"))
# Remove your own stop word
# specify your custom stopwords as a character vector
text_doc <- tm_map(text_doc, removeWords, c("country",
        "company", "team", "region", "can", "re"))
# BUILD A TERM DOCUMENT MATRIX
text_doc_dtm <- TermDocumentMatrix(text_doc)
dtm_m <- as.matrix(text_doc_dtm)
# Sort by decreasing value of frequency
dtm_v <- sort(rowSums(dtm_m), decreasing = TRUE)
dtm_d <- data.frame(word = names(dtm_v), freq = dtm_v)
height <- c(head(dtm_v, 5))
names <- c(names(height))
# Print TDM
# Display the top 5 most frequent words
print(top5 <- head(dtm_d, 5))
# Plot the most frequent words
barplot(height, las = 2, names.arg = names,
        col = "lightgreen", main = "Top 5 most frequent words",
        ylab = "Word frequencies")
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 1,
        max.words = 200, random.order = FALSE, rot.per = 0.35,
        colors = brewer.pal(8, "Dark2"))