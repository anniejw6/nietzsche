library(tm)
library(SnowballC)
library(slam)
library(Matrix)

# Import data
dat <- read.csv("nietzsche.csv", stringsAsFactors = F)
aphorism <- dat$text

# Remove Non-Ascii characters
aphorism <- iconv(aphorism, to = "utf-8", sub="")
aphorism <- gsub("[^a-zA-Z0-9 ]","",aphorism)

# Make a corpus object
corp <- Corpus(VectorSource(aphorism))
# Remove common words
corp <- tm_map(corp, removeWords, c(stopwords("english"), stopwords("smart")))
# Remove punc
corp <- tm_map(corp, removePunctuation)
# Remove numbers
corp <- tm_map(corp, removeNumbers)
# Remove extra spaces
corp <- tm_map(corp, stripWhitespace)

# Make word-frequency matrix
dtm.tm <- DocumentTermMatrix(x=corp, control=list(weighting=weightTf))

# Convert to sparse matrix
vocab <- Terms(dtm.tm) # vocab
docnames <- Docs(dtm.tm) # names
# Create sparse matrix
dtm.Matrix <- sparseMatrix(i=dtm.tm$i, j=dtm.tm$j, x=dtm.tm$v,
                           dims=c(dtm.tm$nrow, dtm.tm$ncol))
colnames(dtm.Matrix) <- vocab
rownames(dtm.Matrix) <- docnames

#dim(dtm.Matrix)

# Calculate TF-IDF-weighted matrix

idf <- log(nrow(dtm.Matrix) / colSums(dtm.Matrix > 0))

dtm.Matrix <- dtm.Matrix / rowSums(dtm.Matrix)

dtm.Matrix <- t(dtm.Matrix) * idf

dtm.Matrix <- t(dtm.Matrix)
