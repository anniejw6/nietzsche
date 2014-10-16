library(tm)
library(SnowballC)

dat <- read.csv("nietzsche.csv", stringsAsFactors = F)
aphorism <- dat$text
aphorism <- iconv(aphorism, to = "utf-8", sub="")
aphorism <- gsub("[^a-zA-Z0-9 ]","",aphorism)
corp <- Corpus(VectorSource(aphorism)) # make a corpus object

#corp <- tm_map(corp, content_transformer(tolower), mc.cores = 1) # make everything lowercase

corp <- tm_map(corp, removeWords, c(stopwords("english"), stopwords("smart"))) # remove stopwords

corp <- tm_map(corp, removePunctuation) # remove punctuation

corp <- tm_map(corp, removeNumbers) # remove numbers

corp <- tm_map(corp, stripWhitespace) # get rid of extra spaces

########################
# Make the DTM
########################
dtm.tm <- DocumentTermMatrix(x=corp, control=list(weighting=weightTf)) # dtm of raw word counts


# Convert the document term matrix to a `Matrix` sparse matrix & drop additional terms
library(slam)
library(Matrix)
########################
# Conversion
########################
vocab <- Terms(dtm.tm) # preserve our vocabulary
docnames <- Docs(dtm.tm) # preserve document names

dtm.Matrix <- sparseMatrix(i=dtm.tm$i, j=dtm.tm$j, x=dtm.tm$v,  # converts to a sparse dtm
                           dims=c(dtm.tm$nrow, dtm.tm$ncol))

colnames(dtm.Matrix) <- vocab
rownames(dtm.Matrix) <- docnames

dim(dtm.Matrix) # how big is this matrix?

########################
# Drop Additional Terms
########################

doc.freq <- colSums(dtm.Matrix > 0) # get document frequency

# drop terms in half or more of the documents or words that appear in 3 or fewer documents
dtm.Matrix <- dtm.Matrix[ , doc.freq < nrow(dtm.Matrix)/2 & doc.freq > 3 ]

dim(dtm.Matrix) # now how big is this matrix?

# preview
dtm.Matrix[ 1:10, 1:10 ]


# ####Make our DTM into a TF-IDF-weighted DTM ####

idf <- log(nrow(dtm.Matrix) / colSums(dtm.Matrix > 0))

dtm.Matrix <- dtm.Matrix / rowSums(dtm.Matrix) # normalize document length

dtm.Matrix <- t(dtm.Matrix) * idf # transpose for correct multiplication

dtm.Matrix <- t(dtm.Matrix) # get it back in the right format



### Get a cosine similarity matrix & convert it to an R `dist()` object

# make every vector unit length
dtm.Matrix <- t(apply(dtm.Matrix, 1, function(x){
  x <- x / sqrt(sum( x * x ))
}))

# matrix multiplication is fast in R!
cosine.sim <- dtm.Matrix %*% t(dtm.Matrix)

# make me a dist() object
cos.dist <- as.dist( 1 - cosine.sim )


### Hierarchical clustering
clustering <- hclust(cos.dist, method="ward")
clustering <- cutree(clustering, k=10) # 10 clusters is arbitrary


# display our clusters nicely

for( j in sort(unique(clustering))){
  print("_________________________________________")
  print(paste("cluster ", j))
  print(names(clustering)[ clustering == j ])
  print("_________________________________________")
}