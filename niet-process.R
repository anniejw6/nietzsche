library(tm)
library(SnowballC)
library(slam)
library(Matrix)

# Import data
dat <- read.csv("nietzsche.csv", stringsAsFactors = F)
aphorism <- dat$text


##### Text Parsing #####
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


##### Dimensonality Reduction #####
library(tsne)

text <- as.data.frame(as.matrix(dtm.Matrix))
text$names <- row.names(text)

# Create display function for t-sne
colors = rainbow(length(unique(text$names)))
names(colors) = unique(text$names)
ecb = function(x,y){
  plot(x,t='n')
  #text(x,labels=text$names, col=colors[text$names])
  text(x,labels=text$names)
}

x = tsne(text[, 1:(ncol(text) - 1)], 
         initial_dims = 30,
         epoch_callback = ecb, perplexity = 5,
         max_iter = 3000
)


x <- data.frame(x)
x$name <- text$names

##### Clustering  #######

cluster <- hclust(dist(x[, 1:2]), method = 'ward.D')

# Store clusters varying in size from 5 to 20
for(i in c(5, 10, 15, 20)){
  clustering <- cutree(cluster, k=i)
  x[paste0('clust', i)] <- clustering
}

# clustering <- cutree(cluster, k= 10) # Just pick whichever one you like
names(clustering) <- x$name

# Print our clusters nicely:
# once with random colors, once by cluster colors
par(mfrow=c(1,2))

plot(x[,1:2], t = 'n')
colors = rainbow(length(unique(x$name)))
names(colors) = unique(x$name)
text(x,labels=x$name, col=colors[x$name])

plot(x[,1:2], t = 'n')
colors = sample(rainbow(length(unique(clustering))), 
	length(unique(clustering)))
names(colors) = unique(clustering)
text(x,labels=x$name, col=colors[clustering])

# Add text and store results
x$text <- dat$text
write.csv(x, 'results.csv', row.names = F)
