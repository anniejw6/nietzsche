text <- as.data.frame(as.matrix(dtm.Matrix))
#text <- text[,!is.ana(text[,1])]

# Drop text with no terms

library(tsne)

#text$names <- apply(text, 1, function(x) names(x)[which.max(as.numeric(x))])
text$names <- row.names(text)
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

#### Clustering  ####

cluster <- hclust(dist(x[, 1:2]), method = 'ward.D')


for(i in c(5, 10, 15, 20)){
  clustering <- cutree(cluster, k=i)
  x[paste0('clust', i)] <- clustering
}

names(clustering) <- text$names

# display our clusters nicely
par(mfrow=c(1,2))

plot(x[,1:2], t = 'n')
colors = rainbow(length(unique(x$name)))
names(colors) = unique(x$name)
text(x,labels=x$name, col=colors[x$name])

plot(x[,1:2], t = 'n')
colors = sample(rainbow(length(unique(clustering))), length(unique(clustering)))
names(colors) = unique(clustering)
text(x,labels=x$name, col=colors[clustering])



x$text <- dat$text
write.csv(x, 'results.csv', row.names = F)
