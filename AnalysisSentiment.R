library(lda)
library(tm)
df <- read.csv(file=paste('air_tweet_train.csv'), 
               col.names = c('sentiment','text'),
               stringsAsFactors = F)

# generate stop words
aircompanies_accounts <- c('VirginAmerica', 'United', 'SouthwestAir', 'JetBlue', 'Delta', 'USAirways', 'AmericanAir')
other_sw <- c('fly', 'flying', 'flight', 'flights', 'plane')
stop_words <- c(tm::stopwords("SMART"), tolower(aircompanies_accounts), other_sw )

# tweets
texts <- df$text
head(texts)

# tweet sentiments
sentiment <- df$sentiment
sentiment.f <- rep('Neutral', length(sentiment))
sentiment.f[sentiment < 0] <- 'Negative'
sentiment.f[sentiment > 0] <- 'Positive'
sentiment.f <- as.factor(sentiment.f)


# clean tweets
texts <- gsub("http[^[:space:]]+", " ", texts)  # remove http links
texts <- gsub("[^a-zA-Z']+", " ", texts)        # change non-letter sequences to whitespaces
texts <- gsub("(^ | $)", "", texts)             # remove whitespace at beginning and end of documents
texts <- tolower(texts)                         # force to lowercase

# split documents to words:
doc.list <- strsplit(texts, " ")
head(doc.list)
```
# compute table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

 
# remove terms that are stop words or occur fewer than 5 times or have length < 3:
del_idx <- names(term.table) %in% stop_words | term.table < 5 | nchar(names(term.table)) < 3
term.table <- term.table[!del_idx]
#  most popular words
head(term.table, 10) 
# least popular words
tail(term.table, 10)

#Now vocabulary becomes:
vocab <- names(term.table)
str(vocab)
#Prepare the documents for the package lda.
# put the documents into the format required by the lda package:
get.terms <- function(x) { # x is vector of words
        index <- match(x, vocab)
        index <- index[!is.na(index)]
        rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}

documents <- lapply(doc.list, get.terms)
head(documents)
shortVector<-c("cancelled","like","great","bad")
get.terms(shortVector)
match(shortVector, vocab)

# remove documents that do not contain words from vocab
notempty_documents_idx <- sapply(documents, function(doc) ncol(doc) > 0 )
head(notempty_documents_idx)
corp<-summary(notempty_documents_idx)
#After removing logical empty documents the corpus of documents that will be used to fit a model contains 547 documents.
suppressWarnings(library(lda))

# fit model
set.seed(60134)
num.topics <- 6
alpha <- 0.1 
eta <- 0.1
params <- sample(c(-1,0, 1), num.topics, replace=TRUE)
fit <- slda.em(documents=documents[notempty_documents_idx],
               K=num.topics,
               vocab=vocab,
               num.e.iterations=100,
               num.m.iterations=40,
               alpha=alpha, #Dirichlet distribution parmeter for prior for theta_d
               eta=eta,  #Dirichlet distribution parameter for prior for beta_k
               annotations=sentiment[notempty_documents_idx], 
               params=params,  #numeric vector of initial regression coefficients for EM
               variance=0.25, # initial value for response variance
               method="sLDA")

summary(fit$model)

top.topic.words(fit$topics, 5, by.score=TRUE)
#Plot the estimates of the model coefficients

suppressWarnings(require("ggplot2"))
## plot coefs for each topic
Topics <- apply(top.topic.words(fit$topics, 5, by.score=TRUE), 2, paste, collapse=" ")
coefs <- data.frame(coef(summary(fit$model)))
coefs <- cbind(coefs, Topics=factor(Topics, Topics[order(coefs$Estimate)]))
(coefs <- coefs[order(coefs$Estimate),])




qplot(Topics, Estimate, colour=Estimate, size=abs(t.value), data=coefs) +
        geom_errorbar(width=0.5, aes(ymin=Estimate-Std..Error, ymax=Estimate+Std..Error)) + 
        coord_flip()
#Plot predicted distribution densities of positive, neutral and negative sentiments.

# PREDICTION
set.seed(12345)
predictions <- slda.predict(documents,
                            fit$topics, 
                            fit$model,
                            alpha=alpha,
                            eta=eta)


range(predictions[!is.nan(predictions)])        

#The range of predictions is practically the same as the range of known responses: from -1 to 1.
# plot predictions for negative, neutral and positive tweets
set.seed(60134)
qplot(predictions,
      fill=sentiment.f,
      xlab = "predicted rating",
      ylab = "density",
      alpha=I(0.5),
      geom="density") +
        geom_vline(aes(xintercept=0)) +
        geom_vline(aes(xintercept=-.65),col="red",lwd=2) +
        geom_vline(aes(xintercept=.08),col="blue",lwd=2)



#The three densities can be used to generate a decision rule.
#For example, if one decides to break the range of predictions using red and blue vertical lines shown on the graph, the confusion matrix is going to be


confusion<-table(data.frame(predict=findInterval(predictions,c(-.65,.8)),actual=sentiment.f))
rownames(confusion)<-c("Negative","Neutral","Positive")
confusion

bins <- seq(from=-1,to=0.9,by=0.1)
probVeclist <- list()
for(i in 1:(length(bins)-1)){
        print(bins[i])
        print(bins[i+1])
        binIdx<-findInterval(predictions,c(bins[i],bins[i+1]))==1
        probVec<-table(sentiment[binIdx])
        probVec<-probVec/sum(probVec)
        probVeclist[[i]] <- probVec
        
}

# save model in binary to use it in real time handler:
save(list=c('stop_words', 'vocab', 'get.terms', 'alpha', 'eta', 'fit'), file = "slda.model.Rdata")


