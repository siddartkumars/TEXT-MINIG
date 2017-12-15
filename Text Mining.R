## Siddarth Kumar S
## SU ID : 541866744
### Step 1 Reading the positive and negative words into a vector

## positive words
pw <- readLines("positive-words.txt")
pw
pw <- pw[-c(1:35)]
pw ## positive words

## negative words
nw <- readLines("negative-words.txt")
nw <- nw[-c(1:35)]
nw #### negative words


## Step 2 Process the MLK speech
library(readtext)
mlk <- readtext("MLK-speech.txt")
mlk


library(tm)

## loading txt as corpus

docs <- Corpus(VectorSource(mlk))
inspect(docs)

#transforming

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

## Cleaning text/words 
#Convert the text to lower case

docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2","will")) 

# Remove punctuations
docs <- tm_map(docs, removePunctuation)

# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
inspect(docs)
library(stringr)
txtbag <- str_split(docs, pattern = " ")
txtbag <- txtbag[[1]]
txtbag <- txtbag[-c(1)]
txtbag


## TermDocument Matrix
dtm <- TermDocumentMatrix(docs)
dtm
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

## word cloud
library(wordcloud)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

## word frequencies
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
      
        ylab = "Word frequencies")

### step 3 positive words

pos <- scan('positive-words.txt',what = 'character',comment.char = ";")
nos <- scan('negative-words.txt',what = 'character',comment.char = ";")

###Positive word count
no_pw <- sum(!is.na(match(txtbag,pos)))
no_pw
wordcloud(words = pos, freq = d$freq, min.freq = 1,
          max.words=800, random.order=FALSE, rot.per=0.35, 
          colors="green")


### Step 4 Negative word count
no_nw <- sum(!is.na(match(txtbag,nos)))
no_nw
wordcloud(words = nos, freq = d$freq, min.freq = 1,
          max.words=800, random.order=FALSE, rot.per=0.35, 
          colors="red")

### STEP 5 
## 25% of the word count
## txtbag consists of sorted and processed words from speech
txtbag1 <- as.data.frame(txtbag)
txtbag1
qrter <- 1/4*nrow(txtbag1)
qrter ## We select only 207 words from the speech

ttbag1 <- txtbag1[1:207,]
ttbag2 <- txtbag1[208:414,]
ttbag3 <- txtbag1[415:621,]
ttbag4 <- txtbag1[622:828,]

## positive word count
counter <- function(ttbag){
no_pwqt <- sum(!is.na(match(ttbag,pos)))
qnos <- no_pwqt ### positive words in 25% speech
## negatice words
no_nwqt <- sum(!is.na(match(ttbag,nos)))
pnos <- no_nwqt ##Negative words in 25% speech
count <- c(pnos,qnos)
names(count) <- c("POS","NEG")
return(count)
}

x <- counter(ttbag1)
y = counter(ttbag2)
z = counter(ttbag3)
v = counter(ttbag4)

##  Compare the result (SIMPLE BARCHART FOR 4 numbers)
barplot(x)
barplot(y)
barplot(z)
barplot(v)

## combined plot for all quarters
xyzv <- data.frame(x,y,z,v)
xyzv <- as.matrix(xyzv)
barplot(xyzv,xlab = c("Dark=Pos _ words,White= Neg_words"))
