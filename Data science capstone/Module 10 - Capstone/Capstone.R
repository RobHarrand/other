#https://eight2late.wordpress.com/2015/05/27/a-gentle-introduction-to-text-mining-using-r/
#http://amunategui.github.io/speak-like-a-doctor
#https://cran.r-project.org/web/packages/quanteda/quanteda.pdf

options( java.parameters = "-Xmx4g" )

library("tm")
library("SnowballC")
library("stringr")
library("readr")
library('stringdist')

#getwd()

#Questions to consider

#Some words are more frequent than others - what are the distributions of word frequencies?

#What are the frequencies of 2-grams and 3-grams in the dataset?

#How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?

#How do you evaluate how many of the words come from foreign languages?

#Can you think of a way to increase the coverage -- identifying words that may not be in the corpora or using a smaller 
#number of words in the dictionary to cover the same number of phrases?


#Create Corpus
#-----------------------------

#docs_en = Corpus(DirSource("texts/en_US/"))
#docs_de = Corpus(DirSource("/home/rob/Desktop/Coursera/Capstone/texts/de_DE/"))
#docs_ru = Corpus(DirSource("/home/rob/Desktop/Coursera/Capstone/texts/ru_RU/"))
#docs_fl = Corpus(DirSource("/home/rob/Desktop/Coursera/Capstone/texts/fl_FL/"))

#Encoding(docs_en[[1]]$content)  <- "UTF-8"

en_blogs = read_lines("texts/en_US/en_US.blogs.txt")
en_tweets = readLines("texts/en_US/en_US.twitter.txt", skipNul = T)
en_news = read_lines("texts/en_US/en_US.news.txt")
dict = read_lines("texts/Dict.txt")
#idioms = read_lines("idioms.txt")

# en_blogs_10pc[1:3]
# en_tweets_10pc[1:3]
# en_news_10pc[1:10]

prop = 0.1

set.seed(100)
en_blogs_sample = sample(en_blogs, (length(en_blogs)*prop))
set.seed(100)
en_tweets_sample = sample(en_tweets, (length(en_tweets)*prop))
set.seed(100)
en_news_sample = sample(en_news, (length(en_news)*prop))

en_blogs_sample = iconv(en_blogs_sample, "latin1", "ASCII", "")
en_tweets_sample = iconv(en_tweets_sample, "latin1", "ASCII", "")
en_news_sample = iconv(en_news_sample, "latin1", "ASCII", "")


#Create test set

# set.seed(500)
# en_blogs_sample_test = sample(en_blogs, (length(en_blogs)*prop))
# set.seed(500)
# en_tweets_sample_test = sample(en_tweets, (length(en_tweets)*prop))
# set.seed(500)
# en_news_sample_test = sample(en_news, (length(en_news)*prop))
# 
# en_blogs_sample_test = iconv(en_blogs_sample_test, "latin1", "ASCII", "")
# en_tweets_sample_test = iconv(en_tweets_sample_test, "latin1", "ASCII", "")
# en_news_sample_test = iconv(en_news_sample_test, "latin1", "ASCII", "")



# dir.create("10pc_sample", showWarnings = FALSE)
# 
# write(en_blogs_10pc, "10pc_sample/sample.blogs.txt")
# write(en_tweets_10pc, "10_pcsample/sample.news.txt")
# write(en_news_10pc, "10_pcsample/sample.twitter.txt")

rm(en_blogs)
rm(en_tweets)
rm(en_news)

all_sample = c(en_blogs_sample, en_tweets_sample, en_news_sample)
all_sample_corpus = Corpus(VectorSource(list(all_sample)))

#all_sample_test = c(en_blogs_sample_test, en_tweets_sample_test, en_news_sample_test)
#all_sample_corpus_test = Corpus(VectorSource(list(all_sample_test)))

rm(en_blogs_sample)
rm(en_tweets_sample)
rm(en_news_sample)
#rm(all_sample)

#rm(en_blogs_sample_test)
#rm(en_tweets_sample_test)
#rm(en_news_sample_test)
#rm(all_sample_test)


#head(all_sample_corpus[[1]]$content)
#head(all_sample_corpus_test[[1]]$content)



#all_sample_corpus[1:3]

#writeLines(as.character(all_sample_corpus[[1]]))

# en_blogs_10pc_corpus = Corpus(VectorSource(en_blogs_10pc))
# en_tweets_10pc_corpus = Corpus(VectorSource(en_tweets_10pc))
# en_news_10pc_corpus = Corpus(VectorSource(en_news_10pc))
# 
# tm_combine

# head(en_blogs, 3)
# 
# # Delete all the titles. They're not going to be useful 'next words' and the full-stops confuse the sentence endings,
# en_blogs <- gsub(pattern='Mr.|Mrs.|Miss.|Ms.', x=en_blogs, replacement=' ')
# 
# # swap all sentence ends with code 'zyxcba'
# en_blogs <- gsub(pattern=';|\\. |!|\\?', x=en_blogs, replacement='zyxcba')
#   
# # remove all non-alpha text (numbers etc)
# en_blogs <- gsub(pattern="[^[:alpha:]]", x=en_blogs, replacement = ' ')
#   
# # force all characters to lower case
# en_blogs <- tolower(en_blogs)
#   
# # remove any small words {size} or {min,max}
# #en_blogs <- gsub(pattern="\\W*\\b\\w{1,2}\\b", x=en_blogs, replacement=' ')
#   
# # remove contiguous spaces
# en_blogs <- gsub(pattern="\\s+", x=en_blogs, replacement=' ')
#   
# # split sentences by split code
# en_blogs = unlist(strsplit(x=en_blogs, split='zyxcba',fixed = TRUE))
# 
# head(en_blogs, 3)


#Delete all the titles. They're not going to be useful 'next words' and the full-stops confuse the sentence endings.
#To do this, create the TitletoSpace content transformer
# TitletoSpace = content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})
# 
# #Apply this new function,
# docs_en = tm_map(docs_en, TitletoSpace, "Mr.")
# docs_en = tm_map(docs_en, TitletoSpace, "Mrs.")
# docs_en = tm_map(docs_en, TitletoSpace, "Ms.")
# docs_en = tm_map(docs_en, TitletoSpace, "Miss.")


#Remove swearwords

swearwords = read.delim("swearWords.txt", sep = "\n", header = F)
all_sample_corpus = tm_map(all_sample_corpus, removeWords, swearwords$V1)
#all_sample_corpus_test = tm_map(all_sample_corpus_test, removeWords, swearwords$V1)
rm(swearwords)


#Tidy
#---------------------------------------

#convertSpace = content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})

#There are still some uncommon punctuation marks remaining. Remove these with the custom function,
#docs_en <- tm_map(docs_en, convertSpace, "â€™")
#docs_en <- tm_map(docs_en, convertSpace, "â€œ")
#docs_en <- tm_map(docs_en, convertSpace, "â€")
#docs_en <- tm_map(docs_en, convertSpace, "'")

#docs_en <- tm_map(docs_en, toSpace, "`")
#docs_en <- tm_map(docs_en, toSpace, " -")

#all_sample_corpus[[1]]$content[11]

#Remove punctuation – replace punctuation marks with a space
all_sample_corpus = tm_map(all_sample_corpus, removePunctuation)
all_sample_corpus = tm_map(all_sample_corpus, content_transformer(tolower))
all_sample_corpus = tm_map(all_sample_corpus, removeNumbers) 
all_sample_corpus = tm_map(all_sample_corpus, stripWhitespace)
# 
# all_sample_corpus_test = tm_map(all_sample_corpus_test, removePunctuation)
# all_sample_corpus_test = tm_map(all_sample_corpus_test, content_transformer(tolower))
# all_sample_corpus_test = tm_map(all_sample_corpus_test, removeNumbers) 
# all_sample_corpus_test = tm_map(all_sample_corpus_test, stripWhitespace)

#inspect the start of a particular document,
# writeLines(as.character(all_sample_corpus[[1]]$content[1:5]))
# writeLines(as.character(all_sample_corpus[[1]]$content[6:10]))
# writeLines(as.character(all_sample_corpus[[1]]$content[11:15]))

#docs_en = tm_map(docs_en, function(x) iconv(enc2utf8(x[[1]]$content), sub = "byte"))

#Create a sub-samples of docs_en - blogs,

#docs_en_blog_10pc = docs_en[[1]]$content[1:(floor(length(docs_en[[1]]$content)/10))]



#Save the corpus
#-------------------------------------

#writeCorpus(all_sample_corpus, filenames="all_sample_corpus.txt")
#all_10pc <- readLines("all_sample_corpus.txt")
#all_sample_corpus = Corpus(VectorSource(list(all_10pc)))



#DTM
#------------------------------------

# dtm <- DocumentTermMatrix(all_sample_corpus)
# dtm
# Terms(dtm)
# 
# inspect(dtm[1,1000:1010])
# 
# freq <- colSums(as.matrix(dtm))
# length(freq)
# ord <- order(freq,decreasing=TRUE)
# freq[head(ord, 20)]
# freq[tail(ord)]

#length(findFreqTerms(dtm,lowfreq=80))

#findAssocs(dtm,"work",0.6)

#barplot(freq[head(ord, 10)])


#Wordcloud
#-------------------------------------------------

#library(wordcloud)
#set.seed(100)
#wordcloud(names(freq),freq, min.freq=500,colors=brewer.pal(6,"Dark2"))


#Ngrams (RWeka package)
#-------------------------------------------------

library(RWeka)

OnegramTokenizer = function(x) WordTokenizer(x)
dtm <- DocumentTermMatrix(all_sample_corpus, control = list(tokenize = OnegramTokenizer, wordLengths = c(1, Inf)))

TwogramTokenizer = function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
dtm2 <- DocumentTermMatrix(all_sample_corpus, control = list(tokenize = TwogramTokenizer))

ThreegramTokenizer = function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
dtm3 <- DocumentTermMatrix(all_sample_corpus, control = list(tokenize = ThreegramTokenizer))

FourgramTokenizer = function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
dtm4 <- DocumentTermMatrix(all_sample_corpus, control = list(tokenize = FourgramTokenizer))

FivegramTokenizer = function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))
dtm5 <- DocumentTermMatrix(all_sample_corpus, control = list(tokenize = FivegramTokenizer))



# dtm = removeSparseTerms(dtm, 0.75)
# dtm2 = removeSparseTerms(dtm2, 0.75)
# dtm3 = removeSparseTerms(dtm3, 0.75)
# dtm4 = removeSparseTerms(dtm4, 0.75)
# dtm5 = removeSparseTerms(dtm5, 0.75)
#inspect(dtm3_b[1,1000])

# head(all_sample_corpus[[1]]$content,100)
# 
# OnegramTokenizer = function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
# dtm_test <- DocumentTermMatrix(all_sample_corpus_test, control = list(tokenize = OnegramTokenizer))
# 
# TwogramTokenizer = function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
# dtm2_test <- DocumentTermMatrix(all_sample_corpus_test, control = list(tokenize = TwogramTokenizer))
# 
# ThreegramTokenizer = function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
# dtm3_test <- DocumentTermMatrix(all_sample_corpus_test, control = list(tokenize = ThreegramTokenizer))
# 
# FourgramTokenizer = function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
# dtm4_test <- DocumentTermMatrix(all_sample_corpus_test, control = list(tokenize = FourgramTokenizer))
# 
# FivegramTokenizer = function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))
# dtm5_test <- DocumentTermMatrix(all_sample_corpus_test, control = list(tokenize = FivegramTokenizer))
# 
# dtm_test = removeSparseTerms(dtm_test, 0.75)
# dtm2_test = removeSparseTerms(dtm2_test, 0.75)
# dtm3_test = removeSparseTerms(dtm3_test, 0.75)
# dtm4_test = removeSparseTerms(dtm4_test, 0.75)
# dtm5_test = removeSparseTerms(dtm5_test, 0.75)



#Save and load
#------------------------------------

# save(dtm, file = "dtm.RData")
# save(dtm2, file = "dtm2.RData")
# save(dtm3, file = "dtm3.RData")
# save(dtm4, file = "dtm4.RData")
# save(dtm5, file = "dtm5.RData")
# 
# save(dtm_test, file = "dtm_test.RData")
# save(dtm2_test, file = "dtm2_test.RData")
# save(dtm3_test, file = "dtm3_test.RData")
# save(dtm4_test, file = "dtm4_test.RData")
# save(dtm5_test, file = "dtm5_test.RData")
# 
#save(all_sample_corpus, file = "all_sample_corpus.RData")
#save(all_sample_corpus_test, file = "all_sample_corpus_test")

load(file = "dtm.RData")
load(file = "dtm2.RData")
load(file = "dtm3.RData")
load(file = "dtm4.RData")
load(file = "dtm5.RData")

# load(dtm_test, file = "dtm_test.RData")
# load(dtm2_test, file = "dtm2_test.RData")
# load(dtm3_test, file = "dtm3_test.RData")
# load(dtm4_test, file = "dtm4_test.RData")
# load(dtm5_test, file = "dtm5_test.RData")

load(file = "all_sample_corpus.RData")
#load(all_sample_corpus_test, file = "all_sample_corpus_test")





#Probabilities
#-----------------------------------

freq = colSums(as.matrix(dtm)) 
counts_dtm = data.frame(Word=names(freq), Freq=freq) 
counts_dtm = counts_dtm[counts_dtm$Freq>50,] #Pruning

freq2 = colSums(as.matrix(dtm2)) 
counts_dtm2 = data.frame(Word=names(freq2), Freq=freq2) 
counts_dtm2 = counts_dtm2[counts_dtm2$Freq>5,] #Pruning

freq3 = colSums(as.matrix(dtm3))
counts_dtm3 = data.frame(Word=names(freq3), Freq=freq3) 
counts_dtm3 = counts_dtm3[counts_dtm3$Freq>1,] #Pruning

freq4 = colSums(as.matrix(dtm4)) 
counts_dtm4 = data.frame(Word=names(freq4), Freq=freq4) 
counts_dtm4 = counts_dtm4[counts_dtm4$Freq>2,] #Pruning

freq5 = colSums(as.matrix(dtm5)) 
counts_dtm5 = data.frame(Word=names(freq5), Freq=freq5) 
counts_dtm5 = counts_dtm5[counts_dtm5$Freq>2,] #Pruning


#Drop levels,
counts_dtm$Word = droplevels(counts_dtm$Word)
counts_dtm2$Word = droplevels(counts_dtm2$Word)
counts_dtm3$Word = droplevels(counts_dtm3$Word)
counts_dtm4$Word = droplevels(counts_dtm4$Word)
counts_dtm5$Word = droplevels(counts_dtm5$Word)

rm(dtm)
rm(dtm2)
rm(dtm3)
rm(dtm4)
rm(dtm5)


#Maximum likelihood estimate for a bigram, P(Wi | Wi-1) = count(Wi-1, Wi) / count(Wi-1)
#For example, P(Park | Car) = the number of bigrams of 'Car Park' divided by the number of unigrams of 'Car'

#Unigram probabilities
Uni_length = sum(counts_dtm$Freq)
counts_dtm$Prob = counts_dtm$Freq / Uni_length




#Bigram probabiltiies
counts_dtm2$Prob = 0
i=1

while (i <= length(counts_dtm2$Word)) {

  uni_count = counts_dtm$Freq[counts_dtm$Word == (strsplit(as.character(counts_dtm2$Word[i]), " ")[[1]][1])]
  bi_count = counts_dtm2$Freq[i]
  counts_dtm2$Prob[i] = bi_count / uni_count
  i=i+1
  print((i/length(counts_dtm2$Word)*100))
  
}



#Trigram probabiltiies

#Maximum likelihood estimate for a trigram, P(wi | wi-1 wi-2 ) = count(wi, wi-1, wi-2 ) / count(wi-1, wi-2 )
#For example, P(Park | The car) = the number of trigrams of 'The car park' divided by the number of bigrams of 'The car'

#https://gist.github.com/ttezel/4138642
#https://class.coursera.org/nlp/lecture

counts_dtm3$Prob = 0
i=1

while (i <= length(counts_dtm3$Word)) {
  
  bi_count = counts_dtm2$Freq[counts_dtm2$Word == paste(strsplit(as.character(counts_dtm3$Word[i]), " ")[[1]][1],
                                                    strsplit(as.character(counts_dtm3$Word[i]), " ")[[1]][2])]
  tri_count = counts_dtm3$Freq[i]
  counts_dtm3$Prob[i] = tri_count / bi_count
  i=i+1
  print((i/length(counts_dtm3$Word)*100))
  
}



#Quadgram probabiltiies

counts_dtm4$Prob = 0
i=1

while (i <= length(counts_dtm4$Word)) {
  
  tri_count = counts_dtm3$Freq[counts_dtm3$Word == paste(strsplit(as.character(counts_dtm4$Word[i]), " ")[[1]][1],
                                                        strsplit(as.character(counts_dtm4$Word[i]), " ")[[1]][2],
                                                        strsplit(as.character(counts_dtm4$Word[i]), " ")[[1]][3])]
  quad_count = counts_dtm4$Freq[i]
  counts_dtm4$Prob[i] = quad_count / tri_count
  i=i+1
  print((i/length(counts_dtm4$Word)*100))
  
}





#Fivegram probabiltiies

counts_dtm5$Prob = 0
i=1

while (i <= length(counts_dtm5$Word)) {
  
  quad_count = counts_dtm4$Freq[counts_dtm4$Word == paste(strsplit(as.character(counts_dtm5$Word[i]), " ")[[1]][1],
                                                         strsplit(as.character(counts_dtm5$Word[i]), " ")[[1]][2],
                                                         strsplit(as.character(counts_dtm5$Word[i]), " ")[[1]][3],
                                                         strsplit(as.character(counts_dtm5$Word[i]), " ")[[1]][4])]
  five_count = counts_dtm5$Freq[i]
  counts_dtm5$Prob[i] = five_count / quad_count
  i=i+1
  print((i/length(counts_dtm5$Word)*100))
  
}




#Simple interpolation (lambdas are static for all cases)
#------------------------------------------------------------------

#P(beer | case of) = l1 * P(beer | case of) + l2 * P(beer | of) + l3 * P(beer)

#Replace low prob words with UNK? I think the KN deals with this

save(counts_dtm, file = "counts_dtm.RData")
save(counts_dtm2, file = "counts_dtm2.RData")
save(counts_dtm3, file = "counts_dtm3.RData")
save(counts_dtm4, file = "counts_dtm4.RData")
save(counts_dtm5, file = "counts_dtm5.RData")

save(freq, file = "freq.RData")
save(freq2, file = "freq2.RData")
save(freq3, file = "freq3.RData")
save(freq4, file = "freq4.RData")
save(freq5, file = "freq5.RData")

load(file = "counts_dtm.RData")
load(file = "counts_dtm2.RData")
load(file = "counts_dtm3.RData")
load(file = "counts_dtm4.RData")
load(file = "counts_dtm5.RData")

load(file = "freq.RData")
load(file = "freq2.RData")
load(file = "freq3.RData")
load(file = "freq4.RData")
load(file = "freq5.RData")




  
# Bi_length = length(counts_dtm2$Word)
# Tri_length = length(counts_dtm3$Word)
# Four_length = length(counts_dtm4$Word)
# Five_length = length(counts_dtm5$Word)


# counts_dtm2$Prob = counts_dtm2$Freq / Bi_length
# counts_dtm3$Prob = counts_dtm3$Freq / Tri_length
# counts_dtm4$Prob = counts_dtm4$Freq / Four_length
# counts_dtm5$Prob = counts_dtm5$Freq / Five_length




# freq_test = colSums(as.matrix(dtm_test)) 
# counts_dtm_test = data.frame(Word=names(freq_test), Freq=freq_test) 
# counts_dtm_test = counts_dtm_test[counts_dtm_test$Freq>100,]
# 
# freq2_test = colSums(as.matrix(dtm2_test)) 
# counts_dtm2_test = data.frame(Word=names(freq2_test), Freq=freq2_test) 
# counts_dtm2_test = counts_dtm2_test[counts_dtm2_test$Freq>50,]
# 
# freq3_test = colSums(as.matrix(dtm3_test))
# counts_dtm3_test = data.frame(Word=names(freq3_test), Freq=freq3_test) 
# counts_dtm3_test = counts_dtm3_test[counts_dtm3_test$Freq>5,]
# 
# freq4_test = colSums(as.matrix(dtm4_test)) 
# counts_dtm4_test = data.frame(Word=names(freq4_test), Freq=freq4_test) 
# counts_dtm4_test = counts_dtm4_test[counts_dtm4_test$Freq>2,]
# 
# freq5_test = colSums(as.matrix(dtm5_test)) 
# counts_dtm5_test = data.frame(Word=names(freq5_test), Freq=freq5_test) 
# counts_dtm5_test = counts_dtm5_test[counts_dtm5_test$Freq>2,]









#Kneser-Ney smoothing
#---------------------------------------------

#http://www-rohan.sdsu.edu/~gawron/compling/course_core/lectures/kneser_ney.pdf

#First, we need the corpus as bigrams (not just the unique bigrams, but the whole lot),

bis = as.data.frame(strsplit(as.character(counts_dtm2$Word), " "))
bis = as.data.frame(t(bis))
bis = cbind(bis, counts_dtm2$Freq)
colnames(bis) = c("Word1", "Word2", "Freq")



#Next, we need to know what are the most common 2nd words in the bigrams

most_bi = sort(bis$Freq, decreasing = T)
most_bis = bis[bis$Freq %in% most_bi,]

most_bis = most_bis[order(-most_bis$Freq),]

most_bis = droplevels(most_bis)

unique_w2 = unique(most_bis$Word2)
unique_w1 = unique(most_bis$Word1)

length_element_w1 = length(unique_w1)

#Then, from these common endings, we need to know how many bigrams each completes
#Finally, we work out the continuum probability based upon this

kn = data.frame()
i=1

while (i <= length(unique_w2)) {
  
  sum_element = sum(most_bis$Freq[most_bis$Word2 == unique_w2[i]])
  length_element_w2 = length(most_bis$Word1[most_bis$Word2 == unique_w2[i]])
  prob_element = length_element_w2 / length_element_w1 #No. of word types seen to precede / no. of words preceding all words
  kn[i,1] = unique_w2[i]
  kn[i,2] = sum_element
  kn[i,3] = length_element_w2
  kn[i,4] = prob_element
  i=i+1
  
}

colnames(kn) = c("2nd Word", "Freq", "No. of bigrams it completes", "Pcont")

kn = kn[order(-kn$Pcont),]

#save(kn, file = "kn.RData")
load(file = "kn.RData")

unknown1 = as.character(kn[1,1])
unknown2 = as.character(kn[2,1])
unknown3 = as.character(kn[3,1])

unknowns = data.frame(unknown1,unknown2,unknown3)



#Prediction function
#-------------------------------------



NWP = function(new_word, unknowns) {
  
  #new_word_full = 'least I'
  
  new_word_full = new_word
  
  #new_word_full = "you get" #Length is less than 5, so no need to split
  new_word_full = tolower(new_word_full)
  new_word_full = removePunctuation(new_word_full)
  new_word_full = removeNumbers(new_word_full)
  new_word_full = stripWhitespace(new_word_full)
  new_word_full = str_trim(new_word_full)
  
  new_word_full_original = new_word_full
  
  new_word_split = strsplit(new_word_full, " ")
  
  len_new_word = length(new_word_split[[1]])
  
  i=1
  
  
  if (length(new_word_split[[1]]) > 4) {
    
    new_word_full = paste(new_word_split[[1]][len_new_word-3],new_word_split[[1]][len_new_word-2],
                          new_word_split[[1]][len_new_word-1], new_word_split[[1]][len_new_word])
    
    new_word_split = strsplit(new_word_full, " ")
    
  } 
  
  no_match = 0
  
  
  
  #5GRAM CHECKER
  ---------------------
    
    if(length(new_word_split[[1]]) <= 4) {
      
      if(length(new_word_split[[1]]) == 4) {
        
        Five_matches = as.character(counts_dtm5$Word[grep(paste("^",new_word_full, "\\b", sep = ""), counts_dtm5$Word)])
        Five_matches_f = as.numeric(counts_dtm5$Freq[grep(paste("^",new_word_full, "\\b", sep = ""), counts_dtm5$Word)])
        
        if (length(Five_matches) == 0 | (sum(Five_matches_f) / length(Five_matches_f)) < 2) {
          
          #print("No match in the 5grams. Let's keep checking...")
          new_word_full = paste(new_word_split[[1]][2],new_word_split[[1]][3],new_word_split[[1]][4])
          new_word_split = strsplit(new_word_full, " ")
          
        } 
        
        else {
          
          Five_split = strsplit(Five_matches, " ")
          
          i=1
          Token = ""
          
          while (i <= length(Five_split)) {
            
            Token[i] = as.list(paste(Five_split[[1]][1], Five_split[[1]][2], Five_split[[1]][3], Five_split[[1]][4]))
            Token[[i]][2] = Five_split[[i]][5]
            i=i+1
          }
          
          markov = data.frame(1:length(Five_split))
          markov$Word = " "
          markov[,1] = NULL
          
          
          i = 1
          
          while (i <= length(Five_split)) {
            
            markov$Word[i] = Token[[i]][2]
            i=i+1
            
          }
          
          markov$No = as.numeric(Five_matches_f)
          markov_total = sum(markov$No)
          markov$Prob = markov$No/markov_total
          colnames(markov) = c("Word", "No", "Prob")
          
          markov = markov[order(-markov$Prob),]
          
          i=1
          x = length(markov$Word)
          if (x > 10) {x = 10}
          
          pred = data.frame()
          
          while (i <= x) {
            
            pred[i,1] = (paste("Prediction",i,":", new_word_full_original, "|", as.character(markov$Word[i])))
            pred[i,2] = markov[i,2]
            pred[i,3] = counts_dtm5$Prob[counts_dtm5 == (paste(new_word_full, as.character(markov$Word[i])))]
            i=i+1
            
          }
          
          colnames(pred) = c("Fivegram Prediction", "Freq", "Prob")
          pred$Prob = round(pred$Prob, 3)
          
        }
      }
      
      
      if(length(new_word_split[[1]]) == 3) { #QUADGRAM CHECKER
        
        Quad_matches = as.character(counts_dtm4$Word[grep(paste("^",new_word_full, "\\b", sep = ""), counts_dtm4$Word)])
        Quad_matches_f = as.numeric(counts_dtm4$Freq[grep(paste("^",new_word_full, "\\b", sep = ""), counts_dtm4$Word)])
        
        if (length(Quad_matches) == 0 | (sum(Quad_matches_f) / length(Quad_matches_f)) < 2) {
          
          #print("No match in the 4grams. Let's keep checking...")
          new_word_full = paste(new_word_split[[1]][2],new_word_split[[1]][3])
          new_word_split = strsplit(new_word_full, " ")
          
        } 
        
        else {
          
          Quad_split = strsplit(Quad_matches, " ")
          
          i=1
          Token = ""
          
          while (i <= length(Quad_split)) {
            
            Token[i] = as.list(paste(Quad_split[[1]][1], Quad_split[[1]][2], Quad_split[[1]][3]))
            Token[[i]][2] = Quad_split[[i]][4]
            i=i+1
          }
          
          markov = data.frame(1:length(Quad_split))
          markov$Word = " "
          markov[,1] = NULL
          
          
          i = 1
          
          while (i <= length(Quad_split)) {
            
            markov$Word[i] = Token[[i]][2]
            i=i+1
            
          }
          
          markov$No = as.numeric(Quad_matches_f)
          markov_total = sum(markov$No)
          markov$Prob = markov$No/markov_total
          colnames(markov) = c("Word", "No", "Prob")
          
          markov = markov[order(-markov$Prob),]
          
          i=1
          x = length(markov$Word)
          if (x > 10) {x = 10}
          
          pred = data.frame()
          
          while (i <= x) {
            
            pred[i,1] = (paste("Prediction",i,":", new_word_full_original, "|", as.character(markov$Word[i])))
            pred[i,2] = markov[i,2]
            pred[i,3] = counts_dtm4$Prob[counts_dtm4 == (paste(new_word_full, as.character(markov$Word[i])))]
            i=i+1
            
          }
          
          colnames(pred) = c("Quadgram Prediction", "Freq", "Prob")
          pred$Prob = round(pred$Prob, 3)
          
        }
      }
      
      
      if(length(new_word_split[[1]]) == 2) { #TRIGRAM CHECKER
        
        Tri_matches = as.character(counts_dtm3$Word[grep(paste("^",new_word_full, "\\b", sep = ""), counts_dtm3$Word)])
        Tri_matches_f = as.numeric(counts_dtm3$Freq[grep(paste("^",new_word_full, "\\b", sep = ""), counts_dtm3$Word)])
        
        
        if (length(Tri_matches) == 0 | (sum(Tri_matches_f) / length(Tri_matches_f)) < 2) {
          
          #print("No match in the 3grams. Let's keep checking...")
          new_word_full = new_word_split[[1]][2]
          new_word_split = strsplit(new_word_full, " ")
          
        } 
        
        else {
          
          Tri_split = strsplit(Tri_matches, " ")
          
          i=1
          Token = ""
          
          while (i <= length(Tri_split)) {
            
            Token[i] = as.list(paste(Tri_split[[1]][1], Tri_split[[1]][2]))
            Token[[i]][2] = Tri_split[[i]][3]
            i=i+1
          }
          
          markov = data.frame(1:length(Tri_split))
          markov$Word = " "
          markov[,1] = NULL
          
          
          i = 1
          
          while (i <= length(Tri_split)) {
            
            markov$Word[i] = Token[[i]][2]
            i=i+1
            
          }
          
          markov$No = as.numeric(Tri_matches_f)
          markov_total = sum(markov$No)
          markov$Prob = markov$No/markov_total
          colnames(markov) = c("Word", "No", "Prob")
          
          markov = markov[order(-markov$Prob),]
          
          i=1
          x = length(markov$Word)
          if (x > 10) {x = 10}
          
          pred = data.frame()
          
          while (i <= x) {
            
            pred[i,1] = (paste("Prediction",i,":", new_word_full_original, "|", as.character(markov$Word[i])))
            pred[i,2] = markov[i,2]
            pred[i,3] = counts_dtm3$Prob[counts_dtm3 == (paste(new_word_full, as.character(markov$Word[i])))]
            i=i+1
            
          }
          
          colnames(pred) = c("Trigram Prediction", "Freq", "Prob")
          pred$Prob = round(pred$Prob, 3)
          
        }
      }
      
      
      
      if(length(new_word_split[[1]]) == 1) { #BIGRAM CHECKER
        
        Bi_matches = as.character(counts_dtm2$Word[grep(paste("^\\b",new_word_full, "\\b","\\b", sep = ""), counts_dtm2$Word)])
        Bi_matches_f = as.numeric(counts_dtm2$Freq[grep(paste("^\\b",new_word_full, "\\b","\\b", sep = ""), counts_dtm2$Word)])
        
        if (length(Bi_matches) == 0) {
          
          #print("No match in the 2grams. Let's keep checking...")
          no_match = 1
          
        } 
        
        else {
          
          Bi_split = strsplit(Bi_matches, " ")
          
          i=1
          Token = ""
          
          while (i <= length(Bi_split)) {
            
            Token[i] = as.list(Bi_split[[1]][1])
            Token[[i]][2] = Bi_split[[i]][2]
            i=i+1
          }
          
          markov = data.frame(1:length(Bi_split))
          markov$Word = " "
          markov[,1] = NULL
          
          
          i = 1
          
          while (i <= length(Bi_split)) {
            
            markov$Word[i] = Token[[i]][2]
            i=i+1
            
          }
          
          markov$No = as.numeric(Bi_matches_f)
          markov_total = sum(markov$No)
          markov$Prob = markov$No/markov_total
          colnames(markov) = c("Word", "No", "Prob")
          
          markov = markov[order(-markov$Prob),]
          
          i=1
          x = length(markov$Word)
          if (x > 10) {x = 10}
          
          pred = data.frame()
          
          while (i <= x) {
            
            pred[i,1] = (paste("Prediction",i,":", new_word_full_original, "|", as.character(markov$Word[i])))
            pred[i,2] = markov[i,2]
            pred[i,3] = counts_dtm2$Prob[counts_dtm2 == (paste(new_word_full, as.character(markov$Word[i])))]
            i=i+1
            
          }
          
          colnames(pred) = c("Bigram Prediction", "Freq", "Prob")
          pred$Prob = round(pred$Prob, 3)
          
        }
        
      }
      
      
      
      #No match from a single word!
      #---------------------------------
      
      if (no_match == 1) {
        
        #pred = data.frame()  
        #pred[1,1] = "Prediction: 123"
        
        if (new_word_split %in% dict) {
          
          #       max = head(sort(counts_dtm$Freq, decreasing = T),1)
          #       max_w = counts_dtm[counts_dtm$Freq == max,]
          #       max_w = as.character(max_w$Word)
          
          pred = data.frame()
          pred[1,1] = (paste("Prediction",i,":", new_word_full_original, "|", as.character(unknowns[1,1])))
          pred[2,1] = (paste("Prediction",i,":", new_word_full_original, "|", as.character(unknowns[1,2])))
          pred[3,1] = (paste("Prediction",i,":", new_word_full_original, "|", as.character(unknowns[1,3])))
          colnames(pred) = c("Unigram Prediction", "Probability")
          pred$Probability = round(pred$Probability, 3)
          
        } else {
          
          nearest = amatch(new_word_split, counts_dtm$Word, maxDist = 5, method = "lcs")
          
          pred = data.frame()
          pred[i,1] = paste("Word not found. Did you mean '", counts_dtm$Word[nearest], "' ?", sep = "")
          
        }
        
      } 
      
      return(pred)
      
    }
}






#Individual prediction
#-----------------------------------------


#Do I have to be a bit clevered than just 'if there are less than 2 go to lower-gram' and actually work out Ps?


test_word = "a case of"

counter = 0
eval = data.frame()

test = test_word
test_split = strsplit(test, " ")
len_new_word = length(test_split[[1]])


if (length(test_split[[1]]) > 4) {
  
  test = paste(test_split[[1]][len_new_word-3],test_split[[1]][len_new_word-2],
               test_split[[1]][len_new_word-1], test_split[[1]][len_new_word])
  
  test_split = strsplit(test, " ")
  
} 


#if (length(test_split[[1]]) > 1) {test_split[[1]] = test_split[[1]][1:length(test_split[[1]])-1]}

if (length(test_split[[1]]) == 4) {test_func=paste(test_split[[1]][1],test_split[[1]][2],test_split[[1]][3],test_split[[1]][4])}
if (length(test_split[[1]]) == 3) {test_func=paste(test_split[[1]][1],test_split[[1]][2],test_split[[1]][3])}
if (length(test_split[[1]]) == 2) {test_func=paste(test_split[[1]][1],test_split[[1]][2])}
if (length(test_split[[1]]) == 1) {test_func=test_split[[1]][1]}

p_return = NWP(test_func, unknowns)

p_return

#Adjust the probabilities 

l1 = 1/3
l2 = 1/3
l3 = 1/3

i=1

while (i <= length(p_return$`Trigram Prediction`)) {

  split = strsplit(p_return$`Trigram Prediction`[i], " ")
  
  p_return$Adjust_prob[i] =  round(l1*(p_return$Prob[i]) + 
    l2*(counts_dtm2$Prob[counts_dtm2$Word == paste(split[[1]][length(split[[1]])-2],split[[1]][length(split[[1]])])]) + 
    l3*(counts_dtm$Prob[counts_dtm$Word == split[[1]][length(split[[1]])]]), 3)

  i=i+1
  
}

p_return = p_return[order(-p_return$Adjust_prob),]
p_return




beer5 = counts_dtm5$Freq[counts_dtm5$Word == 'and a pack of the']
beer4 = counts_dtm4$Freq[counts_dtm4$Word == 'a pack of beer']
beer3 = counts_dtm3$Freq[counts_dtm3$Word == 'pack of beer']
beer2 = counts_dtm2$Freq[counts_dtm2$Word == 'of beer']
beer1 = counts_dtm$Freq[counts_dtm$Word == 'beer']

l1*(beer1 / sum(counts_dtm$Freq)) + l2*(beer2 / beer1) + l3*(beer3 / beer2)



the5 = counts_dtm5$Freq[counts_dtm5$Word == 'and a pack of the']
the4 = counts_dtm4$Freq[counts_dtm4$Word == 'a pack of the']
the3 = counts_dtm3$Freq[counts_dtm3$Word == 'pack of the']
the2 = counts_dtm2$Freq[counts_dtm2$Word == 'of the']
the1 = counts_dtm$Freq[counts_dtm$Word == 'the']

l1*(the1 / sum(counts_dtm$Freq)) + l2*(the2 / the1) + l3*(the3 / the2)


counts_dtm4$Prob[counts_dtm4$Word == 'a pack of cards']
counts_dtm3$Freq[counts_dtm3$Word == 'a pack of']


#Evaluation
#-----------------------------------------

test_set_2 = as.character(sample(counts_dtm2_test$Word, size = 100))
test_set_3 = as.character(sample(counts_dtm3_test$Word, size = 100))
test_set_4 = as.character(sample(counts_dtm4_test$Word, size = 100))
test_set_5 = as.character(sample(counts_dtm5_test$Word, size = 100))

test_set = c(test_set_3,test_set_4,test_set_5)


i = 1
counter = 0
eval = data.frame()

while (i <= length(test_set)) {
  
  test = test_set[i]
  test_split = strsplit(test, " ")
  len_new_word = length(test_split[[1]])
  
  
  if (length(test_split[[1]]) > 4) {
    
    test = paste(test_split[[1]][len_new_word-3],test_split[[1]][len_new_word-2],
                 test_split[[1]][len_new_word-1], test_split[[1]][len_new_word])
    
    test_split = strsplit(test, " ")
    
  } 
  
  
  test_split[[1]] = test_split[[1]][1:length(test_split[[1]])-1]
  
  if (length(test_split[[1]]) == 4) {test_func=paste(test_split[[1]][1],test_split[[1]][2],test_split[[1]][3],test_split[[1]][4])}
  if (length(test_split[[1]]) == 3) {test_func=paste(test_split[[1]][1],test_split[[1]][2],test_split[[1]][3])}
  if (length(test_split[[1]]) == 2) {test_func=paste(test_split[[1]][1],test_split[[1]][2])}
  if (length(test_split[[1]]) == 1) {test_func=test_split[[1]][1]}
  
  p_return = NWP(test_func)
  
  p1 = p_return[1,1]
  p1 = strsplit(p1, ": ")
  p1 = p1[[1]][2]
  
  if (dim(p_return)[1] == 2) {
    
    p2 = p_return[2,1]
    p2 = strsplit(p2, ": ")
    p2 = p2[[1]][2]
  } else {p2 = " "}
  
  if (dim(p_return)[1] == 3) {
    
    p3 = p_return[2,1]
    p3 = strsplit(p3, ": ")
    p3 = p3[[1]][2]
  } else {p3 = " "}
  
  
  eval[i,1] = test
  eval[i,2] = p1
  eval[i,3] = p2
  eval[i,4] = p3
  
  if (test == eval[i,2] | test == eval[i,3] | test == eval[i,4]) {counter = counter+1}
  
  i=i+1
  print(i/length(test_set)*100)
  
}


accuracy = counter / length(test_set)
accuracy*100

eval


#Latest (18/03/16): 5% used. Accuracy ~ 42% Look into the 5grams and the test vs pred bit towards the end. Not quite right 





#Quiz 2 hack
#----------------------------

counts_dtm2[grep("^of\\b", counts_dtm2$Word),]
counts_dtm3[grep("^case of", counts_dtm3$Word),]
counts_dtm4[grep("^a case of", counts_dtm4$Word),]
counts_dtm5[grep("^and a case of", counts_dtm5$Word),]

#and a case of mountain x 1

#a case of beer x 1
#a case of the x 10
#a case of mistaken x 2
#a case of waiting x 2

#case of a x25
#case of beer x1





"The guy in front of me just bought a pound of bacon, a bouquet, and a case of"

#Algorithm:     the
#Grep looking:  beer
#3rd:           beer (correct)


"You're the reason why I smile everyday. Can you follow me please? It would mean the"

#Algorithm:     world
#Grep looking:  world 
#3rd:           world (correct)


"Hey sunshine, can you follow me and make me the"

#Algorithm:     most
#Grep looking:  ? 
#3rd:           happiest (correct)


"Very early observations on the Bills game: Offense still struggling but the"

#Algorithm:     best
#Grep looking:  ? (correct = defence)
#3rd:           best


"Go on a romantic date at the"

#Algorithm:     beverly 
#Grep looking:  ? (don't know, movies was wrong)
#3rd:           end


"Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my"

#Algorithm:     way 
#Grep looking:  way 
#3rd:           way (correct)


"Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some"

#Algorithm:     time 
#Grep looking:  time 
#3rd:           time (correct)


"After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little"

#Algorithm:     brother
#Grep looking:  ? (correct = fingers)
#3rd:           brother


"Be grateful for the good times and keep the faith during the"

#Algorithm:     day
#Grep looking:  ? (correct = bad)
#3rd:           day


"If this isn't the cutest thing you've ever seen, then you must be"


#Algorithm:     a 
#Grep looking:   (correct = insane)
#3rd:           a

#-------------------------------------------------------------------
