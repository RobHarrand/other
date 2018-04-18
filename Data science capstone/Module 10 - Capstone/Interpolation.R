options( java.parameters = "-Xmx4g" ) #Set memory options

#Load libraries,
library("tm")
library("SnowballC")
library("stringr")
library("readr")
library('stringdist')
library(RWeka)

#Load text files,
en_blogs = read_lines("texts/en_US/en_US.blogs.txt")
en_tweets = readLines("texts/en_US/en_US.twitter.txt", skipNul = T)
en_news = read_lines("texts/en_US/en_US.news.txt")
dict = read_lines("texts/Dict.txt")

prop = 0.02

set.seed(100)
en_blogs_sample = sample(en_blogs, (length(en_blogs)*prop))
set.seed(100)
en_tweets_sample = sample(en_tweets, (length(en_tweets)*prop))
set.seed(100)
en_news_sample = sample(en_news, (length(en_news)*prop))

en_blogs_sample = iconv(en_blogs_sample, "latin1", "ASCII", "")
en_tweets_sample = iconv(en_tweets_sample, "latin1", "ASCII", "")
en_news_sample = iconv(en_news_sample, "latin1", "ASCII", "")

rm(en_blogs)
rm(en_tweets)
rm(en_news)

all_sample = c(en_blogs_sample, en_tweets_sample, en_news_sample)
all_sample_corpus = Corpus(VectorSource(list(all_sample)))

rm(en_blogs_sample)
rm(en_tweets_sample)
rm(en_news_sample)
rm(all_sample)


#Tidy
#---------------------------------------

#Remove punctuation – replace punctuation marks with a space
all_sample_corpus = tm_map(all_sample_corpus, removePunctuation)
all_sample_corpus = tm_map(all_sample_corpus, content_transformer(tolower))
all_sample_corpus = tm_map(all_sample_corpus, removeNumbers) 
all_sample_corpus = tm_map(all_sample_corpus, stripWhitespace)


#Remove swearwords,

swearwords = read.delim("swearWords.txt", sep = "\n", header = F)
all_sample_corpus = tm_map(all_sample_corpus, removeWords, swearwords$V1)
rm(swearwords)


#Save the corpus
#-------------------------------------

#writeCorpus(all_sample_corpus, filenames="all_sample_corpus.txt")
#all_10pc <- readLines("all_sample_corpus.txt")
#all_sample_corpus = Corpus(VectorSource(list(all_10pc)))


#Wordcloud
#-------------------------------------------------

#library(wordcloud)
#set.seed(100)
#wordcloud(names(freq),freq, min.freq=500,colors=brewer.pal(6,"Dark2"))


#Ngrams (RWeka package)
#-------------------------------------------------

OnegramTokenizer = function(x) WordTokenizer(x)
dtm = DocumentTermMatrix(all_sample_corpus, control = list(tokenize = OnegramTokenizer, wordLengths = c(1, Inf)))

freq = colSums(as.matrix(dtm)) 
counts_dtm = data.frame(Word=names(freq), Freq=freq) 
counts_dtm = counts_dtm[counts_dtm$Freq>2,] #Pruning

counts_dtm$Word = droplevels(counts_dtm$Word)
save(counts_dtm, file = "counts_dtm.RData")
save(freq, file = "freq.RData")
rm(dtm)
rm(counts_dtm)
rm(freq)


TwogramTokenizer = function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
dtm2 = DocumentTermMatrix(all_sample_corpus, control = list(tokenize = TwogramTokenizer))

freq2 = colSums(as.matrix(dtm2)) 
counts_dtm2 = data.frame(Word=names(freq2), Freq=freq2) 
counts_dtm2 = counts_dtm2[counts_dtm2$Freq>2,] #Pruning

counts_dtm2$Word = droplevels(counts_dtm2$Word)
save(counts_dtm2, file = "counts_dtm2.RData")
save(freq2, file = "freq2.RData")
rm(dtm2)
rm(counts_dtm2)
rm(freq2)


ThreegramTokenizer = function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
dtm3 = DocumentTermMatrix(all_sample_corpus, control = list(tokenize = ThreegramTokenizer))

freq3 = colSums(as.matrix(dtm3))
counts_dtm3 = data.frame(Word=names(freq3), Freq=freq3) 
counts_dtm3 = counts_dtm3[counts_dtm3$Freq>2,] #Pruning

counts_dtm3$Word = droplevels(counts_dtm3$Word)
save(counts_dtm3, file = "counts_dtm3.RData")
save(freq3, file = "freq3.RData")
rm(dtm3)
rm(counts_dtm3)
rm(freq3)


FourgramTokenizer = function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
dtm4 = DocumentTermMatrix(all_sample_corpus, control = list(tokenize = FourgramTokenizer))

freq4 = colSums(as.matrix(dtm4)) 
counts_dtm4 = data.frame(Word=names(freq4), Freq=freq4) 
counts_dtm4 = counts_dtm4[counts_dtm4$Freq>2,] #Pruning

counts_dtm4$Word = droplevels(counts_dtm4$Word)
save(counts_dtm4, file = "counts_dtm4.RData")
save(freq4, file = "freq4.RData")
rm(dtm4)
rm(counts_dtm4)
rm(freq4)


FivegramTokenizer = function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))
dtm5 = DocumentTermMatrix(all_sample_corpus, control = list(tokenize = FivegramTokenizer))

freq5 = colSums(as.matrix(dtm5)) 
counts_dtm5 = data.frame(Word=names(freq5), Freq=freq5) 
counts_dtm5 = counts_dtm5[counts_dtm5$Freq>2,] #Pruning

counts_dtm5$Word = droplevels(counts_dtm5$Word)
save(counts_dtm5, file = "counts_dtm5.RData")
save(freq5, file = "freq5.RData")
rm(dtm5)
rm(counts_dtm5)
rm(freq5)


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
save(all_sample_corpus, file = "all_sample_corpus.RData")
#save(all_sample_corpus_test, file = "all_sample_corpus_test")


# load(dtm_test, file = "dtm_test.RData")
# load(dtm2_test, file = "dtm2_test.RData")
# load(dtm3_test, file = "dtm3_test.RData")
# load(dtm4_test, file = "dtm4_test.RData")
# load(dtm5_test, file = "dtm5_test.RData")

#load(all_sample_corpus_test, file = "all_sample_corpus_test")


load("counts_dtm.RData")
load("counts_dtm2.RData")
load("counts_dtm3.RData")
load("counts_dtm4.RData")
load("counts_dtm5.RData")

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


save(counts_dtm, file = "counts_dtm.RData")
save(counts_dtm2, file = "counts_dtm2.RData")
save(counts_dtm3, file = "counts_dtm3.RData")
save(counts_dtm4, file = "counts_dtm4.RData")
save(counts_dtm5, file = "counts_dtm5.RData")





#Kneser-Ney smoothing
#---------------------------------------------

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
  print(i/length(unique_w2)*100)
  
}

colnames(kn) = c("2nd Word", "Freq", "No. of bigrams it completes", "Pcont")

kn = kn[order(-kn$Pcont),]
#save(kn, file = "kn.RData")



#Things to load
#-------------------------------------------

dict = read_lines("texts/Dict.txt")

load(file = "counts_dtm_2pc.RData")
load(file = "counts_dtm2_2pc.RData")
load(file = "counts_dtm3_2pc.RData")
load(file = "counts_dtm4_2pc.RData")
load(file = "counts_dtm5_2pc.RData")

load(file = "kn_2pc.RData")

unknown1 = as.character(kn[1,1])
unknown2 = as.character(kn[2,1])
unknown3 = as.character(kn[3,1])

unknowns = data.frame(unknown1,unknown2,unknown3)




#Prediction function
#-------------------------------------

NWP = function(new_word, unknowns) {
  
  new_word_full = new_word
  
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
        
        Five_matches = as.character(counts_dtm5$Word[grep(paste("^", new_word_full, "\\b", sep = ""), counts_dtm5$Word)])
        Five_matches_f = as.numeric(counts_dtm5$Freq[grep(paste("^", new_word_full, "\\b", sep = ""), counts_dtm5$Word)])
        
        if (length(Five_matches) == 0) {
          
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
          
          pred5 = data.frame()
          
          while (i <= x) {
            
            pred5[i,1] = (paste("(Fivegram)", new_word_full_original, "|", as.character(markov$Word[i])))
            pred5[i,2] = markov[i,2]
            pred5[i,3] = counts_dtm5$Prob[counts_dtm5$Word == (paste(new_word_full, as.character(markov$Word[i])))]
            i=i+1
            
          }
          
          colnames(pred5) = c("Fivegram Prediction", "Freq", "Prob")
          pred5$Prob = round(pred5$Prob, 3)
          
          new_word_full = paste(new_word_split[[1]][2],new_word_split[[1]][3],new_word_split[[1]][4])
          new_word_split = strsplit(new_word_full, " ")

        }
      }
      
      
        
      if(length(new_word_split[[1]]) == 3) { #QUADGRAM CHECKER   
      
        Quad_matches = as.character(counts_dtm4$Word[grep(paste("^",new_word_full, "\\b", sep = ""), counts_dtm4$Word)])
        Quad_matches_f = as.numeric(counts_dtm4$Freq[grep(paste("^",new_word_full, "\\b", sep = ""), counts_dtm4$Word)])
        
        if (length(Quad_matches) == 0 | (sum(Quad_matches_f) / length(Quad_matches_f)) < 2) {
          
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
          
          pred4 = data.frame()
          
          while (i <= x) {
            
            pred4[i,1] = (paste("(Quadgram)", new_word_full_original, "|", as.character(markov$Word[i])))
            pred4[i,2] = markov[i,2]
            pred4[i,3] = counts_dtm4$Prob[counts_dtm4$Word == (paste(new_word_full, as.character(markov$Word[i])))]
            i=i+1
            
          }
          
          colnames(pred4) = c("Quadgram Prediction", "Freq", "Prob")
          pred4$Prob = round(pred4$Prob, 3)
          
          new_word_full = paste(new_word_split[[1]][2],new_word_split[[1]][3])
          new_word_split = strsplit(new_word_full, " ")
        }
      
      }
      
        
        
      if(length(new_word_split[[1]]) == 2) { #TRIGRAM CHECKER  
      
        Tri_matches = as.character(counts_dtm3$Word[grep(paste("^",new_word_full, "\\b", sep = ""), counts_dtm3$Word)])
        Tri_matches_f = as.numeric(counts_dtm3$Freq[grep(paste("^",new_word_full, "\\b", sep = ""), counts_dtm3$Word)])
        
        
        if (length(Tri_matches) == 0 | (sum(Tri_matches_f) / length(Tri_matches_f)) < 2) {
          
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
          
          pred3 = data.frame()
          
          while (i <= x) {
            
            pred3[i,1] = (paste("(Trigram)", new_word_full_original, "|", as.character(markov$Word[i])))
            pred3[i,2] = markov[i,2]
            pred3[i,3] = counts_dtm3$Prob[counts_dtm3 == (paste(new_word_full, as.character(markov$Word[i])))]
            i=i+1
            
          }
          
          colnames(pred3) = c("Trigram Prediction", "Freq", "Prob")
          pred3$Prob = round(pred3$Prob, 3)
          
          new_word_full = new_word_split[[1]][2]
          new_word_split = strsplit(new_word_full, " ")
          
        }
      }
      
      
      if(length(new_word_split[[1]]) == 1) { #BIGRAM CHECKER      

        Bi_matches = as.character(counts_dtm2$Word[grep(paste("^\\b",new_word_full, "\\b","\\b", sep = ""), counts_dtm2$Word)])
        Bi_matches_f = as.numeric(counts_dtm2$Freq[grep(paste("^\\b",new_word_full, "\\b","\\b", sep = ""), counts_dtm2$Word)])
        
        if (length(Bi_matches) == 0) {
          
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
          
          pred2 = data.frame()
          
          while (i <= x) {
            
            pred2[i,1] = (paste("(Bigram)", new_word_full_original, "|", as.character(markov$Word[i])))
            pred2[i,2] = markov[i,2]
            pred2[i,3] = counts_dtm2$Prob[counts_dtm2 == (paste(new_word_full, as.character(markov$Word[i])))]
            i=i+1
            
          }
          
          colnames(pred2) = c("Bigram Prediction", "Freq", "Prob")
          pred2$Prob = round(pred2$Prob, 3)
          no_match = 1
          
        }
      }
        
      
      
      
      
      #No match from a single word
      #---------------------------------
      
      if (no_match == 1) {
        
        if (new_word_split %in% dict) {

          pred1 = data.frame()
          pred1[1,1] = (paste("(Unigram)", new_word_full_original, "|", as.character(unknowns[1,1])))
          pred1[2,1] = (paste("(Unigram)", new_word_full_original, "|", as.character(unknowns[1,2])))
          pred1[3,1] = (paste("(Unigram)", new_word_full_original, "|", as.character(unknowns[1,3])))
          pred1[1,2] = 1
          pred1[1,3] = 0.001
          pred1[2,2] = 1
          pred1[2,3] = 0.001
          pred1[3,2] = 1
          pred1[3,3] = 0.001
          colnames(pred1) = c("Unigram Prediction", "Freq", "Prob")

        } else {
          
          nearest = amatch(new_word_split, counts_dtm$Word[counts_dtm$Word != new_word_split], maxDist = 5, method = "lcs")
          
          pred1 = data.frame()
          pred1[1,1] = paste("Word not found. Did you mean '", counts_dtm$Word[nearest], "' ?", sep = "")
          pred1[1,2] = "2"
          pred1[1,3] = "2"
          
        }
        
      } 
      
      if (!exists("pred5")) {pred5 = data.frame("No Fivegram match", " ", " ")}
      if (!exists("pred4")) {pred4 = data.frame("No Quadgram match", " ", " ")}
      if (!exists("pred3")) {pred3 = data.frame("No Trigram match", " ", " ")}
      if (!exists("pred2")) {pred2 = data.frame("No Bigram match", " ", " ")}
      if (!exists("pred1")) {pred1 = data.frame("No Unigram match", " ", " ")}
        
      colnames(pred5) = c("Prediction", "Freq", "Prob")
      colnames(pred4) = c("Prediction", "Freq", "Prob")
      colnames(pred3) = c("Prediction", "Freq", "Prob")
      colnames(pred2) = c("Prediction", "Freq", "Prob")
      colnames(pred1) = c("Prediction", "Freq", "Prob")
      
      pred5[,1] = as.character(pred5[,1])
      pred5$Freq = as.numeric(pred5$Freq)
      pred5$Prob = as.numeric(pred5$Prob)
      
      pred4[,1] = as.character(pred4[,1])
      pred4$Freq = as.numeric(pred4$Freq)
      pred4$Prob = as.numeric(pred4$Prob)
      
      pred3[,1] = as.character(pred3[,1])
      pred3$Freq = as.numeric(pred3$Freq)
      pred3$Prob = as.numeric(pred3$Prob)
      
      pred2[,1] = as.character(pred2[,1])
      pred2$Freq = as.numeric(pred2$Freq)
      pred2$Prob = as.numeric(pred2$Prob)
      
      pred1[,1] = as.character(pred1[,1])
      pred1$Freq = as.numeric(pred1$Freq)
      pred1$Prob = as.numeric(pred1$Prob)
      
      pred_all = rbind(pred5, pred4, pred3, pred2, pred1)
        
      return(pred_all)
      
    }
}



#Adjust the probabilities 
#------------------------------------

l1 = 0.03125
l2 = 0.0625
l3 = 0.125
l4 = 0.25
l5 = 0.53125


#Adjust Fivegrams
#----------------

i=1

if (prediction5$Prediction[1] != 'No Fivegram match') {

  while (i <= length(prediction5$Prediction)) {
  
  split = strsplit(prediction4$Prediction[i], " ")
  
  prediction5$Adjust_prob[i] =  round(l5*(prediction5$Prob[i]) + 
                                  l4*(counts_dtm4$Prob[counts_dtm4$Word == paste(split[[1]][length(split[[1]])-4],split[[1]][length(split[[1]])-3],split[[1]][length(split[[1]])-2],split[[1]][length(split[[1]])])]) +
                                  l3*(counts_dtm3$Prob[counts_dtm3$Word == paste(split[[1]][length(split[[1]])-3],split[[1]][length(split[[1]])-2],split[[1]][length(split[[1]])])]) +
                                  l2*(counts_dtm2$Prob[counts_dtm2$Word == paste(split[[1]][length(split[[1]])-2],split[[1]][length(split[[1]])])]) +
                                  l1*(counts_dtm$Prob[counts_dtm$Word == split[[1]][length(split[[1]])]]), 3)
  
  i=i+1
  }
  
} else {prediction5$Adjust_prob[1] = 0}


#Adjust Fourgrams
#----------------

i=1

if (prediction4$Prediction[1] != 'No Quadgram match') {
  
  while (i <= length(prediction4$Prediction)) {
    
    split = strsplit(prediction4$Prediction[i], " ")
    
    prediction4$Adjust_prob[i] =  round(l4*(prediction4$Prob[i]) +
                                    l3*(counts_dtm3$Prob[counts_dtm3$Word == paste(split[[1]][length(split[[1]])-3],split[[1]][length(split[[1]])-2],split[[1]][length(split[[1]])])]) +
                                    l2*(counts_dtm2$Prob[counts_dtm2$Word == paste(split[[1]][length(split[[1]])-2],split[[1]][length(split[[1]])])]) + 
                                    l1*(counts_dtm$Prob[counts_dtm$Word == split[[1]][length(split[[1]])]]), 3)
    
    i=i+1
  }
  
} else {prediction4$Adjust_prob[1] = 0}




#Adjust Trigrams
#----------------

i=1

if (prediction3$Prediction[1] != 'No Trigram match') {

  while (i <= length(prediction3$Prediction)) {
    
    split = strsplit(prediction3$Prediction[i], " ")
    
    prediction3$Adjust_prob[i] =  round(l3*(prediction3$Prob[i]) + 
                                       l2*(counts_dtm2$Prob[counts_dtm2$Word == paste(split[[1]][length(split[[1]])-2],split[[1]][length(split[[1]])])]) + 
                                       l1*(counts_dtm$Prob[counts_dtm$Word == split[[1]][length(split[[1]])]]), 3)
    
    i=i+1
  }
}  else {prediction3$Adjust_prob[1] = 0}
  

#Adjust Bigrams
#----------------

i=1

if (prediction2$Prediction[1] != 'No Bigram match') {

  while (i <= length(prediction2$Prediction)) {
    
    split = strsplit(prediction2$Prediction[i], " ")
    
    prediction2$Adjust_prob[i] =  round(l2*(prediction2$Prob[i]) + 
                                    l1*(counts_dtm$Prob[counts_dtm$Word == split[[1]][length(split[[1]])]]), 3)
    
    i=i+1
  } 
} else {prediction2$Adjust_prob[1] = 0}


#Adjust Unigrams
#----------------

i=1

  while (i <= length(prediction1$Prediction)) {
    
    prediction1$Adjust_prob[i] =  l1*(prediction1$Prob[i])
    
    i=i+1
}




#Re-merge,

pred_all = rbind(prediction5, 
                 prediction4, 
                 prediction3, 
                 prediction2, 
                 prediction1)


pred_all = pred_all[order(-pred_all$Adjust_prob),]

pred_all



#Wordcloud
#-------------------------------------------------

library(wordcloud)
set.seed(100)

pred_word = pred_all[(!grepl("No Fivegram|No Quadgram|No Trigram|No Bigram|Word not found", pred_all$Prediction)),]
pred_words = strsplit(pred_word$Prediction, " | ")
pred_words = pred_words

i=1
pred_wordcloud = data.frame()

while (i <= length(pred_words)) {

  pred_wordcloud[i,1] = pred_words[[i]][length(pred_words[[i]])]
  pred_wordcloud[i,2] = pred_words[[i]][length(pred_words[[i]])]
  i=i+1
  
}

pred_wordcloud$Freq = pred_word$Adjust_prob*1000
colnames(pred_wordcloud) = c("Word1", "Word2", "Freq")

if (nrow(pred_word) >3) {

  wordcloud(pred_wordcloud$Word1,pred_wordcloud$Freq, min.freq=1,colors=brewer.pal(6,"Dark2"))

}
  


#Evaluation
#-----------------------------------------


#Create test set

en_blogs = read_lines("texts/en_US/en_US.blogs.txt")
en_tweets = readLines("texts/en_US/en_US.twitter.txt", skipNul = T)
en_news = read_lines("texts/en_US/en_US.news.txt")

prop = 0.01

set.seed(500)
en_blogs_sample_test = sample(en_blogs, (length(en_blogs)*prop))
set.seed(500)
en_tweets_sample_test = sample(en_tweets, (length(en_tweets)*prop))
set.seed(500)
en_news_sample_test = sample(en_news, (length(en_news)*prop))

en_blogs_sample_test = iconv(en_blogs_sample_test, "latin1", "ASCII", "")
en_tweets_sample_test = iconv(en_tweets_sample_test, "latin1", "ASCII", "")
en_news_sample_test = iconv(en_news_sample_test, "latin1", "ASCII", "")

all_sample_test = c(en_blogs_sample_test, en_tweets_sample_test, en_news_sample_test)
all_sample_corpus_test = Corpus(VectorSource(list(all_sample_test)))

rm(en_blogs_sample_test)
rm(en_tweets_sample_test)
rm(en_news_sample_test)
rm(all_sample_test)

OnegramTokenizer = function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
dtm_test <- DocumentTermMatrix(all_sample_corpus_test, control = list(tokenize = OnegramTokenizer))

TwogramTokenizer = function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
dtm2_test <- DocumentTermMatrix(all_sample_corpus_test, control = list(tokenize = TwogramTokenizer))

ThreegramTokenizer = function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
dtm3_test <- DocumentTermMatrix(all_sample_corpus_test, control = list(tokenize = ThreegramTokenizer))

FourgramTokenizer = function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
dtm4_test <- DocumentTermMatrix(all_sample_corpus_test, control = list(tokenize = FourgramTokenizer))

FivegramTokenizer = function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))
dtm5_test <- DocumentTermMatrix(all_sample_corpus_test, control = list(tokenize = FivegramTokenizer))

freq_test = colSums(as.matrix(dtm_test)) 
counts_dtm_test = data.frame(Word=names(freq_test), Freq=freq_test) 
counts_dtm_test = counts_dtm_test[counts_dtm_test$Freq>2,]

freq2_test = colSums(as.matrix(dtm2_test)) 
counts_dtm2_test = data.frame(Word=names(freq2_test), Freq=freq2_test) 
counts_dtm2_test = counts_dtm2_test[counts_dtm2_test$Freq>2,]

freq3_test = colSums(as.matrix(dtm3_test))
counts_dtm3_test = data.frame(Word=names(freq3_test), Freq=freq3_test) 
counts_dtm3_test = counts_dtm3_test[counts_dtm3_test$Freq>2,]

freq4_test = colSums(as.matrix(dtm4_test)) 
counts_dtm4_test = data.frame(Word=names(freq4_test), Freq=freq4_test) 
counts_dtm4_test = counts_dtm4_test[counts_dtm4_test$Freq>2,]

freq5_test = colSums(as.matrix(dtm5_test)) 
counts_dtm5_test = data.frame(Word=names(freq5_test), Freq=freq5_test) 
counts_dtm5_test = counts_dtm5_test[counts_dtm5_test$Freq>2,]


test_set_2 = as.character(sample(counts_dtm2_test$Word, size = 25))
test_set_3 = as.character(sample(counts_dtm3_test$Word, size = 25))
test_set_4 = as.character(sample(counts_dtm4_test$Word, size = 25))
test_set_5 = as.character(sample(counts_dtm5_test$Word, size = 25))

test_set = c(test_set_3,test_set_4,test_set_5)



y = 1
counter = 0
eval = data.frame()

while (y <= length(test_set)) {
  
  test = test_set[y]
  test_split = strsplit(test, " ")
  len_new_word = length(test_split[[1]])
  
  
  if (length(test_split[[1]]) > 4) {
    
    test = paste(test_split[[1]][len_new_word-3],test_split[[1]][len_new_word-2],
                 test_split[[1]][len_new_word-1], test_split[[1]][len_new_word])
    
    test_split = strsplit(test, " ")
    
  } 
  
  
  test_split_partial = test_split[[1]][1:length(test_split[[1]])-1]
  
  if (length(test_split_partial) == 4) {test_func=paste(test_split_partial[1],test_split_partial[2],test_split_partial[3],test_split_partial[4])}
  if (length(test_split_partial) == 3) {test_func=paste(test_split_partial[1],test_split_partial[2],test_split_partial[3])}
  if (length(test_split_partial) == 2) {test_func=paste(test_split_partial[1],test_split_partial[2])}
  if (length(test_split_partial) == 1) {test_func=test_split_partial[1]}
  
  p_return = NWP(test_func, unknowns)
  
  prediction5 = p_return[grep("Five", p_return$Prediction),]
  prediction4 = p_return[grep("Quad", p_return$Prediction),]
  prediction3 = p_return[grep("Tri", p_return$Prediction),]
  prediction2 = p_return[grep("Bi", p_return$Prediction),]
  
  if (length(grep("Uni", p_return$Prediction)) != 0) {
    
    prediction1 = p_return[grep("Uni", p_return$Prediction),]
    
  } else {prediction1 = p_return[grep("not found", p_return$Prediction),]}
  
  
  
  #Adjust the probabilities 
  #------------------------------------
  
  l1 = 0.03125
  l2 = 0.0625
  l3 = 0.125
  l4 = 0.25
  l5 = 0.53125

  
  #Adjust Fivegrams
  #----------------
  
  i=1
  
  if (prediction5$Prediction[1] != 'No Fivegram match') {
    
    while (i <= length(prediction5$Prediction)) {
      
      split = strsplit(prediction4$Prediction[i], " ")
      
      prediction5$Adjust_prob[i] =  round(l5*(prediction5$Prob[i]) + 
                                            l4*(counts_dtm4$Prob[counts_dtm4$Word == paste(split[[1]][length(split[[1]])-4],split[[1]][length(split[[1]])-3],split[[1]][length(split[[1]])-2],split[[1]][length(split[[1]])])]) +
                                            l3*(counts_dtm3$Prob[counts_dtm3$Word == paste(split[[1]][length(split[[1]])-3],split[[1]][length(split[[1]])-2],split[[1]][length(split[[1]])])]) +
                                            l2*(counts_dtm2$Prob[counts_dtm2$Word == paste(split[[1]][length(split[[1]])-2],split[[1]][length(split[[1]])])]) +
                                            l1*(counts_dtm$Prob[counts_dtm$Word == split[[1]][length(split[[1]])]]), 3)
      
      i=i+1
    }
    
  } else {prediction5$Adjust_prob[1] = 0}
  
  
  #Adjust Fourgrams
  #----------------
  
  i=1
  
  if (prediction4$Prediction[1] != 'No Quadgram match') {
    
    while (i <= length(prediction4$Prediction)) {
      
      split = strsplit(prediction4$Prediction[i], " ")
      
      prediction4$Adjust_prob[i] =  round(l4*(prediction4$Prob[i]) +
                                            l3*(counts_dtm3$Prob[counts_dtm3$Word == paste(split[[1]][length(split[[1]])-3],split[[1]][length(split[[1]])-2],split[[1]][length(split[[1]])])]) +
                                            l2*(counts_dtm2$Prob[counts_dtm2$Word == paste(split[[1]][length(split[[1]])-2],split[[1]][length(split[[1]])])]) + 
                                            l1*(counts_dtm$Prob[counts_dtm$Word == split[[1]][length(split[[1]])]]), 3)
      
      i=i+1
    }
    
  } else {prediction4$Adjust_prob[1] = 0}
  
  
  
  
  #Adjust Trigrams
  #----------------
  
  i=1
  
  if (prediction3$Prediction[1] != 'No Trigram match') {
    
    while (i <= length(prediction3$Prediction)) {
      
      split = strsplit(prediction3$Prediction[i], " ")
      
      prediction3$Adjust_prob[i] =  round(l3*(prediction3$Prob[i]) + 
                                            l2*(counts_dtm2$Prob[counts_dtm2$Word == paste(split[[1]][length(split[[1]])-2],split[[1]][length(split[[1]])])]) + 
                                            l1*(counts_dtm$Prob[counts_dtm$Word == split[[1]][length(split[[1]])]]), 3)
      
      i=i+1
    }
  }  else {prediction3$Adjust_prob[1] = 0}
  
  
  #Adjust Bigrams
  #----------------
  
  i=1
  
  if (prediction2$Prediction[1] != 'No Bigram match') {
    
    while (i <= length(prediction2$Prediction)) {
      
      split = strsplit(prediction2$Prediction[i], " ")
      
      prediction2$Adjust_prob[i] =  round(l2*(prediction2$Prob[i]) + 
                                            l1*(counts_dtm$Prob[counts_dtm$Word == split[[1]][length(split[[1]])]]), 3)
      
      i=i+1
    } 
  } else {prediction2$Adjust_prob[1] = 0}
  
  
  #Adjust Unigrams
  #----------------
  
  i=1
  
  while (i <= length(prediction1$Prediction)) {
    
    prediction1$Adjust_prob[i] =  l1*(prediction1$Prob[i])
    
    i=i+1
  }
  
  
  
  
  #Re-merge,
  
  pred_all = rbind(prediction5, 
                   prediction4, 
                   prediction3, 
                   prediction2, 
                   prediction1)
  
  
  pred_all = pred_all[order(-pred_all$Adjust_prob),]
  pred_all
  
  pred_all_split = strsplit(pred_all$Prediction, " | ")

  
  
  
#Make the top 3 results unique,
  
if (pred_all_split[[1]][length(pred_all_split[[1]])] == pred_all_split[[2]][length(pred_all_split[[2]])]) {
  
  pred_all = pred_all[-2,]
  
}
  
  pred_all_split = strsplit(pred_all$Prediction, " | ")
  
  if (pred_all_split[[2]][length(pred_all_split[[2]])] == pred_all_split[[3]][length(pred_all_split[[3]])]) {
    
    pred_all = pred_all[-3,]
    
  }
  
  
  
  p1 = pred_all[1,1]
  p1 = strsplit(p1, " | ")
  p1 = p1[[1]][length(p1[[1]])]
  
  if (dim(pred_all)[1] >= 2) {
    
    p2 = pred_all[2,1]
    p2 = strsplit(p2, " | ")
    p2 = p2[[1]][length(p2[[1]])]
    
  } else {p2 = " "}
  
  if (dim(p_return)[1] >= 3) {
    
    p3 = pred_all[3,1]
    p3 = strsplit(p3, " | ")
    p3 = p3[[1]][length(p3[[1]])]
    
  } else {p3 = " "}
  
  
  eval[y,1] = test
  eval[y,2] = p1
  eval[y,3] = p2
  eval[y,4] = p3
  
  if (test_split[[1]][length(test_split[[1]])] == eval[y,2] | 
      test_split[[1]][length(test_split[[1]])] == eval[y,3] | 
      test_split[[1]][length(test_split[[1]])] == eval[y,4]) {counter = counter+1}
  
  print(y/length(test_set)*100)
  y=y+1
  
}


accuracy = counter / length(test_set)
accuracy*100

eval





#Lambda generator
#----------------------------------

lambda_results = data.frame()
i=1

while (i <= 50) {
  
  order = sample(1:5)
  
  l1 = runif(1)
  l2 = runif(1, min = 0, max = (1-l1))
  l3 = runif(1, min = 0, max = (1-l1-l2))
  l4 = runif(1, min = 0, max = (1-l1-l2-l3))
  l5 = 1-l1-l2-l3-l4
  
  lambdas = data.frame(order)
  lambdas[1,2] = l1
  lambdas[2,2] = l2
  lambdas[3,2] = l3
  lambdas[4,2] = l4
  lambdas[5,2] = l5
  
  l1 = lambdas$V2[lambdas$order == 1]
  l2 = lambdas$V2[lambdas$order == 2]
  l3 = lambdas$V2[lambdas$order == 3]
  l4 = lambdas$V2[lambdas$order == 4]
  l5 = lambdas$V2[lambdas$order == 5]
  
  lambda_results[i,1] = l1
  lambda_results[i,2] = l2
  lambda_results[i,3] = l3
  lambda_results[i,4] = l4
  lambda_results[i,5] = l5
  lambda_results[i,6] = 0
  
  i=i+1
  
}

colnames(lambda_results) = c("l1", "l2", "l3", "l4", "l5", "Accuracy")



#Lambda finder
#----------------------------------

z=1

while (z <= nrow(lambda_results)) {
  
  l1 = lambda_results[z,1] 
  l2 = lambda_results[z,2] 
  l3 = lambda_results[z,3] 
  l4 = lambda_results[z,4]
  l5 = lambda_results[z,5]
  
  
  y = 1
  counter = 0
  eval = data.frame()
  
  while (y <= length(test_set)) {
    
    test = test_set[y]
    test_split = strsplit(test, " ")
    len_new_word = length(test_split[[1]])
    
    
    if (length(test_split[[1]]) > 4) {
      
      test = paste(test_split[[1]][len_new_word-3],test_split[[1]][len_new_word-2],
                   test_split[[1]][len_new_word-1], test_split[[1]][len_new_word])
      
      test_split = strsplit(test, " ")
      
    } 
    
    
    test_split_partial = test_split[[1]][1:length(test_split[[1]])-1]
    
    if (length(test_split_partial) == 4) {test_func=paste(test_split_partial[1],test_split_partial[2],test_split_partial[3],test_split_partial[4])}
    if (length(test_split_partial) == 3) {test_func=paste(test_split_partial[1],test_split_partial[2],test_split_partial[3])}
    if (length(test_split_partial) == 2) {test_func=paste(test_split_partial[1],test_split_partial[2])}
    if (length(test_split_partial) == 1) {test_func=test_split_partial[1]}
    
    p_return = NWP(test_func, unknowns)
    
    prediction5 = p_return[grep("Five", p_return$Prediction),]
    prediction4 = p_return[grep("Quad", p_return$Prediction),]
    prediction3 = p_return[grep("Tri", p_return$Prediction),]
    prediction2 = p_return[grep("Bi", p_return$Prediction),]
    
    if (length(grep("Uni", p_return$Prediction)) != 0) {
      
      prediction1 = p_return[grep("Uni", p_return$Prediction),]
      
    } else {prediction1 = p_return[grep("not found", p_return$Prediction),]}
    

    
    #Adjust Fivegrams
    #----------------
    
    i=1
    
    if (prediction5$Prediction[1] != 'No Fivegram match') {
      
      while (i <= length(prediction5$Prediction)) {
        
        split = strsplit(prediction4$Prediction[i], " ")
        
        prediction5$Adjust_prob[i] =  round(l5*(prediction5$Prob[i]) + 
                                              l4*(counts_dtm4$Prob[counts_dtm4$Word == paste(split[[1]][length(split[[1]])-4],split[[1]][length(split[[1]])-3],split[[1]][length(split[[1]])-2],split[[1]][length(split[[1]])])]) +
                                              l3*(counts_dtm3$Prob[counts_dtm3$Word == paste(split[[1]][length(split[[1]])-3],split[[1]][length(split[[1]])-2],split[[1]][length(split[[1]])])]) +
                                              l2*(counts_dtm2$Prob[counts_dtm2$Word == paste(split[[1]][length(split[[1]])-2],split[[1]][length(split[[1]])])]) +
                                              l1*(counts_dtm$Prob[counts_dtm$Word == split[[1]][length(split[[1]])]]), 3)
        
        i=i+1
      }
      
    } else {prediction5$Adjust_prob[1] = 0}
    
    
    #Adjust Fourgrams
    #----------------
    
    i=1
    
    if (prediction4$Prediction[1] != 'No Quadgram match') {
      
      while (i <= length(prediction4$Prediction)) {
        
        split = strsplit(prediction4$Prediction[i], " ")
        
        prediction4$Adjust_prob[i] =  round(l4*(prediction4$Prob[i]) +
                                              l3*(counts_dtm3$Prob[counts_dtm3$Word == paste(split[[1]][length(split[[1]])-3],split[[1]][length(split[[1]])-2],split[[1]][length(split[[1]])])]) +
                                              l2*(counts_dtm2$Prob[counts_dtm2$Word == paste(split[[1]][length(split[[1]])-2],split[[1]][length(split[[1]])])]) + 
                                              l1*(counts_dtm$Prob[counts_dtm$Word == split[[1]][length(split[[1]])]]), 3)
        
        i=i+1
      }
      
    } else {prediction4$Adjust_prob[1] = 0}
    
    
    
    
    #Adjust Trigrams
    #----------------
    
    i=1
    
    if (prediction3$Prediction[1] != 'No Trigram match') {
      
      while (i <= length(prediction3$Prediction)) {
        
        split = strsplit(prediction3$Prediction[i], " ")
        
        prediction3$Adjust_prob[i] =  round(l3*(prediction3$Prob[i]) + 
                                              l2*(counts_dtm2$Prob[counts_dtm2$Word == paste(split[[1]][length(split[[1]])-2],split[[1]][length(split[[1]])])]) + 
                                              l1*(counts_dtm$Prob[counts_dtm$Word == split[[1]][length(split[[1]])]]), 3)
        
        i=i+1
      }
    }  else {prediction3$Adjust_prob[1] = 0}
    
    
    #Adjust Bigrams
    #----------------
    
    i=1
    
    if (prediction2$Prediction[1] != 'No Bigram match') {
      
      while (i <= length(prediction2$Prediction)) {
        
        split = strsplit(prediction2$Prediction[i], " ")
        
        prediction2$Adjust_prob[i] =  round(l2*(prediction2$Prob[i]) + 
                                              l1*(counts_dtm$Prob[counts_dtm$Word == split[[1]][length(split[[1]])]]), 3)
        
        i=i+1
      } 
    } else {prediction2$Adjust_prob[1] = 0}
    
    
    #Adjust Unigrams
    #----------------
    
    i=1
    
    while (i <= length(prediction1$Prediction)) {
      
      #split = strsplit(prediction1$Prediction[i], " ")
      
      prediction1$Adjust_prob[i] =  l1*(prediction1$Prob[i])
      
      i=i+1
    }
    
    
    
    
    #Re-merge,
    
    pred_all = rbind(prediction5, 
                     prediction4, 
                     prediction3, 
                     prediction2, 
                     prediction1)
    
    
    pred_all = pred_all[order(-pred_all$Adjust_prob),]
    pred_all
    
    pred_all_split = strsplit(pred_all$Prediction, " | ")
    
    
    
    
    #Make the top 3 results unique,
    
    if (pred_all_split[[1]][length(pred_all_split[[1]])] == pred_all_split[[2]][length(pred_all_split[[2]])]) {
      
      pred_all = pred_all[-2,]
      
    }
    
    pred_all_split = strsplit(pred_all$Prediction, " | ")
    
    if (pred_all_split[[2]][length(pred_all_split[[2]])] == pred_all_split[[3]][length(pred_all_split[[3]])]) {
      
      pred_all = pred_all[-3,]
      
    }
    
    
    
    p1 = pred_all[1,1]
    p1 = strsplit(p1, " | ")
    p1 = p1[[1]][length(p1[[1]])]
    
    if (dim(pred_all)[1] >= 2) {
      
      p2 = pred_all[2,1]
      p2 = strsplit(p2, " | ")
      p2 = p2[[1]][length(p2[[1]])]
      
    } else {p2 = " "}
    
    if (dim(p_return)[1] >= 3) {
      
      p3 = pred_all[3,1]
      p3 = strsplit(p3, " | ")
      p3 = p3[[1]][length(p3[[1]])]
      
    } else {p3 = " "}
    
    
    eval[y,1] = test
    eval[y,2] = p1
    eval[y,3] = p2
    eval[y,4] = p3
    
    if (test_split[[1]][length(test_split[[1]])] == eval[y,2] | 
        test_split[[1]][length(test_split[[1]])] == eval[y,3] | 
        test_split[[1]][length(test_split[[1]])] == eval[y,4]) {counter = counter+1}
    
    print(paste("z",z, ":", y/length(test_set)*100))
    y=y+1
    
  }
  
  
  accuracy = counter / length(test_set)
  lambda_results[z,6] = accuracy*100
  z=z+1
  
}


best = head(lambda_results[order(-lambda_results$Accuracy),],1)

l1 = best[1]
l2 = best[2]
l3 = best[3]
l4 = best[4]
l5 = best[5]

lambdas_chosen = data.frame(l1,l2,l3,l4,l5)

save(lambdas_chosen, file = "lambdas_chosen.RData")
load("lambdas_chosen.RData")


