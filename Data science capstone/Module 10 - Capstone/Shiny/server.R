############################
#R code for Data Science Specialisation Capstone - server.R
#Completed 19/04/16
#Created by RH
############################

#Load libraries,

library("tm")
library("SnowballC")
library("stringr")
library("readr")
library('stringdist')
library(wordcloud)

#Load files,
#-------------------------------------------

load(file = "lambdas_chosen.RData") #Load the lambda values

l1 = lambdas_chosen[1]
l2 = lambdas_chosen[2]
l3 = lambdas_chosen[3]
l4 = lambdas_chosen[4]
l5 = lambdas_chosen[5]

dict = read_lines("texts/Dict.txt") #Load the dictionary file

load(file = "counts_dtm_2pc.RData")
load(file = "counts_dtm2_2pc.RData")
load(file = "counts_dtm3_2pc.RData")
load(file = "counts_dtm4_2pc.RData")
load(file = "counts_dtm5_2pc.RData")

load(file = "kn_2pc.RData") #Load Kneser-Ney data

unknown1 = as.character(kn[1,1])
unknown2 = as.character(kn[2,1])
unknown3 = as.character(kn[3,1])

unknowns = data.frame(unknown1,unknown2,unknown3)
rm(kn)


#Prediction function
#-------------------------------------

NWP = function(new_word, unknowns) {

  new_word_full = new_word
  
  #Clean and format the user-entered word,
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
            
            pred5[i,1] = (paste("(Fivegram) Prediction", new_word_full_original, "|", as.character(markov$Word[i])))
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
            
            pred4[i,1] = (paste("(Quadgram) Prediction:", new_word_full_original, "|", as.character(markov$Word[i])))
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
            
            pred3[i,1] = (paste("(Trigram) Prediction:", new_word_full_original, "|", as.character(markov$Word[i])))
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
            
            pred2[i,1] = (paste("(Bigram) Prediction:", new_word_full_original, "|", as.character(markov$Word[i])))
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
          pred1[1,1] = (paste("(Unigram) Prediction:", new_word_full_original, "|", as.character(unknowns[1,1])))
          pred1[2,1] = (paste("(Unigram) Prediction:", new_word_full_original, "|", as.character(unknowns[1,2])))
          pred1[3,1] = (paste("(Unigram) Prediction:", new_word_full_original, "|", as.character(unknowns[1,3])))
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
          pred1[1,1] = paste("Word not found in dictionary. Did you mean '", counts_dtm$Word[nearest], "' ?", sep = "")
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



#Shiny server
#-----------------------------

shinyServer(
    function(input, output, session) {
        
        observeEvent(input$Predict, {
            
            test_word = input$Sentence
            save(test_word, file = "Sentence.RData")
            
            if (test_word == "" | test_word == " ") {
              
              output$text1 <- renderText("Please enter a word")
              
            } else {
            
            p_return = NWP(test_word, unknowns) 
            
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
              
              prediction1$Adjust_prob[i] =  l1*(prediction1$Prob[i])
              
              i=i+1
            }
            
            
            #Re-merge,
            
            pred_all = rbind(prediction5, 
                             prediction4, 
                             prediction3, 
                             prediction2, 
                             prediction1)
            
            pred_all$Adjust_prob = as.numeric(pred_all$Adjust_prob)
            pred_all = pred_all[order(-pred_all$Adjust_prob),]
            
            pred_all_split = strsplit(pred_all$Prediction, " | ")
            
            #Make the top 3 results unique,
            
            if (pred_all_split[[1]][length(pred_all_split[[1]])] == pred_all_split[[2]][length(pred_all_split[[2]])]) {
              
              pred_all = pred_all[-2,]
              
            }
            
            pred_all_split = strsplit(pred_all$Prediction, " | ")
            
            if (pred_all_split[[2]][length(pred_all_split[[2]])] == pred_all_split[[3]][length(pred_all_split[[3]])]) {
              
              pred_all = pred_all[-3,]
              
            }
            
            
            pred_output1 = pred_all$Prediction[1]
            pred_output2 = pred_all$Prediction[2]
            pred_output3 = pred_all$Prediction[3]

            output$text1 <- renderText({{pred_output1}})
            output$text2 <- renderText({{pred_output2}})
            output$text3 <- renderText({{pred_output3}})
            
            save(pred_output1, file = "pred_output1.RData")
            save(pred_output2, file = "pred_output2.RData")
            save(pred_output3, file = "pred_output3.RData")
            
            
            #Wordcloud
            #-------------------------------------------------
            
            set.seed(100)
            
            pred_word = pred_all[(!grepl("No Fivegram|No Quadgram|No Trigram|No Bigram|Word not found", pred_all$Prediction)),]
            pred_words = strsplit(pred_word$Prediction, " | ")
            pred_words = pred_words
            
            if (length(pred_words) != 0) {
            
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
                
                # Make the wordcloud drawing predictable during a session
                wordcloud_rep <- repeatable(wordcloud)
                
                output$plot <- renderPlot({
                  wordcloud_rep(pred_wordcloud$Word1,pred_wordcloud$Freq, min.freq=1,colors=brewer.pal(6,"Dark2"))
                }) 
                
              }
            }
           }
        })
        
      observeEvent(input$Update1, {
        load("pred_output1.RData")
        u1 = strsplit(pred_output1, " | ")
        u1 = u1[[1]][length(u1[[1]])]
        u1 = paste(input$Sentence, u1)
        updateTextInput(session, "Sentence", value = u1)
        })
        
        observeEvent(input$Update2, {
          load("pred_output2.RData")
          u2 = strsplit(pred_output2, " | ")
          u2 = u2[[1]][length(u2[[1]])]
          u2 = paste(input$Sentence, u2)
          updateTextInput(session, "Sentence", value = u2)
        })
        
        observeEvent(input$Update3, {
          load("pred_output3.RData")
          u3 = strsplit(pred_output3, " | ")
          u3 = u3[[1]][length(u3[[1]])]
          u3 = paste(input$Sentence, u3)
          updateTextInput(session, "Sentence", value = u3)
        })
        
        observeEvent(input$Alt, {
          load("pred_output1.RData")
          u1 = strsplit(pred_output1, "'")
          u1 = u1[[1]][2]
          input_s = input$Sentence
          input_s = strsplit(input_s, " ")
          input_s = head(input_s[[1]],-1)
          input_s = paste(input_s, collapse = " ")
          u1 = paste(input_s, u1)
          updateTextInput(session, "Sentence", value = u1)
        })
        
        observeEvent(input$Keep, {
          load("Sentence.RData")
          #test_word = "Leaving EU would liberate UK"
          sentence = strsplit(test_word, " ")
          sentence = head(sentence[[1]])
          sentence = paste(sentence, collapse = " ")
          pred_output1 = paste("Prediction 1:", sentence, "|", as.character(unknowns[1,1]))
          pred_output2 = paste("Prediction 2:", sentence, "|", as.character(unknowns[1,2]))
          pred_output3 = paste("Prediction 3:", sentence, "|", as.character(unknowns[1,3]))
          save(pred_output1, file = "pred_output1.RData")
          save(pred_output2, file = "pred_output2.RData")
          save(pred_output3, file = "pred_output3.RData")
          output$text1 <- renderText({{pred_output1}})
          output$text2 <- renderText({{pred_output2}})
          output$text3 <- renderText({{pred_output3}})
        })
        
    }
)