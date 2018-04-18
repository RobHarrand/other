# Load libraries and key data ---------------------------------------------

library(twitteR)
library(ROAuth)
library(httr)
library(streamR)
library(translateR)
library('wordcloud')
library('tm')

setwd('C:\\Users\\rob.harrand\\Dropbox\\Kaggle\\JihadWatch')

# STREAMING

# The following four lines assign the right values to the variables that
# are needed for the API call.

requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"

# The string within the quotation marks has to be replaced with the actual
# consumerKey and consumerSecret.
consumerKey <- "UNebmewdL7fjHIyVPkuQGjVOO"
consumerSecret <- "XRvonr6F4VjlAwY0AOmKAdP1zqdx79IDkbZAl2xE71YvLOgCrp"

# Set API Keys
api_key <- "UNebmewdL7fjHIyVPkuQGjVOO"
api_secret <- "XRvonr6F4VjlAwY0AOmKAdP1zqdx79IDkbZAl2xE71YvLOgCrp"
access_token <- "868190366805229568-w9npyLxKg0jqi4IoZcU8lFUbDYY3JL6"
access_token_secret <- "EyNTquy0NfUK8V5lr0CZXKpN8qIJPQ9STXnaCaFkDf8K7"
my_oauth = setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

# The next two lines establish a connection to the Twitter API.
# The system will print a URL which should be copied in a browser to receive a PIN number.
# This PIN has to be entered in the R-console.
my_oauth <- OAuthFactory$new(consumerKey = consumerKey, 
                             consumerSecret = consumerSecret, 
                             requestURL = requestURL, 
                             accessURL = accessURL, 
                             authURL = authURL)

my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
# Once the connection is established we can save it so that we do not have
# repeat this process.



# Load/save data ----------------------------------------------------------

save(my_oauth, file = "my_oauth.Rdata") #Authenication data
load(file = "my_oauth.Rdata") #Authenication data

#setwd('c:\\r')
#save(tweets.all, file = 'tweets.RData')
load(file = 'tweets.RData') #LOAD THIS AS IT'S THE TWEET TEMPLATE

# noTs_history = data.frame(Date=as.Date(character()),
#                           noTs=character(),
#                           noRTs=character(),
#                           stringsAsFactors=FALSE)

#write.csv(noTs_history, file = 'noTs.csv')

noTs_history = read.csv('noTs.csv') #New
noTs_history = read.csv('noTs_history.csv', stringsAsFactors = F) #Restarting
noTs_history$X = NULL
noTs_history$Date = as.POSIXct(noTs_history$Date,format="%Y-%m-%d %H:%M:%S")


# Scraping setup and code - 'Good Morning' -------------------------------------------------

i = 1
Sys.time()

#while (i <= 280) {
while (i <= 100000) {
  
filterStream("tw_t.json", 
             timeout = 3600, 
             oauth = my_oauth, 
             track = c('taghut', 
                       'mushrikeen', 
                       'rafidah', 
                       'kufar', 
                       'kuffir',
                       'kuffar', 
                       'jizya', 
                       'khilafa', 
                       'kufir'), 
             language = 'en')
#filterStream("tw_t.json", timeout = 30, oauth = my_oauth, track = c('dog', 'cat'), language = 'en')
#filterStream("tw_t.json", timeout = 10, oauth = my_oauth, track = 'hello', language = 'en')
tweets_t = parseTweets("tw_t.json")

#ex = tweets_gm$lang != 'ar'
#tweets_gm = tweets_gm[!ex,]
    
#ex = grepl('RT', tweets_gm$text, ignore.case = FALSE) #Remove the RTs
#tweets_gm = tweets_gm[!ex,]

#ex = grepl('taghut|mushrikeen|rafidah|kufar', tweets_gm$text, ignore.case = TRUE) #Remove anything without good morning in the main tweet text
#tweets_gm = tweets_gm[ex,]

#ex = is.na(tweets_gm$place_lat) #Remove any with missing place_latitude information
#tweets_gm = tweets_gm[!ex,]

#tweets.all = rbind(tweets.all, tweets_t) #Add to the collection


# 
# res <- translate(content.vec = "Hello world", 
#                  microsoft.client.id = "dc10d31f-4c1f-4e27-8928-8bc85bfd0e6d", 
#                  microsoft.client.secret = "790dc39f94ba40ea86cc11eb71aa2f2a", 
#                  source.lang = "en", 
#                  target.lang = "de")
# res



removeURL <- function(x) gsub("http[[:alnum:][:punct:]]*", "", x)
tweets_t$text = removeURL(tweets_t$text)

removeStuff <- function(x) gsub("[[:punct:]]", " ", x)
tweets_t$text = removeStuff(tweets_t$text)

noTs = length(tweets_t$text)
noRTs = table(grepl('RT', tweets_t$text, ignore.case = FALSE))[2]

tweets_t$text <- iconv(tweets_t$text, 'UTF-8', 'ASCII')

corpus = Corpus(VectorSource(list(tweets_t$text)))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers) 
corpus = tm_map(corpus, stripWhitespace)
#corpus = tm_map(corpus, function(x) iconv(enc2utf8(x), sub = "byte"))
corpus = tm_map(corpus, removeWords, stopwords('english'))

dtm_tweets = DocumentTermMatrix(VCorpus(VectorSource(corpus[[1]]$content)))
freq_tweets <- colSums(as.matrix(dtm_tweets))

#sentiments = calculate_sentiment(names(freq_tweets))
#sentiments = cbind(sentiments, as.data.frame(freq_tweets))

freq = as.data.frame(freq_tweets)

keywords = c('taghut', 'mushrikeen', 'rafidah', 'kufar', 'kuffir', 'kuffar', 'jizya', 'khilafa', 'kufir')

freq$Words = row.names(freq)
ex = freq$Words %in% keywords
freq = freq[!ex,]

noTs_new = data.frame('Date' = Sys.time(), 'noTs' = noTs, 'noRTs' = noRTs)
noTs_history = rbind(noTs_history, noTs_new)

date_for_file = gsub(' ', '_', noTs_new$Date)
date_for_file = gsub(':', '_', date_for_file)


if (length(freq$freq_tweets) == 0) {i=i+1} else {
  
  png(paste('t', date_for_file, '.png', sep = ""))
  
  
  par(mfrow=c(2,1))
  par(mar=c(4,4,1,4))
  
  set.seed(100)
  wordcloud(row.names(freq),
            freq$freq_tweets, 
            min.freq=max(freq$freq_tweets)*0.3,
            colors=brewer.pal(6,"Dark2"),
            random.order = T,
            random.color = T,
            rot.per = 0.3,
            scale = c(1.5, 0.1))
  
  
  plot(as.POSIXct(noTs_history$Date,format="%Y-%m-%d %H:%M:%S"), 
       noTs_history$noTs, 
       xlim = c(as.POSIXct(noTs_history$Date[1], format="%Y-%m-%d %H:%M:%S"), 
                as.POSIXct(noTs_history$Date[length(noTs_history$Date)], format="%Y-%m-%d %H:%M:%S")),
       ylim = c(0, max(noTs_history$noTs)*1.2),
       ylab = 'No. of tweets',
       xlab = 'Date',
       type = 'l',
       lwd = 2)
  points(as.POSIXct(noTs_history$Date), 
         noTs_history$noTs,
         pch = 16)
  lines(as.POSIXct(noTs_history$Date), 
         noTs_history$noRTs,
         col = 'red',
        lwd = 2)
  points(as.POSIXct(noTs_history$Date), 
         noTs_history$noRTs,
         pch = 16,
         col = 'red')
  legend(as.POSIXct(noTs_history$Date[1], format="%Y-%m-%d %H:%M:%S")+100,
         max(noTs_history$noTs)*1.1,
         lty=c(1,1),
         lwd=c(2.5,2.5),
         legend =c('Tweets', 'Re-tweets'),
         col=c('black', 'red'))
  
  dev.off()
  
  i=i+1
  
  tweet(paste("Update at", Sys.time(), '#radicalism #R'), 
        mediaPath = paste('t', date_for_file, '.png', sep = ""))
  
  
  fn <- "tw_t.json"
  if (file.exists(fn)) file.remove(fn)
  
  write.csv(noTs_history, 'noTs_history.csv')

}
  
#tweet('test')


}











# Scraping setup and code - 'dog vets' ---------------------------------


i = 1

#while (i <= 280) {
  
while (i <= 1) {
  
  filterStream("tw_dog.json", timeout = 300, oauth = my_oauth, track = 'dog, vets', language = 'en')
  tweets_dog = parseTweets("tw_dog.json")
  
  ex = grepl('RT', tweets_dog$text, ignore.case = FALSE) #Remove the RTs
  tweets_dog = tweets_dog[!ex,]
  
  #ex = grepl('dog & vets', tweets_dog$text, ignore.case = TRUE) #Remove anything without 'walk the dog' in the main tweet text
  #tweets_dog = tweets_dog[ex,]
  
  #ex = is.na(tweets_dog$place_lat) #Remove any with missing place_latitude information
  #tweets_dog = tweets_dog[!ex,]
  
  tweets.all = rbind(tweets.all, tweets_dog) #Add to the collection
  
  dup = duplicated(tweets.all$text)
  tweets.all = tweets.all[!dup,]
  
  i=i+1
  
  Sys.sleep(5)
  
}


#write.csv(tweets.all, 'vet_tweets_raw.csv')
#tweets.all = read.csv('vet_tweets_raw.csv')

#dup = duplicated(tweets.all$text)
#tweets.unique = tweets.all[!dup,]

#write.csv(tweets.unique, 'vet_tweets_unique.csv')

tweets.unique = read.csv('vet_tweets_unique.csv', stringsAsFactors = F)

plot(tweets.unique$place_lat, tweets.unique$place_lon)




#Load libraries,
require(maptools)

#Get a world map,
data(wrld_simpl)

par(mar = c(0,0,1,0),
    pin = c(4,2),
    pty = "m",
    xaxs = "i",
    xaxt = 'n',
    xpd = FALSE,
    yaxs = "i",
    yaxt = 'n')


  # plot(wrld_simpl, 
  #      col='dark green', 
  #      bg='white', 
  #      border='black', 
  #      ann=FALSE, 
  #      axes = FALSE, 
  #      main = "Walk my dog", 
  #      xlim = c(-120,10), 
  #      ylim = c(25,53))
  
  plot(wrld_simpl, 
       col='dark green', 
       bg='white', 
       border='black', 
       ann=FALSE, 
       axes = FALSE, 
       main = "Vets!")
  
  points(tweets.unique$place_lon, 
         tweets.unique$place_lat, 
         pch = 16, 
         cex = 0.5, 
         col = 'red')
  
  # text_temp = tweets.unique$text[1]
  # text_temp
  # 
  # text(x=tweets.unique$place_lon[1]-5, 
  #      y=tweets.unique$place_lat[1]-6, 
  #      labels=tweets.unique$text[1],
  #      col = 'red',
  #      cex = 0.5)
  # 
  # arrows(x0=tweets.unique$place_lon[1]-5, 
  #        y0=tweets.unique$place_lat[1]-5, 
  #        x1=tweets.unique$place_lon[1], 
  #        y1=tweets.unique$place_lat[1], 
  #        col='blue', 
  #        length=0.1, 
  #        lwd=3)
  
dev.off()
  

# Cleaning and plotting ---------------------------------------------------

#load(file = 'tweets_all.RData')

#ex = duplicated(tweets.all$id_str)
#tweets.all = tweets.all[!ex,]

library(lubridate)
library(ggplot2)
library(dplyr)

tweets.unique$Date = sapply(strsplit(tweets.unique$created_at, " "), "[[", 3)
tweets.unique$Date = paste(tweets.unique$Date,"-12-16", sep = "")
tweets.unique$Time = sapply(strsplit(tweets.unique$created_at, " "), "[[", 4)
tweets.unique$DateTime = paste(tweets.unique$Date, tweets.unique$Time)
tweets.unique$DateTime = as.POSIXct(tweets.unique$DateTime, format = "%d-%m-%y %H:%M:%S")
tweets.unique$Time = as.POSIXct(tweets.unique$Time, format = "%H:%M:%S")

tweets.unique.arranged = arrange(tweets.unique, DateTime)

plot(tweets.unique.arranged$DateTime)
hist(tweets.unique.arranged$DateTime, breaks = 20)



#tweets.unique$Time = as.POSIXlt(tweets.unique$Time, format = "%H:%M:%S")

tweets.unique.arranged$Hour = hour(tweets.unique.arranged$Time)
tweets.unique.arranged$Minute = minute(tweets.unique.arranged$Time)

#qplot(seq(1,length(tweets.unique$text),1), tweets.unique$DateTime)



# i=0
# j=0
# k=0
# 
# while (i<=23) {
# 
#     while (j<=59) {
#     
#         tweets.unique.arranged$Time_bin[tweets.unique.arranged$Hour == i & tweets.unique.arranged$Minute >= j & tweets.unique.arranged$Minute < j+5] = k
#         k=k+1
#         j=j+5
#     }
#     i=i+1
#     j=0
# }



l = length(tweets.unique.arranged$text)
n = floor(l / 100)

tweets.unique.arranged$Time_bin = 100

k = seq(0,l,n)

i=1

while (i<length(k)) {

    tweets.unique.arranged$Time_bin[(k[i]:k[i+1])] = i
    i=i+1

    }

plot(tweets.unique.arranged$Time_bin)

#qplot(tweets.unique$lon, tweets.unique$lat, col = tweets.unique$Time_bin)


#table(is.na(tweets.unique$lat))
#table(is.na(tweets.unique$place_lat))


#save(tweets.unique, file = 'tweets_all.RData')
write.csv(tweets.unique, file = 'tweets_all.csv')

# 
# 
# tweets.unique$Time = sapply(strsplit(tweets.unique$created_at, " "), "[[", 4)
# tweets.unique$Time = as.POSIXlt(tweets.unique$Time, format = "%H:%M:%S")
# tweets.unique$Time = tweets.unique$Time - 18000
# 
# 
# hist(tweets.unique$Time, breaks = 50)
# 
# ex = grepl('good morning', tweets.unique$text, ignore.case = TRUE)
# tweets.good = tweets.unique[ex,]
# 
# ex = grepl('Thu', tweets.unique$created_at, ignore.case = TRUE)
# tweets.good = tweets.unique[ex,]
# 
# hist(tweets.good$Time, breaks = 50)




# Animation plotting ------------------------------------------------------


#Load libraries,
require(maptools)
require(animation) 

#Get a world map,
data(wrld_simpl)

par(mar = c(0,0,1,0),
    pin = c(4,2),
    pty = "m",
    xaxs = "i",
    xaxt = 'n',
    xpd = FALSE,
    yaxs = "i",
    yaxt = 'n')


#plot(tweets.unique$Time_bin)


i = 1

#Loop through the rows and save the gif...

saveGIF(while (i <= length(table(tweets.unique.arranged$Time_bin))) {
    
    plot(wrld_simpl, col='dark green', bg='white', border='black', ann=FALSE, axes = FALSE, main = "Good morning, Twitter!")
    
    points(tweets.unique.arranged$place_lon[tweets.unique.arranged$Time_bin == i], tweets.unique.arranged$place_lat[tweets.unique.arranged$Time_bin == i], pch = 16, col = 'red')

    #abline(v = mean(tweets.unique.arranged$place_lon[tweets.unique.arranged$Time_bin == i]), col = 'red')
    
     #Plot some text,
#      text(-125,110, paste("Time = ", mean(tweets.unique.arranged$Hour[tweets.unique.arranged$Time_bin == i]), " hrs ",
#                           floor(mean(tweets.unique.arranged$Minute[tweets.unique.arranged$Time_bin == i])), " mins", sep=""),
#                           col = "black", cex = 0.9, font = 2)
    
    temp = tweets.unique.arranged$DateTime[tweets.unique.arranged$Time_bin == i][1]    
    
    text(-40,110, paste("Date/Time = ", temp, " GMT"), col = "black", cex = 1.2, font = 2)
    
#     text(125,110, "Colour = Day's weather", col = "black", cex = 0.9, font = 2)
#     
#     points(85, 100, pch = 21, cex = 2, col = "black", bg = "grey")
#     text(100, 101, " - cloud ", col = "black", cex = 0.8, font = 2)
#     
#     points(115, 100, pch = 21, cex = 2, col = "black", bg = "blue")
#     text(129, 101, " - rain ", col = "black", cex = 0.8, font = 2)
#     
#     points(85, 92, pch = 21, cex = 2, col = "black", bg = "light blue")
#     text(98, 93, " - fine ", col = "black", cex = 0.8, font = 2)
#     
#     points(115, 92, pch = 21, cex = 2, col = "black")
#     text(142, 93, " - not recorded ", col = "black", cex = 0.8, font = 2)
#     
#     #text(-110,103, "Ship's log: ", col = "black", cex = 1, font = 2)
#     text(0, 110, paste("Date: ", Endeavour$Day[i],"/",Endeavour$Month[i],"/",Endeavour$Year[i]), col = "black", cex = 1, font = 2)
#     #text(0, 100, Endeavour$Clearness[i], col = "red", cex = 1, font = 2)
#     #text(0, 90, Endeavour$AllWindForces[i], col = "red", cex = 1, font = 2)
#     #text(0, 90, Endeavour$PrecipitationDescriptor[i], col = "red", cex = 1, font = 2)
#     
#     #Wait a while between plots,
#     ani.pause()
#     
#     #Wipe the text,
#     #rect(-160,85,160,119, col = "light blue", border = NA)
    
    i = i+1
    
}, movie.name = "goodmorning.gif", img.name = "Rplot", interval = 0.1, convert = "convert", ani.width = 800, 
ani.height = 800)



qplot(tweets.unique.arranged$place_lon, tweets.unique.arranged$place_lat, col = as.factor(tweets.unique.arranged$Time_bin))




map <- NULL
mapWorld <- borders("world", colour="black", fill="gray")
map <- ggplot() +   mapWorld

#Now Layer the cities on top
map <- map + geom_point(aes(x=tweets.unique.arranged$place_lon, y=tweets.unique.arranged$place_lat, 
                         color=as.factor(tweets.unique.arranged$Time_bin)) , size=3) + theme(legend.position="none")
map


plot(wrld_simpl, col='dark green', bg='white', border='black', ann=FALSE, axes = FALSE, main = "Good morning, Twitter!")

points(tweets.unique.arranged$place_lon, tweets.unique.arranged$place_lat, pch = 16, col = as.factor(tweets.unique.arranged$Time_bin))




library(ggplot2)
library(grid)
filterStream("tweetsUS.json", locations = c(-125, 25, -66, 50), timeout = 300, 
             oauth = my_oauth)
tweets.df <- parseTweets("tweetsUS.json", verbose = FALSE)
map.data <- map_data("state")
points <- data.frame(x = as.numeric(tweets.df$lon), y = as.numeric(tweets.df$lat))
points <- points[points$y > 25, ]
ggplot(map.data) + geom_map(aes(map_id = region), map = map.data, fill = "white", 
                            color = "grey20", size = 0.25) + expand_limits(x = map.data$long, y = map.data$lat) + 
    theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
          axis.title = element_blank(), panel.background = element_blank(), panel.border = element_blank(), 
          panel.grid.major = element_blank(), plot.background = element_blank(), 
          plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) + geom_point(data = points, 
                                                                                   aes(x = x, y = y), size = 1, alpha = 1/5, color = "darkblue")



table(tweets.df$country)





# General Twitter searching -----------------------------------------------


# Set API Keys
api_key <- "WgI0Z0p5feozqtasJdyKKG0ow"
api_secret <- "8J87c6d5yxwig86yHdT8E5hX8GcBKWuLRzCpabNpAsYojXoOWE"
access_token <- "23829577-ma8Jk6S5GogQCvYOiQppdIYNSHdMKknakHh22JY1c"
access_token_secret <- "hhi7um7oev6QSKRxGmzl2TjKca8zYrfGH67YiBEy4rOEr"
my_oauth = setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)



# Grab latest tweets
tweets_harris <- userTimeline('SamHarrisOrg', n = 100, includeRts = F)
tweets_greenwald <- userTimeline('@ggreenwald', n = 1000, includeRts = T)
tweets_greenwald = twListToDF(tweets_greenwald)

tweets_article <- searchTwitter('A liberal writes in @Guardian how he was radicalized by @SamHarrisOrg', n=1000)
tweets_article = twListToDF(tweets_article)

?searchTwitter

ggplot(tweets_article, aes(x=created)) +
    geom_histogram() +
    theme_set(theme_bw(base_size = 18)) +
    xlab("Date/Time created") + 
    ggtitle("Histogram of Tweets for the 'A liberal writes...' article")



#tweets_harris = tweets_harris[tweets_harris$created > '2016-10-19 00:00:00',]


tweets_SamHarrisOrg <- searchTwitter('from:SamHarrisOrg', n=100)
tweets_SamHarrisOrg = twListToDF(tweets_SamHarrisOrg)

tweets_greenwald = tweets_SamHarrisOrg[tweets_SamHarrisOrg$screenName == 'ggreenwald',]


tweets_SamHarrisOrg_all <- searchTwitter('SamHarrisOrg', n=3000)
tweets_SamHarrisOrg_all = twListToDF(tweets_SamHarrisOrg_all)
tweets_greenwald_about_harris = tweets_SamHarrisOrg_all[tweets_SamHarrisOrg_all$screenName == 'ggreenwald',]

range(tweets_harris$created)
range(tweets_greenwald$created)

tweets_harris = twListToDF(tweets_harris)


# Loop over tweets and extract text
#library(plyr)

#feed_harris = laply(tweets_harris, function(t) t$getText())
#feed_greenwald = laply(tweets_greenwald, function(t) t$getText())

harris_mentions_greenwald = length(grep('@ggreenwald', tweets_harris$text))
greenwald_mentions_harris = length(grep('@SamHarrisOrg', tweets_greenwald$text))

harris_mentions_islam = length(grep('islam', tweets_harris$text))

plot(tweets_harris$created[grep('@ggreenwald', tweets_harris$text)], rownames(tweets_harris)[grep('@ggreenwald', tweets_harris$text)])
plot(tweets_greenwald$created[grep('@SamHarrisOrg', tweets_greenwald$text)], rownames(tweets_greenwald)[grep('@SamHarrisOrg', tweets_greenwald$text)])

plot(tweets_harris$created[grep('islam', tweets_harris$text)], rownames(tweets_harris)[grep('islam', tweets_harris$text)])

plot(tweets_greenwald_all$created, rownames(tweets_greenwald_all), type = 'l')
lines(tweets_SamHarrisOrg$created, rownames(tweets_SamHarrisOrg), col = 'red')







doInstall <- TRUE  # Change to FALSE if you don't want packages installed.
toInstall <- c("ROAuth", "igraph", "ggplot2", "wordcloud", "devtools", "tm",
               "R2WinBUGS", "rmongodb", "scales")
if(doInstall){
    install.packages(toInstall, repos = "http://cran.r-project.org")
    library(devtools)
    # R packages to get twitter and Facebook data
    install_github("streamR", "pablobarbera", subdir="streamR")
    install_github("Rfacebook", "pablobarbera", subdir="Rfacebook")
    # smapp R package
    install_github("smappR", "SMAPPNYU")
}