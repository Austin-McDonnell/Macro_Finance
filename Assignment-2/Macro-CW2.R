library(plyr)
library(dplyr)
library(zoo)
library(lubridate)
library(plotly)
library(ts)
library(readtext)
library(tokenizers)
library(SnowballC)
library(SentimentAnalysis)

setwd("C:\\Users\\austi\\Documents\\Github_Repos\\Macro_Finance\\Assignment-2\\Data")

# Reading in Data
sp500 = read.csv('SP500.csv')
txtData = readtext(paste0(getwd(), "/Policy_Statement/*.txt"),
         docvarsfrom = "filenames", 
         docvarnames = c("date"))
stoppingWords = readtext(paste0(getwd(), "/*.txt"),
                   docvarsfrom = "filenames")

#Transforming dates and removing extra data
txtData$date = strptime(ymd(txtData$date), format = '%Y-%m-%d')
sp500$date = as_date(sp500$Date)
sp500 = select (sp500,-c(Volume, High, Low, Date))

stoppingWords = unlist(tokenize_words(stoppingWords$text))


#Stripping the stopping words out of each FOMC Transcript
strippedWords = list()
stemmedWords = list()
uniqueWords = list()
totalWordCount = c()
uniqueWordCount = c()

for(i in seq.int(nrow(txtData))){
  
  wordList = unlist(tokenize_words(txtData$text[i]))
  strippedWordList = setdiff(wordList, stoppingWords)
  uniqueWordList = unique(strippedWordList)
  
  strippedWords[[i]] = strippedWordList
  uniqueWords[[i]] = uniqueWordList
  stemmedWords[[i]] = wordStem(strippedWordList, language = "en")
  
  totalWordCount[i] = length(wordList)
  uniqueWordCount[i] = length(uniqueWordList)
}

#Flattening lists into a dataframe
strippedWordsTotal <- plyr::ldply(strippedWords, data.frame)
colnames(strippedWordsTotal) = c('words')

stemmedWordsTotal <- plyr::ldply(stemmedWords, data.frame)
colnames(stemmedWordsTotal) = c('stems')

#Counting the frequency of words produced
wordCount = strippedWordsTotal %>%
  dplyr::group_by(words) %>%
  dplyr::summarise(Total = n()) %>%
  dplyr::arrange(desc(Total))

stemCount = stemmedWordsTotal %>%
  dplyr::group_by(stems) %>%
  dplyr::summarise(Total = n()) %>%
  dplyr::arrange(desc(Total))

##############################################################################

temp = stemCount %>% head(20)
#plot_ly(x=temp$stems, y=temp$Total, type = "histogram")


#############################################################################

baseOutput = analyzeSentiment(txtData$text, language = "english", aggregate = NULL,
                              removeStopwords = FALSE, stemming = FALSE)




