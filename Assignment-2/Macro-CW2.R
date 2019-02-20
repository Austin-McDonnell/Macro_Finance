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

'%ni%' <- Negate('%in%')

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

stoppingWordsList = unlist(tokenize_words(stoppingWords$text))


#Stripping the stopping words out of each FOMC Transcript
#Created diffferent types of lists based on different criteria
strippedWords = list()
stemmedWords = list()
uniqueWords = list()
totalWordCount = c()
uniqueWordCount = c()

for(i in seq.int(nrow(txtData))){
  
  wordList = unlist(tokenize_words(txtData$text[i]))
  strippedWordList = wordList[wordList %ni% stoppingWordsList]
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
#Plotting

temp = stemCount %>% head(20)
#plot_ly(x=temp$stems, y=temp$Total, type = "histogram")


#############################################################################

#Pulling in the data and analyzing the sentiment via the HE and LM dictionaries
harvardDict = loadDictionaryGI()
lmDict = loadDictionaryLM()

posH = c()
negH = c()
neuH = c()
posLM = c()
negLM = c()
neuLM = c()
total = c()

#Counts the frequency of Positive, Negative and Neutral words in the Policy statements based
# on the LM and H Dictionaries
for(i in seq.int(81)){
  policy = unlist(stemmedWords[i])
  
  total[i] = length(policy)
  
  posH[i] = sum(policy %in% unlist(harvardDict$positiveWords))
  negH[i] = sum(policy %in% unlist(harvardDict$negativeWords))
  neuH[i] = total[i] - (posH[i] + negH[i])
  
  
  posLM[i] = sum(policy %in% unlist(lmDict$positiveWords))
  negLM[i] = sum(policy %in% unlist(lmDict$negativeWords))
  neuLM[i] = total[i] - (posLM[i] + negLM[i])
  
}

sentimentH = (posH - negH)/total
sentimentLM = (posLM - negLM)/total
#############################################################################################

plot_ly(y=sentimentH, name = 'Harvard Dictionary Sentiment',
        type = "scatter", mode = 'lines') %>%
  
  add_trace(y = sentimentLM, name = 'Loughran and McDonald Dictionary Sentiment',
            type = 'scatter', mode = 'lines', line = list(dash = 'dot', yaxis = "y2")) %>%
  
  layout(
    title = 'FOMC Policy Statement Sentiment Over Time',
    xaxis = list(title = 'Date', tickangle = -45),
    yaxis = list(title = 'FOMC Policy Statement Sentiment'),
    legend = list(x = 0.40, y = 0.93)
  )


#######################################################################################################
# Runing the regression



