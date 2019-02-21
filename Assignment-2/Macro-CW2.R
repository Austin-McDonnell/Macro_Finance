library(plyr)
library(dplyr)
library(zoo)
library(lubridate)
library(plotly)
library(tseries)
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
txtData$date = as_date(ymd(txtData$date))
txtData$num = seq.int(81)
write.csv(txtData, 'full_text.csv')

#txtData$date = format(as.POSIXct(txtData$date,format='%Y-%m-%d %H:%M:%S'),format='%Y-%m-%d')
sp500$date = as_date(sp500$Date)
sp500 = select (sp500,-c(Volume, High, Low, Date))

stoppingWordsList = unlist(tokenize_words(stoppingWords$text))

#Closing from day before policy announcement to closing after policy announcement return
sp500$returnPercentDayBeforeClose = (lag(sp500$Close) - sp500$Close)/sp500$Close*100

#Return for the day of the Policy Announcement
sp500$returnPercentOpenToClose = (sp500$Close - sp500$Open)/sp500$Open*100

#Open to Next day close return; policy announcement starts on the open
sp500$returnPercentDayOpenDayAfterClose = (lag(sp500$Close) - sp500$Open)/sp500$Open*100


#Stripping the stopping words out of each FOMC Transcript
#Created diffferent types of lists based on different criteria
strippedWords = list()
stemmedWords = list()
uniqueWords = list()
totalWordCount = c()
uniqueWordCount = c()
date = list()

for(i in seq.int(nrow(txtData))){
  
  wordList = unlist(tokenize_words(txtData$text[i]))
  strippedWordList = wordList[wordList %ni% stoppingWordsList]
  uniqueWordList = unique(strippedWordList)
  stemmedWordList = wordStem(strippedWordList, language = "en")
  
  
  strippedWords[[i]] = strippedWordList
  uniqueWords[[i]] = uniqueWordList
  stemmedWords[[i]] = stemmedWordList
  
  date[[i]] = rep(toString(txtData$date[i]), length(stemmedWordList))
  
  totalWordCount[i] = length(wordList)
  uniqueWordCount[i] = length(uniqueWordList)
}


#Flattening lists into a dataframe

stemmedWordsTotal <- plyr::ldply(stemmedWords, data.frame)
colnames(stemmedWordsTotal) = c('stems')

stemWordsCategory = data.frame(stems = unlist(stemmedWords), date = as_date(unlist(date)))
write.csv(stemWordsCategory, 'words_and_dates.csv')

#Counting the frequency of words produced

stemCount = stemmedWordsTotal %>%
  dplyr::group_by(stems) %>%
  dplyr::summarise(Total = n()) %>%
  dplyr::arrange(desc(Total))

#Groups each policy statements word frequency
stemWordsGroup = stemWordsCategory %>%
  dplyr::group_by(date, stems) %>%
  dplyr::summarise(Total = n()) %>%
  dplyr::top_n(5)%>%
  dplyr::arrange(desc(date)) %>%
  dplyr::ungroup()

stemWordsGroup = left_join(stemWordsGroup, txtData %>% select(date, num), by = 'date')

write.csv(stemCount, 'aggregate_stemmed_words_count.csv')
write.csv(stemmedWordsTotal, 'raw_stemmed_words.csv')
write.csv(stemWordsGroup, 'stem_words_group.csv')


##############################################################################
#Plotting

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
    yaxis = list(title = 'FOMC Policy Statement Sentiment'),
    legend = list(orientation = 'h')
  )



#######################################################################################################
# Runing the regressions

mappedData = left_join(txtData, sp500, by='date')

mappedData$sentimentH = sentimentH
mappedData$sentimentLM = sentimentLM

#replacing the only NA value with the following day returns
mappedData$returnPercentDayBeforeClose[is.na(mappedData$returnPercentDayBeforeClose)] = 0.340887260
mappedData$returnPercentOpenToClose[is.na(mappedData$returnPercentOpenToClose)] = -0.051883533
mappedData$returnPercentDayOpenDayAfterClose[is.na(mappedData$returnPercentDayOpenDayAfterClose)] = 0.2888268626


returnList = list(mappedData$returnPercentDayBeforeClose, mappedData$returnPercentOpenToClose,
                  mappedData$returnPercentDayOpenDayAfterClose)
coeffLM = list()
coeffH = list()
rsquaredLM = c()
rsquaredH = c()

for(i in seq.int(3)){
  print(adf.test(unlist(returnList[i])))
  regLM = summary(lm(unlist(returnList[i]) ~ sentimentLM))
  regH = summary(lm(unlist(returnList[i]) ~ sentimentH))
  
  coeffLM[[i]] = regLM$coefficients
  coeffH[[i]] = regH$coefficients
  
  rsquaredLM[i] = regLM$r.squared
  rsquaredH[i] = regH$r.squared
}
