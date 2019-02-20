##########################################################################################
#           Manual Code for Text Mining in Harvard IV Sentiment Dictionary
##########################################################################################
library(SentimentAnalysis)
library(readtext)
library(tokenizers)
library(tm)

#Q1
dictGI=loadDictionaryGI()
pos=dictGI$positiveWords
neg=dictGI$negativeWords
pathtext="C:/Users/YZ1117/Dropbox/BS0357-1819/draft/pset5_2019_ans/20150128.txt"
txt=readtext(pathtext,encoding = "utf8")

#cite in the new stopping words
pathofstop="C:/Users/YZ1117/Dropbox/BS0357-1819/draft/pset5_2019_ans/StopWords_GenericLong.txt"
newstop=(read.csv(pathofstop,header = F,stringsAsFactors = F))[,1]
newstop=tolower(newstop)

tmp_txt=txt[1,2]
tokens=(tokenize_ngrams(tmp_txt,T,1,stopwords = newstop))[[1]]
tokens=tolower(tokens)
tokens=removeNumbers(tokens);tokens=tokens[tokens!=""]
tokens=removePunctuation(tokens);tokens=tokens[tokens!=""]
tokens=tokenize_word_stems(tokens,language = "en",stopwords = newstop)

newtoken=c()
for (ii in 1:length(tokens)){
  newtoken=c(newtoken,tokens[[ii]])
}
tokens=newtoken
tokenlong=c(tokenlong,tokens)
ratio_pos=sum(tokens %in% pos)/length(tokens)
ratio_neg=sum(tokens %in% neg)/length(tokens)
p=tokens[tokens %in% pos]
n=tokens[tokens %in% neg]
ratio_dif=ratio_pos-ratio_neg

mytime=strsplit(txt[,1],'.txt')
mytime1=as.Date(paste(substr(mytime,1,4),substr(mytime,5,6),substr(mytime,7,8),sep="-"),'%Y-%m-%d')
print(p) #positive word list in the document
print("Negative Word List:\n")
print(n) #negative word list in the document
print(ratio_pos)
print(ratio_neg)
print(ratio_dif)

##########################################################################################
#                         R Packages for Text Mining in Harvard 
##########################################################################################
library(SentimentAnalysis)

tmp_txt=txt[,2]
docs=Corpus(VectorSource(tmp_txt))

dtm=DocumentTermMatrix(docs, control = list(weighting = weightTf,
                                            removePunctuation=T,
                                            stopwords=T,
                                            stemming=T))

sentimentGI.dif= analyzeSentiment(dtm,rules=list("SentimentGI"=list(ruleSentiment,loadDictionaryGI())),removeNumbers=T)
sentimentGI.neg= analyzeSentiment(dtm,rules=list("SentimentGI"=list(ruleNegativity,loadDictionaryGI())),removeNumbers=T)
sentimentGI.pos= analyzeSentiment(dtm,rules=list("SentimentGI"=list(rulePositivity,loadDictionaryGI())),removeNumbers=T)
print(sentimentGI.dif)
print(sentimentGI.neg)
print(sentimentGI.pos)
