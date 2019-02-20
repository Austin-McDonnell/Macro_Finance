###########################################################################################
#                                BDD Economic Policy Uncertainty
###########################################################################################
library(readtext)
phrases1=c("economic","economy")
phrases2=c("uncertain","uncertainty","uncertainties")
phrases3=c("Congress","deficit","Federal Reserve","legislation","regulation","White House",
           "regulatory","the Fed")


#count the numnber of the news papers that have the three words
pathtext="H:/w6/20021016.txt"
pathtext="H:/w6/Policy Statement/*.txt"

txt=readtext(pathtext,encoding = "utf8")
identifier=c()

for (i in 1:dim(txt)[1]){
  
  tmp_txt=txt[i,2]
  
  #search for the 1st phrase
  ord1=vapply(phrases1,function(x) length(grep(x,tmp_txt)),1)
  #ord1=vapply(phrases1,myFun,y=tmp_txt,1)
  #search for the second phrase
  ord2=vapply(phrases2,function(x) length(grep(x,tmp_txt)),1)
  #ord2=vapply(phrases2,myFun,y=tmp_txt,1)
  #search for the third phrase
  ord3=vapply(phrases3,function(x) length(grep(x,tmp_txt)),1)
  #ord3=vapply(phrases3,myFun,y=tmp_txt,1)
  
  ord=any(ord1>0) & any(ord2>0) & any(ord3>0)
  identifier=c(identifier,ord)
  
}

PU_ratio=sum(identifier)/length(identifier)
print(PU_ratio)

#alternatively, you could deine a function in this format
myFun=function(x,y=tmp_txt){
  l=length(grep(x,y))
  return(l)
} #fucntion to identify whether there is phrase in the ariticle