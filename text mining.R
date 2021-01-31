rm(list=ls())
library(dplyr)
library(stringr)
library(writexl)
library(feather)
library(openxlsx)
library(readxl)
library(rJava)
library(RLongLexTo)
library(wordcloud2)
#df$hx<-target column data goes here
filter(df,hx!="NULL")->df
#----------------------------extract CC--------------------
for(i in 1:nrow(df)){
  r=regmatches(df[i,"hx"],regexpr(pattern = "((\n| |^|\\W)cc(\\W| )|complain|อาการสำคัญ).{5,}?\r",
                                  text = df[i,"hx"],ignore.case = T))
  if(length(r)!=0)r->df[i,"cc"]
  gsub("\r\n","",df[i,"cc"])->df[i,"cc"]
  gsub("เเ","แ",df[i,"cc"])->df[i,"cc"]
}
filter(df,!is.na(cc))->df
#----------------------------split word----------------------
for(i in 1:nrow(df)){
  str_c(RLongLexToC(enc2native(df[i,"cc"])),collapse = " ")->df$split[i]
}
#----------------------------grouping----------------------
{df$group=" "
read_excel("dic.xlsx",trim_ws = F)->dict
for(j in 1:nrow(dict)){
  for(i in 1:nrow(df))
    if(grepl(dict$key[j],df$split[i],ignore.case = T))
      if(df$group[i]==" ") dict$group[j]->df$group[i]
      else paste(dict$group[j], df$group[i],sep = ",")-> df$group[i]}
}

{df$group=" "
read_excel("dic.xlsx",trim_ws = F)->dict
for(j in 1:nrow(dict)){
  for(i in 1:nrow(df))
    if(df$group[i]==" ")
      if(grepl(dict$key[j],df$split[i],ignore.case = T)) dict$group[j]->df$group[i]}      
}
#--------------------------wordcloud---------------------------------------
write.table(df$split,file = "word.txt",row.names = FALSE)
RLongLexToF(inputfilename = "word.txt",outputfilename = "wordcc.txt")
wordcc<-as.vector(strsplit(paste(readLines("wordcc.txt"),collapse = ""),"[|]"))
wordfq<-as.data.frame(table(wordcc))
wordfq<-wordfq[nchar(as.vector(wordfq$wordcc))>1,]
wordfq<-wordfq[order(wordfq$Freq,decreasing = T),]
wordcloud2(wordfq,color = "random-dark",backgroundColor = "white",size = 0.8,
           shape = "cloud")
#--------------------------------------------------------------------------
wordfq<-as.data.frame(table(df$group))
wordfq<-wordfq[order(wordfq$Freq,decreasing = T),]
wordcloud2(wordfq,color = "random-dark",backgroundColor = "white",size = 0.8,
           shape = "cloud")
#--------------------------------------------------------------------------

