########################### IMDB SCRAPE START ###########################
#   5941 webpages to be scraped

require(XML)
require(RCurl)
require(reshape2)
require(plyr)
require(stringr)
require(zoo)

######################################
# Main dataframe
######################################

#Web addresses of scrape source
url<-paste("http://www.imdb.com/event/ev0000003/",1970:2013,sep="")

main.func<-function(x){
  get<-getURI(x)
  doc<-htmlParse(get)
  award<-xpathSApply(doc,"//div[@class='award']//blockquote//h2",xmlValue)
  #First grab the names of the nominated films (winner is always first row)
  film<-data.frame('film'=xpathSApply(doc,"//*[@id='main']/div[1]/blockquote/blockquote[1]/div/strong/a",xmlValue))
  #Second grab the href of the nominated films (winner is always first row)
  film.href<-xpathSApply(doc,"//*[@id='main']/div[1]/blockquote/blockquote[1]/div/strong/a",xmlGetAttr,'href')
  #Third grab the nominee name of the nominated films
  film.person<-NA
  #Fourth grab the href of the nominee of the nominated film
  film.person.href<-NA
  film.href<-paste("http://www.imdb.com",film.href,sep="") #Add root url
  film.person.href<-NA
  final1<-cbind(film,'award'=award[1],film.person,film.href,film.person.href)
  final1$Year<-xpathSApply(doc,"//div[@id='main']/h2",xmlValue) #Dating the Oscar ceremony
  final1$Year<-str_extract(final1$Year,"\\d{4}") #Taking only the year number
  final1$Won<-ifelse(row.names(final1)==1,1,0) #Mark winner
  #Repeat for Lead Actor
  film<-data.frame('film'=xpathSApply(doc,"//*[@id='main']/div[1]/blockquote/blockquote[2]/div/strong/a",xmlValue))
  film.href<-xpathSApply(doc,"//*[@id='main']/div[1]/blockquote/blockquote[2]/div/strong/a",xmlGetAttr,'href')
  film.person<-xpathSApply(doc,"//*[@id='main']/div[1]/blockquote/blockquote[2]/div/a",xmlValue)
  film.person.href<-xpathSApply(doc,"//*[@id='main']/div[1]/blockquote/blockquote[2]/div/a",xmlGetAttr,'href')
  film.href<-paste("http://www.imdb.com",film.href,sep="") #Add root url
  film.person.href<-paste("http://www.imdb.com",film.person.href,sep="") #Add root url
  final2<-cbind(film,'award'=award[2],film.person,film.href,film.person.href)
  final2$Year<-xpathSApply(doc,"//div[@id='main']/h2",xmlValue) #Dating the Oscar ceremony
  final2$Year<-str_extract(final2$Year,"\\d{4}") #Taking only the year number
  final2$Won<-ifelse(row.names(final2)==1,1,0) #Mark winner
  #Repeat for Lead Actress
  film<-data.frame('film'=xpathSApply(doc,"//*[@id='main']/div[1]/blockquote/blockquote[3]/div/strong/a",xmlValue))
  film.href<-xpathSApply(doc,"//*[@id='main']/div[1]/blockquote/blockquote[3]/div/strong/a",xmlGetAttr,'href')
  film.person<-xpathSApply(doc,"//*[@id='main']/div[1]/blockquote/blockquote[3]/div/a",xmlValue)
  film.person.href<-xpathSApply(doc,"//*[@id='main']/div[1]/blockquote/blockquote[3]/div/a",xmlGetAttr,'href')
  film.href<-paste("http://www.imdb.com",film.href,sep="") #Add root url
  film.person.href<-paste("http://www.imdb.com",film.person.href,sep="") #Add root url
  final3<-cbind(film,'award'=award[3],film.person,film.href,film.person.href)
  final3$Year<-xpathSApply(doc,"//div[@id='main']/h2",xmlValue) #Dating the Oscar ceremony
  final3$Year<-str_extract(final3$Year,"\\d{4}") #Taking only the year number
  final3$Won<-ifelse(row.names(final3)==1,1,0) #Mark winner
  #Repeat for Supporting Actor
  film<-data.frame('film'=xpathSApply(doc,"//*[@id='main']/div[1]/blockquote/blockquote[4]/div/strong/a",xmlValue))
  film.href<-xpathSApply(doc,"//*[@id='main']/div[1]/blockquote/blockquote[4]/div/strong/a",xmlGetAttr,'href')
  film.person<-xpathSApply(doc,"//*[@id='main']/div[1]/blockquote/blockquote[4]/div/a",xmlValue)
  film.person.href<-xpathSApply(doc,"//*[@id='main']/div[1]/blockquote/blockquote[4]/div/a",xmlGetAttr,'href')
  film.href<-paste("http://www.imdb.com",film.href,sep="") #Add root url
  film.person.href<-paste("http://www.imdb.com",film.person.href,sep="") #Add root url
  final4<-cbind(film,'award'=award[4],film.person,film.href,film.person.href)
  final4$Year<-xpathSApply(doc,"//div[@id='main']/h2",xmlValue) #Dating the Oscar ceremony
  final4$Year<-str_extract(final4$Year,"\\d{4}") #Taking only the year number
  final4$Won<-ifelse(row.names(final4)==1,1,0) #Mark winner
  #Repeat for Support Actress
  film<-data.frame('film'=xpathSApply(doc,"//*[@id='main']/div[1]/blockquote/blockquote[5]/div/strong/a",xmlValue))
  film.href<-xpathSApply(doc,"//*[@id='main']/div[1]/blockquote/blockquote[5]/div/strong/a",xmlGetAttr,'href')
  film.person<-xpathSApply(doc,"//*[@id='main']/div[1]/blockquote/blockquote[5]/div/a",xmlValue)
  film.person.href<-xpathSApply(doc,"//*[@id='main']/div[1]/blockquote/blockquote[5]/div/a",xmlGetAttr,'href')
  film.href<-paste("http://www.imdb.com",film.href,sep="") #Add root url
  film.person.href<-paste("http://www.imdb.com",film.person.href,sep="") #Add root url
  final5<-cbind(film,'award'=award[5],film.person,film.href,film.person.href)
  final5$Year<-xpathSApply(doc,"//div[@id='main']/h2",xmlValue) #Dating the Oscar ceremony
  final5$Year<-str_extract(final5$Year,"\\d{4}") #Taking only the year number
  final5$Won<-ifelse(row.names(final5)==1,1,0) #Mark winner
  #Repeat for Director
  film<-data.frame('film'=xpathSApply(doc,"//*[@id='main']/div[1]/blockquote/blockquote[6]/div/strong/a",xmlValue))
  film.href<-xpathSApply(doc,"//*[@id='main']/div[1]/blockquote/blockquote[6]/div/strong/a",xmlGetAttr,'href')
  film.person<-xpathSApply(doc,"//*[@id='main']/div[1]/blockquote/blockquote[6]/div/a[1]",xmlValue)
  film.person.href<-xpathSApply(doc,"//*[@id='main']/div[1]/blockquote/blockquote[6]/div/a[1]",xmlGetAttr,'href')
  film.href<-paste("http://www.imdb.com",film.href,sep="") #Add root url
  film.person.href<-paste("http://www.imdb.com",film.person.href,sep="") #Add root url
  final6<-cbind(film,'award'=award[6],film.person,film.href,film.person.href)
  final6$Year<-xpathSApply(doc,"//div[@id='main']/h2",xmlValue) #Dating the Oscar ceremony
  final6$Year<-str_extract(final6$Year,"\\d{4}") #Taking only the year number
  final6$Won<-ifelse(row.names(final6)==1,1,0) #Mark winner
  free(doc)
  #Now combine them all
  final<-rbind(final1,final2,final3,final4,final5,final6)
  return(final)
}
main<-ldply(lapply(url,main.func),data.frame)
main$id<-paste(main$film.person,main$Year) #Make unique id variable

main$award<-as.character(main$award)
main$award[with(main,award=="Best Actor in a Leading Role"|award=="Best Performance by an Actor in a Leading Role")]<-"Best Actor"
main$award[with(main,award=="Best Actress in a Leading Role"|award=="Best Performance by an Actress in a Leading Role")]<-"Best Actress"
main$award[with(main,award=="Best Actor in a Supporting Role"|award=="Best Performance by an Actor in a Supporting Role")]<-"Best Supporting Actor"
main$award[with(main,award=="Best Actress in a Supporting Role"|award=="Best Performance by an Actress in a Supporting Role")]<-"Best Supporting Actress"
main$award[with(main,award=="Best Achievement in Directing")]<-"Best Director"
main$award[with(main,award=="Best Motion Picture of the Year")]<-"Best Picture"

######################################
# Past Oscar win/nomination Data
######################################

#Get data on nominees past oscar wins/nominations
past.func<-function(x){
  require(zoo)
  if(is.na(x)==F){
  url<-paste(unique(x),"awards",sep="")
  get2<-getURI(url)
  doc2<-htmlParse(get2)
  x<-data.frame(readHTMLTable(doc2,stringsAsFactors=FALSE)[1])
  colnames(x)[1:2]<-c("Year","Result")
  x$test<-str_length(x$Year) #Index which rows have no Year column
  #This means an actor has been nominated more than once in one year
  x$Year[x$test>4]<-NA #Replace any Year which has no Year number with an NA
  x$Year<-na.locf(x$Year) #Replace the NA with the previous year
  x$Result<-na.locf(x$Result) #Replace NA with previous result (result of multi-header imdb tables)
  x<-x[order(x$Year),] #Order by ascending years
  x$nominated<-ifelse(x$Result=="WonHonorary Award",0,1) #Make counts for competitive nominations
  x$past.nom<-cumsum(x$nominated) #Cumulative sum for my nominated counts
  x$past.nom<-ifelse(x$nominated==1,x$past.nom-1,x$past.nom) #This says to minus 1 every year where
  #there is a nomination to ensure we are looking at all PAST nominations and ignoring the current year
  x$won<-ifelse(x$Result=="WonOscar",1,0) #Make Oscar won counts
  x$past.win<-cumsum(x$won) #Make cumulative sum for my won counts
  x$past.win<-ifelse(x$won==1,x$past.win-1,x$past.win) #This says to minus 1 for the Won count in every year where
  #the awards are won. This ensures we are looking at all PAST wins and ignoring the current year
  x$Name<-xpathSApply(doc2,"//div//div//div//div[@class='parent']//h3//a",xmlValue) #Pull name
  free(doc2)
  x$id<-paste(x$Name,x$Year) #create unique id variable
  x$test<-duplicated(x$id) #Find the duplicate rows
  x<-subset(x,x$test==FALSE)#Remove any additional rows in the same year.This makes the id unique
  #and assures us we are not taking a current nomination to predict a film's Oscar chances
  x<-x[,c(6,8,10)] #Extracting only variables of interest
  return(x)}
}
past<-ldply(lapply(main$film.person.href,past.func),data.frame)

past<-unique(past)
main<-merge(main,past,by='id',all.x=T)

######################################
# Basic film data
######################################

#Getting basic film statistics
film.func<-function(x){
  get<-getURI(x)
  doc<-htmlParse(get)
  genre<-xpathSApply(doc,"//*[@id='overview-top']/div[2]/a",xmlValue) #Grab all genres listed
  genre<-data.frame(t(data.frame(genre))) #coerce to proper df form
  awards<-xpathSApply(doc,"//*[@id='titleAwardsRanks']/span[2]",xmlValue) #Grab all other wins/noms
  awards<-colsplit(awards,"&",c("other.wins","other.noms")) #Coercing
  awards<-apply(awards,2,function(x){as.numeric(str_extract(x,"\\d+"))}) #Keep only the digits of wins/noms
  awards<-data.frame(t(data.frame(awards)))#coercing
  runtime<-xpathSApply(doc,'//*[@id="overview-top"]/div[2]/time',xmlValue)
  runtime<-str_trim(gsub('\n',"",runtime,fixed=T),'both')
  runtime<-as.numeric(gsub(" min","",runtime)) #remove min. units
  total.oscar<-xpathSApply(doc,'//*[@id="titleAwardsRanks"]/span[1]/b',xmlValue)
  total.oscar<-as.numeric(str_extract(total.oscar,"\\d+"))
  final<-cbind(x,awards,runtime,total.oscar,genre)
  return(final)
}
stat<-ldply(lapply(unique(main$film.href),film.func),data.frame)

#We have too many genres in many different columns. Must clean.
colnames(stat)[6]<-"X4"
stat$drama<-with(stat,ifelse(X1=="Drama"|X2=="Drama"|X3=="Drama"|X4=="Drama",1,0))
stat$comedy<-with(stat,ifelse(X1=="Comedy"|X2=="Comedy"|X3=="Comedy"|X4=="Comedy",1,0))
stat$biopic<-with(stat,ifelse(X1=="Biography"|X2=="Biography"|X3=="Biography"|X4=="Biography"|
                                X1=="History"|X2=="History"|X3=="History"|X4=="History",1,0))
stat$rom<-with(stat,ifelse(X1=="Romance"|X2=="Romance"|X3=="Romance"|X4=="Romance",1,0))
stat$action<-with(stat,ifelse(X1=="Adventure"|X2=="Adventure"|X3=="Adventure"|X4=="Adventure"|
                                X1=="Action"|X2=="Action"|X3=="Action"|X4=="Action",1,0))
stat$drama[is.na(stat$drama)]<-0
stat$comedy[is.na(stat$comedy)]<-0
stat$biopic[is.na(stat$biopic)]<-0
stat$rom[is.na(stat$rom)]<-0
stat$action[is.na(stat$action)]<-0
stat<-stat[,c(-5:-9)]

main<-merge(main,stat,by.x='film.href',by.y='x',all.x=T)


######################################
# Release date data
######################################

#Lets collect release date info (US wide release and Toronto Film premieres only)
release.func<-function(x){
  url<-paste(x,"releaseinfo",sep="") #adding additional url branch
  get<-getURI(url)
  doc<-htmlParse(get)
  release<-data.frame(readHTMLTable(doc,stringsAsFactors=F)[1])
  free(doc)
  r2<-subset(release,release_dates.V1=='USA')
  if(nrow(r2)>1)
  {
  r2<-subset(r2,release_dates.V1=="USA"&(release_dates.V3==''|
              release_dates.V3=="(limited)"|release_dates.V3=="(premiere)"|
               release_dates.V3=='(Los Angeles, California)'|
               release_dates.V3=='(Los Angeles, California)\n (premiere)'|
               release_dates.V3=='(New York City, New York)'|
               release_dates.V3=='(New York City, New York)\n (premiere)'))
  }
  date<-strptime(r2$release_dates.V2,format="%d %b %Y") #Convert to dates
  date<-min(date,na.rm=T) #Take the earliest US release date
  date<-format(date, "%B") #Keep month and day
  tiff.premiere<-sum(str_detect(release$release_dates.V3,"Toronto International Film Festival"))
  cbind(as.character(x),date,tiff.premiere)
}
release<-ldply(lapply(unique(main$film.href),release.func),data.frame)
#Small corrections
release$date[is.na(release$date)]<-c("October","June","December")

main<-merge(main,release,by.x='film.href',by.y='V1',all.x=T)

######################################
# Financial Earnings Data
######################################

mojo.func<-function(x){
  clean<-function(u){
    u<-gsub("'s","",u,fixed=T)
    u<-gsub("&","and",u,fixed=T)
    u<-gsub(" vs.","",u,fixed=T)
    u<-str_replace(u,"é","e")
    u<-gsub(",","",u,fixed=T)
    u<-gsub(".","",u,fixed=T)
    u<-gsub(" ","%20",u,fixed=T)
    return(u)
  }
  name<-clean(x)
  url<-paste("http://www.boxofficemojo.com/search/?q=",name,sep="")
  get<-getURI(url)
  doc<-htmlParse(get)
  href<-xpathSApply(doc,"//*[@id='body']/table[2]/tr/td/table[2]/tr[@bgcolor='#FFFF99']/td/b/font/a",xmlGetAttr,'href')
  film<-xpathSApply(doc,"//*[@id='body']/table[2]/tr/td/table[2]/tr[@bgcolor='#FFFF99']/td[1]/b/font/a",xmlValue)
  if(is.null(href)==T){
    href<-xpathSApply(doc,"//*[@id='body']/table[2]/tr/td/table[2]/tr[2]/td/b/font/a",xmlGetAttr,'href')
    film<-xpathSApply(doc,"//*[@id='body']/table[2]/tr/td/table[2]/tr[2]/td/b/font/a",xmlValue)}
  if(is.null(href)==F){
    href<-paste("http://www.boxofficemojo.com",href,sep="")}
  free(doc)
  if(is.null(href)==F){
    get2<-getURI(href)
    doc2<-htmlParse(get2)
    info<-data.frame(readHTMLTable(doc2,stringsAsFactors=F)[6])
    free(doc2)
    info<-rbind(data.frame('v1'=info[,1]),data.frame('v1'=info[,2]))
    info<-colsplit(na.omit(info$v1),": ",c('v1','v2'))
    info<-data.frame(t(info),stringsAsFactors=F)
    colnames(info)<-info[1,]
    info<-info[-1,]
    bad<-ifelse(is.na(match(as.character(x),film)),1,0)
    cbind(x,film,info,bad,href)}
}

mojo<-ldply(rbind(lapply(unique(main$film)[c(-163,-213,-436)],mojo.func)))
mojo<-mojo[,c("x","Domestic Total Gross","MPAA Rating","Production Budget","bad","href")]
#Clean a bit...drop bad rows
mojo<-mojo[,-c(5,6)]
m<-colsplit(mojo[,2],"Domestic",names=c('v1','v2'))
mojo["Domestic.Total.Gross"]<-m$v1
colnames(mojo)<-c("film","domestic.gross","rating","budget")
x<-c('Z','The Happy Ending','Last Summer',"Alice's Restaurant",'The Hospital',
     'Day for Night','Julia','Equus','Anna','In & Out','Elizabeth')
mojo<-subset(mojo,!film %in% x)

main<-merge(main,mojo,by='film',all.x=T)

#Add some handcoded data
supplement<-read.csv("Finance Supplemental.csv",na.strings="",stringsAsFactors=F)
supplement[28,1]<-'La môme'
main<-merge(main,supplement,by.x='film',by.y='x',all.x=T)

#Keep supplementing missing data with some from numbers.com
get<-getURI("http://www.the-numbers.com/movie/budgets/all")
doc<-htmlParse(get)
tab<-data.frame(readHTMLTable(doc))
tab<-na.omit(tab)
free(doc)
tab1<-subset(tab,NULL.Movie %in% main$film)
tab1<-tab1[,c(3,4,5)]
main<-merge(main,tab1,by.x='film','by.y'='NULL.Movie',all.x=T)
clean<-function(u){
  u<-gsub("$","",u,fixed=T)
  u<-gsub(",","",u,fixed=T)
  u<-gsub(" million","000000",u,fixed=T)
  u<-gsub("N/A",NA,u,fixed=T)
  u<-str_trim(u,side="both")
  return(as.numeric(u))
}
main[,c(29,31,32,34:36)]<-apply(main[,c(29,31,32,34:36)],2,clean)
#Fill NA entries of domestic gross with any possible supplemental entries
main$domestic.gross[is.na(main$domestic.gross)]<-main$Domestic.Total.Gross[is.na(main$domestic.gross)]
main$domestic.gross[is.na(main$domestic.gross)]<-main$NULL.Domestic.Gross[is.na(main$domestic.gross)]
main$budget.x[is.na(main$budget.x)]<-main$budget.y[is.na(main$budget.x)]
main$budget.x[is.na(main$budget.x)]<-main$NULL.Production.Budget[is.na(main$budget.x)]
main$rating.x[is.na(main$rating.x)]<-main$rating.y[is.na(main$rating.x)]
main$domestic.gross<-main$domestic.gross/1000000 #Scale down by 1 million
main$budget<-main$budget.x/1000000
main$rating<-main$rating.x
#Now drop the spent supplemental columns
main<-main[,-c(30:36)]


######################################
# Adapted or Original Film data
######################################

#Now we want to find if the film was adapted. Cannot rely on Adapted screenplay award, because some adapted
#films may not be good enough to be nominated. Look at writers and identify any writers whose contribution is
#NOT (screenplay) or (story). If any, the film will be adapted
adapted.func<-function(x){
  url<-paste(x,"fullcredits",sep="") #adding additional url branch
  get<-getURI(url)
  doc<-htmlParse(get)
  script<-data.frame(readHTMLTable(doc)[2][1])
  detect<-function(x){ #Function to index all writing credits: screenplay and story
    test1<-as.numeric(str_detect(x,"screenplay")) #if 0, writer is not screenwriter
    test2<-as.numeric(str_detect(x,"story")) #if 0, writer is not story artist
    test3<-as.numeric(str_detect(x,"written by")) #if 0, writer is not sole writer
    test4<-as.numeric(str_detect(x,'translation'))#if 0, writer is not a translater
    test4<-test1+test2+test3+test4 #If none of these things, writer is adapted
    test4<-ifelse(test4==0,1,0) #if 1, there is adapted writer, if 0/NA, all original writers
    return(test4)
  }
  free(doc)
  script$adapted<-detect(script[,ncol(script)]) #use function to index all adapted writers
  adapted<-ifelse(sum(script$adapted,na.rm=T)>=1,1,0) #if at least 1 writer is not screenplay/story, it is an adapted film
  cbind(data.frame('film.href'=x),adapted)
}

adapted0<-ldply(lapply(unique(main$film.href)[1:99],adapted.func),data.frame)
adapted<-ldply(lapply(unique(main$film.href)[100:400],adapted.func),data.frame)
adapted2<-ldply(lapply(unique(main$film.href)[401:658],adapted.func),data.frame)
adapted<-rbind(adapted0,adapted,adapted2)
main<-merge(main,adapted,by='film.href',all.x=T)

######################################
#Personal Nominee Data
######################################

#Getting age of actor at time of ceremony
age.func<-function(x){
  if(is.na(x)==F){
  get<-getURI(x)
  doc<-htmlParse(get)
  age<-xpathSApply(doc,"//*[@id='name-born-info']/time/a[2]",xmlValue)
  if(length(age)==0){age<-NA}
  free(doc)
  data.frame(age,'film.person.href'=x)
  }
}
age<-ldply(lapply(unique(main$film.person.href),age.func),data.frame)
main<-merge(main,age,by='film.person.href',all.x=T)
main$age<-as.numeric(main$Year)-as.numeric(as.character(main$age))


#Classify films into categories based on keywords
#HOT: Racism, slavery, gay rights, abortion, poverty, aids, hiv, homophobia
#DARK: terrorism, rape, gang violence, sex trade, human trafficking, torture, heroin, cocaine, serial killer, mass murderer, pedophile, pedophelia

topic.func<-function(x){
  name<-paste(x,'keywords',sep='')
  get<-getURI(name)
  doc<-htmlParse(get)
  tab<-data.frame(readHTMLTable(doc)[1])
  hot<-function(x){
    ifelse(x=='racism'|x=='slavery'|x=='gay rights'|x=='abortion'|x=='poverty'|
                           x=='aids'|x=='hiv'|x=='homophobia',1,0)}
  dark<-function(x){
    ifelse(x=='terrorism'|x=='rape'|x=='gang violence'|x=='sex trade'|x=='human trafficking'|
             x=='torture'|x=='heroin'|x=='cocaine'|x=='serial killer'|x=='mass murderer',1,0)}
  tab2<-data.frame(cbind('film.href'=as.character(x),'Hot'=sum(hot(tab)),'Dark'=sum(dark(tab))))
  tab2[,2:3]<-apply(tab2[,2:3],2,function(x){ifelse(x>=1,1,0)})
  return(tab2)
  }
topic<-ldply(lapply(unique(main$film.href),topic.func),data.frame)
main<-merge(main,topic,by='film.href',all.x=T)
  
#Get ethnicity/ nationality/ sexual orientation?
ethnic.func<-function(x){
  name<-gsub(" ","%20",gsub("\\.","",x))
  url1<-paste("http://search.nndb.com/search/nndb.cgi?unspecified&query=",name,sep="")
  get<-getURL(url1)
  doc<-htmlParse(get)
  url2<-xpathSApply(doc,"/html/body/center/font/table/tr[2]/td[1]/font/a",xmlGetAttr,'href') #Get links from url
  get2<-getURL(url2) #Parse these links
  doc2<-htmlParse(get2)
  profile<-xpathSApply(doc2,"/html/body/center/table/tr/td[1]/table/tr[3]/td/table/tr/td/p",xmlValue)
  if(str_detect(profile[2],"Gender")){
    race<-profile[2]
  }else{race<-profile[3]}
  race2<-strsplit(gsub("([a-z])[A-Z]|[[0-9]]","\\1~",race),"~") #Regex argument to sort string [not perfect]
  race2<-colsplit(unlist(race2),pattern=": ",name=c("v1","v2")) #split on :
  if(sum(str_detect(race2$v1,"Race or Ethnicity"))>=1){race2$v1[race2$v1=="Race or Ethnicity"]<-"Ethnicity"}else{
    race2$v1[race2$v1=="ace or Ethnicity"]<-"Ethnicity"
  }
  race2$v1[race2$v1=="exual orientation"]<-"Sexuality"
  race2<-data.frame(t(race2),stringsAsFactors=F)
  colnames(race2)<-race2[1,]
  if(sum(str_detect(colnames(race2),"Sexuality"))==0){race2$Sexuality<-NA}
  if(sum(str_detect(colnames(race2),'Ethnicity'))==0){race2$Ethnicity<-NA}
  race2<-race2[-1,c("Ethnicity","Sexuality")]
  if(str_detect(profile[3],"Nationality")){
    nation<-profile[3]
  }else{nation<-profile[4]}
  nation<-unlist(strsplit(gsub("([a-z])[A-Z]","\\1~",nation),"~"))[1] #regex to sort string
  nation<-colsplit(nation,pattern=": ",name=c("v1","v2")) #split on :
  nation<-data.frame(t(nation),stringsAsFactors=F)
  nation2<-data.frame(nation[-1,])
  colnames(nation2)<-nation[1,]
  final<-cbind(x,race2,nation2)
  free(doc)
  return(final)
}
ethnic<-ldply(lapply(unique(main$film.person[is.na(main$film.person)==F]),ethnic.func),data.frame)
#Fix small errors
ethnic$Sexuality<-mapvalues(ethnic$Sexuality,from=c("Bisexual [","Gay [","Matter of Dispute","Matter of Dispute ["),
                            to=c("Bisexual","Gay","Straight","Straight"))
ethnic$Sexuality[is.na(ethnic$Sexuality)]<-"Straight"
ethnic$Ethnicity[ethnic$Ethnicity=="White ["]<-"White"
ethnic$Sexuality[ethnic$Sexuality=="Lesbian"]<-'Gay'
ethnic$Sexuality[ethnic$Sexuality=="Lesbian ["]<-'Gay'
ethnic$Sexuality[ethnic$Sexuality=="Straight ["]<-'Straight'
ethnic$Nationality[ethnic$Nationality=="Wales"]<-'Great Britain'
ethnic$Nationality[ethnic$Nationality=='Northern Ireland']<-'Great Britain'
ethnic$Nationality[ethnic$Nationality=='England']<-'Great Britain'
ethnic$Nationality[ethnic$Nationality=='Scotland']<-'Great Britain'



ethnic<-rbind(ethnic1,ethnic2,ethnic3,ethnic4,ethnic5,ethnic6)

######################################
# Metacritic Data
######################################

#Lets also grab the metacritic score (if any)
meta.func<-function(x){
  url<-paste(x,"criticreviews",sep="")
  get<-getURI(url)
  doc<-htmlParse(get)
  meta<-xpathSApply(doc,"//*[@id='main']/div[1]/div[2]/div[1]/div/span",xmlValue)
  free(doc)
  meta<-ifelse(is.null(meta),NA,meta)
  data.frame(meta,'film.href'=x)
}
meta<-ldply(lapply(unique(main$film.href)[1:50],meta.func),data.frame)



######################################
# Other film award wins
######################################

#Get the outcome of the BAFTAs in this category
bafta.url<-paste("http://www.imdb.com/event/ev0000123/",1970:2013,sep="")
bafta.func<-function(x){
  get<-getURI(x)
  doc<-htmlParse(get)
  award<-xpathSApply(doc,"//div[1][@class='award']//blockquote//h2",xmlValue)
  film<-xpathSApply(doc,"//*[@id='main']/div[1]/blockquote/blockquote/div[1]/strong/a[1]",xmlValue)
  if(xpathSApply(doc,'//*[@id="main"]/div[3]/h1',xmlValue)=="David Lean Award for Direction"){
    award2<-xpathSApply(doc,'//*[@id="main"]/div[3]/h1',xmlValue)
    film2<-xpathSApply(doc,'//*[@id="main"]/div[3]/blockquote/blockquote/div[1]/strong/a',xmlValue)
  }
  if(xpathSApply(doc,'//*[@id="main"]/div[4]/h1',xmlValue)=="David Lean Award for Direction"){
    award2<-xpathSApply(doc,'//*[@id="main"]/div[4]/h1',xmlValue)
    film2<-xpathSApply(doc,'//*[@id="main"]/div[4]/blockquote/blockquote/div[1]/strong/a',xmlValue)
  }
  if(is.null(xpathSApply(doc,'//*[@id="main"]/div[5]/h1',xmlValue))==F){
  if(xpathSApply(doc,'//*[@id="main"]/div[5]/h1',xmlValue)=="David Lean Award for Direction"){
    award2<-xpathSApply(doc,'//*[@id="main"]/div[5]/h1',xmlValue)
    film2<-xpathSApply(doc,'//*[@id="main"]/div[5]/blockquote/blockquote/div[1]/strong/a',xmlValue)
  }}
  free(doc)
  award<-award[1:length(film)]
  award<-rbind(data.frame(award),data.frame(award=award2))
  film<-rbind(data.frame(film),data.frame(film=film2))
  data<-data.frame(cbind(award,film))
  data$award<-as.character(data$award)
  data$award[with(data,award=="David Lean Award for Direction"|award=="Best Direction")]<-"Best Director"
  data$award[with(data,award=="Best Film")]<-"Best Picture"
  data$award[with(data,award=="Best Performance by an Actor in a Leading Role"|
                    award=="Best Actor in a Leading Role"|award=="Best Leading Actor")]<-"Best Actor"
  data$award[with(data,award=="Best Performance by an Actress in a Leading Role"|
                    award=="Best Actress in a Leading Role"|award=="Best Leading Actress")]<-"Best Actress"
  data$award[with(data,award=="Best Actor in a Supporting Role"|
                    award=="Best Performance by an Actor in a Supporting Role")]<-"Best Supporting Actor"
  data$award[with(data,award=="Best Actress in a Supporting Role"|
                    award=="Best Performance by an Actress in a Supporting Role")]<-"Best Supporting Actress"
  final<-subset(data,award=="Best Director"|award=="Best Picture"|award=="Best Actor"|award=="Best Actress"|
                 award=="Best Supporting Actor"|award=="Best Supporting Actress")
  return(final)
}
bafta<-ldply(lapply(bafta.url,bafta.func),data.frame)
bafta$bafta<-1
bafta$film<-as.character(bafta$film)
bafta$id<-paste(bafta$film,bafta$award,sep=" ")
bafta<-bafta[,-c(1,2)]

main$id<-paste(main$film,main$award,sep=" ")
main<-merge(main,bafta,by='id',all.x=T)
main$bafta[is.na(main$bafta)]<-0


#Get the outcome of the Golden Globes in this category
globes.url<-paste("http://www.imdb.com/event/ev0000292/",1970:2013,sep="")
globes.func<-function(x){
  get<-getURI(x)
  doc<-htmlParse(get)
  award<-xpathSApply(doc,"//*[@id='main']/div[1]/blockquote/h2",xmlValue)
  film<-xpathSApply(doc,"//*[@id='main']/div[1]/blockquote/blockquote/div[1]/strong/a[1]",xmlValue)
  free(doc)
  data<-data.frame(cbind(award,film))
  data$award<-as.character(data$award)
  data$award[with(data,award=="Best Director - Motion Picture"|
                    award=="Best Motion Picture Director")]<-"Best Director"
  data$award[with(data,award=="Best Motion Picture - Drama")]<-"Best Picture"
  data$award[with(data,award=="Best Motion Picture Actor - Drama"|
                    award=="Best Performance by an Actor in a Motion Picture - Drama"|
                    award=="Best Actor in a Motion Picture - Drama")]<-"Best Actor"
  data$award[with(data,award=="Best Motion Picture Actress - Drama"|
                    award=="Best Performance by an Actress in a Motion Picture - Drama"|
                    award=="Best Actress in a Motion Picture - Drama")]<-"Best Actress"
  data$award[with(data,award=="Best Supporting Actor"|award=="Best Supporting Actor - Motion Picture"|
                    award=="Best Motion Picture Actor in a Supporting Role"|
                    award=="Best Performance by an Actor in a Supporting Role in a Motion Picture"|
                    award=="Best Actor in a Supporting Role - Motion Picture")]<-"Best Supporting Actor"
  data$award[with(data,award=="Best Supporting Actress"|award=="Best Supporting Actress - Motion Picture"|
                    award=="Best Motion Picture Actress in a Supporting Role"|
                    award=="Best Performance by an Actress in a Supporting Role in a Motion Picture"|
                    award=="Best Actress in a Supporting Role - Motion Picture")]<-"Best Supporting Actress"
  final<-subset(data,award=="Best Director"|award=="Best Picture"|award=="Best Actor"|award=="Best Actress"|
                  award=="Best Supporting Actor"|award=="Best Supporting Actress")
  return(final)
}
globes<-ldply(lapply(globes.url,globes.func),data.frame)
globes$globes<-1
globes$film<-as.character(globes$film)
globes$id<-paste(globes$film,globes$award,sep=" ")
globes<-globes[,-c(1,2)]


main<-merge(main,globes,by='id',all.x=T)
main$globes[is.na(main$globes)]<-0

#Get outcome of the SAG awards in this category
sag.url<-paste("http://www.imdb.com/event/ev0000598/",1995:2013,sep="")
sag.func<-function(x){
  get<-getURI(x)
  doc<-htmlParse(get)
  award<-xpathSApply(doc,"//*[@id='main']/div[1]/blockquote/h2",xmlValue)
  film<-xpathSApply(doc,"//*[@id='main']/div[1]/blockquote/blockquote/div[1]/strong/a[1]",xmlValue)
  free(doc)
  data<-data.frame(cbind(award,film))
  data$award<-as.character(data$award)
  data$award[with(data,award=="Outstanding Performance by a Male Actor in a Leading Role")]<-"Best Actor"
  data$award[with(data,award=="Outstanding Performance by a Female Actor in a Leading Role")]<-"Best Actress"
  data$award[with(data,award=="Outstanding Performance by a Male Actor in a Supporting Role")]<-"Best Supporting Actor"
  data$award[with(data,award=="Outstanding Performance by a Female Actor in a Supporting Role")]<-"Best Supporting Actress"
  final<-subset(data,award=="Best Actor"|award=="Best Actress"|
                award=="Best Supporting Actor"|award=="Best Supporting Actress")
  return(final)
}
sag<-ldply(lapply(sag.url,sag.func),data.frame)
sag$sag<-1
sag$film<-as.character(sag$film)
sag$id<-paste(sag$film,sag$award,sep=" ")
sag<-sag[,-c(1,2)]

main<-merge(main,sag,by='id',all.x=T)
main$sag[is.na(main$sag)]<-0
main$sag[main$Year<1995]<-NA

#Get outcome of the DGA awards in film direction
dga.url<-paste("http://www.imdb.com/event/ev0000212/",1970:2013,sep="")
dga.func<-function(x){
  get<-getURI(x)
  doc<-htmlParse(get)
  if(xpathSApply(doc,'//*[@id="main"]/div/blockquote/h2[1]',xmlValue)==
       "Outstanding Directorial Achievement in Motion Pictures"){
    film<-xpathSApply(doc,'//*[@id="main"]/div/blockquote/blockquote[1]/div[1]/strong/a[1]',xmlValue)
    award<-xpathSApply(doc,'//*[@id="main"]/div/blockquote/h2[1]',xmlValue)
  }
  if(xpathSApply(doc,'//*[@id="main"]/div/blockquote/h2[2]',xmlValue)==
       "Outstanding Directorial Achievement in Motion Pictures"){
    film<-xpathSApply(doc,'//*[@id="main"]/div/blockquote/blockquote[2]/div[1]/strong/a[1]',xmlValue)
    award<-xpathSApply(doc,'//*[@id="main"]/div/blockquote/h2[2]',xmlValue)
  }
  if(length(xpathSApply(doc,'//*[@id="main"]/div/blockquote/h2[3]',xmlValue))>0){
  if(xpathSApply(doc,'//*[@id="main"]/div/blockquote/h2[3]',xmlValue)==
       "Outstanding Directorial Achievement in Motion Pictures"){
    film<-xpathSApply(doc,'//*[@id="main"]/div/blockquote/blockquote[3]/div[1]/strong/a[1]',xmlValue)
    award<-xpathSApply(doc,'//*[@id="main"]/div/blockquote/h2[3]',xmlValue)
  }}
  if(length(xpathSApply(doc,'//*[@id="main"]/div/blockquote/h2[4]',xmlValue))>0){
  if(xpathSApply(doc,'//*[@id="main"]/div/blockquote/h2[4]',xmlValue)==
       "Outstanding Directorial Achievement in Motion Pictures"){
    film<-xpathSApply(doc,'//*[@id="main"]/div/blockquote/blockquote[4]/div[1]/strong/a[1]',xmlValue)
    award<-xpathSApply(doc,'//*[@id="main"]/div/blockquote/h2[4]',xmlValue)
  }}
  if(length(xpathSApply(doc,'//*[@id="main"]/div/blockquote/h2[5]',xmlValue))>0){
  if(xpathSApply(doc,'//*[@id="main"]/div/blockquote/h2[5]',xmlValue)==
       "Outstanding Directorial Achievement in Motion Pictures"){
    film<-xpathSApply(doc,'//*[@id="main"]/div/blockquote/blockquote[5]/div[1]/strong/a[1]',xmlValue)
    award<-xpathSApply(doc,'//*[@id="main"]/div/blockquote/h2[5]',xmlValue)
  }}
  data<-data.frame(cbind(award,film))
  data$award<-as.character(data$award)
  data$award[with(data,award=="Outstanding Directorial Achievement in Motion Pictures")]<-'Best Director'
  final<-subset(data,award=="Best Director")
  return(final)
}
dga<-ldply(lapply(dga.url,dga.func),data.frame)
dga$dga<-1
dga<-dga[,-1]
main<-merge(main,dga,by='film',all.x=T)
main$dga[is.na(main$dga)]<-0

#Get outcome of WGA (Writer's Guild Awards) - drama, adapted and original
wga.url<-paste('http://www.imdb.com/event/ev0000710/',1970:2013,sep='')
wga.func<-function(x){
  get<-getURI(x)
  doc<-htmlParse(get)
  award<-xpathSApply(doc,'//*[@id="main"]/div[1]/blockquote/h2',xmlValue)
  film<-xpathSApply(doc,'//*[@id="main"]/div[1]/blockquote/blockquote/div[1][@class="alt"]/strong/a',xmlValue)
  data<-cbind(award,film)
  data<-subset(data,award=='Best Drama Written Directly for the Screen'|
                 award=='Best Drama Adapted from Another Medium'|
                 award=='Best Original Screenplay'|award=='Best Adapted Screenplay'|
                 award=='Best Screenplay Written Directly for the Screen'|
                 award=='Best Screenplay Based on Material Previously Produced or Published'|
                 award=='Best Screenplay Based on Material from Another Medium')
  return(data)
}
wga<-ldply(lapply(wga.url,wga.func),data.frame)
wga$wga<-1
wga<-wga[,-1]
main<-merge(main,wga,by='film',all.x=T)
main$wga[is.na(main$wga)]<-0

#Get outcome of ACE Eddie awards for editing
eddie.url<-paste('http://www.imdb.com/event/ev0000017/',1970:2013,sep='')
eddie.func<-function(x){
  get<-getURI(x)
  doc<-htmlParse(get)
  award<-xpathSApply(doc,'//*[@id="main"]/div[1]/blockquote/h2',xmlValue)
  award<-award[!award=='Student Category']
  film<-xpathSApply(doc,'//*[@id="main"]/div[1]/blockquote/blockquote/div[1][@class="alt"]/strong/a',xmlValue)
  data<-cbind(award,film)
  data<-subset(data,award=='Best Edited Feature Film'|
                 award=='Best Edited Feature Film - Dramatic'|
                 award=='Best Edited Feature Film (Dramatic)')
  return(data)
}
eddie<-ldply(lapply(eddie.url,eddie.func),data.frame)
eddie$eddie<-1
eddie<-eddie[,-1]
main<-merge(main,eddie,by='film',all.x=T)
main$eddie[is.na(main$eddie)]<-0

#Get outcome of Critic's Choice Awards
critic.url<-paste('http://www.imdb.com/event/ev0000133/',1996:2013,sep='')
critic.func<-function(x){
  get<-getURI(x)
  doc<-htmlParse(get)
  award1<-xpathSApply(doc,'//*[@id="main"]/div/blockquote/h2[1]',xmlValue)
  film1<-xpathSApply(doc,'//*[@id="main"]/div/blockquote/blockquote[1]/div[1]/strong/a',xmlValue)
  award2<-xpathSApply(doc,'//*[@id="main"]/div/blockquote/h2[2]',xmlValue)
  film2<-xpathSApply(doc,'//*[@id="main"]/div/blockquote/blockquote[2]/div[1]/strong/a',xmlValue)
  award3<-xpathSApply(doc,'//*[@id="main"]/div/blockquote/h2[3]',xmlValue)
  film3<-xpathSApply(doc,'//*[@id="main"]/div/blockquote/blockquote[3]/div[1]/strong/a',xmlValue)
  award4<-xpathSApply(doc,'//*[@id="main"]/div/blockquote/h2[4]',xmlValue)
  film4<-xpathSApply(doc,'//*[@id="main"]/div/blockquote/blockquote[4]/div[1]/strong/a',xmlValue)
  if(is.null(film4)==T){film4<-NA}
  award5<-xpathSApply(doc,'//*[@id="main"]/div/blockquote/h2[5]',xmlValue)
  film5<-xpathSApply(doc,'//*[@id="main"]/div/blockquote/blockquote[5]/div[1]/strong/a',xmlValue)
  award6<-xpathSApply(doc,'//*[@id="main"]/div/blockquote/h2[6]',xmlValue)
  film6<-xpathSApply(doc,'//*[@id="main"]/div/blockquote/blockquote[6]/div[1]/strong/a',xmlValue)
  award7<-xpathSApply(doc,'//*[@id="main"]/div/blockquote/h2[7]',xmlValue)
  film7<-xpathSApply(doc,'//*[@id="main"]/div/blockquote/blockquote[7]/div[1]/strong/a',xmlValue)
  award8<-xpathSApply(doc,'//*[@id="main"]/div/blockquote/h2[8]',xmlValue)
  film8<-xpathSApply(doc,'//*[@id="main"]/div/blockquote/blockquote[8]/div[1]/strong/a',xmlValue)
  free(doc)
  data<-data.frame(rbind(cbind(award1,film1),cbind(award2,film2),cbind(award3,film3),cbind(award4,film4),cbind(award5,film5),
        cbind(award6,film6),cbind(award7,film7),cbind(award8,film8)))
  data<-subset(data,award1=='Best Picture'|award1=='Best Actor'|award1=='Best Actress'|
                 award1=='Best Supporting Actor'|award1=='Best Supporting Actress'|award1=='Best Director')
  return(data)
}
critic<-ldply(lapply(critic.url,critic.func),data.frame)
critic$critic<-1
critic$film1<-as.character(critic$film1)
critic$id<-paste(critic$film1,critic$award,sep=" ")
critic<-critic[,-c(1,2)]

main<-merge(main,critic,by='id',all.x=T)
main$critic[is.na(main$critic)]<-0
main$critic[main$Year<1996]<-NA

#Get nominees of the Oscar screenplay (original and adapted) award (SPECIAL...want all nominees)
script.func<-function(x){
  get<-getURI(x)
  doc<-htmlParse(get)
  award<-xpathSApply(doc,'//*[@id="main"]/div[1]/blockquote/h2[7]',xmlValue)
  award2<-xpathSApply(doc,'//*[@id="main"]/div[1]/blockquote/h2[8]',xmlValue)
  film<-xpathSApply(doc,"//*[@id='main']/div[1]/blockquote/blockquote[7]/div/strong/a",xmlValue)
  film2<-xpathSApply(doc,"//*[@id='main']/div[1]/blockquote/blockquote[8]/div/strong/a",xmlValue)
  data<-cbind(data.frame(film),award)
  data2<-cbind(data.frame('film'=film2),'award'=award2)
  data<-rbind(data,data2)
  return(data)
}
script.nom<-ldply(lapply(url,script.func),data.frame)
script.nom$script.nom<-1
script.nom<-script.nom[,-2]
main<-merge(main,script.nom,by.y='film',all.x=T)
main$script.nom[is.na(main$script.nom)]<-0

#Get outcome of the Oscar editing award (SPECIAL...want all nominees)
edit.func<-function(x){
  get<-getURI(x)
  doc<-htmlParse(get)
  award<-xpathSApply(doc,'//*[@id="main"]/div[1]/blockquote/h2[13]',xmlValue)
  film<-xpathSApply(doc,"//*[@id='main']/div[1]/blockquote/blockquote[13]/div/strong/a",xmlValue)
  data<-cbind(data.frame(film),award)
  return(data)
}
edit.nom<-ldply(lapply(url,edit.func),data.frame)
edit.nom$edit.nom<-1
edit.nom<-edit.nom[,-2]
main<-merge(main,edit.nom,by.y='film',all.x=T)
main$edit.nom[is.na(main$edit.nom)]<-0

#Get outcome of Oscar director award (SPECIAL...want all nominees)
direct.func<-function(x){
  get<-getURI(x)
  doc<-htmlParse(get)
  award<-xpathSApply(doc,'//*[@id="main"]/div[1]/blockquote/h2[6]',xmlValue)
  film<-xpathSApply(doc,"//*[@id='main']/div[1]/blockquote/blockquote[6]/div/strong/a",xmlValue)
  data<-cbind(data.frame(film),award)
  return(data)
}
direct.nom<-ldply(lapply(url,direct.func),data.frame)
direct.nom$direct.nom<-1
direct.nom<-direct.nom[,-2]
main<-merge(main,direct.nom,by.y='film',all.x=T)
main$direct.nom[is.na(main$direct.nom)]<-0

#Get outcome of Oscar picture award (SPECIAL...want all nominees)
picture.func<-function(x){
  get<-getURI(x)
  doc<-htmlParse(get)
  award<-xpathSApply(doc,'//*[@id="main"]/div[1]/blockquote/h2[1]',xmlValue)
  film<-xpathSApply(doc,"//*[@id='main']/div[1]/blockquote/blockquote[1]/div/strong/a",xmlValue)
  data<-cbind(data.frame(film),award)
  return(data)
}
picture.nom<-ldply(lapply(url,picture.func),data.frame)
picture.nom$picture.nom<-1
picture.nom<-picture.nom[,-2]
main<-merge(main,picture.nom,by.y='film',all.x=T)
main$picture.nom[is.na(main$picture.nom)]<-0



#We're done!
