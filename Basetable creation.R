
#Importing the Data files
AnalyticDataInternetGambling<- read_sas("C:/Users/user/Downloads/IESEG Studies/R Programming/Group Assignment/AnalyticDataInternetGambling.sas7bdat")
RawDataIDemographics<-read_sas("C:/Users/user/Downloads/IESEG Studies/R Programming/Group Assignment/RawDataIDemographics.sas7bdat")
RawDataIIUserDailyAggregation<- read_sas("C:/Users/user/Downloads/IESEG Studies/R Programming/Group Assignment/RawDataIIUserDailyAggregation.sas7bdat")
RawDataIIIPokerChipConversions<- read_sas("C:/Users/user/Downloads/IESEG Studies/R Programming/Group Assignment/RawDataIIIPokerChipConversions.sas7bdat")

#Reading the Country names, Language Description, Application Description and Product Description from csv files
#Countryname - Country Names 
#Appdesc - Application Description 
#Langdesc - Language Description  
#Productdesc - Product Description
Countryname<-read.csv2("C:/Users/user/Downloads/IESEG Studies/R Programming/Group Assignment/Country.csv",sep=",",stringsAsFactors = FALSE)
Appdesc<-read.csv2("C:/Users/user/Downloads/IESEG Studies/R Programming/Group Assignment/Appdesc.csv",sep=",",stringsAsFactors = FALSE)
Langdesc<-read.csv2("C:/Users/user/Downloads/IESEG Studies/R Programming/Group Assignment/Langdesc.csv",sep=",",stringsAsFactors = FALSE)
Productdesc<-read.csv2("C:/Users/user/Downloads/IESEG Studies/R Programming/Group Assignment/Productdesc.csv",sep=",",stringsAsFactors = FALSE)


#Converting characters into date format
RawDataIIUserDailyAggregation$Date<- as.Date(RawDataIIUserDailyAggregation$Date,format="%Y%m%d")
RawDataIIIPokerChipConversions$TransDateTime<- as.POSIXct(RawDataIIIPokerChipConversions$TransDateTime,format="%Y-%m-%d %H:%M:%S")

RawDataIDemographics$RegDate<- as.Date(RawDataIDemographics$RegDate,format="%Y%m%d")
RawDataIDemographics$FirstPay<- as.Date(RawDataIDemographics$FirstPay,format="%Y%m%d")
RawDataIDemographics$FirstAct<- as.Date(RawDataIDemographics$FirstAct,format="%Y%m%d")
RawDataIDemographics$FirstSp<- as.Date(RawDataIDemographics$FirstSp,format="%Y%m%d")
RawDataIDemographics$FirstCa<- as.Date(RawDataIDemographics$FirstCa,format="%Y%m%d")
RawDataIDemographics$FirstGa<- as.Date(RawDataIDemographics$FirstGa,format="%Y%m%d")
RawDataIDemographics$FirstPo<- as.Date(RawDataIDemographics$FirstPo,format="%Y%m%d")


#Merging datasets
#Merging datasets
RawDataIDemographics=merge(RawDataIDemographics,Countryname, by="Country")
RawDataIDemographics$Country=RawDataIDemographics$Country.Name
RawDataIDemographics=merge(RawDataIDemographics,Appdesc, by="ApplicationID")
RawDataIDemographics$ApplicationID=RawDataIDemographics$Application.Description

RawDataIDemographics=merge(RawDataIDemographics,Langdesc, by.x="Language",by.y = "LANGUAGE", all.x = T)
RawDataIDemographics$Language=RawDataIDemographics$Language.Description

AnalyticDataInternetGambling=merge(AnalyticDataInternetGambling,Langdesc, by="LANGUAGE", all.x = T)
AnalyticDataInternetGambling$LANGUAGE=AnalyticDataInternetGambling$Language.Description
RawDataIIUserDailyAggregation=merge(RawDataIIUserDailyAggregation,Productdesc, by="ProductID")
RawDataIIUserDailyAggregation$ProductID=RawDataIIUserDailyAggregation$Product.Description

#Converting Stakes Bets and Winnings less than zero to zero
RawDataIIUserDailyAggregation$Stakes[RawDataIIUserDailyAggregation$Stakes<0]<-0
RawDataIIUserDailyAggregation$Winnings[RawDataIIUserDailyAggregation$Winnings<0]<-0
RawDataIIUserDailyAggregation$Bets[RawDataIIUserDailyAggregation$Bets<0]<-0

#Taking the required fields from AnalyticDataInternetGambling
igdata<-AnalyticDataInternetGambling[,c("USERID","AGE")]
#Taking the required fields from RawDataIDemographics
demog<-RawDataIDemographics[,c("ApplicationID","Country","UserID", "Language", "RegDate","Gender")]
#Alias for RawDataIIUserDailyAggregation
uda<-RawDataIIUserDailyAggregation

# RawDataIIUserDailyAggregation ~~ RFM variables
#RECENCY
#   udarDiff - Number of days since the user had a last betting activity
#   udarwinrecent - total days since the last win
#   udarlossrecent - total days since the last loxx
#   udar_LOS - total since first betting
# FREQUENCY:
#   udaf_betfreq - Total bets
#   udaf_betf2months - Total bets from Feb to March
#   udaf_betm4months - Total bets from April to July
#   udaf_betl2months - Total bets from August to September
#   udaf_betswon - Total bets where winning = 0
#   udaf_betslost - Total bets where winning = 0
#MONETARY
#   udam_stakemoney - Total monetary amount in  stakes
#   udam_winmoney - Total monetary amount user wins
#   udam_profitmoney - Total monetary amount user gains as profit
#   udam_LOSstakeswon - Total monetary amount of stakes compared to LOS(Length of Subscription)

#Since the betting activity is till August 30, 2015, we consider the last date as 1st October 2015
lastdate<-as.Date("2005-10-01")

#Cleaning the Poker Data  
uda_clean <- uda %>%
  group_by(UserID,ProductID) %>%
  mutate(timeperiod = ifelse(Date<as.Date('2005-04-01'),1,ifelse(Date<as.Date('2005-08-01'),2,3))) %>%
            summarize(udarwinrecent = as.numeric(difftime(lastdate,max(Date[Winnings != 0]), units = "days")),
            udarlossrecent=as.numeric(difftime(lastdate,max(Date[Winnings == 0]),units="days")),
            udarLOS=as.numeric(difftime(lastdate,min(Date),units = "days")),
            udaf_betfreq= sum(Bets),
            udaf_betf2months=sum(Bets[timeperiod==1]),
            udaf_betm4months=sum(Bets[timeperiod==2]),
            udaf_betl2months=sum(Bets[timeperiod==3]),
            udaf_betswon=sum(Bets!=0),
            udaf_betslost=sum(Bets==0),
            udam_stakemoney =sum(Stakes),
            udam_winmoney=sum(Winnings),  
            udam_profitmoney=udam_winmoney-udam_stakemoney,
            udam_stakemoneyonLOS=sum(Stakes)/udarLOS,
            udam_winmoneyONLOS=sum(Winnings)/udarLOS,
            udam_ProfitonLOS=udam_profitmoney/udarLOS)

#Eliminating warnings by removing infinite values
uda_clean$udarLOS[is.infinite(uda_clean$udarLOS)] = NA
uda_clean$udarwinrecent[is.infinite(uda_clean$udarwinrecent)] = NA
uda_clean$udarlossrecent[is.infinite(uda_clean$udarlossrecent)] = NA

poker<- RawDataIIIPokerChipConversions
# RawDataIIIPokerChipConversions ~~ RFM variables
#RECENCY
#   poker_Rtransrecent - Number of days since the user had a last poker activity
#   poker_Rbuyrecent - total days since the last poker buy
#   poker_Rsellrecent - total days since the last poker sell
#   poker_RLOS - total days since first poker
# FREQUENCY:
#   pokerF_overall - Overall poker frequency
#   poker_f2months - Total poker plays from Feb to March
#   poker_m4months - Total poker plays from April to July
#   poker_l2months - Total poker plays from August to September
#   pokerF_Buy - Total purchases in poker
#   pokerF_Sell - Total selling in poker

#MONETARY
#   poker_Mbuy - Total money spent in purchasing  poker stakes
#   poker_Msell - Total money received in selling  poker stakes
#   pokerprofit - Total monetary amount user gains as profit
#   poker_Mprofit - Total monetary IN Poker compared to LOS(Length of Subscription)

#Converting the date time format in TransDateTime from POSIXCT to YYYYmmdd

poker$TransDateTime<- as.Date(RawDataIIIPokerChipConversions$TransDateTime,format="%Y-%m-%d")
poker$TransType = ifelse(poker$TransType == 124, 'Buy', 'Sell')


#Cleaning the Poker Data
pokerclean<-poker %>%
  group_by(UserID) %>%
  mutate(timeperiod = ifelse(TransDateTime<as.Date('2005-04-01'),1,ifelse(TransDateTime<as.Date('2005-08-01'),2,3))) %>%
          summarize(poker_Rtransrecent=as.numeric(difftime(lastdate,max(TransDateTime), units = "days")),
                    poker_Rbuyrecent=as.numeric(difftime(lastdate,max(TransDateTime[TransType=="Buy"]),units="days")),
                    poker_Rsellrecent=as.numeric(difftime(lastdate,max(TransDateTime[TransType=="Sell"]),units="days")),
                    poker_RLOS=as.numeric(difftime(lastdate,min(TransDateTime),units="days")),
                    pokerF_overall=n(),
                    pokerF_Buy=sum(TransType=="Buy"),
                    pokerF_Sell=sum(TransType=="Sell"),
                    poker_f2months=sum(timeperiod==1),
                    poker_m4months=sum(timeperiod==2),
                    poker_l2months=sum(timeperiod==3),
                    poker_Mbuy=sum(TransAmount[TransType=="Buy"]),
                    poker_Msell=sum(TransAmount[TransType=="Sell"]),
                    poker_Mprofit=((poker_Msell-poker_Mbuy)/poker_RLOS))

pokerclean$poker_RLOS[is.infinite(pokerclean$poker_RLOS)] = NA
pokerclean$poker_Rbuyrecent[is.infinite(pokerclean$poker_Rbuyrecent)] = NA
pokerclean$poker_Rsellrecent[is.infinite(pokerclean$poker_Rsellrecent)] = NA
pokerclean$poker_Rtransrecent[is.infinite(pokerclean$poker_Rtransrecent)] = NA
                               
############################################################################




#merging the tables to form the basetable
igdata_demog<-merge(igdata, demog, by.x="USERID", by.y="UserID",all.y=T )
igdata_demog_pokerclean<-merge(x=igdata_demog, y=pokerclean, by.x="USERID", by.y="UserID", all.x=T)
igdata_demog_poker_uda_clean<-merge(x=igdata_demog_pokerclean,y=uda_clean,by.x="USERID",by.y="UserID",all.x=T)

igdata_demog_poker_uda_clean$Gender = ifelse(igdata_demog_poker_uda_clean$Gender == 1,"Male","Female")
basetable<-igdata_demog_poker_uda_clean

write.csv(igdata_demog_poker_uda_clean,file="C:/Users/user/Downloads/IESEG Studies/R Programming/Group Assignment/basetable.csv")
