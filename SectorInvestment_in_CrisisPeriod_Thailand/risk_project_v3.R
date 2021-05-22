library(readxl)
library(tidyverse)
library(data.table)
#install.packages("janitor")
library(janitor)
library(imputeTS)


test1<- read_excel("health/ahc.xlsx", sheet = 1, col_names = FALSE, col_types = NULL, na = "", skip = 20)
test1

test2<- read_excel("health/svh.xlsx", sheet = 1, col_names = FALSE, col_types = NULL, na = "", skip = 20)
test2


test3<- read_excel("health/bdms.xlsx", sheet = 1, col_names = FALSE, col_types = NULL, na = "", skip = 20)
test3


test4<- read_excel("food/apure.xlsx", sheet = 1, col_names = FALSE, col_types = NULL, na = "", skip = 20)
test4

#### step 1 step 20 rows

# step 2 specify  header row 30 excel , row 10 in R


names(test1) <- test1 %>%
  slice(1) %>% 
  unlist(., use.names=FALSE)


test1<- test1[!is.na(names(test1))]

## step 3 sperate trading , statistic , financial data and  ratios 

trading1 <- test1[4:7,]
trading1


statistic1 <- test1[11:21,]
statistic1


fin_data1 <- test1[26:40,][-c(4,11),]
fin_data1

fin_ratio1 <- test1[42:62,][-c(6,9,12),]
fin_ratio1



t_trading1 <- as.data.frame(t(as.matrix(trading1)))
t_trading1<- t_trading1 %>%
  row_to_names(row_number = 1)






statistic1 <- as.data.frame(t(as.matrix(statistic1)))
statistic1<- statistic1 %>%
  row_to_names(row_number = 1)
statistic1



fin_data1 <- as.data.frame(t(as.matrix(fin_data1)))
fin_data1<- fin_data1 %>%
  row_to_names(row_number = 1)
fin_data1



fin_ratio1 <- as.data.frame(t(as.matrix(fin_ratio1)))
fin_ratio1<- fin_ratio1 %>%
  row_to_names(row_number = 1)
fin_ratio1



### step 4 add column "stock_name"


t_trading1$stock_name <- "ahc"
statistic1$stock_name <- "ahc"
fin_data1$stock_name <- "ahc"
fin_ratio1$stock_name <- "ahc"


### step 4.5 create empty dataframe 

trading_base<- t_trading1[0,]
statistic1_base<- statistic1[0,]
fin_data1_base<- fin_data1[0,]
fin_ratio1_base<- fin_ratio1[0,]


### step5  for loop for generic dataloader

stock_list = c("bch","bdms","bh")
for (ssym in stock_list){
  print(ssym)
}


stock_list = c("ahc","bch","bdms","bh","chg","cmr","ekh","kdh","ntv","pr9","princ","rjh","rph","svh","vih")
length(stock_list)
for (ssym in stock_list){
  print(ssym)
  # step 1
  link <- paste(paste("health/",ssym),".xlsx")
  link <- gsub(" ", "", link, fixed = TRUE)
  print(link)
  df_temp <- read_excel(link, sheet = 1, col_names = FALSE, col_types = NULL, na = "", skip = 20)
  
  ## step 2
  names(df_temp) <- df_temp %>%
    slice(1) %>% 
    unlist(., use.names=FALSE)
  
  df_temp<- df_temp[!is.na(names(df_temp))]
  
  # step 3
  trading <- df_temp[4:7,]

  statistic <- df_temp[11:21,]

  fin_data <- df_temp[26:40,][-c(4,11),]

  fin_ratio <- df_temp[42:62,][-c(6,9,12),]

  trading <- as.data.frame(t(as.matrix(trading)))
  trading<- trading %>%
    row_to_names(row_number = 1)
   
  statistic <- as.data.frame(t(as.matrix(statistic)))
  statistic<- statistic %>%
    row_to_names(row_number = 1)

  fin_data <- as.data.frame(t(as.matrix(fin_data)))
  fin_data<- fin_data %>%
    row_to_names(row_number = 1)

  fin_ratio <- as.data.frame(t(as.matrix(fin_ratio)))
  fin_ratio<- fin_ratio %>%
    row_to_names(row_number = 1)

  
  #step 4 
  trading$stock_name <- ssym
  statistic$stock_name <- ssym
  fin_data$stock_name <- ssym
  fin_ratio$stock_name <- ssym
  
  ### step 5 union base
  
  trading_base<- union_all(trading_base , trading)
  statistic1_base<- union_all(statistic1_base , statistic)
  fin_data1_base<- union_all(fin_data1_base , fin_data)
  fin_ratio1_base<- union_all(fin_ratio1_base , fin_ratio)
  }



########### set string









trading_base %>% group_by(stock_name) %>% count()
fin_ratio1_base %>% group_by(stock_name) %>% count()

fin_ratio1_base %>% filter(stock_name=='ahc')
trading_base %>% count()
fin_ratio1_base %>% count()


library(zoo)


trading_base$quarter <- as.yearqtr(gsub(" ", "", substr( rownames(trading_base) ,1,10)), format = "Q%q/%Y")
statistic1_base$quarter <- as.yearqtr(gsub(" ", "", substr( rownames(statistic1_base) ,1,10)), format = "Q%q/%Y")
fin_data1_base$quarter <- as.yearqtr(gsub(" ", "", substr( rownames(fin_data1_base) ,1,10)), format = "Q%q/%Y")
fin_ratio1_base$quarter <- as.yearqtr(gsub(" ", "", substr( rownames(fin_ratio1_base) ,1,10)), format = "Q%q/%Y")


## super big dataframe
names(trading_base)
names(statistic1_base)
names(fin_data1_base)
names(fin_ratio1_base)



big_fin_data <- left_join(left_join(left_join(trading_base, statistic1_base, by = c("stock_name", "quarter")) , fin_data1_base ,  by = c("stock_name", "quarter")),fin_ratio1_base , by = c("stock_name", "quarter") )

names(big_fin_data) <- gsub( "Shareholders'Equity"   , "ShareholdersEquity" , 
  gsub("\\([Baht)]*\\)" , "_Baht" ,
    gsub("\\([Days)]*\\)" , "_Days" ,
       gsub("%", "Percent_" , 
            gsub("/", "_by_", 
                 gsub(paste(c(" ","\\."), collapse="|"), "", names(big_fin_data)))))))


names(big_fin_data)
str(big_fin_data)

# install.packages("taRifx")
#library( taRifx )
#dat <- japply( big_fin_data, which(sapply(big_fin_data, class)=="character"), as.numeric )
#str(dat)


cols_not_apply <- c("stock_name","quarter")
every_other_col <- names(big_fin_data)[!names(big_fin_data) %in% cols_not_apply]

big_fin_data[every_other_col] <- sapply( big_fin_data[every_other_col], as.numeric )
str(big_fin_data)




big_fin_data %>% dim()

######## done for health




big_fin_data
names(big_fin_data)







### trading data ## for loop

#t_trading1
#install.packages("imputeTS")
library(imputeTS)

t_trading_test <- t_trading1

t_trading_test$quarter <- as.yearqtr(gsub(" ", "", substr( rownames(t_trading_test) ,1,10)), format = "Q%q/%Y")
t_trading_test<- t_trading_test %>% arrange(quarter)



library(dplyr)


t_trading_test <- t_trading1
t_trading_test$quarter <- as.yearqtr(gsub(" ", "", substr( rownames(t_trading_test) ,1,10)), format = "Q%q/%Y")
t_trading_test<- t_trading_test %>% arrange(quarter)
#t_trading_test <- na_ma(sapply( t_trading_test$Close, as.numeric ) , k = 3 , weighting = "simple")

t_trading_test$Close <- na_ma(sapply( t_trading_test$Close, as.numeric ) , k = 3 , weighting = "simple")
t_trading_test['Close_lead'] <- c( tail(t_trading_test['Close'], dim(t_trading_test)[1] - 1)[[1]] , NA )
t_trading_test['ret'] <- (t_trading_test$Close_lead - t_trading_test$Close)/(t_trading_test$Close)

#<- t_trading_test <- t_trading_test %>% head(-1)

head(t_trading_test)

#ret_trading  <- as.vector(diff(as.matrix(log(na_ma(sapply( t_trading_test$Close, as.numeric ) , k = 3 , weighting = "simple")))))
#str(ret_trading)

t_trading_test2 <- t_trading_test %>% select(quarter  , ret) %>% head(-1)
t_trading_test2
#t_trading_test2$ret <- ret_trading
t_trading_test2$stock_name <- "ahc"
rownames(t_trading_test2) <- c()






trading_ret_base <-  t_trading_test2[0,]
trading_ret_base
## start for loop 



stock_list = c("ahc","bch","bdms","bh","chg","cmr","ekh","kdh","ntv","pr9","princ","rjh","rph","svh","vih")
for (ssym in stock_list){
  print(ssym)
  # step 1
  link <- paste(paste("health/",ssym),".xlsx")
  link <- gsub(" ", "", link, fixed = TRUE)
  print(link)
  df_temp <- read_excel(link, sheet = 1, col_names = FALSE, col_types = NULL, na = "", skip = 20)
  
  ## step 2
  names(df_temp) <- df_temp %>%
    slice(1) %>% 
    unlist(., use.names=FALSE)
  
  df_temp<- df_temp[!is.na(names(df_temp))]
  
  # step 3
  trading <- df_temp[4:7,]
  
#  statistic <- df_temp[11:21,]
  
#  fin_data <- df_temp[26:40,][-c(4,11),]
  
#  fin_ratio <- df_temp[42:62,][-c(6,9,12),]
  
  trading <- as.data.frame(t(as.matrix(trading)))
  trading<- trading %>%
    row_to_names(row_number = 1)
  
 # statistic <- as.data.frame(t(as.matrix(statistic)))
#  statistic<- statistic %>%
#    row_to_names(row_number = 1)
  
 # fin_data <- as.data.frame(t(as.matrix(fin_data)))
#  fin_data<- fin_data %>%
#    row_to_names(row_number = 1)
  
#  fin_ratio <- as.data.frame(t(as.matrix(fin_ratio)))
#  fin_ratio<- fin_ratio %>%
#    row_to_names(row_number = 1)
#  library(imputeTS)
#  t_trading_test <- trading
#  t_trading_test$quarter <- as.yearqtr(gsub(" ", "", substr( rownames(t_trading_test) ,1,10)), format = "Q%q/%Y")
#  t_trading_test<- t_trading_test %>% arrange(quarter)
  
#  ret_trading<- as.vector(diff(as.matrix(log(na_ma(sapply( t_trading_test$Close, as.numeric ) , k = 3 , weighting = "simple")))))
  
  
#  t_trading_test2 <- t_trading_test %>% select(quarter) %>% tail(-1)
#  t_trading_test2$ret <- ret_trading
  
  #####################################################################################################
  #####################################################################################################
  
  t_trading_test <- trading
  t_trading_test$quarter <- as.yearqtr(gsub(" ", "", substr( rownames(t_trading_test) ,1,10)), format = "Q%q/%Y")
  t_trading_test<- t_trading_test %>% arrange(quarter)
  #t_trading_test <- na_ma(sapply( t_trading_test$Close, as.numeric ) , k = 3 , weighting = "simple")
  
  t_trading_test$Close <- na_ma(sapply( t_trading_test$Close, as.numeric ) , k = 3 , weighting = "simple")
  t_trading_test['Close_lead'] <- c( tail(t_trading_test['Close'], dim(t_trading_test)[1] - 1)[[1]] , NA )
  t_trading_test['ret'] <- (t_trading_test$Close_lead - t_trading_test$Close)/(t_trading_test$Close)
  t_trading_test2 <- t_trading_test %>% select(quarter , ret) %>% head(-1)
  t_trading_test2
  
  #####################################################################################################
  #####################################################################################################
  
  #step 4 
  t_trading_test2$stock_name <- ssym
#  statistic$stock_name <- ssym
#  fin_data$stock_name <- ssym
#  fin_ratio$stock_name <- ssym
  names(t_trading_test2)
  ### step 5 union base
  
  trading_ret_base<- union_all(trading_ret_base , t_trading_test2)
#  statistic1_base<- union_all(statistic1_base , statistic)
#  fin_data1_base<- union_all(fin_data1_base , fin_data)
#  fin_ratio1_base<- union_all(fin_ratio1_base , fin_ratio)
}

names(trading_ret_base)


trading_ret_base %>% dim()
trading_ret_base %>% head()


trading_ret1 <- trading_ret_base

### join both together

names(trading_ret1)
trading_ret1
#big_fin_data %>% dim()
#names(big_fin_data)
#names(trading_ret_base)


#trading_ret_base <- as_tibble(trading_ret_base)

#names(trading_ret_base)[names(trading_ret_base) == "Close"] <- "return"
#head(trading_ret_base)
#tempp <- trading_ret1 %>% head(5)

#tempp %>% head(-1)
#big_fin_data %>% head()


#trading_ret2 <- trading_ret1 %>% as_tibble() 

#names(trading_ret2)[2] <- "ret"
#trading_ret2


library(dplyr)
ret_inner_big <- inner_join(trading_ret1, big_fin_data, by = c("quarter","stock_name"))
ret_inner_big %>% dim()
head(ret_inner_big)



### data_frame

ret_inner_big
names(ret_inner_big)

tomyumkung <- as.yearqtr(c('1996 Q3','1996 Q4','1997 Q1','1997 Q2','1997 Q3','1997 Q4','1998 Q1','1998 Q2','1998 Q3'))
gfc <- as.yearqtr(c('2007 Q2','2007 Q3','2007 Q4','2008 Q1','2008 Q2','2008 Q3','2008 Q4','2009 Q1'))
covid <- as.yearqtr(c('2019 Q1','2019 Q2','2019 Q3','2019 Q4','2020 Q1','2020 Q2','2020 Q3','2020 Q4'))


str(ret_inner_big)
ret_inner_big %>% select(ReturnonAsset) %>% na_remove() %>% mean()

grp_roa = ret_inner_big  %>% group_by(quarter) %>% summarise(mean_ROA = mean(ReturnonAsset, na.rm = TRUE))

write.csv(grp_roa , "grp_roa.csv")


####################3

ret_inner_big2 <- ret_inner_big %>% mutate(crisis_period = ifelse(quarter %in% tomyumkung, 1997, ifelse(quarter %in% gfc , 2008,
                                                 ifelse(quarter %in% covid , 2019, "Non_crisis")
                                                        ))) 

specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))



names(ret_inner_big2)

col_sel = "CashCycle_Days"

grp_fin <- ret_inner_big2 %>% group_by(crisis_period, quarter) %>% summarise(mean_ratios = mean(eval(parse(text = col_sel)), na.rm = TRUE)) %>% arrange(quarter)
grp_fin2 <- grp_fin[!is.nan(grp_fin$mean_ratios),]
grp_fin3 <- grp_fin2 %>% group_by(crisis_period) %>% summarise(mean_ratios = mean(mean_ratios, na.rm = TRUE)) %>% arrange(crisis_period)



ggplot(grp_fin3, aes(x= crisis_period, y= mean_ratios , fill = mean_ratios)) + 
  geom_bar(stat="identity") +
  ggtitle(paste(paste("Average of all",col_sel),"across different crisis period")) +
  geom_label(label=specify_decimal(grp_fin3$mean_ratios,2), fill="#FFFFFF")



  
#Average of All ROA In Healthcare Sector

  

write.csv(grp_fin , "fin_ratios_test.csv")

#is.nan(X)
#grp_roa = ret_inner_big  %>% group_by(quarter) %>% summarise(mean_ROA = mean(ReturnonAsset, na.rm = TRUE))
#write.csv(grp_roa , "fin_ratios_test.csv")

ret_inner_big %>% str()
summary(ret_inner_big)

library(plm)

trading_panel <- pdata.frame(ret_inner_big, index=c("stock_name","quarter"), drop.index=TRUE, row.names=TRUE)
# drop.index=FALSE
trading_panel
names(trading_panel)


### TOO MANY NA's columns

# PEG 
# Percent_12MDividendYield 
# Beta



# install.packages("plm")
library(plm)
#install.packages("punitroots")
library(punitroot)



trading_panel2 <- trading_panel %>% select( -c("High","Low","DailyAvgValue_Baht"))
names(trading_panel2)
head(trading_panel2)

names(trading_panel2)[names(trading_panel2) == "Close"] <- "Close2"


trading_panel.fe <- plm(Close~ ReturnonEquity+ReturnonAsset+NetProfitMargin+EBITMargin+GrossProfitMargin+D_by_ERatio+InterestCoverage+CurrentRatio+QuickRatio+FixedAssetTurnover+TotalAssetTurnover+InventoryTurnover+AverageSalePeriod_Days+AccountReceivableTurnover+AverageCollectionPeriod_Days+AccountPayableTurnover+AveragePaymentPeriod_Days+CashCycle_Days , data = trading_panel2, model = "within")
trading_panel_no <- plm(Close~ ReturnonEquity+ReturnonAsset+NetProfitMargin+EBITMargin+GrossProfitMargin+D_by_ERatio+InterestCoverage+CurrentRatio+QuickRatio+FixedAssetTurnover+TotalAssetTurnover+InventoryTurnover+AverageSalePeriod_Days+AccountReceivableTurnover+AverageCollectionPeriod_Days+AccountPayableTurnover+AveragePaymentPeriod_Days+CashCycle_Days , data = trading_panel2)

trading_panel.re <- plm(Close~. , data = trading_panel2, model = "random")

summary(trading_panel.fe)
summary(trading_panel_no)



# PANELPANEL
'%!in%' <- function(x,y)!('%in%'(x,y))
# c(1,3,11)%!in%1:10


tomyumkung <- as.yearqtr(c('1996 Q3','1996 Q4','1997 Q1','1997 Q2','1997 Q3','1997 Q4','1998 Q1','1998 Q2','1998 Q3'))
gfc <- as.yearqtr(c('2007 Q2','2007 Q3','2007 Q4','2008 Q1','2008 Q2','2008 Q3','2008 Q4','2009 Q1'))
covid <- as.yearqtr(c('2019 Q1','2019 Q2','2019 Q3','2019 Q4','2020 Q1','2020 Q2','2020 Q3','2020 Q4'))
all_crisis <- c(tomyumkung ,gfc , covid )





df_1997 <- ret_inner_big %>% filter(quarter %in% tomyumkung )
df_2008 <- ret_inner_big %>% filter(quarter %in% gfc )
df_2019 <- ret_inner_big %>% filter(quarter %in% covid )
df_all_crisis <- ret_inner_big %>% filter(quarter %in% all_crisis )
df_non_crisis <- ret_inner_big %>% filter(quarter %!in% all_crisis )



df_1997_pn <- pdata.frame(df_1997, index=c("stock_name","quarter"), drop.index=TRUE, row.names=TRUE)
df_2008_pn <- pdata.frame(df_2008, index=c("stock_name","quarter"), drop.index=TRUE, row.names=TRUE)
df_2019_pn <- pdata.frame(df_2019, index=c("stock_name","quarter"), drop.index=TRUE, row.names=TRUE)
df_all_crisis_pn <- pdata.frame(df_all_crisis, index=c("stock_name","quarter"), drop.index=TRUE, row.names=TRUE)
df_non_crisis_pn <- pdata.frame(df_non_crisis, index=c("stock_name","quarter"), drop.index=TRUE, row.names=TRUE)



#ret~ P_by_E+ EPS_Baht + ReturnonAsset + ReturnonEquity + GrossProfitMargin + D_by_ERatio + CurrentRatio + FixedAssetTurnover , data = df_1997_pn, model = "within"

trading_panel.re_1997 <- plm(ret~ P_by_E + ReturnonAsset + GrossProfitMargin + D_by_ERatio + CurrentRatio + FixedAssetTurnover , data = df_1997_pn, model = "within")
summary(trading_panel.re_1997)


trading_panel.re_2008 <- plm(ret~ P_by_E + ReturnonAsset  + GrossProfitMargin + D_by_ERatio + CurrentRatio + FixedAssetTurnover , data = df_2008_pn, model = "within")
summary(trading_panel.re_2008)

trading_panel.re_2019 <- plm(ret~ P_by_E + ReturnonAsset  + GrossProfitMargin + D_by_ERatio + CurrentRatio + FixedAssetTurnover , data = df_2019_pn, model = "within")
summary(trading_panel.re_2019)

trading_panel.re_all_crisis <- plm(ret~ P_by_E + ReturnonAsset  + GrossProfitMargin + D_by_ERatio + CurrentRatio + FixedAssetTurnover , data = df_all_crisis_pn, model = "within")
summary(trading_panel.re_all_crisis)

trading_panel.re_non_crisis <- plm(ret~ P_by_E + ReturnonAsset + GrossProfitMargin + D_by_ERatio + CurrentRatio + FixedAssetTurnover , data = df_non_crisis_pn, model = "within")
summary(trading_panel.re_non_crisis)



# non crisis









names(df_1997)



# install.packages("installr")
# library(installr) # install+load installr
# updateR() # updating R.

my.u <- as.matrix(window(u, start = c(1979,4), end = c(2010,1)))

LLC <- purtest(trading_panel2$Close, test = "levinlin", exo = "intercept",lags = "AIC", pmax = 5 , index=c("stock_name","quarter"))

trading_panel2

sum(is.na(trading_panel$Close))


Choi <- pCADFtest(Y=u, type = "drift", max.lag.y = 5, criterion = "AIC")



### input dummy 
library(zoo)
tomyumkung <- as.yearqtr(c('1996 Q3','1996 Q4','1997 Q1','1997 Q2','1997 Q3','1997 Q4','1998 Q1','1998 Q2','1998 Q3'))
gfc <- as.yearqtr(c('2007 Q2','2007 Q3','2007 Q4','2008 Q1','2008 Q2','2008 Q3','2008 Q4','2009 Q1'))
covid <- as.yearqtr(c('2019 Q1','2019 Q2','2019 Q3','2019 Q4','2020 Q1','2020 Q2','2020 Q3','2020 Q4'))

crisis_vec <- c(c(tomyumkung,gfc) , covid)


ret_inner_big_dummy1 <- ret_inner_big %>% mutate(dummy_crisis =  ifelse(quarter %in% crisis_vec,1,0) )
names(ret_inner_big_dummy1)

ret_inner_big_dummy2 <- ret_inner_big_dummy1 %>% mutate(ReturnonEquity_dummy = ReturnonEquity * dummy_crisis) %>% 
  mutate(ReturnonAsset_dummy = ReturnonAsset * dummy_crisis ) %>% 
  mutate(NetProfitMargin_dummy = NetProfitMargin * dummy_crisis ) %>% 
  mutate(EBITMargin_dummy = EBITMargin * dummy_crisis ) %>% 
  mutate(GrossProfitMargin_dummy = GrossProfitMargin * dummy_crisis ) %>% 
  mutate(D_by_ERatio_dummy = D_by_ERatio * dummy_crisis ) %>% 
  mutate(InterestCoverage_dummy = InterestCoverage * dummy_crisis ) %>% 
  mutate(CurrentRatio_dummy = CurrentRatio * dummy_crisis ) %>% 
  mutate(QuickRatio_dummy = QuickRatio * dummy_crisis ) %>% 
  mutate(FixedAssetTurnover_dummy = FixedAssetTurnover * dummy_crisis ) %>% 
  mutate(TotalAssetTurnover_dummy = TotalAssetTurnover * dummy_crisis ) %>% 
  mutate(InventoryTurnover_dummy = InventoryTurnover * dummy_crisis ) %>% 
  mutate(AverageSalePeriod_Days_dummy = AverageSalePeriod_Days * dummy_crisis ) %>% 
  mutate(AccountReceivableTurnover_dummy = AccountReceivableTurnover * dummy_crisis ) %>% 
  mutate(AverageCollectionPeriod_Days_dummy = AverageCollectionPeriod_Days * dummy_crisis ) %>% 
  mutate(AccountPayableTurnover_dummy = AccountPayableTurnover * dummy_crisis ) %>% 
  mutate(AveragePaymentPeriod_Days_dummy = AveragePaymentPeriod_Days * dummy_crisis ) %>% 
  mutate(P_by_E_dummy = P_by_E  * dummy_crisis )   %>% 
  mutate(PEG_dummy = PEG  * dummy_crisis )   %>% 
  mutate(P_by_BV_dummy = P_by_BV  * dummy_crisis )   %>% 
  mutate(EV_by_EBITDA_dummy = EV_by_EBITDA  * dummy_crisis )  
                                    
ret_inner_big_dummy2 %>% dim()


######## start regression
ret_inner_big_dummy2








trading_panel_dummy <- pdata.frame(ret_inner_big_dummy2, index=c("stock_name","quarter"), drop.index=TRUE, row.names=TRUE)
names(trading_panel_dummy)[names(trading_panel2) == "Close"] <- "Close2"

names(trading_panel_dummy)
names(trading_panel_dummy)[names(trading_panel_dummy) == "Close"] <- "Close2"
trading_panel.fe <- plm(Close2~ ReturnonEquity+ReturnonAsset+NetProfitMargin+EBITMargin+GrossProfitMargin+D_by_ERatio+InterestCoverage+CurrentRatio+QuickRatio+FixedAssetTurnover+TotalAssetTurnover+InventoryTurnover+AverageSalePeriod_Days+AccountReceivableTurnover+AverageCollectionPeriod_Days+AccountPayableTurnover+AveragePaymentPeriod_Days+CashCycle_Days+dummy_crisis+ReturnonEquity_dummy+ReturnonAsset_dummy+NetProfitMargin_dummy+EBITMargin_dummy+GrossProfitMargin_dummy+D_by_ERatio_dummy+InterestCoverage_dummy+CurrentRatio_dummy+QuickRatio_dummy+FixedAssetTurnover_dummy+TotalAssetTurnover_dummy+InventoryTurnover_dummy+AverageSalePeriod_Days_dummy+AccountReceivableTurnover_dummy+AverageCollectionPeriod_Days_dummy+AccountPayableTurnover_dummy+AveragePaymentPeriod_Days_dummy+CashCycle_Days_dummy , data = trading_panel_dummy, model = "within")
summary(trading_panel.fe)


trading_panel.fe <- plm(Close~ ReturnonEquity+ReturnonAsset, data = trading_panel_dummy, model = "random")
summary(trading_panel.fe)

## 1 
profitabity.re <- plm(Close~ ReturnonEquity+ReturnonAsset+NetProfitMargin+EBITMargin+GrossProfitMargin  + ReturnonEquity_dummy+ReturnonAsset_dummy + NetProfitMargin_dummy+EBITMargin_dummy+GrossProfitMargin_dummy , data = trading_panel_dummy, model = "random")
summary(profitabity.re)

leverage.re <- plm(Close~ D_by_ERatio+InterestCoverage + D_by_ERatio_dummy+InterestCoverage_dummy  , data = trading_panel_dummy, model = "random")
summary(leverage.re)


liquidity.re <- plm(Close~  CurrentRatio+QuickRatio +CurrentRatio_dummy +QuickRatio_dummy , data = trading_panel_dummy, model = "random")
summary(liquidity.re)

activity.re <- plm(Close~  FixedAssetTurnover+TotalAssetTurnover+AverageCollectionPeriod_Days+AccountPayableTurnover+AveragePaymentPeriod_Days+CashCycle_Days+FixedAssetTurnover_dummy+TotalAssetTurnover_dummy+AverageCollectionPeriod_Days_dummy+AccountPayableTurnover_dummy+AveragePaymentPeriod_Days_dummy, data = trading_panel_dummy, model = "random")
summary(activity.re)


activity_dummy.re <- plm(Close~  FixedAssetTurnover_dummy+TotalAssetTurnover_dummy+AverageCollectionPeriod_Days_dummy+AccountPayableTurnover_dummy+AveragePaymentPeriod_Days_dummy+CashCycle_Days_dummy , data = trading_panel_dummy, model = "random")
summary(activity_dummy.re)



price.re <- plm(Close~  P_by_E + PEG + P_by_BV + EV_by_EBITDA +P_by_E_dummy+ PEG_dummy+P_by_BV_dummy+EV_by_EBITDA_dummy, data = trading_panel_dummy, model = "random")
summary(price.re)


############ write summary 
library(tidyverse)
install.packages("permutations")
library(permutations)

library(tidyverse)
library(rlang)
library(tidyquant) # To download the data
library(plotly) # To create interactive charts
library(timetk) # To manipulate the data series
library(tidyr)
library(broom)


tidy(price.re)


write.csv(tidy(profitabity.re), "profitabity.csv")
write.csv(tidy(leverage.re), "leverage.csv")
write.csv(tidy(liquidity.re), "liquidity.csv")
write.csv(tidy(activity.re), "activity.csv")
write.csv(tidy(activity_dummy.re), "activity_dummy.csv")
write.csv(tidy(price.re), "price.csv")




names(trading_panel_dummy)


install.packages("corrplot")
library(corrplot)
df <- trading_panel_dummy %>% select(ReturnonEquity,ReturnonAsset,NetProfitMargin,EBITMargin,GrossProfitMargin  ,  ReturnonEquity_dummy,ReturnonAsset_dummy,NetProfitMargin_dummy,EBITMargin_dummy,GrossProfitMargin_dummy , dummy_crisis)
corrplot(cor(df), method = "number")

library(PerformanceAnalytics)
chart.Correlation(df)




###################################################################
###################################################################
###################################################################

all_col = c("quarter","ret","stock_name","CurrentRatio","QuickRatio" ,"P_by_E","P_by_BV","EV_by_EBITDA","Percent_DividendYield","FixedAssetTurnover","TotalAssetTurnover","InventoryTurnover","AverageSalePeriod_Days","AccountReceivableTurnover","AverageCollectionPeriod_Days","AccountPayableTurnover","AveragePaymentPeriod_Days","CashCycle_Days","D_by_ERatio","InterestCoverage" , "ReturnonEquity","ReturnonAsset","NetProfitMargin","EBITMargin","GrossProfitMargin" )

ret_inner_big %>% names()

ret_ratios  <- ret_inner_big %>% select(all_col)
ret_ratios <- ret_ratios[complete.cases(ret_ratios), ]
ret_ratios



library(corrplot)
# col_use = c("CurrentRatio","QuickRatio" ,"P_by_E","P_by_BV","EV_by_EBITDA","Percent_DividendYield","FixedAssetTurnover","TotalAssetTurnover","InventoryTurnover","AverageSalePeriod_Days","AccountReceivableTurnover","AverageCollectionPeriod_Days","AccountPayableTurnover","AveragePaymentPeriod_Days","CashCycle_Days","D_by_ERatio","InterestCoverage" , "ReturnonEquity","ReturnonAsset","NetProfitMargin","EBITMargin","GrossProfitMargin" )

# PROFIT
col_use = c( "ReturnonEquity","ReturnonAsset","NetProfitMargin","EBITMargin","GrossProfitMargin" )
df <- ret_ratios[,col_use]

corrplot(cor(df), method = "number")
corrplot(cor(df), method = "square")

library(PerformanceAnalytics)
chart.Correlation(df)


# DEBT

col_use = c("D_by_ERatio","InterestCoverage"  )
df <- ret_ratios[,col_use]

corrplot(cor(df), method = "number")
corrplot(cor(df), method = "square")

library(PerformanceAnalytics)
chart.Correlation(df)


# Activity


col_use =  c("FixedAssetTurnover","TotalAssetTurnover","InventoryTurnover","AverageSalePeriod_Days","AccountReceivableTurnover","AverageCollectionPeriod_Days")
df <- ret_ratios[,col_use]


corrplot(cor(df), method = "number",tl.cex=0.55)

corrplot.mixed(cor(df), lower.col = "black",
              tl.cex=0.6)

library(PerformanceAnalytics)
chart.Correlation(df)





food_price_df <- ret_ratios[,c("P_by_E","P_by_BV","EV_by_EBITDA","Percent_DividendYield")]
food_price.pca <- prcomp(food_price_df, center = TRUE,scale. = TRUE)
price_pca <- food_price.pca$x
summary(food_price.pca)


food_profit_df <- ret_ratios[,c("ReturnonEquity","ReturnonAsset","NetProfitMargin","EBITMargin","GrossProfitMargin")]
food_profit.pca <- prcomp(food_profit_df, center = TRUE,scale. = TRUE)
profit_pca <- food_profit.pca$x
summary(food_profit.pca)


food_debt_df <- ret_ratios[,c("D_by_ERatio","InterestCoverage")]
food_debt.pca <- prcomp(food_debt_df, center = TRUE,scale. = TRUE)
debt_pca <- food_debt.pca$x
summary(food_debt.pca)

food_cash_df <- ret_ratios[,c("CurrentRatio","QuickRatio","CashCycle_Days","AccountPayableTurnover","AveragePaymentPeriod_Days")]
food_cash.pca <- prcomp(food_cash_df, center = TRUE,scale. = TRUE)
cash_pca <- food_cash.pca$x
summary(food_cash.pca)



food_activity_df <- ret_ratios[,c("FixedAssetTurnover","TotalAssetTurnover","InventoryTurnover","AverageSalePeriod_Days","AccountReceivableTurnover","AverageCollectionPeriod_Days")]
food_activity.pca <- prcomp(food_activity_df, center = TRUE,scale. = TRUE)
activity_pca <- food_activity.pca$x

summary(food_activity.pca)


library(ggbiplot)
ggbiplot(food_price.pca )+ 
  scale_x_continuous(limits = c(-3, 3)) + 
  scale_y_continuous(limits = c(-3, 3))+
  theme(text = element_text(size = 10) )


ggbiplot(food_price.pca  , choices=c(1,3))+ 
  scale_x_continuous(limits = c(-10, 10)) + 
  scale_y_continuous(limits = c(-10, 10))


ggbiplot(food_price.pca  , choices=c(2,3))+ 
  scale_x_continuous(limits = c(-10, 10)) + 
  scale_y_continuous(limits = c(-10, 10))


ggbiplot(food_profit.pca, choices=c(1,2)  )+ 
  scale_x_continuous(limits = c(-3, 3)) + 
  scale_y_continuous(limits = c(-3, 3))+
  theme(text = element_text(size = 10) )


ggbiplot(food_profit.pca, choices=c(1,3)  )+ 
  scale_x_continuous(limits = c(-3, 3)) + 
  scale_y_continuous(limits = c(-3, 3))+
  theme(text = element_text(size = 10) )

ggbiplot(food_profit.pca, choices=c(2,3)  )+ 
  scale_x_continuous(limits = c(-3, 3)) + 
  scale_y_continuous(limits = c(-3, 3))+
  theme(text = element_text(size = 10) )




ggbiplot(food_debt.pca )+ 
  scale_x_continuous(limits = c(-10, 10)) + 
  scale_y_continuous(limits = c(-10, 10))


ggbiplot(food_cash.pca )+ 
  scale_x_continuous(limits = c(-3, 3)) + 
  scale_y_continuous(limits = c(-3, 3))+
  theme(text = element_text(size = 10) )


ggbiplot(food_cash.pca ,choices=c(1,3))+ 
  scale_x_continuous(limits = c(-3, 3)) + 
  scale_y_continuous(limits = c(-3, 3))+
  theme(text = element_text(size = 10) )

ggbiplot(food_cash.pca ,choices=c(2,3))+ 
  scale_x_continuous(limits = c(-3, 3)) + 
  scale_y_continuous(limits = c(-3, 3))+
  theme(text = element_text(size = 10) )





ggbiplot(food_activity.pca )+ 
  scale_x_continuous(limits = c(-3, 3)) + 
  scale_y_continuous(limits = c(-3, 3))+
  theme(text = element_text(size = 10) )


ggbiplot(food_activity.pca ,choices=c(1,3))+ 
  scale_x_continuous(limits = c(-3, 3)) + 
  scale_y_continuous(limits = c(-3, 3))+
  theme(text = element_text(size = 10) )

ggbiplot(food_activity.pca ,choices=c(2,3))+ 
  scale_x_continuous(limits = c(-3, 3)) + 
  scale_y_continuous(limits = c(-3, 3))+
  theme(text = element_text(size = 10) )








ret_ratios %>% names()
ret_ratios2 <- ret_ratios %>% select("quarter","ret","stock_name","D_by_ERatio","InterestCoverage")
ret_ratios2



summary(food_price.pca) # 2 
summary(food_debt.pca) # non pca
summary(food_profit.pca) #2
summary(food_cash.pca) #3
summary(food_activity.pca) #3


ret_ratios2$activity_pc1 <- activity_pca[,1]
ret_ratios2$activity_pc2 <- activity_pca[,2]
ret_ratios2$activity_pc3 <- activity_pca[,3]


ret_ratios2$price_pc1 <- price_pca[,1]
ret_ratios2$price_pc2 <- price_pca[,2]
# ret_ratios2$price_pc3 <- price_pca[,3]

ret_ratios2$profit_pc1 <- profit_pca[,1]
ret_ratios2$profit_pc2 <- profit_pca[,2]

ret_ratios2$cash_pc1 <- cash_pca[,1]
ret_ratios2$cash_pc2 <- cash_pca[,2]
ret_ratios2$cash_pc3 <- cash_pca[,3]




df_1997 <- ret_ratios2 %>% filter(quarter %in% tomyumkung )
df_2008 <- ret_ratios2 %>% filter(quarter %in% gfc )
df_2019 <- ret_ratios2 %>% filter(quarter %in% covid )
df_all_crisis <- ret_ratios2 %>% filter(quarter %in% all_crisis )
df_non_crisis <- ret_ratios2 %>% filter(quarter %!in% all_crisis )



df_1997_pn <- pdata.frame(df_1997, index=c("stock_name","quarter"), drop.index=TRUE, row.names=TRUE)
df_2008_pn <- pdata.frame(df_2008, index=c("stock_name","quarter"), drop.index=TRUE, row.names=TRUE)
df_2019_pn <- pdata.frame(df_2019, index=c("stock_name","quarter"), drop.index=TRUE, row.names=TRUE)
df_all_crisis_pn <- pdata.frame(df_all_crisis, index=c("stock_name","quarter"), drop.index=TRUE, row.names=TRUE)
df_non_crisis_pn <- pdata.frame(df_non_crisis, index=c("stock_name","quarter"), drop.index=TRUE, row.names=TRUE)


df_non_crisis_pn %>% names()


trading_panel.re_1997 <- plm(ret~ D_by_ERatio +InterestCoverage + cash_pc1 +cash_pc2 +cash_pc3 + activity_pc1 + activity_pc2 + activity_pc3+price_pc1+price_pc2+profit_pc1 +profit_pc2 , data = df_1997_pn, model = "within")
summary(trading_panel.re_1997)


trading_panel.re_2008 <- plm(ret~ D_by_ERatio +InterestCoverage + cash_pc1 +cash_pc2 +cash_pc3 + activity_pc1 + activity_pc2 + activity_pc3+price_pc1+price_pc2+profit_pc1 +profit_pc2 , data = df_2008_pn, model = "within")
summary(trading_panel.re_2008)

trading_panel.re_2019 <- plm(ret~D_by_ERatio +InterestCoverage + cash_pc1 +cash_pc2 +cash_pc3 + activity_pc1 + activity_pc2 + activity_pc3+price_pc1+price_pc2+profit_pc1 +profit_pc2 , data = df_2019_pn, model = "within")
summary(trading_panel.re_2019)

trading_panel.re_all_crisis <- plm(ret~ D_by_ERatio +InterestCoverage + cash_pc1 +cash_pc2 +cash_pc3 + activity_pc1 + activity_pc2 + activity_pc3+price_pc1+price_pc2+profit_pc1 +profit_pc2 , data = df_all_crisis_pn, model = "within")
summary(trading_panel.re_all_crisis)

trading_panel.re_non_crisis <- plm(ret~ D_by_ERatio +InterestCoverage + cash_pc1 +cash_pc2 +cash_pc3 + activity_pc1 + activity_pc2 + activity_pc3+price_pc1+price_pc2+profit_pc1 +profit_pc2 , data = df_non_crisis_pn, model = "within")
summary(trading_panel.re_non_crisis)



################################
## STOP HEERE

###################################################################
###################################################################
###################################################################



##################### start food


library(readxl)
library(tidyverse)
library(data.table)
#install.packages("janitor")
library(janitor)
library(imputeTS)


test1<- read_excel("health/ahc.xlsx", sheet = 1, col_names = FALSE, col_types = NULL, na = "", skip = 20)
test1

test2<- read_excel("health/svh.xlsx", sheet = 1, col_names = FALSE, col_types = NULL, na = "", skip = 20)
test2


test3<- read_excel("health/bdms.xlsx", sheet = 1, col_names = FALSE, col_types = NULL, na = "", skip = 20)
test3


test4<- read_excel("food/apure.xlsx", sheet = 1, col_names = FALSE, col_types = NULL, na = "", skip = 20)
test4

#### step 1 step 20 rows

# step 2 specify  header row 30 excel , row 10 in R


names(test1) <- test1 %>%
  slice(1) %>% 
  unlist(., use.names=FALSE)


test1<- test1[!is.na(names(test1))]

## step 3 sperate trading , statistic , financial data and  ratios 

trading1 <- test1[4:7,]
trading1


statistic1 <- test1[11:21,]
statistic1


fin_data1 <- test1[26:40,][-c(4,11),]
fin_data1

fin_ratio1 <- test1[42:62,][-c(6,9,12),]
fin_ratio1



t_trading1 <- as.data.frame(t(as.matrix(trading1)))
t_trading1<- t_trading1 %>%
  row_to_names(row_number = 1)






statistic1 <- as.data.frame(t(as.matrix(statistic1)))
statistic1<- statistic1 %>%
  row_to_names(row_number = 1)
statistic1



fin_data1 <- as.data.frame(t(as.matrix(fin_data1)))
fin_data1<- fin_data1 %>%
  row_to_names(row_number = 1)
fin_data1



fin_ratio1 <- as.data.frame(t(as.matrix(fin_ratio1)))
fin_ratio1<- fin_ratio1 %>%
  row_to_names(row_number = 1)
fin_ratio1



### step 4 add column "stock_name"


t_trading1$stock_name <- "ahc"
statistic1$stock_name <- "ahc"
fin_data1$stock_name <- "ahc"
fin_ratio1$stock_name <- "ahc"


### step 4.5 create empty dataframe 

trading_base<- t_trading1[0,]
statistic1_base<- statistic1[0,]
fin_data1_base<- fin_data1[0,]
fin_ratio1_base<- fin_ratio1[0,]


### step5  for loop for generic dataloader

stock_list = c("bch","bdms","bh")
for (ssym in stock_list){
  print(ssym)
}



stock_list <- c("apure","asian","br","brr","cbg","cfresh","choti","cm","cpf","cpi",
                "f&d","htc","ichi","kbs","ksl","ktis","lst","m","malee","mint",
                "oishi","osp","pb","pm","prg","rbf","sappe","sauce","sfp","snp",
                "sorkon","ssc","ssf","sst","tc","tfg","tfmama","tipco","tkn","tu",
                "tvo","zen"
)

length(stock_list)

#stock_list = c("ahc","bch","bdms","bh","chg","cmr","ekh","kdh","ntv","pr9","princ","rjh","rph","svh","vih")
for (ssym in stock_list){
  print(ssym)
  # step 1
  link <- paste(paste("food/",ssym),".xlsx")
  link <- gsub(" ", "", link, fixed = TRUE)
  print(link)
  df_temp <- read_excel(link, sheet = 1, col_names = FALSE, col_types = NULL, na = "", skip = 20)
  
  ## step 2
  names(df_temp) <- df_temp %>%
    slice(1) %>% 
    unlist(., use.names=FALSE)
  
  df_temp<- df_temp[!is.na(names(df_temp))]
  
  # step 3
  trading <- df_temp[4:7,]
  
  statistic <- df_temp[11:21,]
  
  fin_data <- df_temp[26:40,][-c(4,11),]
  
  fin_ratio <- df_temp[42:62,][-c(6,9,12),]
  
  trading <- as.data.frame(t(as.matrix(trading)))
  trading<- trading %>%
    row_to_names(row_number = 1)
  
  statistic <- as.data.frame(t(as.matrix(statistic)))
  statistic<- statistic %>%
    row_to_names(row_number = 1)
  
  fin_data <- as.data.frame(t(as.matrix(fin_data)))
  fin_data<- fin_data %>%
    row_to_names(row_number = 1)
  
  fin_ratio <- as.data.frame(t(as.matrix(fin_ratio)))
  fin_ratio<- fin_ratio %>%
    row_to_names(row_number = 1)
  
  
  #step 4 
  trading$stock_name <- ssym
  statistic$stock_name <- ssym
  fin_data$stock_name <- ssym
  fin_ratio$stock_name <- ssym
  
  ### step 5 union base
  
  trading_base<- union_all(trading_base , trading)
  statistic1_base<- union_all(statistic1_base , statistic)
  fin_data1_base<- union_all(fin_data1_base , fin_data)
  fin_ratio1_base<- union_all(fin_ratio1_base , fin_ratio)
}



########### set string









trading_base %>% group_by(stock_name) %>% count()
fin_ratio1_base %>% group_by(stock_name) %>% count()

trading_base %>% count()
fin_ratio1_base %>% count()


library(zoo)


trading_base$quarter <- as.yearqtr(gsub(" ", "", substr( rownames(trading_base) ,1,10)), format = "Q%q/%Y")
statistic1_base$quarter <- as.yearqtr(gsub(" ", "", substr( rownames(statistic1_base) ,1,10)), format = "Q%q/%Y")
fin_data1_base$quarter <- as.yearqtr(gsub(" ", "", substr( rownames(fin_data1_base) ,1,10)), format = "Q%q/%Y")
fin_ratio1_base$quarter <- as.yearqtr(gsub(" ", "", substr( rownames(fin_ratio1_base) ,1,10)), format = "Q%q/%Y")


## super big dataframe
names(trading_base)
names(statistic1_base)
names(fin_data1_base)
names(fin_ratio1_base)



big_fin_data <- left_join(left_join(left_join(trading_base, statistic1_base, by = c("stock_name", "quarter")) , fin_data1_base ,  by = c("stock_name", "quarter")),fin_ratio1_base , by = c("stock_name", "quarter") )

names(big_fin_data) <- gsub( "Shareholders'Equity"   , "ShareholdersEquity" , 
                             gsub("\\([Baht)]*\\)" , "_Baht" ,
                                  gsub("\\([Days)]*\\)" , "_Days" ,
                                       gsub("%", "Percent_" , 
                                            gsub("/", "_by_", 
                                                 gsub(paste(c(" ","\\."), collapse="|"), "", names(big_fin_data)))))))


names(big_fin_data)
str(big_fin_data)

# install.packages("taRifx")
#library( taRifx )
#dat <- japply( big_fin_data, which(sapply(big_fin_data, class)=="character"), as.numeric )
#str(dat)


cols_not_apply <- c("stock_name","quarter")
every_other_col <- names(big_fin_data)[!names(big_fin_data) %in% cols_not_apply]

big_fin_data[every_other_col] <- sapply( big_fin_data[every_other_col], as.numeric )
str(big_fin_data)




big_fin_data %>% dim()

######## done for health




big_fin_data
names(big_fin_data)







### trading data ## for loop

#t_trading1
#install.packages("imputeTS")
library(imputeTS)

t_trading_test <- t_trading1

t_trading_test$quarter <- as.yearqtr(gsub(" ", "", substr( rownames(t_trading_test) ,1,10)), format = "Q%q/%Y")
t_trading_test<- t_trading_test %>% arrange(quarter)



library(dplyr)


t_trading_test <- t_trading1
t_trading_test$quarter <- as.yearqtr(gsub(" ", "", substr( rownames(t_trading_test) ,1,10)), format = "Q%q/%Y")
t_trading_test<- t_trading_test %>% arrange(quarter)
#t_trading_test <- na_ma(sapply( t_trading_test$Close, as.numeric ) , k = 3 , weighting = "simple")

t_trading_test$Close <- na_ma(sapply( t_trading_test$Close, as.numeric ) , k = 3 , weighting = "simple")
t_trading_test['Close_lead'] <- c( tail(t_trading_test['Close'], dim(t_trading_test)[1] - 1)[[1]] , NA )
t_trading_test['ret'] <- (t_trading_test$Close_lead - t_trading_test$Close)/(t_trading_test$Close)

#<- t_trading_test <- t_trading_test %>% head(-1)

head(t_trading_test)

#ret_trading  <- as.vector(diff(as.matrix(log(na_ma(sapply( t_trading_test$Close, as.numeric ) , k = 3 , weighting = "simple")))))
#str(ret_trading)

t_trading_test2 <- t_trading_test %>% select(quarter  , ret) %>% head(-1)
t_trading_test2
#t_trading_test2$ret <- ret_trading
t_trading_test2$stock_name <- "ahc"
rownames(t_trading_test2) <- c()






trading_ret_base <-  t_trading_test2[0,]
trading_ret_base
## start for loop 




stock_list <- c("apure","asian","br","brr","cbg","cfresh","choti","cm","cpf","cpi",
                "f&d","htc","ichi","kbs","ksl","ktis","lst","m","malee","mint",
                "oishi","osp","pb","pm","prg","rbf","sappe","sauce","sfp","snp",
                "sorkon","ssc","ssf","sst","tc","tfg","tfmama","tipco","tkn","tu",
                "tvo","zen"
)
for (ssym in stock_list){
  print(ssym)
  # step 1
  link <- paste(paste("food/",ssym),".xlsx")
  link <- gsub(" ", "", link, fixed = TRUE)
  print(link)
  df_temp <- read_excel(link, sheet = 1, col_names = FALSE, col_types = NULL, na = "", skip = 20)
  
  ## step 2
  names(df_temp) <- df_temp %>%
    slice(1) %>% 
    unlist(., use.names=FALSE)
  
  df_temp<- df_temp[!is.na(names(df_temp))]
  
  # step 3
  trading <- df_temp[4:7,]
  
  #  statistic <- df_temp[11:21,]
  
  #  fin_data <- df_temp[26:40,][-c(4,11),]
  
  #  fin_ratio <- df_temp[42:62,][-c(6,9,12),]
  
  trading <- as.data.frame(t(as.matrix(trading)))
  trading<- trading %>%
    row_to_names(row_number = 1)
  
  # statistic <- as.data.frame(t(as.matrix(statistic)))
  #  statistic<- statistic %>%
  #    row_to_names(row_number = 1)
  
  # fin_data <- as.data.frame(t(as.matrix(fin_data)))
  #  fin_data<- fin_data %>%
  #    row_to_names(row_number = 1)
  
  #  fin_ratio <- as.data.frame(t(as.matrix(fin_ratio)))
  #  fin_ratio<- fin_ratio %>%
  #    row_to_names(row_number = 1)
  #  library(imputeTS)
  #  t_trading_test <- trading
  #  t_trading_test$quarter <- as.yearqtr(gsub(" ", "", substr( rownames(t_trading_test) ,1,10)), format = "Q%q/%Y")
  #  t_trading_test<- t_trading_test %>% arrange(quarter)
  
  #  ret_trading<- as.vector(diff(as.matrix(log(na_ma(sapply( t_trading_test$Close, as.numeric ) , k = 3 , weighting = "simple")))))
  
  
  #  t_trading_test2 <- t_trading_test %>% select(quarter) %>% tail(-1)
  #  t_trading_test2$ret <- ret_trading
  
  #####################################################################################################
  #####################################################################################################
  
  t_trading_test <- trading
  t_trading_test$quarter <- as.yearqtr(gsub(" ", "", substr( rownames(t_trading_test) ,1,10)), format = "Q%q/%Y")
  t_trading_test<- t_trading_test %>% arrange(quarter)
  #t_trading_test <- na_ma(sapply( t_trading_test$Close, as.numeric ) , k = 3 , weighting = "simple")
  
  t_trading_test$Close <- na_ma(sapply( t_trading_test$Close, as.numeric ) , k = 3 , weighting = "simple")
  t_trading_test['Close_lead'] <- c( tail(t_trading_test['Close'], dim(t_trading_test)[1] - 1)[[1]] , NA )
  t_trading_test['ret'] <- (t_trading_test$Close_lead - t_trading_test$Close)/(t_trading_test$Close)
  t_trading_test2 <- t_trading_test %>% select(quarter , ret) %>% head(-1)
  t_trading_test2
  
  #####################################################################################################
  #####################################################################################################
  
  #step 4 
  t_trading_test2$stock_name <- ssym
  #  statistic$stock_name <- ssym
  #  fin_data$stock_name <- ssym
  #  fin_ratio$stock_name <- ssym
  names(t_trading_test2)
  ### step 5 union base
  
  trading_ret_base<- union_all(trading_ret_base , t_trading_test2)
  #  statistic1_base<- union_all(statistic1_base , statistic)
  #  fin_data1_base<- union_all(fin_data1_base , fin_data)
  #  fin_ratio1_base<- union_all(fin_ratio1_base , fin_ratio)
}

names(trading_ret_base)


trading_ret_base %>% dim()
trading_ret_base %>% head()


trading_ret1 <- trading_ret_base

### join both together

names(trading_ret1)
trading_ret1
#big_fin_data %>% dim()
#names(big_fin_data)
#names(trading_ret_base)


#trading_ret_base <- as_tibble(trading_ret_base)

#names(trading_ret_base)[names(trading_ret_base) == "Close"] <- "return"
#head(trading_ret_base)
#tempp <- trading_ret1 %>% head(5)

#tempp %>% head(-1)
#big_fin_data %>% head()


#trading_ret2 <- trading_ret1 %>% as_tibble() 

#names(trading_ret2)[2] <- "ret"
#trading_ret2


library(dplyr)
ret_inner_big <- inner_join(trading_ret1, big_fin_data, by = c("quarter","stock_name"))
ret_inner_big %>% dim()
head(ret_inner_big)



### data_frame

ret_inner_big
names(ret_inner_big)

tomyumkung <- as.yearqtr(c('1996 Q3','1996 Q4','1997 Q1','1997 Q2','1997 Q3','1997 Q4','1998 Q1','1998 Q2','1998 Q3'))
gfc <- as.yearqtr(c('2007 Q2','2007 Q3','2007 Q4','2008 Q1','2008 Q2','2008 Q3','2008 Q4','2009 Q1'))
covid <- as.yearqtr(c('2019 Q1','2019 Q2','2019 Q3','2019 Q4','2020 Q1','2020 Q2','2020 Q3','2020 Q4'))

ret_inner_big
str(ret_inner_big)
ret_inner_big %>% select(ReturnonAsset) %>% na_remove() %>% mean()

grp_roa = ret_inner_big  %>% group_by(quarter) %>% summarise(mean_ROA = mean(ReturnonAsset, na.rm = TRUE))

write.csv(grp_roa , "grp_roa.csv")


####################3

ret_inner_big2 <- ret_inner_big %>% mutate(crisis_period = ifelse(quarter %in% tomyumkung, 1997, ifelse(quarter %in% gfc , 2008,
                                                                                                        ifelse(quarter %in% covid , 2019, "Non_crisis")
))) 

specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))



names(ret_inner_big2)

col_sel = "CashCycle_Days"

grp_fin <- ret_inner_big2 %>% group_by(crisis_period, quarter) %>% summarise(mean_ratios = mean(eval(parse(text = col_sel)), na.rm = TRUE)) %>% arrange(quarter)
grp_fin2 <- grp_fin[!is.nan(grp_fin$mean_ratios),]
grp_fin3 <- grp_fin2 %>% group_by(crisis_period) %>% summarise(mean_ratios = mean(mean_ratios, na.rm = TRUE)) %>% arrange(crisis_period)



ggplot(grp_fin3, aes(x= crisis_period, y= mean_ratios , fill = mean_ratios)) + 
  geom_bar(stat="identity") +
  ggtitle(paste(paste("Average of all",col_sel),"across different crisis period")) +
  geom_label(label=specify_decimal(grp_fin3$mean_ratios,2), fill="#FFFFFF")




#Average of All ROA In Healthcare Sector



write.csv(grp_fin , "fin_ratios_test.csv")

#is.nan(X)
#grp_roa = ret_inner_big  %>% group_by(quarter) %>% summarise(mean_ROA = mean(ReturnonAsset, na.rm = TRUE))
#write.csv(grp_roa , "fin_ratios_test.csv")

ret_inner_big %>% str()
summary(ret_inner_big)

library(plm)

trading_panel <- pdata.frame(ret_inner_big, index=c("stock_name","quarter"), drop.index=TRUE, row.names=TRUE)
# drop.index=FALSE
trading_panel
names(trading_panel)


### TOO MANY NA's columns

# PEG 
# Percent_12MDividendYield 
# Beta



# install.packages("plm")
library(plm)
#install.packages("punitroots")
library(punitroot)



trading_panel2 <- trading_panel %>% select( -c("High","Low","DailyAvgValue_Baht"))
names(trading_panel2)
head(trading_panel2)

names(trading_panel2)[names(trading_panel2) == "Close"] <- "Close2"


trading_panel.fe <- plm(Close~ ReturnonEquity+ReturnonAsset+NetProfitMargin+EBITMargin+GrossProfitMargin+D_by_ERatio+InterestCoverage+CurrentRatio+QuickRatio+FixedAssetTurnover+TotalAssetTurnover+InventoryTurnover+AverageSalePeriod_Days+AccountReceivableTurnover+AverageCollectionPeriod_Days+AccountPayableTurnover+AveragePaymentPeriod_Days+CashCycle_Days , data = trading_panel2, model = "within")
trading_panel_no <- plm(Close~ ReturnonEquity+ReturnonAsset+NetProfitMargin+EBITMargin+GrossProfitMargin+D_by_ERatio+InterestCoverage+CurrentRatio+QuickRatio+FixedAssetTurnover+TotalAssetTurnover+InventoryTurnover+AverageSalePeriod_Days+AccountReceivableTurnover+AverageCollectionPeriod_Days+AccountPayableTurnover+AveragePaymentPeriod_Days+CashCycle_Days , data = trading_panel2)

trading_panel.re <- plm(Close~. , data = trading_panel2, model = "random")

summary(trading_panel.fe)
summary(trading_panel_no)



# PANELPANEL
'%!in%' <- function(x,y)!('%in%'(x,y))
# c(1,3,11)%!in%1:10


tomyumkung <- as.yearqtr(c('1996 Q3','1996 Q4','1997 Q1','1997 Q2','1997 Q3','1997 Q4','1998 Q1','1998 Q2','1998 Q3'))
gfc <- as.yearqtr(c('2007 Q2','2007 Q3','2007 Q4','2008 Q1','2008 Q2','2008 Q3','2008 Q4','2009 Q1'))
covid <- as.yearqtr(c('2019 Q1','2019 Q2','2019 Q3','2019 Q4','2020 Q1','2020 Q2','2020 Q3','2020 Q4'))
all_crisis <- c(tomyumkung ,gfc , covid )





df_1997 <- ret_inner_big %>% filter(quarter %in% tomyumkung )
df_2008 <- ret_inner_big %>% filter(quarter %in% gfc )
df_2019 <- ret_inner_big %>% filter(quarter %in% covid )
df_all_crisis <- ret_inner_big %>% filter(quarter %in% all_crisis )
df_non_crisis <- ret_inner_big %>% filter(quarter %!in% all_crisis )



df_1997_pn <- pdata.frame(df_1997, index=c("stock_name","quarter"), drop.index=TRUE, row.names=TRUE)
df_2008_pn <- pdata.frame(df_2008, index=c("stock_name","quarter"), drop.index=TRUE, row.names=TRUE)
df_2019_pn <- pdata.frame(df_2019, index=c("stock_name","quarter"), drop.index=TRUE, row.names=TRUE)
df_all_crisis_pn <- pdata.frame(df_all_crisis, index=c("stock_name","quarter"), drop.index=TRUE, row.names=TRUE)
df_non_crisis_pn <- pdata.frame(df_non_crisis, index=c("stock_name","quarter"), drop.index=TRUE, row.names=TRUE)



#ret~ P_by_E+ EPS_Baht + ReturnonAsset + ReturnonEquity + GrossProfitMargin + D_by_ERatio + CurrentRatio + FixedAssetTurnover , data = df_1997_pn, model = "within"

trading_panel.re_1997 <- plm(ret~ P_by_E + ReturnonAsset + GrossProfitMargin + D_by_ERatio + CurrentRatio + FixedAssetTurnover , data = df_1997_pn, model = "within")
summary(trading_panel.re_1997)


trading_panel.re_2008 <- plm(ret~ P_by_E + ReturnonAsset  + GrossProfitMargin + D_by_ERatio + CurrentRatio + FixedAssetTurnover , data = df_2008_pn, model = "within")
summary(trading_panel.re_2008)

trading_panel.re_2019 <- plm(ret~ P_by_E + ReturnonAsset  + GrossProfitMargin + D_by_ERatio + CurrentRatio + FixedAssetTurnover , data = df_2019_pn, model = "within")
summary(trading_panel.re_2019)

trading_panel.re_all_crisis <- plm(ret~ P_by_E + ReturnonAsset  + GrossProfitMargin + D_by_ERatio + CurrentRatio + FixedAssetTurnover , data = df_all_crisis_pn, model = "within")
summary(trading_panel.re_all_crisis)

trading_panel.re_non_crisis <- plm(ret~ P_by_E + ReturnonAsset + GrossProfitMargin + D_by_ERatio + CurrentRatio + FixedAssetTurnover , data = df_non_crisis_pn, model = "within")
summary(trading_panel.re_non_crisis)




############################################################################
## STOP THURS ###############################################################
##############################################################################






#########################

ret_ratios  <- ret_inner_big %>% select(all_col)
ret_ratios <- ret_ratios[complete.cases(ret_ratios), ]
ret_ratios




food_price_df <- ret_ratios[,c("P_by_E","P_by_BV","EV_by_EBITDA","Percent_DividendYield")]
food_price.pca <- prcomp(food_price_df, center = TRUE,scale. = TRUE)
price_pca <- food_price.pca$x
summary(food_price.pca)


food_profit_df <- ret_ratios[,c("ReturnonEquity","ReturnonAsset","NetProfitMargin","EBITMargin","GrossProfitMargin")]
food_profit.pca <- prcomp(food_profit_df, center = TRUE,scale. = TRUE)
profit_pca <- food_profit.pca$x
summary(food_profit.pca)


food_debt_df <- ret_ratios[,c("D_by_ERatio","InterestCoverage")]
food_debt.pca <- prcomp(food_debt_df, center = TRUE,scale. = TRUE)
debt_pca <- food_debt.pca$x
summary(food_debt.pca)

food_cash_df <- ret_ratios[,c("CurrentRatio","QuickRatio","CashCycle_Days","AccountPayableTurnover","AveragePaymentPeriod_Days")]
food_cash.pca <- prcomp(food_cash_df, center = TRUE,scale. = TRUE)
cash_pca <- food_cash.pca$x
summary(food_cash.pca)



food_activity_df <- ret_ratios[,c("FixedAssetTurnover","TotalAssetTurnover","InventoryTurnover","AverageSalePeriod_Days","AccountReceivableTurnover","AverageCollectionPeriod_Days")]
food_activity.pca <- prcomp(food_activity_df, center = TRUE,scale. = TRUE)
activity_pca <- food_activity.pca$x

summary(food_activity.pca)


library(ggbiplot)
ggbiplot(food_price.pca )+ 
  scale_x_continuous(limits = c(-3, 3)) + 
  scale_y_continuous(limits = c(-3, 3))+
  theme(text = element_text(size = 10) )


ggbiplot(food_price.pca  , choices=c(1,3))+ 
  scale_x_continuous(limits = c(-3, 3)) + 
  scale_y_continuous(limits = c(-3, 3))+
  theme(text = element_text(size = 10) )

ggbiplot(food_price.pca  , choices=c(2,3))+ 
  scale_x_continuous(limits = c(-3, 3)) + 
  scale_y_continuous(limits = c(-3, 3))+
  theme(text = element_text(size = 10) )


ggbiplot(food_profit.pca )+ 
  scale_x_continuous(limits = c(-3, 3)) + 
  scale_y_continuous(limits = c(-3, 3))+
  theme(text = element_text(size = 10) )

ggbiplot(food_profit.pca , choices=c(1,3))+ 
  scale_x_continuous(limits = c(-3, 3)) + 
  scale_y_continuous(limits = c(-3, 3))+
  theme(text = element_text(size = 10) )


ggbiplot(food_profit.pca , choices=c(2,3))+ 
  scale_x_continuous(limits = c(-3, 3)) + 
  scale_y_continuous(limits = c(-3, 3))+
  theme(text = element_text(size = 10) )

ggbiplot(food_debt.pca )+ 
  scale_x_continuous(limits = c(-3, 3)) + 
  scale_y_continuous(limits = c(-3, 3))+
  theme(text = element_text(size = 10) )

ggbiplot(food_cash.pca )+ 
  scale_x_continuous(limits = c(-3, 3)) + 
  scale_y_continuous(limits = c(-3, 3))+
  theme(text = element_text(size = 10) )




ggbiplot(food_cash.pca  ,choices=c(1,3))+ 
  scale_x_continuous(limits = c(-3, 3)) + 
  scale_y_continuous(limits = c(-3, 3))+
  theme(text = element_text(size = 10) )

ggbiplot(food_cash.pca  ,choices=c(2,3))+ 
  scale_x_continuous(limits = c(-3, 3)) + 
  scale_y_continuous(limits = c(-3, 3))+
  theme(text = element_text(size = 10) )




ggbiplot(food_activity.pca )+ 
  scale_x_continuous(limits = c(-3, 3)) + 
  scale_y_continuous(limits = c(-3, 3))+
  theme(text = element_text(size = 10) )

ggbiplot(food_activity.pca ,choices=c(1,3))+ 
  scale_x_continuous(limits = c(-3, 3)) + 
  scale_y_continuous(limits = c(-3, 3))+
  theme(text = element_text(size = 10) )

ggbiplot(food_activity.pca ,choices=c(2,3))+ 
  scale_x_continuous(limits = c(-3, 3)) + 
  scale_y_continuous(limits = c(-3, 3))+
  theme(text = element_text(size = 10) )





ret_ratios %>% names()
ret_ratios2 <- ret_ratios %>% select("quarter","ret","stock_name","D_by_ERatio","InterestCoverage")
ret_ratios2



summary(food_price.pca) #3
summary(food_profit.pca) #2 
summary(food_debt.pca) ## no pca
summary(food_cash.pca)# 3 
summary(food_activity.pca) # 3 



ret_ratios2$activity_pc1 <- activity_pca[,1]
ret_ratios2$activity_pc2 <- activity_pca[,2]
ret_ratios2$activity_pc3 <- activity_pca[,3]


ret_ratios2$price_pc1 <- price_pca[,1]
ret_ratios2$price_pc2 <- price_pca[,2]
ret_ratios2$price_pc3 <- price_pca[,3]

ret_ratios2$profit_pc1 <- profit_pca[,1]
ret_ratios2$profit_pc2 <- profit_pca[,2]
ret_ratios2$profit_pc3 <- profit_pca[,3]

ret_ratios2$cash_pc1 <- cash_pca[,1]
ret_ratios2$cash_pc2 <- cash_pca[,2]
ret_ratios2$cash_pc3 <- cash_pca[,3]




df_1997 <- ret_ratios2 %>% filter(quarter %in% tomyumkung )
df_2008 <- ret_ratios2 %>% filter(quarter %in% gfc )
df_2019 <- ret_ratios2 %>% filter(quarter %in% covid )
df_all_crisis <- ret_ratios2 %>% filter(quarter %in% all_crisis )
df_non_crisis <- ret_ratios2 %>% filter(quarter %!in% all_crisis )



df_1997_pn <- pdata.frame(df_1997, index=c("stock_name","quarter"), drop.index=TRUE, row.names=TRUE)
df_2008_pn <- pdata.frame(df_2008, index=c("stock_name","quarter"), drop.index=TRUE, row.names=TRUE)
df_2019_pn <- pdata.frame(df_2019, index=c("stock_name","quarter"), drop.index=TRUE, row.names=TRUE)
df_all_crisis_pn <- pdata.frame(df_all_crisis, index=c("stock_name","quarter"), drop.index=TRUE, row.names=TRUE)
df_non_crisis_pn <- pdata.frame(df_non_crisis, index=c("stock_name","quarter"), drop.index=TRUE, row.names=TRUE)


df_non_crisis_pn %>% names()


trading_panel.re_1997 <- plm(ret~ D_by_ERatio +InterestCoverage + cash_pc1 + cash_pc2 +  cash_pc3+ activity_pc1 + activity_pc2 + activity_pc3+price_pc1+price_pc2+price_pc3+profit_pc1 +profit_pc2+profit_pc3 , data = df_1997_pn, model = "within")
summary(trading_panel.re_1997)


trading_panel.re_2008 <- plm(ret~ D_by_ERatio +InterestCoverage + cash_pc1 + cash_pc2 +  cash_pc3+ activity_pc1 + activity_pc2 + activity_pc3+price_pc1+price_pc2+price_pc3+profit_pc1 +profit_pc2+profit_pc3 , data = df_2008_pn, model = "within")
summary(trading_panel.re_2008)

trading_panel.re_2019 <- plm(ret~ D_by_ERatio +InterestCoverage + cash_pc1 + cash_pc2 +  cash_pc3+ activity_pc1 + activity_pc2 + activity_pc3+price_pc1+price_pc2+price_pc3+profit_pc1 +profit_pc2+profit_pc3 , data = df_2019_pn, model = "within")
summary(trading_panel.re_2019)

trading_panel.re_all_crisis <- plm(ret~ D_by_ERatio +InterestCoverage + cash_pc1 + cash_pc2 +  cash_pc3+ activity_pc1 + activity_pc2 + activity_pc3+price_pc1+price_pc2+price_pc3+profit_pc1 +profit_pc2+profit_pc3 , data = df_all_crisis_pn, model = "within")
summary(trading_panel.re_all_crisis)

trading_panel.re_non_crisis <- plm(ret~ D_by_ERatio +InterestCoverage + cash_pc1 + cash_pc2 +  cash_pc3+ activity_pc1 + activity_pc2 + activity_pc3+price_pc1+price_pc2+price_pc3+profit_pc1 +profit_pc2+profit_pc3 , data = df_non_crisis_pn, model = "within")
summary(trading_panel.re_non_crisis)







