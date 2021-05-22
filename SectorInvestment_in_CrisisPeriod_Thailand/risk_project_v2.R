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

ret_trading<- data.frame(diff(as.matrix(log(na_ma(sapply( t_trading_test['Close'], as.numeric ) , k = 3 , weighting = "simple")))))

t_trading_test2 <- t_trading_test %>% select(quarter) %>% head(-1)
t_trading_test2$ret <- ret_trading
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
  t_trading_test <- trading
  t_trading_test$quarter <- as.yearqtr(gsub(" ", "", substr( rownames(t_trading_test) ,1,10)), format = "Q%q/%Y")
  t_trading_test<- t_trading_test %>% arrange(quarter)
  
  ret_trading<- data.frame(diff(as.matrix(log(na_ma(sapply( t_trading_test['Close'], as.numeric ) , k = 3 , weighting = "simple")))))
  
  t_trading_test2 <- t_trading_test %>% select(quarter) %>% tail(-1)
  t_trading_test2$ret <- ret_trading
  
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

trading_ret1 <- trading_ret_base

### join both together

names(trading_ret1)

#big_fin_data %>% dim()
#names(big_fin_data)
#names(trading_ret_base)


#trading_ret_base <- as_tibble(trading_ret_base)

#names(trading_ret_base)[names(trading_ret_base) == "Close"] <- "return"
#head(trading_ret_base)

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




library(plm)

trading_panel <- pdata.frame(ret_inner_big, index=c("stock_name","quarter"), drop.index=TRUE, row.names=TRUE)
# drop.index=FALSE
trading_panel
names(trading_panel)







# install.packages("plm")
library(plm)
install.packages("punitroots")
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



# “within”, “random”, “ht”, “between”, “pooling”, “fd”



trading_panel.re <- plm(Close~ FixedAssetTurnover+FixedAssetTurnover_dummy, data = trading_panel_dummy, model = "random")
summary(trading_panel.fe)





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

ret_trading<- data.frame(diff(as.matrix(log(na_ma(sapply( t_trading_test['Close'], as.numeric ) , k = 3 , weighting = "simple")))))

t_trading_test2 <- t_trading_test %>% select(quarter) %>% head(-1)
t_trading_test2$ret <- ret_trading
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
length(stock_list)
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
  t_trading_test <- trading
  t_trading_test$quarter <- as.yearqtr(gsub(" ", "", substr( rownames(t_trading_test) ,1,10)), format = "Q%q/%Y")
  t_trading_test<- t_trading_test %>% arrange(quarter)
  
  ret_trading<- data.frame(diff(as.matrix(log(na_ma(sapply( t_trading_test['Close'], as.numeric ) , k = 3 , weighting = "simple")))))
  
  t_trading_test2 <- t_trading_test %>% select(quarter) %>% tail(-1)
  t_trading_test2$ret <- ret_trading
  
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

trading_ret1 <- trading_ret_base

### join both together

names(trading_ret1)

#big_fin_data %>% dim()
#names(big_fin_data)
#names(trading_ret_base)


#trading_ret_base <- as_tibble(trading_ret_base)

#names(trading_ret_base)[names(trading_ret_base) == "Close"] <- "return"
#head(trading_ret_base)

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

'%ni%' <- Negate('%in%')
 
test_grp3 <- ret_inner_big2 %>% 
  group_by(stock_name ,crisis_period, quarter) %>%
  summarise(mean_ratios = mean(eval(parse(text = col_sel)), na.rm = TRUE)) %>% 
  arrange(desc(mean_ratios))
# test_grp3


grp_fin <- ret_inner_big2 %>% 
  group_by(stock_name ,crisis_period, quarter) %>%
  summarise(mean_ratios = mean(eval(parse(text = col_sel)), na.rm = TRUE)) %>% 
  arrange(desc(mean_ratios)) %>%
  filter(stock_name %ni% c("cpi","apure","br")) %>% 
  group_by(crisis_period, quarter) %>% 
  summarise(mean_ratios = mean(mean_ratios, na.rm = TRUE)) %>% 
  arrange(quarter)
grp_fin2 <- grp_fin[!is.nan(grp_fin$mean_ratios),]
grp_fin3 <- grp_fin2 %>% group_by(crisis_period) %>% summarise(mean_ratios = mean(mean_ratios, na.rm = TRUE)) %>% arrange(crisis_period)


ggplot(grp_fin3, aes(x= crisis_period, y= mean_ratios , fill = mean_ratios)) + 
  geom_bar(stat="identity") +
  ggtitle(paste(paste("Average of all",col_sel),"across different crisis period")) +
  geom_label(label=specify_decimal(grp_fin3$mean_ratios,2), fill="#FFFFFF")

