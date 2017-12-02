library(dplyr)
library(ggplot2)

### Load the datasets

customer <- read.csv('Customer Personal Details.csv', 
                     stringsAsFactors = FALSE, na.strings = "")


product <- read.csv('Customer Products.csv', 
                    stringsAsFactors = FALSE, na.strings = "")

## Cleaning and preparing datasets ===

## Cleaning and preparing customer table

customer$fecha_dato<-as.Date(customer$fecha_dato)
customer$ind_empleado<-as.factor(customer$ind_empleado)
customer$sexo<- as.factor(customer$sexo)
customer$age <- as.numeric(customer$age)
customer$ind_nuevo<- as.factor(customer$ind_nuevo)
customer$indfall<- as.factor(customer$indfall)
customer$renta<-as.numeric(customer$renta)
customer$ind_actividad_cliente<- as.factor(customer$ind_actividad_cliente)
customer$segmento<-as.factor(customer$segmento)


##Cleaning and preparing Products table

product$ind_nomina_ult1<-as.numeric(product$ind_nomina_ult1)
product$ind_nom_pens_ult1<-as.numeric(product$ind_nom_pens_ult1)

##Data Wrangling

customer <-customer %>% select (-indrel,-indresi,-conyuemp,-indfall, -tipodom) %>%
                        filter (ind_empleado %in% c('N','P')) %>%
                        filter (pais_residencia =='ES') %>%
                        filter  (tiprel_1mes %in% c('A','I')) 
                       


#To replace the column canal_entrada with 'Other' values where count of observations is less than 200

canal_entrada_keep <- group_by(customer, canal_entrada) %>%
  summarise(n = n()) %>% filter(n > 200)

canal_entrada_keep <- canal_entrada_keep$canal_entrada

customer$canal_entrada_clean <- ifelse(customer$canal_entrada %in% canal_entrada_keep, customer$canal_entrada, 'OTHER')

## As you can see all categories have more than 200 elements
table(customer$canal_entrada_clean)

customer$canal_entrada_clean <- as.factor(customer$canal_entrada_clean)

customer <-customer %>% select (-canal_entrada)

#NA's in rows:

apply(customer, 1, function(x) sum(is.na(x)))

# Eliminating rows with 10 or more NAs (the same is keeping rows with less than 10 NAs)

customer <- customer[apply(customer, 1, function(x) sum(is.na(x))) < 10, ]
#NA's in columns:
apply(customer, 2, function(x) sum(is.na(x)))

## Imputation of missing values, here I provide a couple of examples
# Of what you sould do with all other columns:
# sexo
table(customer$sexo)
# Since most of the values are "V" then we will suppose that the 15 
# missing are also from that category:
customer$sexo[is.na(customer$sexo)] <- "V"
# renta: This one is complicated because about 20% of the values
# are missing, however for the sake of simplicity we will do 
# the easiest solution
customer$renta[is.na(customer$renta)] <- median(customer$renta, na.rm = TRUE)
table(customer$segmento)
#Since most of the values are from the 02-PARTICULARES segment we will suppose that 1072 missing entries are also from that segment
customer$segmento[is.na(customer$segmento)] <- "02 - PARTICULARES"

table(customer$canal_entrada_clean)
#Since most of the values are from the KHE Channel we will assume that the 1005 missing entries are also from the KHE channel
customer$canal_entrada_clean[is.na(customer$canal_entrada_clean)]<- "KHE"


table(customer$ult_fec_cli_1t)
#Since there are 72 values in 2015-07-21 we will assume that the 1160891 missing values are in that month which is the last date as primary customer
customer$ult_fec_cli_1t[is.na(customer$ult_fec_cli_1t)]<-"2015-07-21"
customer$ult_fec_cli_1t <-as.Date(customer$ult_fec_cli_1t)


### Descriptve analysis ===

#The number of males are more than females at 62,9132
table(customer$sexo)

## The percentage of males are more than females at 54.12%
100*round(table(customer$sexo)/sum(table(customer$sexo)),5)

##Plotting the sex in a Histogram shows that the count of males are more than females
ggplot(customer, aes(x=sexo)) +
  geom_bar(fill="Red")

## Age === The minumum age of the customer is 2 and maximum is 117 and hence there is anamoly in the data. The mean or average age of the customer is
## around 38 years and the standard deviation is only 17.15 which means they are closer to the mean age.
customer$age <- as.numeric(customer$age)

summary(customer$age)
mean(customer$age, na.rm = TRUE)
sd(customer$age, na.rm = TRUE)
median(customer$age, na.rm = TRUE)
min(customer$age, na.rm = TRUE)
max(customer$age, na.rm = TRUE)

## Plotting the Customer Sex against Age in a Box plot shows that 50% of the females are in the age range 25 to 45 and males are between 27 and 55.
##The median age for females is around 30 and that of males is around 40

ggplot(customer, aes(x=sexo, y=age)) +
  geom_boxplot()


##Channel ==== Customer use KHE Channel the most with a total of 392291 and  account for 33.746% of the total
table(customer$canal_entrada_clean)
100*round(table(customer$canal_entrada_clean)/sum(table(customer$canal_entrada_clean)),5)

## Gross income of household == The mean income is 124,157 . The minimum salary of the Customers is 5341 and the maximum salary is 15711716

summary(customer$renta)
mean(customer$renta, na.rm = TRUE)
sd(customer$renta, na.rm = TRUE)
median(customer$renta, na.rm = TRUE)
min(customer$renta, na.rm = TRUE)
max(customer$renta, na.rm = TRUE)


## Customer Segmentation === The highest segment of customers are 02 - PARTICULARES(Individuals) with 616271 and accounting for 53.01% as shown by the bar graph
table(customer$segmento)
100*round(table(customer$segmento)/sum(table(customer$segmento)),5)
ggplot(customer[!is.na(customer$segmento),], aes(x=segmento, )) +
  geom_bar(fill='red', color='black')

### Joining the Customer and Products table ===
names(customer)
names(product)

joined <- left_join(customer, product, by="ncodpers")
View(head(joined))
View(product)


## Finding out the total number of customers grouped by segment . The total number of accounts are higher in the 02- PARTICUARES segment than the other segments


seg_account<-joined %>%
  group_by(segmento) %>%
  summarise(sum_saving_account=sum(ind_ahor_fin_ult1,na.rm=TRUE),
            sum_guarantees=sum(ind_aval_fin_ult1,na.rm=TRUE),
            sum_current_account= sum(ind_cco_fin_ult1,na.rm=TRUE),
            sum_derivada_account= sum(ind_cder_fin_ult1,na.rm=TRUE),
            sum_payroll_account=sum(ind_cno_fin_ult1,na.rm=TRUE),
            sum_junior_account=sum(ind_ctju_fin_ult1,na.rm=TRUE),
            sum_masparticular= sum(ind_ctma_fin_ult1,na.rm=TRUE),
            sum_particular= sum(ind_ctop_fin_ult1,na.rm=TRUE),
            sum_particularplus=sum(ind_ctpp_fin_ult1,na.rm=TRUE),
            sum_shortterm=sum(ind_deco_fin_ult1,na.rm=TRUE),
            sum_mediumterm=sum(ind_deme_fin_ult1,na.rm=TRUE),
            sum_longterm=sum(ind_dela_fin_ult1,na.rm=TRUE),
            sum_eccount=sum(ind_ecue_fin_ult1,na.rm=TRUE),
            sum_funds=sum(ind_fond_fin_ult1,na.rm=TRUE),
            sum_mortgage=sum(ind_hip_fin_ult1,na.rm=TRUE),
            sum_pensions=sum(ind_plan_fin_ult1,na.rm=TRUE),
            sum_loans=sum(ind_pres_fin_ult1,na.rm=TRUE),
            sum_taxes=sum(ind_reca_fin_ult1,na.rm=TRUE),
            sum_creditcard=sum(ind_tjcr_fin_ult1,na.rm=TRUE),
            sum_securities=sum(ind_valo_fin_ult1,na.rm=TRUE),
            sum_homeaccount=sum(ind_viv_fin_ult1,na.rm=TRUE),
            sum_payroll=sum(ind_nomina_ult1,na.rm=TRUE),
            sum_pensions=sum(ind_nom_pens_ult1,na.rm=TRUE),
            sum_directdebit=sum(ind_recibo_ult1,na.rm=TRUE))


#Bar graph to show that the segment 03- UNIVERSITARIO have higher  current accounts than the segment 02-PARTOCULARES
ggplot(seg_account, aes(x=segmento, y=sum_current_account)) +
  geom_bar(stat="identity",fill='red', color='black')


# Bar graph to show that most of the Current accounts is held by youngsters less than 30 years
age_seg_account<-joined %>%
  group_by(age,segmento) %>%
  summarise(sum_current_account= sum(ind_cco_fin_ult1,na.rm=TRUE))


ggplot(age_seg_account, aes(x=age, y=sum_current_account), fill=segmento) +
  geom_bar(stat="identity",position=position_dodge())


# Regression line shows that the correlation between age and the current account is negative which means as age increases the number of current accounts held by the customer decreases.

ggplot(age_seg_account, aes(x=age,y=sum_current_account)) +
  geom_smooth(method=lm,se=FALSE)
