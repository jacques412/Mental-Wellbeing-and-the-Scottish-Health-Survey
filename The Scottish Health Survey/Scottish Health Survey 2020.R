
# ------------------------------------------- install libraries
#install.packages("ggpubr")
#install.packages('moments')
#install.packages("readstata13")
#install.packages('gmodels')

# load libraries 
#Note that thee are conflicts between 'plyr' and dplyr. If you have issues running code, try restarting R and only load dplyr

library(moments)
library(readstata13)
library(ggplot2)
library('plyr')
library(dplyr)
library('gmodels')
library("ggpubr")

#-------------------------------------------- opening the data set

setwd("C:\\Users\\jarvi\\OneDrive\\Desktop\\The Scottish Health Survey\\Data\\UKDA-8834-stata\\stata\\stata13")

df1 <- read.dta13("data.dta", convert.factors = T ,generate.factors = T)

# Changing where images are saved

setwd("C:\\Users\\jarvi\\OneDrive\\Desktop\\The Scottish Health Survey\\Images")

#-------------------------------------------------------------------- data wrangling

#Selecting relevant variables

df <- df1 %>%
  select(Sex, age90, ag16g10, ag16g4, EmpNow, SIMD20_SGa,
         Pcrisis2, Contact, GenHelf, longill12, WEMWBS_t20)

# Rename columns 

df <- df %>%
  rename(Age = age90, Age_10factors = ag16g10, Age_4factors = ag16g4,
         Employment = EmpNow, SIMD = SIMD20_SGa, Crisis_people = Pcrisis2,
         Gen_health = GenHelf, Long_illness = longill12, WEMWBS_Score = WEMWBS_t20)

# check for missing cases

which(is.na(df)) 
which(!complete.cases(df))

#=========================================================================================================================

#-------------------------------------------- Population description

# --------------------------- Age
# Dropping non-applicable rows

df_age <- df[!(df$Age == "Refused" | df$Employment == "Don't know" | df$Employment== "Schedule not obtained" | 
                         df$Employment== "Schedule not applicable" | df$Employment== "Not applicable"),]

# Resetting age as an integer

df_age$Age <- as.integer(df_age$Age)

# Plotting Age

p<-ggplot(df_age, aes(x=Age)) + 
  geom_histogram(color="steelblue", fill="lightblue", stat = "count") +
  theme_classic() + 
  theme(axis.text = element_text(size=15, face= "bold")) +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  theme(axis.title.y.left = element_text(size=15)) +
  theme(axis.title.x.bottom = element_text(size=15))

# add a percentage line 

p+ geom_vline(aes(xintercept=mean(df_age$Age)),
              color="red", linetype="dashed", size=2) +
  labs(title="Age Histogram",x="Age", y = "Count")

# ------------------------------- sex

# The following provides two side by side histograms of male and female age

mu <- ddply(df_age, "Sex", summarise, grp.mean=mean(Age))

p <-ggplot(df_age, aes(x=Age))+
  geom_histogram(color="steelblue", fill="lightblue")+
  facet_grid(Sex ~ .)

p + geom_vline(data=mu, aes(xintercept=grp.mean, color="blue"),
             linetype="dashed") +
  labs(title="Age and Sex Histogram",x="Age", y = "Count") + 
  theme(legend.position = "none") 
p

#=========================================================================================================================

#----------------------------------------------------Mental Well being

df_wellbeing <- df[!(df$WEMWBS_Score == "Refused" | df$WEMWBS_Score == "Don't know" | df$WEMWBS_Score == "Schedule not obtained" | 
                 df$WEMWBS_Score == "Schedule not applicable" | df$WEMWBS_Score == "Not applicable"),]

df_wellbeing$WEMWBS_Score <- as.integer(as.character(df_wellbeing$WEMWBS_Score))

df_wellbeing %>%
  group_by(Sex) %>%
  summarise(mean = mean(WEMWBS_Score))

df_wellbeing %>%
  group_by(Age_10factors) %>%
  summarise(mean=mean(WEMWBS_Score))

#==========================================================================================================================

#------------------------------------------------------ Employment

#--------------------------- The economically active

df_econ_active <- df[!(df$Employment == "Refused" | df$Employment == "Don't know" | df$Employment== "Schedule not obtained" | 
                  df$Employment== "Schedule not applicable" | df$Employment== "Not applicable" |
          df$Employment == "Permanently sick or disabled" | df$Employment == "Looking after home or family" 
          | df$Employment == "In education at school/college/university" | df$Employment == "Retired" |
            df$Employment == "Unemployed and not seeking work" |
            df$Employment == "In unpaid/voluntary work"),]

#Resetting factor levels

df_econ_active$Employment <- factor(df_econ_active$Employment)
df_econ_active$Age <- as.integer(df_econ_active$Age)

# 9% of econ active age is below 16. These cases are removed for age 

df_econ_active_age <- df_econ_active %>%
  filter(Age >= 16)

df_econ_active_age %>%
  summarise(mean=mean(Age))

ddply(df_econ_active_age, "Sex", summarise,gep.mean=mean(Age))

#Age

p<-ggplot(df_econ_active_age, aes(x=Age)) + 
  geom_histogram(color="steelblue", fill="lightblue", stat = "count")+
  theme_classic() +
  theme(axis.text = element_text(size=15, face= "bold")) +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  theme(axis.title.y.left = element_text(size=15)) +
  theme(axis.title.x.bottom = element_text(size=15)) 

p + geom_vline(aes(xintercept=mean(df_econ_active_age$Age)),
              color="red", linetype="dashed", size=2) +
  labs(title="The economically active age histogram",x="Age", y = "Count") 

# ----------------------- age and sex

#resetting factor levels

df_econ_active$Sex <- factor(df_econ_active$Sex)

# Calculate the mean of each group

mu <- ddply(df_econ_active, "Sex", summarise, grp.mean=mean(Age))
head(mu)

# The EA age and sex histograms

p<-ggplot(df_econ_active, aes(x=Age))+
  geom_histogram(color="steelblue", fill="lightblue")+
  facet_grid(Sex ~ .)
p+geom_vline(data=mu, aes(xintercept=grp.mean, color="blue"),
             linetype="dashed") +
  labs(title="The economically active age and sex histogram") + 
  theme(legend.position = "none")
  
#---------------------------------- employment types

df_econ_active$Employment <- factor(df_econ_active$Employment)

r <- rev(c("Employed and working full time","Employed and working part time",
  "Self-employed and currently working", "Employed but on furlough",
  "Unemployed and seeking work", "Employed but on paid leave (not including furlough)", 
  "Self-employed but not currently working and receiving government support", 
  "Self-employed but not currently working and not receiving government support",
  "Employed and on unpaid leave"))

# create a percentage row

df_econ_active_Percent <- df_econ_active %>%
  dplyr::count(Employment) %>%
  mutate(Percent = n) %>%
  mutate(Percent = (Percent/sum(n)*100))
df_econ_active_Percent$Percent <- round(df_econ_active_Percent$Percent,0)
df_econ_active_Percent

# Plotting Employment types

p <- ggplot(df_econ_active_Percent, aes(x = Employment, y = Percent))
p + geom_bar(stat='identity', color="steelblue", fill="lightblue") +
  theme_minimal () +
  coord_flip() + 
  labs(title="The economically active and employment types", y="Percent", x="") +
  scale_x_discrete(limits=r) +
  theme(axis.text = element_text(size=10, face= "bold")) +
  theme(plot.title = element_text(size = 15, face = "bold")) +
  theme(axis.title.y.left = element_text(size=10)) +
  theme(axis.title.x.bottom = element_text(size=10))

# ------------------------------- Employment and sex weighted by sex

Sex_percent <- df_econ_active %>%
  dplyr::count(Employment, Sex) %>%
  mutate(Sex_per = n) %>%
  mutate(Sex_per = ifelse(Sex == "Male", (Sex_per/366)*100, (Sex_per/530)*100))

# Rounding Percent

Sex_percent$Sex_per <- round(Sex_percent$Sex_per,1)
Sex_percent

#Plotting Employment status and sex of the economically active

v <- rev(c("Employed and working full time","Employed and working part time",
           "Self-employed and currently working","Unemployed and seeking work",
           "Employed but on furlough","Employed but on paid leave (not including furlough)",
           "Employed and on unpaid leave", "Self-employed but not currently working and not receiving government support",
           "Self-employed but not currently working and receiving government support")) 
  
s <- ggplot(Sex_percent, aes(x=Employment, y=Sex_per, fill=Sex)) +
  geom_bar(stat="identity", position = position_dodge(), width = 0.7)+ 
  theme_minimal() + coord_flip() +
  labs(title="The economically active and Sex", y="Percent", x="") +
  theme(axis.text = element_text(size=15, face= "bold")) +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  theme(axis.title.y.left = element_text(size=15)) +
  theme(axis.title.x.bottom = element_text(size=15))+
  scale_fill_manual(values=c('lightblue',"darkblue")) + 
  scale_x_discrete(limits=v)

s 

# Age and employment

z <- df_econ_active %>%
  group_by(Employment) %>%
  summarise(Mean = mean(Age)) %>%
  arrange(Mean)

z

# ----------------------------------------------- The Economically Active and mental well being

# Selecting relevant factors
df_econ_active <- df_econ_active[!(df_econ_active$WEMWBS_Score == "Not applicable" | df_econ_active$WEMWBS_Score == "Schedule not applicable" | df_econ_active$WEMWBS_Score == "Schedule not obtained" | 
                                     df_econ_active$WEMWBS_Score== "Schedule not applicable" | df_econ_active$WEMWBS_Score == "Refused" | df_econ_active$WEMWBS_Score == "Don't know"),]  
#resetting factor levels

df_econ_active$WEMWBS_Score <- factor(df_econ_active$WEMWBS_Score)

# converting to integer

df_econ_active$WEMWBS_Score <- as.numeric(as.character(df_econ_active$WEMWBS_Score))

#Descriptive statistics

df_econ_active %>%
  group_by(Employment, Sex) %>%
  summarise(mean = mean(WEMWBS_Score))

ddply(df_econ_active, "Sex", summarise, grp.mean=mean(WEMWBS_Score))

# Employment type, mental well being and sex of the economically active bar chart

df_econ_active %>%
  summarize(Mean = mean(WEMWBS_Score))

r <- df_econ_active %>%
  group_by(Employment,Sex) %>%
  summarise(Mean = mean(WEMWBS_Score))
  
s <- rev(c("Employed and on unpaid leave","Employed but on paid leave (not including furlough)",
        "Employed and working part time", "Self-employed and currently working", 
        "Self-employed but not currently working and not receiving government support",
        "Employed and working full time", "Employed but on furlough",
        "Self-employed but not currently working and receiving government support", 
        "Unemployed and seeking work")) 
        
ggplot(r, aes(x=Employment, y = Mean, fill= Sex)) +
  geom_bar(stat='identity', position=position_dodge(), width = 0.5) +
  coord_flip() +
  theme_minimal()+ 
  labs(title="Employment type, mean mental wellbeing and sex of the economically active ", x="") +
  scale_x_discrete(limits=s) +
  theme(axis.text = element_text(size=14, face= "bold")) +
  theme(plot.title = element_text(size = 17, face = "bold", hjust = 1)) +
  theme(axis.title.y.left = element_text(size=14)) +
  theme(axis.title.x.bottom = element_text(size=14)) +
  scale_fill_manual(values=c('lightblue',"blue")) 

#------------------------------------------------------ The non-economically active

df_non_econ <- df_age[(df_age$Employment == "Permanently sick or disabled" | df_age$Employment == "Looking after home or family" | 
                         df_age$Employment == "In education at school/college/university" | df_age$Employment == "Retired" |
                         df_age$Employment == "Unemployed and not seeking work"),]

#descriptive statistics

df_non_econ %>%
  count(Age_4factors) %>%
  mutate(Percent = n) %>%
  mutate(Percent = Percent/sum(n)*100)

df_non_econ %>%
  group_by(Sex) %>%
  summarize(mean= mean(Age))
    
#----------- Age histogram of the NEA

p<-ggplot(df_non_econ, aes(x=Age)) + 
  geom_histogram(color="steelblue", fill="lightblue", stat = "count")+
  theme(axis.text = element_text(size=14, face= "bold")) +
  theme(plot.title = element_text(size = 17, face = "bold", hjust = 0.1)) +
  theme(axis.title.y.left = element_text(size=16)) +
  theme(axis.title.x.bottom = element_text(size=14)) +
  theme_minimal()

p+ geom_vline(aes(xintercept=mean(df_non_econ$Age)),
              color="red", linetype="dashed", size=2) +
  labs(title="The non-economically active age histogram",x="Age", y = "Count") 

#------------- employment types

df_non_econ$Employment <- factor(df_non_econ$Employment)

df_non_econ %>%
  count(Employment) %>%
  mutate(Percent = (n/sum(n))*100)
  
# ------------- Non-economically active and sex weighted by gender

Sex_percent <- df_non_econ %>%
  dplyr::count(Employment, Sex) %>%
  mutate(Sex_per = n) %>%
  mutate(Sex_per = ifelse(Sex == "Male", (Sex_per/439)*100, (Sex_per/568)*100))

Sex_percent$Sex_per <- round(Sex_percent$Sex_per,1)

#The non-economically active, employment status and sex bar plot

f <- rev(c("Retired", "Looking after home or family",
       "Permanently sick or disabled", "Unemployed and not seeking work",
       "In education at school/college/university"))

s <- ggplot(Sex_percent, aes(x=Employment, y=Sex_per, fill=Sex)) +
  geom_bar(stat="identity", position= position_dodge())+ 
  theme_classic() + coord_flip() +
  labs(title="The non-economically active, employment status and sex", y="Percentage", x="") 

s + theme(axis.text = element_text(size=14, face= "bold")) +
  theme(plot.title = element_text(size = 17, face = "bold", hjust = 0.1)) +
  theme(axis.title.y.left = element_text(size=16)) +
  theme(axis.title.x.bottom = element_text(size=14)) +
  theme_minimal() + 
  scale_fill_manual(values=c('lightblue',"blue")) +
  scale_x_discrete(limits=f)

s

# --------------- The non-economically active and mental well being

# Selecting relevant factors

df_non_econ_wellbeing_Cleaned <- df_non_econ[!(df_non_econ$WEMWBS_Score == "Not applicable" | df_non_econ$WEMWBS_Score == "Schedule not applicable" | df_non_econ$WEMWBS_Score == "Schedule not obtained" | 
                                           df_non_econ$WEMWBS_Score== "Schedule not applicable" | df_non_econ$WEMWBS_Score == "Refused" | df_non_econ$WEMWBS_Score == "Don't know"),]  

#resetting factor levels

df_non_econ_wellbeing_Cleaned$WEMWBS_Score <- factor(df_non_econ_wellbeing_Cleaned$WEMWBS_Score)
df_non_econ_wellbeing_Cleaned$Employment <- factor(df_non_econ_wellbeing_Cleaned$Employment)

# converting to integer

df_non_econ_wellbeing_Cleaned$WEMWBS_Score <- as.numeric(as.character(df_non_econ_wellbeing_Cleaned$WEMWBS_Score))

#Mental well being and Sex

v <- df_non_econ_wellbeing_Cleaned %>%
  group_by(Sex) %>%
  summarise(mean=mean(WEMWBS_Score))
v

df_tot <- df[!(df$WEMWBS_Score == "Refused" | df$WEMWBS_Score == "Don't know" | df$WEMWBS_Score == "Schedule not obtained" | 
                 df$WEMWBS_Score == "Schedule not applicable" | df$WEMWBS_Score == "Not applicable"),]

df_tot$WEMWBS_Score <- as.integer(as.character(df_tot$WEMWBS_Score))

v <- df_tot %>%
  group_by(Employment, Sex, Age_4factors) %>%
  summarise(mean=mean(WEMWBS_Score)) %>%
  arrange(mean)
v

v %>%
  as_tibble() %>%
  print(n=100)

# mean mental well being and employment categories

r <- df_non_econ_wellbeing_Cleaned %>%
  group_by(Employment, Sex) %>%
  summarise(Mean = mean(WEMWBS_Score)) %>%
  arrange(Mean)

r

# Comparing the employed, self employed and unemployed by mean well being

#Employed, self-employed, unemployed 

df_tot %>%
  filter(Employment == "Employed and working full time" | Employment == "Employed and working part time" |
           Employment == "Employed but on furlough" | Employment ==  "Employed but on paid leave (not including furlough)" |
           Employment == "Employed and on unpaid leave") %>%
  summarize(Mean=mean(WEMWBS_Score))

df_tot %>%
  filter(Employment == "Self-employed and currently working" | Employment == "Self-employed but not currently working and receiving government support" |
           Employment == "Self-employed but not currently working and not receiving government support") %>%
  summarize(Mean=mean(WEMWBS_Score))

df_tot %>%
  filter(Employment == "Unemployed and seeking work" | Employment == "Unemployed and not seeking work") %>%
  summarize(Mean=mean(WEMWBS_Score))

# The non-economically active and current status

s <- c("Permanently sick or disabled","Unemployed and not seeking work",
           "Looking after home or family", "Retired", 
           "In education at school/college/university")

ggplot(r, aes(x=Employment, y = Mean)) +
  geom_bar(stat='identity', color="steelblue", fill="steelblue") +
  coord_flip() +
  theme_minimal()+ 
  labs(title="The non-economically active and current status", x="") +
  scale_x_discrete(limits=s) +
  theme(axis.text = element_text(size=14, face= "bold")) +
  theme(plot.title = element_text(size = 17, face = "bold", hjust = 0.1)) +
  theme(axis.title.y.left = element_text(size=14)) +
  theme(axis.title.x.bottom = element_text(size=14)) 

#splitting by sex

q <- df_non_econ_wellbeing_Cleaned %>%
  group_by(Employment, Sex) %>%
  summarise(Mean = mean(WEMWBS_Score)) 

# Mental well being and sex of the non-economically active bar plot

ggplot(q, aes(x=Employment, y=Mean, fill=Sex)) +
  geom_bar(stat="identity", position = position_dodge(), width = 0.5) +
  coord_flip() + theme_minimal() +
  labs(title="Mental wellbeing and sex of the non-economically active", x="") +
  scale_x_discrete(limits=s) +
  theme(axis.text = element_text(size=14, face= "bold")) +
  theme(plot.title = element_text(size = 17, face = "bold", hjust = 0.1)) +
  theme(axis.title.y.left = element_text(size=14)) +
  theme(axis.title.x.bottom = element_text(size=14)) +
  scale_fill_manual(values=c('lightblue',"darkblue"))

#----------------------------------------------    SIMD

#creating a data frame of SIMD with categorical percentages

f <- df %>%
  count(SIMD) %>%
  mutate(Percent = (n/sum(n))*100)
f
  
#------------cleaning up SIMD

df_SIMD_Cleaned <- df[!(df$SIMD == "Not applicable" | df$SIMD == "Schedule not applicable" | df$SIMD == "Schedule not obtained" | 
                          df$SIMD == "Schedule not applicable" | df$SIMD == "Refused" | df$SIMD == "Don't know"),]  

df_SIMD_Cleaned <- df[!(df$WEMWBS_Score == "Not applicable" | df$WEMWBS_Score == "Schedule not applicable" | df$WEMWBS_Score == "Schedule not obtained" | 
                          df$WEMWBS_Score == "Schedule not applicable" | df$WEMWBS_Score == "Refused" | df$WEMWBS_Score == "Don't know"),]  

#resetting factor levels

df_SIMD_Cleaned$SIMD <- factor(df_SIMD_Cleaned$SIMD)
df_SIMD_Cleaned$WEMWBS_Score <- factor(df_SIMD_Cleaned$WEMWBS_Score)
df_SIMD_Cleaned$WEMWBS_Score <- as.integer(as.character(df_SIMD_Cleaned$WEMWBS_Score))

#  SIMD, sex and mental well being

r <- df_SIMD_Cleaned %>%
    group_by(SIMD, Sex) %>%
  summarise(Mean = mean(WEMWBS_Score))
r

ggplot(data=r, aes(x= SIMD, y = Mean, group=Sex)) +
  geom_line(aes(colour=Sex), size = 2) +
  theme_minimal() + 
  labs(title="SIMD, sex and mental wellbeing", x="SIMD quintiles")+
  theme(axis.text = element_text(size=14, face= "bold")) +
  theme(plot.title = element_text(size = 17, face = "bold", hjust = 0.1)) +
  theme(axis.title.y.left = element_text(size=14)) +
  theme(axis.title.x.bottom = element_text(size=14)) 

# SIMD and mental well being

v <- df_SIMD_Cleaned %>%
  group_by(SIMD) %>%
  summarise(Mean = mean(WEMWBS_Score))

ggplot(data=v, aes(x= SIMD, y = Mean, group=1)) +
  geom_line(size = 2) +
  theme_minimal() + 
  labs(title="SIMD, sex and mental wellbeing", x="SIMD quintiles")+
  theme(axis.text = element_text(size=14, face= "bold")) +
  theme(plot.title = element_text(size = 17, face = "bold", hjust = 0.1)) +
  theme(axis.title.y.left = element_text(size=14)) +
  theme(axis.title.x.bottom = element_text(size=14)) 

#===========================================================================================================================

#--------------------------------------------------------------------------   Social Capital

#----------------------------------------------    Crisis_people

#Cleaning up Crisis_people

df_Social_Capital <- df[!(df$Crisis_people == "Not applicable" | df$Crisis_people == "Schedule not applicable" | df$Crisis_people == "Schedule not obtained" | 
                          df$Crisis_people == "Schedule not applicable" | df$Crisis_people == "Refused" | df$Crisis_people == "Don't know"),]  

df_Social_Capital <- df_Social_Capital[!(df_Social_Capital$Contact == "Not applicable" | df_Social_Capital$Contact == "Schedule not applicable" | df_Social_Capital$Contact == "Schedule not obtained" | 
                                           df_Social_Capital$Contact == "Schedule not applicable" | df_Social_Capital$Contact == "Refused" | df_Social_Capital$Contact == "Don't know"),]  

df_Social_Capital <- df_Social_Capital[!(df_Social_Capital$WEMWBS_Score == "Not applicable" | df_Social_Capital$WEMWBS_Score == "Schedule not applicable" | df_Social_Capital$WEMWBS_Score == "Schedule not obtained" | 
                                           df_Social_Capital$WEMWBS_Score == "Schedule not applicable" | df_Social_Capital$WEMWBS_Score == "Refused" | df_Social_Capital$WEMWBS_Score == "Don't know"),]  

#resetting factor levels

df_Social_Capital$Crisis_people <- factor(df_Social_Capital$Crisis_people)
df_Social_Capital$Contact <- factor(df_Social_Capital$Contact)

# set as integer

df_Social_Capital$WEMWBS_Score <- as.integer(as.character(df_Social_Capital$WEMWBS_Score))

# Descriptive statistics - crisis people and sex

f <- df_Social_Capital  %>%
  count(Crisis_people, Sex) %>%
  mutate(Sex_per = n) %>%
  mutate(Sex_per = ifelse(Sex == "Male", (Sex_per/789)*100, (Sex_per/1079)*100))

f$Sex_per <- round(f$Sex_per,1)

f

# number of people can turn to during a crisis and sex bar plot

ggplot(f, aes(x= Crisis_people, y=Sex_per, fill=Sex)) +
  geom_bar(stat="identity", position = position_dodge(), width = 0.5) +
  theme_minimal() +
  labs(title="Number of people adults can turn to during a crisis", x="", y= "Percent") +
  theme(axis.text = element_text(size=14, face= "bold")) +
  theme(plot.title = element_text(size = 17, face = "bold", hjust = 0.1)) +
  theme(axis.title.y.left = element_text(size=14)) +
  theme(axis.title.x.bottom = element_text(size=14)) +
  scale_fill_manual(values=c('lightblue',"blue"))

#Crisis people and Age

df_Crisis_people_weighted <- df_Social_Capital %>%
  count(Crisis_people, Age_4factors) %>%
  mutate(Percent = n) %>%
  mutate(Percent = ifelse(Age_4factors == "16-44", 
                          (Percent/445)*100,
                          ifelse(Age_4factors == "45-64",
                                 (Percent/692)*100,
                                 ifelse(Age_4factors == "65-74",
                                        (Percent/451)*100,
                                        ifelse(Age_4factors == "75+",
                                               (Percent/280)*100, NA)))))

df_Crisis_people_weighted$Percent <- round(df_Crisis_people_weighted$Percent,1)

# plot - Crisis people and Age 
  
ggplot(df_Crisis_people_weighted, aes(x=Crisis_people, y=Percent, fill=Age_4factors)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_light() +
  labs(title="How many people adults can turn to during a crisis and age", x="")+
  theme(axis.text = element_text(size=14, face= "bold")) +
  theme(plot.title = element_text(size = 17, face = "bold", hjust = 0.1)) +
  theme(axis.title.y.left = element_text(size=14)) +
  theme(axis.title.x.bottom = element_text(size=14)) +
  labs(fill = "Age groups") +
  scale_fill_brewer(palette="Blues")

#--------------------------------------------------------------------- Contact

# contact percent

df_Social_Capital %>%
  count(Contact) %>%
  mutate(Percent = n/sum(n)*100) %>%
  mutate_at(vars(Percent), round, 1)

#contact and sex percent

df_Social_Capital %>%
  count(Contact, Sex) %>%
  mutate(Sex_per = n) %>%
  mutate(Sex_per = ifelse(Sex == "Male", (Sex_per/789)*100, (Sex_per/1079)*100)) %>%
  mutate_at(vars(Sex_per), round, 1)

# How often contact people and age bar plot

df_contact_people_weighted <- df_Social_Capital %>%
  count(Contact, Age_4factors) %>%
  mutate(Percent = n) %>%
  mutate(Percent = ifelse(Age_4factors == "16-44", 
                          (Percent/445)*100,
                          ifelse(Age_4factors == "45-64",
                                 (Percent/692)*100,
                                 ifelse(Age_4factors == "65-74",
                                        (Percent/451)*100,
                                        ifelse(Age_4factors == "75+",
                                               (Percent/280)*100, NA)))))

l <- rev(c("On most days", "Once or twice a week", "Once or twice a month",
       "Less often than once a month", "Never"))

ggplot(df_contact_people_weighted, aes(x=Contact, y=Percent, fill=Age_4factors)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_light() +
  coord_flip() +
  labs(title="How often contact people and age", x="")+
  theme(axis.text = element_text(size=14, face= "bold")) +
  theme(plot.title = element_text(size = 17, face = "bold", hjust = 0.1)) +
  theme(axis.title.y.left = element_text(size=14)) +
  theme(axis.title.x.bottom = element_text(size=14)) +
  labs(fill = "Age groups") +
  scale_fill_brewer(palette="Blues")+
  scale_x_discrete(limits=l)

# ----------------------------------------------------------  Social Capital and mental well being

#--------------------------------------------    turn to in a crisis and mental well being

df_Social_Capital %>%
  group_by(Crisis_people) %>%
  summarise(Mean = mean(WEMWBS_Score))

r <- df_econ_active %>%
  group_by(Employment,Sex) %>%
  summarise(Mean = mean(WEMWBS_Score))

#--------------------------------------------    Contact and mental well being

df_Social_Capital %>%
  group_by(Contact) %>%
  summarise(Mean = mean(WEMWBS_Score))

r <- df_econ_active %>%
  group_by(Employment,Sex) %>%
  summarise(Mean = mean(WEMWBS_Score))

#------------------------------------------ composite Social Capital
#creating a composite variable of Crisis people and contact

df_Social_Capital_Composite <- df_Social_Capital
df_Social_Capital_Composite$Crisis_people <- as.integer(df_Social_Capital_Composite$Crisis_people)
df_Social_Capital_Composite$Crisis_people
df_Social_Capital_Composite$Contact <- as.integer(df_Social_Capital_Composite$Contact)
df_Social_Capital_Composite$Contact
df_Social_Capital_Composite$Composite <- (df_Social_Capital_Composite$Crisis_people + df_Social_Capital_Composite$Contact)
df_Social_Capital_Composite$Composite

df_Social_Capital_Composite %>%
  group_by(Composite) %>%
  summarise(Mean = mean(WEMWBS_Score))

# ------------------------------------------------------------------------- Health 
# ------------------------------------------------------- Gen_health

df %>%
  count(Gen_health) %>%
  mutate(Percent = n/sum(n)*100) %>%
  mutate_at(vars(Percent), round, 1)

#sex

df %>%
  count(Gen_health, Sex) %>%
  mutate(Sex_per = n) %>%
  mutate(Sex_per = ifelse(Sex == "Male", (Sex_per/813)*100, (Sex_per/1107)*100)) %>%
  mutate_at(vars(Sex_per), round, 1)

#age

gen_health_weighted <- df %>%
  count(Gen_health, Age_4factors) %>%
  mutate(Percent = n) %>%
  mutate(Percent = ifelse(Age_4factors == "16-44", 
                          (Percent/448)*100,
                          ifelse(Age_4factors == "45-64",
                                 (Percent/703)*100,
                                 ifelse(Age_4factors == "65-74",
                                        (Percent/466)*100,
                                        ifelse(Age_4factors == "75+",
                                               (Percent/303)*100, NA)))))

# age plot

ggplot(gen_health_weighted, aes(x=Gen_health, y=Percent, fill=Age_4factors)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_light() +
  labs(title="General health and age", x="")+
  theme(axis.text = element_text(size=14, face= "bold")) +
  theme(plot.title = element_text(size = 17, face = "bold", hjust = 0.1)) +
  theme(axis.title.y.left = element_text(size=14)) +
  theme(axis.title.x.bottom = element_text(size=14)) +
  labs(fill = "Age groups") +
  scale_fill_brewer(palette="Blues")

#-------------------------------------------------------------------- Long_illness

df %>%
  count(Long_illness) %>%
  mutate(Percent = n/sum(n)*100) %>%
  mutate_at(vars(Percent), round, 1)

# Sex

df %>%
  count(Long_illness, Sex) %>%
  mutate(Sex_per = n) %>%
  mutate(Sex_per = ifelse(Sex == "Male", (Sex_per/813)*100, (Sex_per/1107)*100)) %>%
  mutate_at(vars(Sex_per), round, 1)

# Age


Long_illness_weighted <- df %>%
  count(Long_illness, Age_4factors) %>%
  mutate(Percent = n) %>%
  mutate(Percent = ifelse(Age_4factors == "16-44", 
                          (Percent/448)*100,
                          ifelse(Age_4factors == "45-64",
                                 (Percent/703)*100,
                                 ifelse(Age_4factors == "65-74",
                                        (Percent/466)*100,
                                        ifelse(Age_4factors == "75+",
                                               (Percent/303)*100, NA))))) %>%
  mutate_at(vars(Percent), round, 1)


Long_illness_weighted <- Long_illness_weighted[!(Long_illness_weighted$Long_illness == "Don't know"),] 
Long_illness_weighted

# General health and age plot

ggplot(Long_illness_weighted, aes(x=Long_illness, y=Percent, fill=Age_4factors)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_light() +
  labs(title="General health and age", x="")+
  theme(axis.text = element_text(size=14, face= "bold")) +
  theme(plot.title = element_text(size = 17, face = "bold", hjust = 0.1)) +
  theme(axis.title.y.left = element_text(size=14)) +
  theme(axis.title.x.bottom = element_text(size=14)) +
  labs(fill = "Age groups") +
  scale_fill_brewer(palette="Blues")

# -------------------------------------------------- General Health and mental well being

df_mental_wellbeing <- df[!(df$WEMWBS_Score == "Not applicable" | df$WEMWBS_Score == "Schedule not applicable" | df$WEMWBS_Score == "Schedule not obtained" | 
                            df$WEMWBS_Score == "Schedule not applicable" | df$WEMWBS_Score == "Refused" | df$WEMWBS_Score == "Don't know"),]  

df_mental_wellbeing$WEMWBS_Score <- as.integer(df_mental_wellbeing$WEMWBS_Score)

r <- df_mental_wellbeing %>%
  group_by(Gen_health) %>%
  summarize(Mean = mean(WEMWBS_Score))
  
r

ggplot(r, aes(x=reorder(Gen_health, desc(Gen_health)), y = Mean, group = 1, )) +
  geom_line(color = "lightblue", size = 1.2) +
  geom_point(color = "darkblue", size = 3) +
  theme_minimal() + 
  labs(title="General health and mental wellbeing", x="Self-rate general health", y = "General wellbeing")+
  theme(axis.text = element_text(size=14, face= "bold")) +
  theme(plot.title = element_text(size = 17, face = "bold", hjust = 0.1)) +
  theme(axis.title.y.left = element_text(size=14)) +
  theme(axis.title.x.bottom = element_text(size=14)) 


# -------------------------------------------------- long illness and mental well being


r <- df_mental_wellbeing %>%
  group_by(Long_illness) %>%
  summarize(Mean = mean(WEMWBS_Score))

r
