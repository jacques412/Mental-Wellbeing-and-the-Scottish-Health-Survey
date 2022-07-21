

# Note that there are conflicts between dplyr and FSA. If the code does not run as expected, restart R without FSA.

# -------------------------------------------- Libraries

#install.packages("ggpubr")
#install.packages("FSA")
library(ggplot2)
library(readstata13)
library(dplyr)
#library("ggpubr")
#library(FSA)

#-------------------------------------------- opening the data set

#set the working directory

setwd("C:\\Users\\jarvi\\OneDrive\\Desktop\\The Scottish Health Survey\\Data\\UKDA-8834-stata\\stata\\stata13")

# reading the dataframe
df1 <- read.dta13("data.dta", convert.factors = T ,generate.factors = T)

# Changing where images are saved

setwd("C:\\Users\\jarvi\\OneDrive\\Desktop\\The Scottish Health Survey\\Images")

#--------------------------------------------- data wrangling

#Selecting relevant variables

df <- df1 %>%
  select(Sex, age90, ag16g10, ag16g4, EmpNow, SIMD20_SGa,
         Pcrisis2, Contact, GenHelf, longill12, WEMWBS_t20)

# Rename columns 

df <- df %>%
  rename(Age = age90, Age_10factors = ag16g10, Age_4factors = ag16g4,
         Employment = EmpNow, SIMD = SIMD20_SGa, Crisis_people = Pcrisis2,
         Gen_health = GenHelf, Long_illness = longill12, WEMWBS_Score = WEMWBS_t20)

# Dropping missing data from mental well-being variable

df <- df[!(df$WEMWBS_Score == "Refused" | df$WEMWBS_Score == "Don't know" | df$WEMWBS_Score == "Schedule not obtained" | 
                       df$WEMWBS_Score == "Schedule not applicable" | df$WEMWBS_Score == "Not applicable"),]

df$WEMWBS_Score <- as.integer(as.character(df$WEMWBS_Score))

#=======================================================================================================================

#----------------------------------------------- Male and female mental well being (all adults)

# Mean mental well being of males and females - Mann-Whitney U test

df_wellbeing <- df[!(df$Sex == "Refused" | df$Sex == "Don't know" | df$Sex == "Schedule not obtained" | 
             df$Sex == "Schedule not applicable" | df$Sex == "Not applicable"),]

wilcox.test(WEMWBS_Score ~ Sex, data=df_wellbeing) 

#=======================================================================================================================

#--------------------------------------------------------------- Mental well being and age (all adults) Spearman's rho

df_age <- df[!(df$Age_4factors == "Refused" | df$Age_4factors == "Don't know" | df$Age_4factors == "Schedule not obtained" | 
                       df$Age_4factors == "Schedule not applicable" | df$Age_4factors == "Not applicable"),]

#Create  dummy for age factors
df_age$age_dummy <- df_age$Age_4factors
df_age$age_dummy <- recode_factor(df_age$age_dummy, "16-44" = 1, "45-64" = 2, "65-74" = 3, "75+" = 4)
df_age$age_dummy <- as.integer(df_age$age_dummy)
df_age$WEMWBS_Score <- as.integer(df_age$WEMWBS_Score)

#Spearman's rho

corr <- cor.test(x=df_age$age_dummy, y=df_age$WEMWBS_Score, method = 'spearman')
corr

# Mental well being and age Kruskal-Wallis test in R

kruskal.test(WEMWBS_Score ~ Age_4factors, data = df_age)

# Pairwise wilcoxon rank sum test test

pairwise.wilcox.test(df_age$WEMWBS_Score, df_age$Age_4factors, p.adjust.method = "BH")

#=========================================================================================================================

#------------------------------------------------------------------------- Employment

# ----------------------------------- The economically active 

#cleaning up Employment variable

df_econ_active <- df[!(df$Employment == "Refused" | df$Employment == "Don't know" | df$Employment== "Schedule not obtained" | 
                         df$Employment== "Schedule not applicable" | df$Employment== "Not applicable" |
                         df$Employment == "Permanently sick or disabled" | df$Employment == "Looking after home or family" |
                        df$Employment == "In education at school/college/university" | df$Employment == "Retired" |
                         df$Employment == "Unemployed and not seeking work" |
                         df$Employment == "In unpaid/voluntary work"),]

#The Economically active: Employment categories and mental wellbeing

#boxplot of EA and mental wellbeing

ggplot(df_econ_active, aes(x = Employment, y = WEMWBS_Score, group = Employment, color=Employment)) +
  geom_boxplot(lwd = 1.2) +
  theme_minimal() +
  labs(title="Employment categories and mental wellbeing of the economically active", x="", y= "Mental Wellbeing scores") +
  theme(axis.text = element_text(size=14, face= "bold")) +
  theme(plot.title = element_text(size = 17, face = "bold", hjust = 0.1)) +
  theme(axis.title.y.left = element_text(size=14)) +
  theme(axis.title.x.bottom = element_text(size=14))+
  theme(legend.position="none") +
  coord_flip()

# ----------------------------------- The non-economically active 

df_non_econ <- df[(df$Employment == "Permanently sick or disabled" | df$Employment == "Looking after home or family" | 
                         df$Employment == "In education at school/college/university" | df$Employment == "Retired" |
                         df$Employment == "Unemployed and not seeking work"),]

# boxplot of NEA and mental wellbeing

ggplot(df_non_econ, aes(x = Employment, y = WEMWBS_Score, group = Employment, color=Employment)) +
  geom_boxplot(lwd = 1.2) +
  theme_minimal() +
  labs(title="Employment categories and mental wellbeing of the non-economically active", x="", y= "Mental Wellbeing scores") +
  theme(axis.text = element_text(size=14, face= "bold")) +
  theme(plot.title = element_text(size = 17, face = "bold", hjust = 0.1)) +
  theme(axis.title.y.left = element_text(size=14)) +
  theme(axis.title.x.bottom = element_text(size=14))+
  theme(legend.position="none") +
  coord_flip()

#Non-economically active, age and mental wellbeing: Spearman's rho

df_non_econ$dummy <- as.integer(recode_factor(df_non_econ$Age_4factors, "16-44" = 1, "45-64" = 2, "65-74" = 3, "75+" = 4))

corr <- cor.test(x=df_non_econ$dummy, y=df_non_econ$WEMWBS_Score, method = 'spearman')

corr

#=========================================================================================================================

#---------------------------------------------------------------------------- Mental wellbeing and employment

#---------------------------- Employed vs self-employed and unemployed

# Remove the NEA

df_wellbeing_Emp_vs_all <- df[!(df$Employment == "Permanently sick or disabled" | df$Employment == "Looking after home or family" | df$Employment == "In education at school/college/university" |
                                                       df$Employment == "Retired" | df$Employment == "In unpaid/voluntary work"),]

#creating a dummy for Employed vs self and unemployed

df_wellbeing_Emp_vs_all <- df_wellbeing_Emp_vs_all %>%
  mutate(dummy = ifelse(Employment == "Employed and working full time", 1,
                        ifelse(Employment == "Employed and working part time", 1,
                               ifelse(Employment == "Employed but on furlough", 1,
                                      ifelse(Employment == "Employed but on paid leave (not including furlough)", 1, 
                                             ifelse(Employment == "Employed and on unpaid leave", 1,
                                                    ifelse(Employment == "Employed but on paid leave (not including furlough)", 1, 0)))))))

# Employed vs self and unemployed statistical test

wilcox.test(WEMWBS_Score ~ dummy, data=df_wellbeing_Emp_vs_all) 

df_wellbeing_Emp_vs_all %>%
  filter(dummy == 0) %>%
  summarise(Mean = mean(WEMWBS_Score))

#--------------------------- Employed vs self employed

df_emp_vs_self <- df_wellbeing_Emp_vs_all[!(df_wellbeing_Emp_vs_all$Employment == "Unemployed and seeking work" | 
                                              df_wellbeing_Emp_vs_all$Employment == "Unemployed and not seeking work"),]

#creating a dummy for employed and self employed 

df_emp_vs_self <- df_emp_vs_self %>%
  mutate(dummy = ifelse(Employment == "Employed and working full time", 1,
                        ifelse(Employment == "Employed but on furlough", 1,
                               ifelse(Employment == "In unpaid/voluntary work", 1,
                               ifelse(Employment == "Employed and working part time", 1,
                               ifelse(Employment == "Employed but on paid leave (not including furlough)", 1, 
                                      ifelse(Employment == "Employed and on unpaid leave", 1,
                                             ifelse(Employment == "Employed but on paid leave (not including furlough)", 1, 0))))))))

# Employed vs self-employed Mann Whitney test

wilcox.test(WEMWBS_Score ~ dummy, data=df_emp_vs_self) 

#------------------------ employed vs unemployed

df_emp_vs_un  <- df_wellbeing_Emp_vs_all[!(df_wellbeing_Emp_vs_all$Employment == "Self-employed and currently working" | 
                                              df_wellbeing_Emp_vs_all$Employment == "Self-employed but not currently working and receiving government support" |
                                             df_wellbeing_Emp_vs_all$Employment == "Self-employed but not currently working and not receiving government support"),]

df_emp_vs_un <- df_emp_vs_un %>%
  mutate(dummy = ifelse(Employment == "Employed and working full time", 1,
                        ifelse(Employment == "Employed and working part time", 1,
                               ifelse(Employment == "Employed but on furlough", 1,
                                      ifelse(Employment == "In unpaid/voluntary work", 1,
                                             ifelse(Employment == "Employed and on unpaid leave", 1,
                                                           ifelse(Employment == "Employed but on paid leave (not including furlough)", 1, 0)))))))

# checking the shape of the distributions

df_empl <- df_emp_vs_un %>%
  filter(dummy == 1)

df_unempl <- df_emp_vs_un %>%
  filter(dummy == 0)

ggplot(df_empl, aes(x=WEMWBS_Score)) +
  geom_histogram(fill="black")

ggplot(df_unempl, aes(x=WEMWBS_Score)) +
  geom_histogram(fill="black")

# Employed vs unemployed Mann Whitney test

wilcox.test(WEMWBS_Score ~ dummy, data=df_emp_vs_un) 

#----------------------- self-employed vs unemployed

df_self_vs_un  <- df[!(df$Employment == "Refused" | df$Employment == "Don't know" |
                                    df$Employment == "Employed and working part time" | df$Employment == "Schedule not obtained" |
                                    df$Employment == "Employed but on furlough" | df$Employment == "Employed but on paid leave (not including furlough)" |
                                    df$Employment == "Not applicable" | df$Employment == "Employed and working full time" |
                                    df$Employment == "Employed and on unpaid leave" | df$Employment == "In unpaid/voluntary work" |
                                    df$Employment == "Employed and on unpaid leave" | df$Employment == "Permanently sick or disabled" |
                                    df$Employment == "Looking after home or family" | df$Employment == "In education at school/college/university" |
                                    df$Employment == "Retired"),]

df_self_vs_un <- df_self_vs_un %>%
  mutate(dummy = ifelse(Employment == "Self-employed and currently working", 1,
                        ifelse(Employment == "Self-employed but not currently working and receiving government support", 1,
                               ifelse(Employment == "Self-employed but not currently working and not receiving government support", 1, 0))))

#Self-employed vs unemployed - Mann-Whitney Wilcoxon test

wilcox.test(WEMWBS_Score ~ dummy, data=df_self_vs_un) 

#=====================================================================================================================

#---------------------------------------------------------------SIMD and mental well being

df_SIMD <- df[!(df$SIMD == "Refused" | df$SIMD == "Don't know" | df$SIMD == "Schedule not obtained" | 
                 df$SIMD == "Schedule not applicable" | df$SIMD == "Not applicable"),]

# Recoding SIMD as integer

df_SIMD$dummy <- as.integer(recode_factor(df_SIMD$SIMD, "Most deprived" = 1, "2" = 2, "3" = 3, "4" = 4, "Least deprived" = 5))

# Boxplot of SIMD and mental wellbeing

ggplot(df_SIMD, aes(x = SIMD, y = WEMWBS_Score, color = SIMD)) +
  geom_boxplot(lwd = 1.2) +
  theme_minimal() +
  labs(title="Scottish Index of Multiple Deprivation vs mental wellbeing", x="SIMD", y= "Mental Wellbeing scores") +
  theme(axis.text = element_text(size=14, face= "bold")) +
  theme(plot.title = element_text(size = 17, face = "bold", hjust = 0.1)) +
  theme(axis.title.y.left = element_text(size=14)) +
  theme(axis.title.x.bottom = element_text(size=14))+
  theme(legend.position="none") 

# SIMD and mental wellbeing Spearman's Rho

corr <- cor.test(x=df_SIMD$dummy, y=df_SIMD$WEMWBS_Score, method = 'spearman')

corr

# SIMD and mental wellbeing: Kruskal-Wallis H Test

kruskal.test(WEMWBS_Score ~ dummy, data = df_SIMD)

# Multiple pairwise-comparison between groups

pairwise.wilcox.test(df_SIMD$WEMWBS_Score, df_SIMD$dummy,
                     p.adjust.method = "bonferroni", exact=F)

#===========================================================================================================================

#----------------------------------------------------------------------------------------Social capital

# number of people adults can turn to in a crisis and mental wellbeing

df_socialcapital <- df[!(df$Crisis_people == "Refused" | df$Crisis_people == "Don't know" | df$Crisis_people == "Schedule not obtained" | 
             df$Crisis_people == "Schedule not applicable" | df$Crisis_people == "Not applicable"),]

df_socialcapital <- df_socialcapital[!(df_socialcapital$Sex == "Refused" | df_socialcapital$Sex == "Don't know" | df_socialcapital$Sex == "Schedule not obtained" | 
                           df_socialcapital$Sex == "Schedule not applicable" | df_socialcapital$Sex == "Not applicable"),]

#create a dummy variable for people in crisis

df_socialcapital$crisis_dummy <- as.integer(recode_factor(df_socialcapital$Crisis_people, "0 people" = 1, "1-5 people" = 2, "6-10 people" = 3, "11-14 people" = 4, "15+ people" = 5))

# people can turn to in a crisis and sex: Mann-Whitney test

wilcox.test(df_socialcapital$crisis_dummy ~ df_socialcapital$Sex)

# Crisis people and age: Spearman's rho  

df_socialcapital <- df_socialcapital[!(df_socialcapital$Age == "Refused" | df_socialcapital$Age == "Don't know" | df_socialcapital$Age == "Schedule not obtained" | 
                           df_socialcapital$Age == "Schedule not applicable" | df_socialcapital$Age == "Not applicable"),]

df_socialcapital$Age <- as.integer(df_socialcapital$Age)

corr <- cor.test(x=df_socialcapital$crisis_dummy, y=df_socialcapital$Age, method = 'spearman')
corr


# Crisis people and age: Kruskal-Wallis test

kruskal.test(crisis_dummy ~ Age_4factors, data = df_socialcapital)

# Multiple pairwise-comparison between groups

pairwise.wilcox.test(df_socialcapital$crisis_dummy, df_socialcapital$Age_4factors,
                     p.adjust.method = "BH")
 
#---------------------------------------------------------------------------------- contact

#contact and sex

df_contact <- df[!(df$Contact == "Refused" | df$Contact == "Don't know" | df$Contact == "Schedule not obtained" | 
                           df$Contact == "Schedule not applicable" | df$Contact == "Not applicable"),]

df_contact$dummy <- as.integer(recode_factor(df_contact$Contact, "On most days" = 1, "Once or twice a week" = 2, "Once or twice a month" = 3, "Less often than once a month" = 4, "Never" = 5))

# Contact and sex: Mann-Whitney U test

wilcox.test(df_contact$dummy ~ df_contact$Sex)

# contact and age

df_contact %>%
  count(Contact, Age_4factors) %>%
  mutate(Percent = n) %>%
  mutate(Percent = ifelse(Age_4factors == "16-44", (Percent/446)*100, 
                          ifelse(Age_4factors == "45-64", (Percent/692)*100,
                                 ifelse(Age_4factors == "65-74", (Percent/453)*100,
                                        ifelse(Age_4factors == "75+", (Percent/280)*100, NA))))) %>%
  mutate(Percent = round(Percent,1))


# Contact and age: Kruskal-Wallis test

kruskal.test(dummy ~ Age_4factors, data = df_contact)

# Multiple pairwise-comparison between groups

pairwise.wilcox.test(df_contact$dummy, df_contact$Age_4factors,
                     p.adjust.method = "BH")

#----------------------------------------------------------------- Social capital and mental well being

# Crisis and mental wellbeing boxplot

ggplot(df_socialcapital, aes(x = Crisis_people, y = WEMWBS_Score, color = Crisis_people, group=Crisis_people)) +
  geom_boxplot(lwd = 1.2) +
  theme_minimal() +
  labs(title="Number of people adults can turn to during a crisis and mental wellbeing", x="", y= "Mental Wellbeing") +
  theme(axis.text = element_text(size=14, face= "bold")) +
  theme(plot.title = element_text(size = 17, face = "bold", hjust = 0.1)) +
  theme(axis.title.y.left = element_text(size=14)) +
  theme(axis.title.x.bottom = element_text(size=14))+
  theme(legend.position="none") 

# Crisis and mental well being line chart

s <- df_socialcapital %>%
  group_by(Crisis_people) %>%
  summarise(M = mean(WEMWBS_Score))

ggplot(s, aes(x = Crisis_people, y = M, group = 1)) +
  geom_line(color="lightblue", size=2) +
  geom_point(color="darkblue", size=3) +
  theme_minimal() +
  labs(title="Number of people adults can turn to during a crisis and mental wellbeing", x="", y= "Mental Wellbeing") +
  theme(axis.text = element_text(size=14, face= "bold")) +
  theme(plot.title = element_text(size = 17, face = "bold", hjust = 0.1)) +
  theme(axis.title.y.left = element_text(size=14)) +
  theme(axis.title.x.bottom = element_text(size=14))+
  theme(legend.position="none") 

# Contact and mental wellbeing boxplot

ggplot(df_socialcapital, aes(x = reorder(Contact, WEMWBS_Score), y = WEMWBS_Score, color = Contact)) +
  geom_boxplot(lwd = 1.2) +
  theme_minimal() +
  labs(title="Frequency adults contact friends and family and mental wellbeing", x="", y= "Mental Wellbeing scores") +
  theme(axis.text = element_text(size=14, face= "bold")) +
  theme(plot.title = element_text(size = 17, face = "bold", hjust = 0.1)) +
  theme(axis.title.y.left = element_text(size=14)) +
  theme(axis.title.x.bottom = element_text(size=10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position="none")

# Contact and mental wellbeing line chart

t <- df_socialcapital %>%
  group_by(Contact) %>%
  summarise(M = mean(WEMWBS_Score))

ggplot(t, aes(x = rev(Contact), y = M, group = 1)) +
  geom_line(color="lightblue", size=2) +
  geom_point(color="darkblue", size=3) +
  theme_minimal() +
  labs(title="Frequency adults contact friends and family and mental wellbeing", x="", y= "Mental Wellbeing") +
  theme(axis.text = element_text(size=14, face= "bold")) +
  theme(plot.title = element_text(size = 17, face = "bold", hjust = 0.1)) +
  theme(axis.title.y.left = element_text(size=14)) +
  theme(axis.title.x.bottom = element_text(size=14))+
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Crisis people and mental wellbeing spearman's rho

corr <- cor.test(x=df_socialcapital$crisis_dummy, y=df_socialcapital$WEMWBS_Score, method = 'spearman')
corr

#contact and mental wellbeing spearman's rho

corr <- cor.test(x=df_contact$dummy, y=df_contact$WEMWBS_Score, method = 'spearman')
corr

#==========================================================================================================================
#------------------------------------------------------------------------------------ Health

df_health <- df[!(df$Gen_health == "Refused" | df$Gen_health == "Don't know" | df$Gen_health == "Schedule not obtained" | 
                     df$Gen_health == "Schedule not applicable" | df$Gen_health == "Not applicable"),]

df_health <- df[!(df$Long_illness == "Refused" | df$Long_illness == "Don't know" | df$Long_illness == "Schedule not obtained" | 
                    df$Long_illness == "Schedule not applicable" | df$Long_illness == "Not applicable"),]

df_health$Long_illness <- droplevels(df_health$Long_illness)
df_health$Sex <- droplevels(df_health$Sex)

#create a dummy variable of self rated health

df_health$gen_dummy <- as.integer(recode_factor(df_health$Gen_health, "Very good" = 1, "good" = 2, "Fair" = 3, "Bad" = 4, "Very bad" = 5))

# General health and sex: Mann whitney test

wilcox.test(gen_dummy ~ Sex, data=df_health) 

# Long illness and sex: Chi-square test of independence

table(df_health$Sex, df_health$Long_illness)
  
chisq.test(df_health$Sex, df_health$Long_illness)

#------------------------------------------------------------- self rated health and mental well being 

# self rated health and mental wellbeing Spearman's rho

corr <- cor.test(x=df_health$gen_dummy, y=df_health$WEMWBS_Score, method = 'spearman')

corr

#=========================================================================================================================

# Multiple regression

