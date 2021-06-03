library(dplyr)
library(tidyverse)
library(tidyr)
library(GGally)
library(hrbrthemes)
library(broom)
library(reshape2)
library(ggpubr)
library(RColorBrewer)
library(waffle)

Df1 <- read.csv("EB071.20210402T230410.csv", header = TRUE, sep = ",")

#Pre-processing
Df1[!complete.cases(Df1),]

#With 2011 Census Year removed
Df2<-Df1[!(Df1$CensusYear =="2011"),]

#With 2016 Census Year removed

Df3 <- Df1[!(Df1$CensusYear =="2016"),]

##### 2016 ANALYSIS - Employment #####

#Removing UNIT column and Statistic Column
Df2<- select(Df2, -c("UNIT", "ï..Statistic"))

#Analyzing Only State
Df2<-Df2[(Df2$County.and.City =="State"),]

#Removing State Column and Census Year Column
Df2<-select(Df2, -c("County.and.City", "CensusYear"))

#Removing rows which tell total

#"All socio-economic groups'
Df2 <- Df2 %>% filter(Socio.Economic.Group != ("All socio-economic groups"))

#'All ages'
Df2 <- Df2 %>% filter(Age.Group != ("All ages"))

#Removing rows for unemployed people and all people not in labour force
Df2 <- Df2 %>% filter(Labour.Force != ("All unemployed persons"),
                      Labour.Force != ("All persons aged 15 years and over not in labour force"),
                      Labour.Force != ("All persons aged 15 years and over"))  

#Removing Labour Force column as not needed
Df2<- select(Df2, -c("Labour.Force"))

Df2 <- Df2 %>% mutate (Socio.Economic.Group = str_sub(Socio.Economic.Group, 4, -1)) %>%
  rename(Counts = VALUE,
         Age_Group = Age.Group,
         Socio_Economic_Group = Socio.Economic.Group)

#Renaming variable to 'others'
Df2$Socio_Economic_Group[Df2$Socio_Economic_Group=="All others gainfully occupied and unknown"] <- "Others"

#By Socio Economic Group - 2016
ggplot(Df2) + geom_col(aes(Age_Group, Counts, fill = Socio_Economic_Group)) +
  facet_wrap(~Socio_Economic_Group, scales = "free_x") +
  coord_flip()+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90)) +
  xlab("Age Group") + ylab("Socio Economic Group") +
  ggtitle("Age Group Composition by Socio Economic Group - 2016") +
  guides(fill=guide_legend(title="Socio Economic Group"))

#By Age - 2016
ggplot(Df2) + geom_col(aes(Socio_Economic_Group, Counts, fill = Age_Group)) +
  facet_wrap(~Age_Group, scales = "free_x") +
  coord_flip()+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90)) +
  ylab("Age Group") + xlab("Socio Economic Group") +
  ggtitle("Socio Economic Group Composition by Age Group - 2016")

#Age Total
ggplot(Df2) + geom_col(aes(Age_Group, Counts), fill = "darkblue") + theme_bw() +
  theme(axis.text.x = element_text(angle=90)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  xlab("Age Group") + ggtitle("Distribution of Persons Employed by Age Group")

#Box Plot - 2016
bp16 <- ggplot(Df2, aes(Age_Group, Counts, fill = Age_Group)) + geom_boxplot() + theme_bw()+
  stat_boxplot(geom ='errorbar') + stat_summary(fun=mean, geom="line", aes(group=1))  + 
  stat_summary(fun=mean, geom="point") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  ggtitle("2016") +
  theme(axis.text.x = element_text(angle=90), plot.title = element_text(hjust = 0.5), 
        axis.title.x=element_blank(), axis.title.y = element_blank()) +
  guides(fill=guide_legend(title="Age Group"))

#Creating Further Charts - 1
Tibble1 <- Df2 %>%
  group_by(Socio_Economic_Group) %>%
  summarise(Total=sum(Counts)) %>%
  arrange(Total, .by_group = TRUE)

Tibble1$Socio_Economic_Group <- factor(Tibble1$Socio_Economic_Group, levels = Tibble1$Socio_Economic_Group)

Tibble1 <- Tibble1 %>%
  arrange(desc(Socio_Economic_Group)) %>%
  mutate(prop = round(Total*100/sum(Total), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)

mycolors = c(brewer.pal(name="Dark2", n = 7), brewer.pal(name="Paired", n = 4))

#Pie Chart
ggplot(Tibble1, aes(x = "", y = prop, fill = Socio_Economic_Group)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  coord_polar("y", start = 0) +
  scale_color_manual(values = mycolors) +
  theme_void()

#Dot Chart - 2016 Groups
dot1 <- ggplot(Tibble1, aes(Socio_Economic_Group, prop)) +
  geom_linerange(
    aes(x = Socio_Economic_Group, ymin = 0, ymax = prop), 
    color = "lightgray", size = 1.5
  ) +
  geom_point(aes(color = Socio_Economic_Group), size = 2) +
  scale_color_manual(values = mycolors)+
  theme_pubclean() +
  ggtitle("2016") +
  theme(axis.text.x = element_text(angle=90), plot.title = element_text(hjust = 0.5), 
        axis.title.x=element_blank(), axis.title.y = element_blank()) +
  scale_color_discrete(name="Socio Economic Group")

#Creating Further charts - 2

Tibble2 <- Df2 %>%
  group_by(Age_Group) %>%
  summarise(Total=sum(Counts)) %>%
  arrange(Total, .by_group = TRUE)

Tibble2 <- Tibble2 %>%
  arrange(desc(Age_Group)) %>%
  mutate(prop = round(Total*100/sum(Total), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)

#Pie Chart
ggplot(Tibble2, aes(x = "", y = prop, fill = Age_Group)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  coord_polar("y", start = 0) +
  scale_color_manual(values = mycolors) +
  theme_void()

#Dot Chart - 2016 AgeGroup
dot2 <- ggplot(Tibble2, aes(Age_Group, prop)) +
  geom_linerange(
    aes(x = Age_Group, ymin = 0, ymax = prop), 
    color = "lightgray", size = 1.5
  ) +
  geom_point(aes(color = Age_Group), size = 2) +
  scale_color_manual(values = mycolors)+
  theme_pubclean() +
  theme(axis.text.x = element_text(angle=90), plot.title = element_text(hjust = 0.5), 
        axis.title.x=element_blank(), axis.title.y = element_blank()) +
  ggtitle("2016") +
  scale_color_discrete(name="Age Group")

##### 2011 ANALYSIS - Employment #####

Df3<- select(Df3, -c("UNIT", "ï..Statistic"))

#Analyzing Only State
Df3<-Df3[(Df3$County.and.City =="State"),]

#Removing State Column and Census Year Column
Df3<-select(Df3, -c("County.and.City", "CensusYear"))

#Removing rows which tell total

#"All socio-economic groups'
Df3 <- Df3 %>% filter(Socio.Economic.Group != ("All socio-economic groups"))

#'All ages'
Df3 <- Df3 %>% filter(Age.Group != ("All ages"))

#Removing rows for unemployed people and all people not in labour force
Df3 <- Df3 %>% filter(Labour.Force != ("All unemployed persons"),
                      Labour.Force != ("All persons aged 15 years and over not in labour force"),
                      Labour.Force != ("All persons aged 15 years and over"))  

#Removing Labour Force column as not needed
Df3<- select(Df3, -c("Labour.Force"))

Df3 <- Df3 %>% mutate (Socio.Economic.Group = str_sub(Socio.Economic.Group, 4, -1)) %>%
  rename(Counts = VALUE,
         Age_Group = Age.Group,
         Socio_Economic_Group = Socio.Economic.Group)

Df3$Socio_Economic_Group[Df3$Socio_Economic_Group=="All others gainfully occupied and unknown"] <- "Others"

#By Socio Economic Group
ggplot(Df3) + geom_col(aes(Age_Group, Counts, fill = Socio_Economic_Group)) +
  facet_wrap(~Socio_Economic_Group, scales = "free_x") +
  coord_flip()+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90)) +
  ylab("Age Group") + xlab("Socio Economic Group") +
  ggtitle("Socio Economic Group Composition by Age Group - 2011") +
  guides(fill=guide_legend(title="Socio Economic Group"))

#By Age
ggplot(Df3) + geom_col(aes(Socio_Economic_Group, Counts, fill = Age_Group)) +
  facet_wrap(~Age_Group, scales = "free_x") +
  coord_flip()+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90)) +
  ylab("Age Group") + xlab("Socio Economic Group") +
  ggtitle("Socio Economic Group Composition by Age Group - 2011") +
  guides(fill=guide_legend(title="Age Group"))

#Age Total
ggplot(Df3) + geom_col(aes(Age_Group, Counts), fill = "darkblue") + theme_bw() +
  theme(axis.text.x = element_text(angle=90)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  xlab("Age Group") + ggtitle("Distribution of Persons Employed by Age Group")

#Box Plot - 2011
bp11 <- ggplot(Df3, aes(Age_Group, Counts, fill = Age_Group)) + geom_boxplot() + theme_bw()+
  stat_boxplot(geom ='errorbar') + stat_summary(fun=mean, geom="line", aes(group=1))  + 
  stat_summary(fun=mean, geom="point") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  ggtitle("2011") +
  theme(axis.text.x = element_text(angle=90), plot.title = element_text(hjust = 0.5), 
        axis.title.x=element_blank(), axis.title.y = element_blank()) +
  guides(fill=guide_legend(title="Age Group"))

#Creating Further Charts - 1
Tibble3 <- Df3 %>%
  group_by(Socio_Economic_Group) %>%
  summarise(Total=sum(Counts)) %>%
  arrange(Total, .by_group = TRUE)

Tibble3$Socio_Economic_Group <- factor(Tibble3$Socio_Economic_Group, levels = Tibble3$Socio_Economic_Group)

Tibble3 <- Tibble3 %>%
  arrange(desc(Socio_Economic_Group)) %>%
  mutate(prop = round(Total*100/sum(Total), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)

mycolors = c(brewer.pal(name="Dark2", n = 7), brewer.pal(name="Paired", n = 4))

#Pie Chart
ggplot(Tibble3, aes(x = "", y = prop, fill = Socio_Economic_Group)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  coord_polar("y", start = 0) +
  scale_color_manual(values = mycolors) +
  theme_void()

#Dot Chart
dot3 <- ggplot(Tibble3, aes(Socio_Economic_Group, prop)) +
  geom_linerange(
    aes(x = Socio_Economic_Group, ymin = 0, ymax = prop), 
    color = "lightgray", size = 1.5
  ) +
  geom_point(aes(color = Socio_Economic_Group), size = 2) +
  scale_color_manual(values = mycolors)+
  theme_pubclean() +
  ggtitle("2011") +
  theme(axis.text.x = element_text(angle=90), plot.title = element_text(hjust = 0.5), 
        axis.title.x=element_blank(), axis.title.y = element_blank()) +
  scale_color_discrete(name="Socio Economic Group")

#Creating Further charts - 2

Tibble4 <- Df3 %>%
  group_by(Age_Group) %>%
  summarise(Total=sum(Counts)) %>%
  arrange(Total, .by_group = TRUE)

Tibble4 <- Tibble4 %>%
  arrange(desc(Age_Group)) %>%
  mutate(prop = round(Total*100/sum(Total), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)

#Pie Chart
ggplot(Tibble4, aes(x = "", y = prop, fill = Age_Group)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  coord_polar("y", start = 0) +
  scale_color_manual(values = mycolors) +
  theme_void()

#Dot Chart
do4 <- ggplot(Tibble4, aes(Age_Group, prop)) +
  geom_linerange(
    aes(x = Age_Group, ymin = 0, ymax = prop), 
    color = "lightgray", size = 1.5
  ) +
  geom_point(aes(color = Age_Group), size = 2) +
  scale_color_manual(values = mycolors)+
  theme_pubclean() +
  scale_color_discrete(name="Age Group") +
  ggtitle("2011") +
  theme(axis.text.x = element_text(angle=90), plot.title = element_text(hjust = 0.5), 
        axis.title.x=element_blank(), axis.title.y = element_blank())

#Boxplots
bparranged <- ggarrange(bp11, bp16, common.legend = TRUE, align = c("h"),ncol = 2, nrow = 1)
bparranged <- annotate_figure(bparranged,
                top = text_grob("Employment by Age Category", color = "black", face = "bold", size = 14, hjust = 0.5),
                bottom = text_grob("Age Groups", color = "black",
                                   hjust = 0.5, face = "italic", size = 12),
                left = text_grob("Total Numbers Employed", color = "black", face = "italic", rot = 90))
bparranged

#Dot Charts - % employment by Socio Economic Group
dotarrangedgroup <- ggarrange(dot3, dot1, common.legend = TRUE, align = c("h"),ncol = 1, nrow = 2)
dotarrangedgroup <- annotate_figure(dotarrangedgroup,
                                  top = text_grob("Employment by Socio Economic Group", color = "black", face = "bold", size = 14, hjust = 0.5),
                                  bottom = text_grob("Group", color = "black",
                                                     hjust = 0.5, face = "italic", size = 12),
                                  left = text_grob("Percentage of Total Numbers Employed", color = "black", face = "italic", rot = 90))
dotarrangedgroup

#% employment by Age Group
dotarrangedage <- ggarrange(do4, dot2, common.legend = TRUE, align = c("h"),ncol = 1, nrow = 2)
dotarrangedage <- annotate_figure(dotarrangedage,
                              top = text_grob("Employment by Age Category", color = "black", face = "bold", size = 14, hjust = 0.5),
                              bottom = text_grob("Age Groups", color = "black",
                                                 hjust = 0.5, face = "italic", size = 12),
                              left = text_grob("Percentage of Total Numbers Employed", color = "black", face = "italic", rot = 90))
dotarrangedage

#####DIFFERENCE IN EMPLOYMENT#####

#Only interested in persons at work
Df5 <- Df1[(Df1$Labour.Force =="Persons at work"),]

#Only interested in state
Df5 <- Df5[(Df5$County.and.City =="State"),]

#Removing i.statistic column, Unit, Labour Force, County and CITY COLUMNS
Df5<-select(Df5, -c("County.and.City", "ï..Statistic", "UNIT", "Labour.Force"))

#Removing rows related to "All ages"
Df5 <- Df5 %>% filter(Age.Group != ("All ages"))

#Removing all socio economic groups variable
Df5 <- Df5 %>% filter(Socio.Economic.Group != ("All socio-economic groups"))

#For exporting to Tableau
Tibble7 <- Df5 %>% group_by(Age.Group, CensusYear) %>%
  summarise(across(
    .cols = is.numeric, 
    .fns = list(Total = sum), na.rm = TRUE, 
    .names = "{col}_{fn}"
  )) %>%
  rename(Total = VALUE_Total,
         Census_Year = CensusYear,
         Age_Group = Age.Group)

#For exporting to Tableau
Tibble8 <-Df5 %>% group_by(Socio.Economic.Group, CensusYear) %>%
  summarise(across(
    .cols = is.numeric, 
    .fns = list(Total = sum), na.rm = TRUE, 
    .names = "{col}_{fn}"
  )) %>%
  mutate(Socio.Economic.Group = str_sub(Socio.Economic.Group, 4, -1)) %>%
  rename(Total = VALUE_Total,
         Census_Year = CensusYear,
         Socio_Economic_Group = Socio.Economic.Group)

Extracted3 <- Tibble8 %>% filter(Census_Year == ("2011"))
Extracted4 <- Tibble8 %>% filter(Census_Year == ("2016"))

Extracted5 <- Tibble7 %>% filter(Census_Year == ("2011"))
Extracted6 <- Tibble7 %>% filter(Census_Year == ("2016"))

#Waffle Chart for employment by category - 2016

Tibble8_2016 <- Tibble8[(Tibble8$Census_Year =="2016"),]

#Removing Census Year column
Tibble8_2016 <- select(Tibble8_2016, -c(Census_Year))

#Creating Vector
Tibble8_2016 <- c("Employers and managers"= 308294, "Higher professional" = 168823,
                  "Lower professional" = 313740,"Non-manual" = 502426,"Manual skilled" = 156293,
                  "Semi-skilled" = 186658, "Unskilled" = 61888, "Own account workers" = 97283,
                  "Farmers" = 73740, "Agricultural workers" = 11577, "Others" = 125919)

#Waffle Chart - 2016
waffle4 <- waffle(Tibble8_2016/1000, rows=15, size=0.6, 
                  colors=c(mycolors), 
                  title="Employment Bifurcation as of 2016", 
                  xlab="1 Square = 1000 Persons") + theme(plot.title = element_text(hjust = 0.5))

#Waffle Chart for employment by category - 2011

Tibble8_2011 <- Tibble8[(Tibble8$Census_Year =="2011"),]

#Removing Census Year column
Tibble8_2011 <- select(Tibble8_2011, -c(Census_Year))

#Creating Vector
Tibble8_2011 <- c("Employers and managers"= 285450, "Higher professional" = 145446,
                  "Lower professional" = 280300,"Non-manual" = 467807,"Manual skilled" = 139495,
                  "Semi-skilled" = 169380, "Unskilled" = 54472, "Own account workers" = 94525,
                  "Farmers" = 76975, "Agricultural workers" = 10247, "Others" = 83263)


#Waffle Chart - 2011
waffle3 <- waffle(Tibble8_2011/1000, rows=15, size=0.6, 
                  colors=c(mycolors), 
                  title="Employment Bifurcation - 2011", 
                  xlab="1 Square = 1000 Persons", ) + theme(plot.title = element_text(hjust = 0.5))

#Combined Waffle Chart

wafflearranged2 <- ggarrange(waffle3, waffle4, common.legend = TRUE, align = c("h"),ncol = 1, nrow = 2)
wafflearranged2<- annotate_figure(wafflearranged2,
                                 top = text_grob("Waffle Chart, 2011 vs 2016", color = "black", face = "bold", size = 14, hjust = 0.5))
wafflearranged2

#Combined column dodge Graphs

Df5 <- Df5 %>%
  mutate(Socio.Economic.Group = str_sub(Socio.Economic.Group, 4, -1)) %>%
  rename(Total = VALUE,
         Age_Group = Age.Group,
         Socio_Economic_Group = Socio.Economic.Group)

#Renaming to others
Df5$Socio_Economic_Group[Df5$Socio_Economic_Group=="All others gainfully occupied and unknown"] <- "Others"

#Age Group Composition by Socio Economic Group
ggplot(Df5) + geom_col(aes(Socio_Economic_Group, Total, fill = factor(CensusYear)), position = "dodge") +
  facet_wrap(~Age_Group, scales = "free_x") +
  coord_flip()+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90)) +
  ylab("Age Group") + xlab("Socio Economic Group") +
  ggtitle("Age Group Composition by Socio Economic Group") +
  guides(fill=guide_legend(title="Census Year")) +
  theme(plot.title = element_text(hjust = 0.5))

#Socio Economic Group Composition by Age Group
ggplot(Df5) + geom_col(aes(Age_Group, Total, fill = factor(CensusYear)), position = "dodge") +
  facet_wrap(~Socio_Economic_Group, scales = "free_x") +
  coord_flip()+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90)) +
  ylab("Socio Economic Group") + xlab("Age Group") +
  ggtitle("Socio Economic Group Composition by Age Group") +
  guides(fill=guide_legend(title="Census Year")) +
  theme(plot.title = element_text(hjust = 0.5))

#Getting % changes - Employment by Age Group

Df5 %>% group_by(Age_Group, CensusYear) %>%
  summarise(across(
    .cols = is.numeric, 
    .fns = list(Total = sum), na.rm = TRUE, 
    .names = "{col}_{fn}"
  )) %>% rename(Total = Total_Total) %>%
  arrange(CensusYear, .by_group = TRUE) %>%
  group_by(Age_Group) %>%
  mutate(pct_change = (Total/lag(Total) - 1) * 100) %>%
  ggplot(.,aes(Age_Group, pct_change, label = (round(pct_change, digits = 1)))) + 
  geom_col(fill = "dodgerblue2", alpha = 0.6) + theme_bw() +
  geom_text(size = 5, nudge_y = -1, color = "firebrick", fontface = "italic") + 
  ggtitle("Percentage Change in Employment from 2011") +
  theme(plot.title = element_text(hjust = 0.5)) + ylab("%") +
  xlab("Age Group")

#Getting % changes - Employment by Socio Economic Group
Df5 %>% group_by(Socio_Economic_Group, CensusYear) %>%
  summarise(across(
    .cols = is.numeric, 
    .fns = list(Total = sum), na.rm = TRUE, 
    .names = "{col}_{fn}"
  )) %>% rename(Total = Total_Total) %>%
  arrange(CensusYear, .by_group = TRUE) %>%
  group_by(Socio_Economic_Group) %>%
  mutate(pct_change = (Total/lag(Total) - 1) * 100) %>%
  ggplot(.,aes(Socio_Economic_Group, pct_change, label = (round(pct_change, digits = 1)))) + 
  geom_col(fill = "dodgerblue2", alpha = 0.6) + theme_bw() +
  geom_text(size = 5, nudge_y = -3, color = "firebrick", fontface = "italic") + 
  ggtitle("Percentage Change in Employment from 2011") +
  theme(axis.text.x = element_text(angle=90), plot.title = element_text(hjust = 0.5)) + ylab("%") +
  xlab("Socio Economic Group")

#####UNEMPLOYMENT#####

#Only rows relating to unemployed persons kept
Df4 <- Df1[(Df1$Labour.Force =="All unemployed persons"),]

#Only interested in state
Df4 <- Df4[(Df4$County.and.City =="State"),]

#Removing i.statistic column, Unit, Labour Force, County and CITY COLUMNS
Df4<-select(Df4, -c("County.and.City", "ï..Statistic", "UNIT", "Labour.Force"))

#Removing rows related to "All ages"
Df4 <- Df4 %>% filter(Age.Group != ("All ages"))

#Removing all socio economic groups variable
Df4 <- Df4 %>% filter(Socio.Economic.Group != ("All socio-economic groups"))

Tibble5 <- Df4 %>% group_by(Age.Group, CensusYear) %>%
  summarise(across(
    .cols = is.numeric, 
    .fns = list(Total = sum), na.rm = TRUE, 
    .names = "{col}_{fn}"
  )) %>%
  rename(Total = VALUE_Total,
         Census_Year = CensusYear,
         Age_Group = Age.Group)

Tibble6 <-Df4 %>% group_by(Socio.Economic.Group, CensusYear) %>%
  summarise(across(
    .cols = is.numeric, 
    .fns = list(Total = sum), na.rm = TRUE, 
    .names = "{col}_{fn}"
  )) %>%
  mutate(Socio.Economic.Group = str_sub(Socio.Economic.Group, 4, -1)) %>%
  rename(Total = VALUE_Total,
         Census_Year = CensusYear,
         Socio_Economic_Group = Socio.Economic.Group)

print.data.frame(Tibble6)

#Getting values
Extracted <- Tibble6 %>% filter(Census_Year == ("2011"))

Extracted1 <- Tibble6 %>% filter(Census_Year == ("2016"))

Extracted7 <- Tibble5 %>% filter(Census_Year == ("2011"))
Extracted8 <- Tibble5 %>% filter(Census_Year == ("2016"))

#Getting % changes - Unemployment by Socio Economic Group

Tibble6$Socio_Economic_Group[Tibble6$Socio_Economic_Group=="All others gainfully occupied and unknown"] <- "Others"

Tibble6 %>%
arrange(Census_Year, .by_group = TRUE) %>%
  group_by(Socio_Economic_Group) %>%
  mutate(pct_change = (Total/lag(Total) - 1) * 100) %>%
  ggplot(.,aes(Socio_Economic_Group, pct_change, label = (round(pct_change, digits = 1)))) + 
  geom_col(fill = "dodgerblue2", alpha = 0.6) + theme_bw() +
  geom_text(size = 5, nudge_y = -3, color = "firebrick", fontface = "italic") + ggtitle("Percentage Change in Unemployment from 2011") +
  theme(axis.text.x = element_text(angle=90), plot.title = element_text(hjust = 0.5)) + ylab("%") +
  xlab("Socio Economic Group")

#Getting % changes - Unemployment by Age Group
Tibble5 %>% 
  arrange(Census_Year, .by_group = TRUE) %>%
  group_by(Age_Group) %>%
  mutate(pct_change = (Total/lag(Total) - 1) * 100) %>%
  ggplot(.,aes(Age_Group, pct_change, label = (round(pct_change, digits = 1)))) + 
  geom_col(fill = "dodgerblue2", alpha = 0.6) + theme_bw() +
  geom_text(size = 5, nudge_y = -3, color = "firebrick", fontface = "italic") + ggtitle("Percentage Change in Unemployment from 2011") +
  theme(plot.title = element_text(hjust = 0.5)) + ylab("%") +
  xlab("Age Group")

#Waffle Chart for unemployment by category - 2016

Tibble6_2016 <- Tibble6[(Tibble6$Census_Year =="2016"),]

#Removing Census Year column
Tibble6_2016 <- select(Tibble6_2016, -c(Census_Year))

#Creating Vector
Tibble6_2016 <- c("Employers and managers"= 17979, "Higher professional" = 5792,
                  "Lower professional" = 13977,"Non-manual" = 53249,"Manual skilled" = 29644,
                  "Semi-skilled" = 26637, "Unskilled" = 16609, "Own account workers" = 8363,
                  "Farmers" = 2672, "Agricultural workers" = 1751, "Others" = 120723)

#Waffle Chart - 2016
waffle2 <- waffle(Tibble6_2016/1000, rows=5, size=0.6, 
       colors=c(mycolors), 
       title="Unemployment Bifurcation as of 2016", 
       xlab="1 Square = 1000 Persons") + theme(plot.title = element_text(hjust = 0.5))

#Waffle Chart for unemployment by category - 2011

Tibble6_2011 <- Tibble6[(Tibble6$Census_Year =="2011"),]

#Removing Census Year column
Tibble6_2011 <- select(Tibble6_2011, -c(Census_Year))

#Creating Vector
Tibble6_2011 <- c("Employers and managers"= 32362, "Higher professional" = 9569,
                  "Lower professional" = 19753,"Non-manual" = 71906,"Manual skilled" = 70591,
                  "Semi-skilled" = 40154, "Unskilled" = 27039, "Own account workers" = 18572,
                  "Farmers" = 3999, "Agricultural workers" = 2465, "Others" = 128433)


#Waffle Chart - 2011
waffle1 <- waffle(Tibble6_2011/1000, rows=5, size=0.6, 
       colors=c(mycolors), 
       title="Unemployment Bifurcation - 2011", 
       xlab="1 Square = 1000 Persons", ) + theme(plot.title = element_text(hjust = 0.5))

#Stacked column chart of distribution of persons employed by age group by census year

#Creating color scheme
manualcolors <- c("dodgerblue4", "dodgerblue2")

#Creating chart - Age 2011 vs Age 2016
ggplot(Tibble5) + geom_col(aes(Age_Group, Total, fill = factor(Census_Year)), position = "dodge", alpha = 0.8) + theme_bw() +
  guides(fill=guide_legend(title="Census Year")) + scale_fill_manual(values=manualcolors) +
  xlab("Age Group") + ggtitle("Unemployment by Age Group 2011 vs 2016") + 
  theme(axis.text.x = element_text(angle=90), plot.title = element_text(hjust = 0.5))

#Socio Economic Group - 2011 vs 2016
ggplot(Tibble6) + geom_col(aes(Socio_Economic_Group, Total, fill = factor(Census_Year)),position = "dodge", alpha = 0.8) + theme_bw() +
  guides(fill=guide_legend(title="Census Year")) + scale_fill_manual(values=manualcolors) +
  xlab("Socio Economic Group") + ggtitle("Unemployment by Socio Economic Group 2011 vs 2016") + 
  theme(axis.text.x = element_text(angle=90), plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

#Waffle Charts combined
wafflearranged <- ggarrange(waffle1, waffle2, common.legend = TRUE, align = c("h"),ncol = 1, nrow = 2)
wafflearranged<- annotate_figure(wafflearranged,
                top = text_grob("Waffle Chart, 2011 vs 2016", color = "black", face = "bold", size = 14, hjust = 0.5))
wafflearranged

#####Exporting to CSV for Tableau for further illustration#####

write.csv(Tibble1, "C:\\Users\\rossd\\Documents\\Tibble1.csv", row.names = FALSE)
write.csv(Tibble2, "C:\\Users\\rossd\\Documents\\Tibble2.csv", row.names = FALSE)
write.csv(Tibble3, "C:\\Users\\rossd\\Documents\\Tibble3.csv", row.names = FALSE)
write.csv(Tibble4, "C:\\Users\\rossd\\Documents\\Tibble4.csv", row.names = FALSE)
write.csv(Tibble5, "C:\\Users\\rossd\\Documents\\Tibble5.csv", row.names = FALSE)
write.csv(Tibble6, "C:\\Users\\rossd\\Documents\\Tibble6.csv", row.names = FALSE)
write.csv(Tibble7, "C:\\Users\\rossd\\Documents\\Tibble7.csv", row.names = FALSE)
write.csv(Tibble8, "C:\\Users\\rossd\\Documents\\Tibble8.csv", row.names = FALSE)