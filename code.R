knitr::opts_chunk$set(echo = TRUE)
pacman::p_load(
  "ggplot2",
  "knitr",
  "arm",
  "foreign",
  "faraway",
  "nnet",
  "VGAM",
  "MASS",
  "rstanarm",
  "magrittr",
  "dplyr",
  "reshape2",
  "stats",
  "tidyverse", 
  "rvest",
  "xts",
  "RColorBrewer",
  "tidyverse",
  "wordcloud2",
  "plotly",
  "forcats",
  "hrbrthemes",
  "viridisLite",
  "viridis",
  "ggridges",
  "GGally",
  "xts",
  "gridExtra",
  "lattice",
  "lm4"
)

# Data cleaning
data_salary<- read.csv("Levels_Fyi_Salary_Data.csv")
data_salary$company<- tolower(data_salary$company)

trim <- function (x){
  gsub("^\\s+|\\s+$", "", x)
}

trim(data_salary$company) 
trim(data_salary$country)
trim(data_salary$date)

# company
data_salary$company[data_salary$company == "amzon"] <- "amazon"
data_salary$company[data_salary$company == "apple inc."] <- "apple"
data_salary$company[data_salary$company == "aruba networks"] <- "aruba"
data_salary$company[data_salary$company == "bloomberg lp"] <- "bloomberg"
data_salary$company[data_salary$company == "booking"] <- "booking.com"
data_salary$company[data_salary$company == "cgi"] <- "cgi group"
data_salary$company[data_salary$company == "dish"] <- "dish network"
data_salary$company[data_salary$company == "intel corporation"] <- "intel"
data_salary$company[data_salary$company == "johnson & johnson"] <- "johnson and johnson"
data_salary$company[data_salary$company == "johnson"] <- "johnson and johnson"
data_salary$company[data_salary$company == "jp morgan chase"] <- "jpmorgan chase"
data_salary$company[data_salary$company == "jp morgan"] <- "jpmorgan"
data_salary$company[data_salary$company == "macy's,"] <- "macy's"
data_salary$company[data_salary$company == "microsoft corporation"] <- "microsoft"
data_salary$company[data_salary$company == "costco"] <- "costco wholesale"
data_salary$company[data_salary$company == "google llc"] <- "google"
data_salary$company[data_salary$company == "nxp"] <- "nxp semiconductors"

# divide location
data_salary %<>% separate(col=location,
                          into = c("city", "states", "country"), 
                          sep = ",",
                          fill = "right")
data_salary$country[is.na(data_salary$country)] <- "United States"
dat_usa<- data_salary %>% filter(country== "United States")

data_salary %<>% separate(col=timestamp,
                          into = c("date", "time"), 
                          sep = " ",
                          fill = "right")

# add education level
data_salary$edu_level <- ifelse(data_salary$Highschool == 1,0,
                                ifelse(data_salary$Some_College == 1,1,
                                       ifelse(data_salary$Bachelors_Degree == 1,2,
                                              ifelse(data_salary$Masters_Degree == 1,3,
                                                     ifelse(data_salary$Doctorate_Degree == 1,4,0
                                                     )))))

# sub-data-set
dat4<- data_salary %>%
  count(company)
dat4[order(dat4$n,decreasing=T),]
dat_company<- data_salary %>% 
  filter(company == "amazon" | company == "microsoft" | company == "google" | company == "facebook" | company == "facebook" |company == "apple" | company == "oracle" | company == "salesforce" | company == "intel" | company == "ibm" | company == "cisco")
dat5<- dat_company %>%
  count(company,title,level)

# add race
df_1 <- data_salary %>% 
  na.omit() %>%
  filter(country == "United States")
df_1$gender[df_1$gender == "Male"]<- "1"
df_1$gender[df_1$gender == "Female"]<- "0"
df_1$gender<- as.numeric(df_1$gender)
df_1$race <- ifelse(df_1$Race_Asian == 1,1,
                    ifelse(df_1$Race_White == 1,2,
                           ifelse(df_1$Race_Two_Or_More == 1,3,
                                  ifelse(df_1$Race_Black == 1,4,
                                         ifelse(df_1$Race_Hispanic == 1,5,0
                                         )))))

#####################################################################################
#EDA

# salary and title(volin plot)
dat3<- data_salary %>%
  count(company,title,level,Education)
p1 <- data_salary %>%# Reorder data
  ggplot( aes(x=title, y=log(totalyearlycompensation), fill=title, color=title)) +
  geom_violin(width=2.1, size=0.2) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  xlab("job title") +
  ylab("log total yearly compensation")

# salary and company
p2<- dat_company %>%
  ggplot(aes(y=company, x=log(totalyearlycompensation), fill=company)) +
  geom_density_ridges(alpha=0.6, stat="binline", bins=20) +
  xlab("log total yearly compensation") +
  ylab("company")
grid.arrange(p1,p2,nrow=1)

# boxplot(salary, years of experience, education level)
data_salary$work_experience <- ifelse(data_salary$yearsofexperience <= 1,"Graduates",
                                      ifelse(data_salary$yearsofexperience <= 5,"Novices",
                                             ifelse(data_salary$yearsofexperience <= 10,"intermediates",
                                                    ifelse(data_salary$yearsofexperience >10,"Experienced","grades"                                                                                                                  ))))
df_2 <- data_salary %>% 
  na.omit() %>%
  filter(country == "United States")

p3<- ggplot(df_2, aes(x =Education, y = log(totalyearlycompensation), color = work_experience)) +  # ggplot function
  geom_boxplot()+
  xlab("Education Level") +
  ylab("log total yearly experience") 

# violin plot(salary, gender and race)
p4<- ggplot(df_2,aes(fill=gender, y=log(totalyearlycompensation), x=Race)) + 
  geom_violin(position="dodge", alpha=0.5, outlier.colour="transparent") +
  scale_fill_viridis(discrete=T, name="") +
  xlab("race") +
  ylab("log total yearly experience") 
grid.arrange(p3,p4,nrow=1)

library(ggcorrplot)
data_num<- df_1[,c(10,11,13,14,15,33,34)]
#data_num$totalyearlycompensation<-data_num$totalyearlycompensation/1000
data_num$basesalary<- data_num$basesalary/1000
data_num$bonus<- data_num$bonus/1000
data_num$stockgrantvalue<- data_num$stockgrantvalue/1000
corr <- cor(data_num)
p5<- ggcorrplot(corr,
                hc.order = TRUE, type = "lower",
                outline.color = "white",
                ggtheme = ggplot2::theme_gray,
                colors = c("#6D9EC1", "white", "#E46726")
)

p6<- ggplot(df_1, aes(log(totalyearlycompensation), colour= title)) + 
  geom_density(aes(y = ..density..), alpha=0.3) + 
  geom_density(aes(colour = title)) +
  xlab("log totalcomensation") +
  ylab("density")

grid.arrange(p5,p6,nrow=1)

# More EDA
dat1<- data_salary %>% count(company)
wordcloud2(dat1,size=3,color='random-dark')

# Density plot
## years of experience distribution
ggplot(data_salary, aes(x=yearsofexperience))+
  geom_histogram(aes(y=..density..), color="black", fill="white")+
  geom_density(alpha=.2,fill="#FF6666") +
  xlab("years of experience") +
  ylab("density") 

## total yearly compensation distribution
ggplot(data_salary, aes(x=totalyearlycompensation))+
  geom_histogram(aes(y=..density..), color="black", fill="white")+
  geom_density(alpha=.2,fill="#69b3a2") +
  xlab("total yearly compensation") +
  ylab("density") 

## education level distribution
df_2 <- data_salary %>% 
  na.omit() %>%
  filter(country == "United States")
ggplot(data=df_2, aes(x=log(totalyearlycompensation),group=Education, fill = Education))+
  geom_histogram(aes(y=..density..))+
  geom_density(adjust=1.5,alpha=.2)+
  #theme_ipsum()+
  facet_wrap(~Education)+
  xlab("total yearly compensation") +
  ylab("density") 

## company in states distribution
dat6<- dat_usa %>% count(states)
ggplot(dat6, aes(x=states, y=log(n)))+
  geom_segment( aes(x=states, xend=states, y=0, yend=log(n)), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  #theme_light() +
  coord_flip() +
  xlab("States") +
  ylab("log frequency") 

## compare job title and education level
data_title<- df_2 %>% count(Education,title)
data<- data_title
empty_bar <- 5
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$Education), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$Education), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(Education)
data$id <- seq(1, nrow(data))

# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# Make the plot
p <- ggplot(data, aes(x=as.factor(id), y=n, fill=Education)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat="identity", alpha=0.5) +
  ylim(-100,120) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=n+10, label=title, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 

p+theme_minimal() +
  theme(
    #legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm"))
 
#####################################################################
# Model Fitting
model2<- lmer(log(totalyearlycompensation)~ yearsofexperience + yearsatcompany + gender + race + edu_level + (1+yearsofexperience|title)+(1+ gender|title)+(1+race|title)+(1+edu_level|title)+(1+yearsatcompany|title), data=df_1)
summary(model2)

ranef(model2)
fixef(model2)
coef(model2)

# Model Validation
re <- plot(model2)
qq <- qqmath(model2)
grid.arrange(re,qq,nrow=1)

ggplot(data.frame(lev=hatvalues(model2),pearson=residuals(model2,type="pearson")),
       aes(x=lev,y=pearson)) +
  geom_point() +
  theme_bw()


