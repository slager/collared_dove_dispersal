## Get coastal effort

effort_coastal <- readRDS('effort_coastal.rds')
monthly_coastal_effort <-
  effort_coastal %>% use_series(observation_date) %>% get_month %>% table %>% as.vector
names(monthly_coastal_effort) <- month.abb
monthly_coastal_effort
rm(effort_coastal)

## Get pelagic effort

effort_pelagic <- readRDS('effort_pelagic.rds')
monthly_pelagic_effort <-
  effort_pelagic %>% use_series(observation_date) %>% get_month %>% table %>% as.vector
names(monthly_pelagic_effort) <- month.abb
monthly_pelagic_effort
rm(effort_pelagic)

## Get coastal observations

readRDS("eucd_coastal.rds") -> eucd_coastal

## Get pelagic observations

readRDS("eucd_pelagic.rds") -> eucd_pelagic

## Monthly pelagic observations

monthly_pelagic_records <-
  eucd_pelagic %>%
  use_series(observation_date) %>%
  get_month %>%
  month.abb[.] %>%
  factor(levels=month.abb) %>%
  table %>%
  as.vector
names(monthly_pelagic_records) <- month.abb

## Raw monthly pelagic chisq.test (don't use)
#monthly_pelagic_records %>%
#  chisq.test(simulate.p.value=T)

## Graph and chisq.test of effort-corrected monthly pelagic observations
# Calc density
monthly_pelagic_density <-
  monthly_pelagic_records/monthly_pelagic_effort /
  sum( monthly_pelagic_records/monthly_pelagic_effort )
# Graph density
monthly_pelagic_density %>% barplot(ylab='Density')
# Chisq.test ~ density
monthly_pelagic_records %>% chisq.test(
  p=(monthly_pelagic_effort/sum(monthly_pelagic_effort)),
  simulate.p.value=T
  )



### BACK TO COASTAL STUFF

# Coastal summary stats
nrow(eucd_coastal)
eucd_coastal %>% use_series(observer_id) %>% unique %>% length
eucd_coastal %>% use_series(locality_id) %>% unique %>% length

## Get records with comments and >=10 birds
eucd_10_comments <- 
  eucd_coastal %>%
  filter(species_comments != "" ) %>%
  filter(observation_count != 'X') %>%
  filter(as.numeric(observation_count) >= 10)
#write.csv(eucd_10_comments,"eucd_10_comments.csv")
saveRDS(eucd_10_comments,"eucd_10_comments.rds")
readRDS("eucd_10_comments.rds") -> eucd_10_comments

# Get summary stats for the manual coding dataset
eucd_10_comments %>% nrow
#841
eucd_10_comments %>% use_series('locality_id') %>% unique %>% length
#440
eucd_10_comments %>% use_series('observer_id') %>% unique %>% length
#336



# Get coastal sample size by month

month_N <-
readRDS("eucd_coastal.rds") %>%
  use_series(observation_date) %>% 
  get_month %>%
  table
names(month_N) <- month.abb


# Monthly # of species comments -- all observations
month_spcom <-
  readRDS("eucd_coastal.rds") %>%
  filter(country != "Mexico" & species_comments != "") %>%
  use_series(observation_date) %>%
  get_month %>% factor(levels=1:12) %>% table

# Monthly # of species comments -- all observations >10 indiv

month_spcom_10 <-readRDS("eucd_coastal.rds") %>%
  filter(country != "Mexico") %>%
  filter(observation_count != "X") %>%
  filter(species_comments != "") %>%
  filter(as.numeric(observation_count) >= 10) %>%
  use_series(observation_date) %>%
  get_month %>% factor(levels=1:12) %>% table
names(month_spcom_10) <- month.abb

##### MANUAL CODING HAPPENED HERE #####

# Get month barplot of Flocks in Flight

# Analysis of coded data!
read.csv("coded_final.csv",header=T,stringsAsFactors=F) -> coded

#coded %>% filter(FiF==1 & ! Dir %in% c('C',NA)) %>% use_series(observation_date) %>% get_month %>% as.numeric %>% hist
month_FiF <- coded %>%
  filter(FiF==1) %>%
  use_series(observation_date) %>%
  get_month %>% factor(levels=1:12) %>% table
names(month_FiF) <- month.abb

## Compass direction analysis

coded$compass <-
  coded$Dir %>%
  recode(.default=as.numeric(NA),
         NE=45,E=90,SE=135,S=180,SW=225,W=270,NW=315,N=360)

#checking
coded %>% filter(!is.na(FiF) & !is.na(Dir) & Dir != "C") %>% use_series(compass)
coded %>% filter(!is.na(FiF) & !is.na(Dir)) %>% use_series(compass) %>% na.omit


##Flight directions totals
#Overall
coded %>% filter(!is.na(FiF)) %>% nrow
coded %>% filter(!is.na(FiF) & !is.na(Dir)) %>% nrow
coded %>% filter(!is.na(FiF) & !is.na(Dir) & Dir != "C") %>% nrow
#March-May
coded %>% filter(!is.na(FiF) & get_month(observation_date) %in% 3:5) %>% nrow
coded %>% filter(!is.na(FiF) & !is.na(Dir) & get_month(observation_date) %in% 3:5) %>% nrow
coded %>% filter(!is.na(FiF) & !is.na(Dir) & Dir != "C" & get_month(observation_date) %in% 3:5) %>% nrow
#NOT March-May
coded %>% filter(!is.na(FiF) & get_month(observation_date) %in% c(1:2,6:12)) %>% nrow
coded %>% filter(!is.na(FiF) & get_month(observation_date) %in% c(1:2,6:12)& !is.na(compass)) %>% use_series(compass)

## 95 coastal "flocks in flight" info:
# N
coded %>% filter(!is.na(FiF)) %>% nrow
# localities
coded %>% filter(!is.na(FiF)) %>% use_series(locality_id) %>% unique %>% length
# observers
coded %>% filter(!is.na(FiF)) %>% use_series(observer_id) %>% unique %>% length

## Coastal "flocks in flight" with direction in March-May
#N
coded %>% filter(!is.na(FiF) & !is.na(Dir) & Dir != "C" & get_month(observation_date) %in% 3:5) %>% nrow
# localities
coded %>% filter(!is.na(FiF) & !is.na(Dir) & Dir != "C" & get_month(observation_date) %in% 3:5) %>% use_series(locality_id) %>% unique %>% length
# observers
coded %>% filter(!is.na(FiF) & !is.na(Dir) & Dir != "C" & get_month(observation_date) %in% 3:5) %>% use_series(observer_id) %>% unique %>% length


## Generate flight directions data frame FLOCKS IN FLIGHT March-May

df <-
  coded %>% filter(!is.na(FiF) & !is.na(Dir)) %>%
  filter(get_month(observation_date) %in% 3:5) %>%
  use_series(compass) %>%
  na.omit %>%
  factor(levels=seq(from=45,to=360,by=45)) %>%
  table %>%
  as.data.frame
names(df) <- c('compass','freq')


## Plot flight directions using ggplot2
library(ggplot2)
pdf("flight_directions.pdf",5,5)
ggplot(df,aes(x=compass,y=freq)) +
  coord_polar(start=22.5/(180/pi)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(breaks = seq(0, 360, 45),
                   labels=c("N","NE","E","SE","S","SW","W","NW","N"),
                   name="Flight direction") +
  scale_y_continuous(name="Frequency")
dev.off()  

## Circular stats flight directions
library(circular)

angles_degrees <-
  coded %>% filter(!is.na(FiF) & !is.na(Dir)) %>%
  filter(get_month(observation_date) %in% 3:5) %>%
  use_series(compass) %>%
  na.omit %>%
  factor(levels=seq(from=45,to=360,by=45)) %>%
  as.character %>% as.numeric

str(angles_degrees)
angles_circular <- circular(angles_degrees,units="degrees",zero=circular(pi/2),rotation="clock")
plot(angles_circular)
mean(angles_circular) #mean direction in degrees, theta-hat
rho.circular(angles_circular) #mean result length, bigRrho-hat
median.circular(angles_circular)
sd.circular(angles_circular)
1-rho.circular(angles_circular) #circular variance, one minus Rbar, be specific
range.circular(angles_circular) #circular range:  Arc length, little w in degrees

# Significant non-uniformity of angles, Chapter 5.1.1
rt <- rayleigh.test(angles_circular)
rt
rt$statistic
rt$p

# Test null hyp = uniformity vs. alternative: mean=north
mu=circular(0,units="degrees",zero=circular(pi/2),rotation="clock")
rt0 <- rayleigh.test(angles_circular,mu)
rt0
rt0$statistic
rt0$p



## Generate flight directions data frame FLOCKS IN FLIGHT OUTSIDE OF MARCH-MAY

df <-
  coded %>% filter(!is.na(FiF) & !is.na(Dir)) %>%
  filter(get_month(observation_date) %in% c(1:2,6:12)) %>%
  use_series(compass) %>%
  na.omit %>%
  factor(levels=seq(from=45,to=360,by=45)) %>%
  table %>%
  as.data.frame
names(df) <- c('compass','freq')

## Circular stats flight directions
library(circular)
angles_degrees <-
  coded %>% filter(!is.na(FiF) & !is.na(Dir)) %>%
  filter(get_month(observation_date) %in% c(1:2,6:12)) %>%
  use_series(compass) %>%
  na.omit %>%
  factor(levels=seq(from=45,to=360,by=45)) %>%
  as.character %>% as.numeric
#fB11 <- c(fisherB11,8)
str(angles_degrees)
angles_circular <- circular(angles_degrees,units="degrees",zero=circular(pi/2),rotation="clock")
plot(angles_circular)
mean(angles_circular) #mean direction in degrees, theta-hat
rho.circular(angles_circular) #mean result length, bigRrho-hat
median.circular(angles_circular)
sd.circular(angles_circular)
1-rho.circular(angles_circular) #circular variance, one minus Rbar, be specific
range.circular(angles_circular) #circular range:  Arc length, little w in degrees
# Significant non-uniformity of angles, Chapter 5.1.1
rt <- rayleigh.test(angles_circular)
rt
rt$statistic
rt$p







#Raw counts of FiF by month
pdf("FiF_month_raw.pdf")
barplot(month_FiF,ylab="Number of reports")
dev.off()
chisq.test(month_FiF,simulate.p.value=T)

#Raw counts of FiF by month, with pelagics
pdf("FiF_month_raw_with_pelagics.pdf")
barplot(rbind(month_FiF,monthly_pelagic_records),beside=T,col=c('grey','blue'),ylab="Number of reports")
dev.off()

## Effort-corrected density historgrams of coastal & pelagic reports
monthly_coastal_density <-
  month_FiF/monthly_coastal_effort /
  sum( month_FiF/monthly_coastal_effort )

## Density histogram of coastal and pelagic together
#pdf("coastal_pelagic_density.pdf")
#barplot(rbind(monthly_coastal_density,monthly_pelagic_density),beside=T,col=c('grey','blue'),ylab="Density",)
#dev.off()

## Density histogram of just coastal (USING)  PART OF COMBINED PLOT
pdf("coastal_bymonth.pdf")
par(mar=c(3.1,4.1,2.1,1.1))
#Based on flocks in flight observations among >10 indiv w/ species comments
barplot(month_FiF/monthly_coastal_effort*1e3,col='#DDCC77',ylab="Flying flock reports per 1,000 coastal eBird checklists")
dev.off()
sum(monthly_coastal_effort)
sum(monthly_coastal_density[3:5])

## Density histogram of just pelagic (USING)
pdf("pelagic_bymonth.pdf")
par(mar=c(3.1,4.1,2.1,1.1))
#Based on all proofed pelagic observations
barplot(monthly_pelagic_records/monthly_pelagic_effort*1e3,col='#88CCEE',ylab="Reports per 1,000 pelagic eBird checklists")
dev.off()
sum(monthly_pelagic_effort)
sum(monthly_pelagic_density[3:5])

##### NEXT IS TWO ABOVE PLOTS COMBINED INTO ONE GRAPH  -- USING #####

## Density histogram of just coastal (USING)  PART OF COMBINED PLOT
pdf("coastal_pelagic_bymonth_stack.pdf",width=5,height=11/6*5)
par(mfrow=c(2,1))
par(mar=c(3.1,4.1,2.1,1.1))
#Based on flocks in flight observations among >10 indiv w/ species comments
barplot(month_FiF/monthly_coastal_effort*1e3,col='#DDCC77',ylab="Flying flock reports per 1,000 coastal checklists",names.arg=(month.abb %>% substr(1,1)))
mtext(text="A",side=3,cex=1.25,adj=1)
#dev.off()
sum(monthly_coastal_effort)
#sum(monthly_coastal_density[3:5])

## Density histogram of just pelagic (USING)
#pdf("pelagic_bymonth.pdf")
par(mar=c(3.1,4.1,2.1,1.1))
#Based on all proofed pelagic observations
barplot(monthly_pelagic_records/monthly_pelagic_effort*1e3,col='#88CCEE',ylab="Reports per 1,000 pelagic checklists",names.arg=(month.abb %>% substr(1,1)))
mtext(text="B",side=3,cex=1.25,adj=1)
dev.off()
sum(monthly_pelagic_effort)
#sum(monthly_pelagic_density[3:5])




## EUCD in flight by month chisq.test, correcting for total coastal effort (USING)
# Chisq.test ~ density
month_FiF %>% chisq.test(
  p=(monthly_coastal_effort/sum(monthly_coastal_effort)),
  simulate.p.value=T
)
sum(monthly_coastal_effort)

# OLD chisq test corrected for monthly spcom_10 obsevations, same result as new version
#chisq.test(month_FiF,p=month_spcom_10/sum(month_spcom_10),simulate.p.value=T)




# EUCD in flight, as proportion of monthly reports w/spcom and >= 10
pdf("FiF_month_proportion_spcom_10.pdf")
barplot(month_FiF/month_spcom_10, ylab="Proportion of reports")
dev.off()
chisq.test(month_FiF,p=month_spcom_10/sum(month_spcom_10),simulate.p.value=T)

# EUCD in flight as proportion of ALL monthly reports
pdf("FiF_month_proportion_all.pdf")
barplot(month_FiF/month_N, ylab="Proportion of reports")
dev.off()
test <- chisq.test(month_FiF,p=month_N/sum(month_N),simulate.p.value=T)

sum(month_FiF[3:5])/sum(month_FiF) #1
sum((month_FiF/month_spcom_10)[3:5])/sum(month_FiF/month_spcom_10) #2
sum((month_FiF/month_N)[3:5])/sum(month_FiF/month_N) #3




# When are eBirders most likely to provide EUCD species comments
pdf("proportion_with_spcom.pdf")
barplot(month_spcom/month_N,names.arg=month.abb,ylab="Proportion of records with species comments")
dev.off()
sum(month_spcom)/sum(month_N) # 5.6% of records have spcom on average

# Flight directions Mar-May
coded %>%
  filter(!Dir %in% c("",NA,"C")) %>%
  filter(get_month(observation_date) %in% 3:5) %>%
  use_series(Dir) %>%
  table %>%
  barplot

## All flocks in flight March-May by year (similar for ones w/ direction)
fif_mm <-
  coded %>%
  filter(FiF ==1) %>%
  filter(get_month(observation_date) %in% 3:5) %>%
  use_series(observation_date) %>%
  get_year %>% factor(levels=2010:2018) %>% table %>% as.vector %>% setNames(2010:2018)

## All coded March-May obs by year
obs_mm <-
  coded %>%
  filter(get_month(observation_date) %in% 3:5) %>%
  use_series(observation_date) %>%
  get_year %>% factor(levels=2007:2018) %>% table

#effort_coastal <- readRDS('effort_coastal.rds')
effort_mm <-
  effort_coastal %>%
  filter(get_month(observation_date) %in% 3:5) %>%
  use_series(observation_date) %>%
  get_year %>% factor(levels=2007:2018) %>% table

# 
# ### Year plot version, controlling for eBird effort (number of checklists) -- OLD
# pdf("year_plot.pdf")
# plot(x=2007:2018,y=(fif_mm/effort_mm * 1e5),xlab="Year",ylab="Flying flock reports per 100,000 eBird checklists")
# axis(side=2)
# df<- data.frame(year=2007:2018,fif_mm=as.numeric(fif_mm),effort_mm=as.numeric(effort_mm))
# linear.model <- lm((fif_mm/effort_mm*1e5)~year,data=df)
# linear.model %>% abline(lty=2)
# dev.off()
# linear.model %>% summary
# linear.model.predictions <- predict(linear.model)
# names(linear.model.predictions) <- 2007:2018
# 
# (linear.model.predictions['2018']-linear.model.predictions['2010'])/linear.model.predictions['2010']  ## Percent increase from 2010 to 2018
# (linear.model.predictions['2018']/linear.model.predictions['2010']) ^ .125 ## 1 minus this for annual percent increase 2010-2018
# #linear.model.predictions['2010'] * 1.207332^8 ## Checking the calculation
# >350% increase from 2010 (the year of the 1st Mach-May FiF reports) to 2018



##### Across years, flying flock frequency (of all EUCD reports) ######  -- USING
readRDS("eucd_coastal.rds") -> eucd_coastal
yearly_spring_coastal_reports <- eucd_coastal %>% filter(get_month(observation_date) %in% 3:5) %>% use_series(observation_date) %>% get_year %>% factor(levels=2010:2018) %>% table %>% as.vector %>% setNames(2010:2018)
df<- data.frame(year=2010:2018,fif_mm=as.numeric(fif_mm),yearly_spring_coastal_reports=as.numeric(yearly_spring_coastal_reports))
# #lm(fif_mm~year,data=df) %>% summary
# #lm(obs_mm~year,data=df) %>% summary
# ##Plot:  # FiF / # obs w/ spcomm and >=10 indiv
pdf("year_plot.pdf",6,6)
plot(df$fif_mm/df$yearly_spring_coastal_reports*1e3 ~ df$year,xlab="Year",ylab="Flying flock reports per 1,000 spring sightings")
linear.model <- lm((fif_mm/yearly_spring_coastal_reports*1e3)~year,data=df)
#linear.model %>% abline(lty=2)
dev.off()

linear.model %>% summary
linear.model.predictions <- predict(linear.model) %>% setNames(2010:2018)
linear.model.predictions

(linear.model.predictions['2018']-linear.model.predictions['2010'])/linear.model.predictions['2010']*100  ## Percent increase from 2010 to 2018
((linear.model.predictions['2018']/linear.model.predictions['2010']) ^ .125 - 1)*100 ## Annual geometric percent increase 2010-2018
linear.model.predictions['2010'] * 1.079802^8 ## Checking the calculation, should equal 2018 fitted value

### Binomial model stuff
# 
# year <- 2007:2018
# yearly_spring_coastal_reports <- eucd_coastal %>% filter(get_month(observation_date) %in% 3:5) %>% use_series(observation_date) %>% get_year %>% factor(levels=2007:2018) %>% table %>% as.vector %>% setNames(2007:2018)
# yesno <- cbind(yes=fif_mm, no=yearly_spring_coastal_reports-fif_mm)
# plot(2007:2018,fif_mm/yearly_spring_coastal_reports)
# glm(yesno ~ year, family='binomial') %>% predict %>% exp %>% {./(1+.)} %>% points(2007:2018,.,pch=4)
# glm(yesno ~ year, family='binomial') %>% summary
# 
# %>% predict %>% exp %>% multiply_by(yearly_spring_coastal_reports)
# 
#  %>% exp %>% {./(1+.)}
