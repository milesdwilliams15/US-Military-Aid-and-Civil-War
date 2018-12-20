
# Targeting Threats:
# US Military Aid and Civil War

# Open libraries.
library(haven)
library(dplyr)
library(AER)
library(lmtest)
library(ggplot2)
library(dotwhisker)
library(broom)
library(gridExtra)

# Get data:
data <- read_dta("Replication Data for Compliant Rebels.dta")
aid_data <- read_dta("finaldata.dta")
new_data <- merge(data %>%
                    group_by(country,year) %>%
                    summarize(reb_kills=sum(rebbest2010,na.rm=T),
                              gov_kills=sum(govbest_L1,na.rm=T),
                              duration=max(duration,na.rm=T),
                              reb_strength=max(rebstrength1,na.rm=T),
                              n_rebs=length(rebelname)),
                  aid_data %>%
                    mutate(country=recipient),all.y=T,
                  by=c("country","year")) %>%
  filter(donor=="US")
new_data$reb_kills[is.na(new_data$reb_kills)==T&new_data$civilwar==0] <- 0
new_data$gov_kills[is.na(new_data$gov_kills)==T&new_data$civilwar==0] <- 0

new_data <- new_data %>% 
  mutate(lusmil=log(usmil+1),
         listock=log(istock+1),
         ltrade=log(trade+1),
         lincome=log(gdpcap),
         lpop=log(population),
         laid=log(commit3a+1),
         ldist=log(distance),
         ldisaster=log(disaster+1)) %>%
  group_by(recipient) %>%
  mutate(listock_lag=lag(listock,order.by='year'),
         ltrade_lag=lag(ltrade,order.by='year'),
         civilwar_lag=lag(civilwar,order.by='year'),
         lincome_lag=lag(lincome,order.by='year'),
         lpop_lag=lag(lpop,order.by='year'),
         fh_lag=lag(fh,order.by='year'))

# Estimate models.
tobit_1 <- tobit(lusmil ~ civilwar_lag + listock_lag + ltrade_lag +
                   lincome_lag + lpop_lag + fh_lag + ldist +
                   as.factor(year) + frailty.gaussian(recipient,sparse=F),
                 data=new_data%>%filter(year>=2001))
tobit_2 <- tobit(lusmil ~ civilwar_lag*listock_lag + ltrade_lag +
                   lincome_lag + lpop_lag + fh_lag + ldist +
                   as.factor(year) + frailty.gaussian(recipient,sparse=F),
                 data=new_data%>%filter(year>=2001))
tobit_3 <- tobit(lusmil ~ civilwar_lag + listock_lag + civilwar_lag*ltrade_lag +
                   lincome_lag + lpop_lag + fh_lag + ldist +
                   as.factor(year) + frailty.gaussian(recipient,sparse=F),
                 data=new_data%>%filter(year>=2001))

# Visualize with coefficient plot.
rbind(tidy(coeftest(tobit_1)) %>% mutate(model='Model 1'),
      tidy(coeftest(tobit_2)) %>% mutate(model='Model 2'),
      tidy(coeftest(tobit_3)) %>% mutate(model='Model 3')) %>%
  filter(term=='civilwar_lag'|
           term=='listock_lag'|
           term=='ltrade_lag'|
           term=='lincome_lag'|
           term=='lpop_lag'|
           term=='fh_lag'|
           term=='ldist'|
           term=='civilwar_lag:listock_lag'|
           term=='civilwar_lag:ltrade_lag') %>%
  relabel_predictors(c(civilwar_lag='Civil War',
                       listock_lag='Migrants',
                       ltrade_lag='Trade',
                       lincome_lag='Income',
                       lpop_lag='Population',
                       fh_lag='Democracy',
                       ldist='Distance',
                       `civilwar_lag:listock_lag`='Civil War   \n * Migrants',
                       `civilwar_lag:ltrade_lag`='Civil War   \n * Trade')) %>%
  dwplot() +
  geom_vline(xintercept=0) +
  labs(x='Estimated Coefficient\n(95% CI shown)') +
  theme_classic() +
  theme(text=element_text(family='serif'),
        axis.text=element_text(color='black'),
        legend.position = c(.2,.2),
        legend.title=element_blank())

# Visualize marginal effect of civil war.
b_mig <- c()
se_mig <- c()
mig_vals <- 0:round(max(model.frame(tobit_1)$listock_lag))
b_trd <- c()
se_trd <- c()
trd_vals <- 0:round(max(model.frame(tobit_1)$ltrade_lag))
for(i in 1:length(mig_vals)){
  model <- tobit(lusmil ~ civilwar_lag*listock_lag + ltrade_lag +
                   lincome_lag + lpop_lag + fh_lag + ldist +
                   as.factor(year) + frailty.gaussian(recipient),
                 data=new_data%>%filter(year>=2001) %>%
                   mutate(listock_lag=listock_lag-mig_vals[i]))
  model_sum <- coeftest(model)
  b_mig[i] <- model_sum[2,1]
  se_mig[i] <- model_sum[2,2]
}
for(i in 1:length(trd_vals)){
  model <- tobit(lusmil ~ civilwar_lag + listock_lag + civilwar_lag*ltrade_lag +
                   lincome_lag + lpop_lag + fh_lag + ldist +
                   as.factor(year) + frailty.gaussian(recipient),
                 data=new_data%>%filter(year>=2001) %>%
                   mutate(ltrade_lag=ltrade_lag-trd_vals[i]))
  model_sum <- coeftest(model)
  b_trd[i] <- model_sum[2,1]
  se_trd[i] <- model_sum[2,2]
}
g1 <- ggplot(NULL,aes(mig_vals,b_mig)) +
  geom_line(size=1) +
  geom_ribbon(aes(ymin=b_mig-1.96*se_mig,
                  ymax=b_mig+1.96*se_mig),
              alpha=.5) +
  geom_hline(yintercept=0,linetype=2,size=1) +
  labs(x='Migrants (log)',
       y='Marginal Effect of Civil War',
       title='U.S. Migrants') +
  theme_classic() +
  theme(text=element_text(family='serif'),
        plot.title=element_text(hjust=.5))
g2 <- ggplot(NULL,aes(trd_vals,b_trd)) +
  geom_line(size=1) +
  geom_ribbon(aes(ymin=b_trd-1.96*se_trd,
                  ymax=b_trd+1.96*se_trd),
              alpha=.5) +
  geom_hline(yintercept=0,linetype=2,size=1) +
  labs(x='Trade (log)',
       y='',
       title='U.S. Trade') +
  theme_classic() +
  theme(text=element_text(family='serif'),
        plot.title=element_text(hjust=.5))
grid.arrange(g1,g2,ncol=2)
