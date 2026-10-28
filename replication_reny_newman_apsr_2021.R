library(tidyverse)
library(haven)
library(lubridate)
library(rdrobust)
library(MASS)
library(broom)
library(simcf)
library(ggrepel)

########
# Figure 1: George Floyd Media Coverage, Social Media Posts, and Search Behavior ---
########

media <- read_csv('floyd_mediacloud.csv')
media <- media %>%
  rename('newspapers'='floyd',
         'day'='date') %>%
  mutate(day = lubridate::mdy(day)) %>%
  mutate(newspapers = (newspapers-min(newspapers))/(max(newspapers)-min(newspapers)))

twitter <- read_csv('floyd_twitter.csv')
twitter <- twitter %>%
  mutate(day = mdy(date)) %>%
  rename('twitter'='posts') %>%
  dplyr::select(day, twitter) %>%
  mutate(twitter = (twitter-min(twitter))/(max(twitter)-min(twitter)))

p1 <- left_join(media, twitter, by='day') %>%
  mutate(Twitter = case_when(
    is.na(twitter) ~ 0,
    TRUE ~ as.numeric(twitter)
  ), Newspapers=newspapers) %>%
  dplyr::select(day, newspapers, twitter) %>%
  pivot_longer(2:3) %>%
  mutate(name = case_when(
    name=='newspapers'~'Newspapers',
    name=='twitter'~'Twitter'
  )) %>%
  ggplot(aes(day, value, linetype=name, color=name)) +
  geom_line() +
  theme_bw() +
  labs(x='',y='Volume') +
  scale_color_brewer(palette='Dark2', name="'George Floyd'") +
  scale_linetype_manual(values=c(1,2,3), name="'George Floyd'") +
  scale_x_date(limits=c(ymd('2020-05-01'),ymd('2020-06-10'))) +
  geom_segment(aes(x=ymd('2020-05-25'), xend=ymd('2020-05-20'),y=0, yend=0.15), size=0.1) +
  geom_segment(aes(x=ymd('2020-05-26'), xend=ymd('2020-05-21'),y=0.12, yend=0.45),size=0.1) +
  geom_segment(aes(x=ymd('2020-05-27'), xend=ymd('2020-05-22'),y=0.38, yend=0.76),size=0.1) +
  geom_segment(aes(x=ymd('2020-05-28'), xend=ymd('2020-05-26'),y=0.6, yend=0.85), size=0.1) +
  annotate('text', x=ymd('2020-05-19'), y=0.15,label='Floyd\nKilled\nMay 25',size=3) +
  annotate('text', x=ymd('2020-05-19'), y=0.5,label='Video Released\nProtests Minnesota\nMay 26', size=3) +
  annotate('text', x=ymd('2020-05-21'), y=0.8,label='Protests Spread\nAcross US\nMay 27', size=3) +
  annotate('text', x=ymd('2020-05-26'), y=0.95,label='International\nProtests\nBegin\nMay 28', size=3) 
  ggsave(width=10,height=5, 'replication_figures_new/figure_1a.tiff')

# Insert the name of the working directory that you created above, where you are storing this file
path <- '' 

files <- list.files(path,full.names = T)
files <- files[grep('gtrends',files)]
terms <- c('Black Lives\nMatter','George Floyd','Police\nViolence',
           'Protest')
dat <- vector('list',length(files))
for(i in 1:length(dat)){
  dat[[i]] <- read_csv(files[i],col_names = c('day','volume'),skip=1) %>%
    mutate(term=terms[i])
  print(i)
}
dat <- do.call('rbind', dat)
dat$day <- mdy(dat$day)
p2 <- dat %>%
  dplyr::mutate(volume = as.numeric(volume)/100) %>%
  ggplot(aes(day, volume, linetype=term, color=term)) +
  geom_line() +
  theme_bw() +
  labs(x='',y='Volume') +
  scale_color_brewer(palette='Dark2', name="Google Trends") +
  scale_linetype_manual(values=c(1,2,3,4,5,6,7), name="Google Trends") +
  scale_x_date(limits=c(ymd('2020-05-01'),ymd('2020-06-10'))) 
  ggsave(width=10,height=5, 'replication_figures/figure_1b.tiff')

grobz <- gridExtra::arrangeGrob(p1,p2, ncol=1) 
ggsave(grobz, width=10, height=10,
       filename = 'replication_figures_new/figure_1_full.tiff')

### Nationscape Data
df <- read_dta('nationscape_clean_replication.dta')

# recode vars of interest
blacks <- df %>% filter(race_ethnicity == 2 & hispanic == 1)
whites <- df %>% filter(race_ethnicity == 1 & hispanic == 1)
latinos <- df %>% filter(hispanic > 1)
asians <- df %>% filter(race_ethnicity %in% c(4:14))

# scale for converting X-axis
conversion = df %>%
  dplyr::select(wave, day) %>%
  group_by(wave) %>%
  slice(which.min(day))


returnPlot <- function(full_plot, yaxis, title){
  plot <- full_plot$vars_bins %>%
    na.omit() %>%
    filter(rdplot_N != 1) %>%
    ggplot(aes(rdplot_mean_x, rdplot_mean_y)) +
    geom_point(size=1, alpha=0.5) +
    geom_line(data=full_plot$vars_poly[full_plot$vars_poly<0,],
              aes(rdplot_x, rdplot_y),
              color='red', size=0.5) +
    geom_line(data=full_plot$vars_poly[full_plot$vars_poly>0,],
              aes(rdplot_x, rdplot_y),
              color='red', size=0.5) +
    geom_vline(xintercept=0, linetype=2) +
    theme_bw() + 
    labs(x='Days',y=yaxis,title=title) + 
    theme(plot.title = element_text(hjust=0.5),
          axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0))) 
  return(plot)
}

####
# Main RDD Figure Full Sample ----
###

# FULL SAMPLE
full_plot <- rdplot(df$group_favorability_the_police,df$day_running,
                    p=1,kernel = 'uniform')
p1 <- returnPlot(full_plot,yaxis='Police Unfavorability',title='Full Sample') 

full_plot <- rdplot(df$discrimination_blacks,df$day_running,
                    p=1,kernel = 'uniform')
p2 <-returnPlot(full_plot=full_plot,yaxis='Discrimination is Problem',title='Full Sample')

# White sample
full_plot <- rdplot(whites$group_favorability_the_police,whites$day_running,
                    p=1,kernel = 'uniform')
p3 <- returnPlot(full_plot,yaxis='Police Unfavorability',title='Non-Hispanic White') 

full_plot <- rdplot(whites$discrimination_blacks,whites$day_running,
                    p=1,kernel = 'uniform')
p4 <- returnPlot(full_plot=full_plot,yaxis='Discrimination is Problem',title='Non-Hispanic White') 

# Black sample
full_plot <- rdplot(blacks$group_favorability_the_police,blacks$day_running,
                    p=1,kernel = 'uniform')
p5 <- returnPlot(full_plot,yaxis='Police Unfavorability',title='Black/African-American') 

full_plot <- rdplot(blacks$discrimination_blacks,blacks$day_running,
                    p=1,kernel = 'uniform')
p6 <- returnPlot(full_plot=full_plot,yaxis='Discrimination is Problem',title='Black/African-American') 

# Latino sample
full_plot <- rdplot(latinos$group_favorability_the_police,latinos$day_running,
                    p=1,kernel = 'uniform')
p7 <- returnPlot(full_plot,yaxis='Police Unfavorability',title='Latinos') 

full_plot <- rdplot(latinos$discrimination_blacks,latinos$day_running,
                    p=1,kernel = 'uniform')
p8 <- returnPlot(full_plot=full_plot,yaxis='Discrimination is Problem',title='Latinos') 

# Asians
full_plot <- rdplot(asians$group_favorability_the_police,asians$day_running,
                    p=1,kernel = 'uniform')
p9 <- returnPlot(full_plot,yaxis='Police Unfavorability',title='Asian Americans') 

full_plot <- rdplot(asians$discrimination_blacks,asians$day_running,
                    p=1,kernel = 'uniform')
p10 <- returnPlot(full_plot=full_plot,yaxis='Discrimination is Problem',title='Asian Americans') 

grobz <- gridExtra::arrangeGrob(p1,p2,p3,p4,p5,
                                p6,p7,p8,p9,p10, 
                                ncol=2)
ggsave(grobz, 
       width=12, 
       height=15,
       filename='replication_figures_new/figure_2_full.tiff')




####
# Table B.1 RDIT estimates ---
####

full_plot_est <- rdrobust(df$group_favorability_the_police,df$day_running,
                          p=1,kernel = 'triangular')
full_police <- tibble(coef=full_plot_est$coef, 
                      se=full_plot_est$se, 
                      pv=full_plot_est$pv) %>%
  mutate(sample='Full',
         outcome='Police')

full_plot_est_discrim <- rdrobust(df$discrimination_blacks,df$day_running,
                                  p=1,kernel = 'triangular')
full_discrim <- tibble(coef=full_plot_est_discrim$coef, 
                       se=full_plot_est_discrim$se, 
                       pv=full_plot_est_discrim$pv) %>%
  mutate(sample='Full',
         outcome='Discrim') 

fav_bws <- full_plot_est
dis_bws <- full_plot_est_discrim


# White Respondents Only
full_plot_est <- rdrobust(whites$group_favorability_the_police,whites$day_running,
                          p=1,kernel = 'triangular')
whites_police <- tibble(coef=full_plot_est$coef, 
                        se=full_plot_est$se, 
                        pv=full_plot_est$pv) %>%
  mutate(sample='White',
         outcome='Police')

full_plot_est_discrim <- rdrobust(whites$discrimination_blacks,whites$day_running,
                                  p=1,kernel = 'triangular')
whites_discrim <- tibble(coef=full_plot_est_discrim$coef, 
                         se=full_plot_est_discrim$se, 
                         pv=full_plot_est_discrim$pv) %>%
  mutate(sample='White',
         outcome='Discrim') 

# Black Respondents Only
full_plot_est <- rdrobust(blacks$group_favorability_the_police,blacks$day_running,
                          p=1,kernel = 'triangular')
blacks_police <- tibble(coef=full_plot_est$coef, 
                        se=full_plot_est$se, 
                        pv=full_plot_est$pv) %>%
  mutate(sample='Black',
         outcome='Police')

full_plot_est_discrim <- rdrobust(blacks$discrimination_blacks,blacks$day_running,
                                  p=1,kernel = 'triangular')
blacks_discrim <- tibble(coef=full_plot_est_discrim$coef, 
                         se=full_plot_est_discrim$se, 
                         pv=full_plot_est_discrim$pv) %>%
  mutate(sample='Black',
         outcome='Discrim') 

# Latino Responents Only
full_plot_est <- rdrobust(latinos$group_favorability_the_police,latinos$day_running,
                          p=1,kernel = 'triangular')
latinos_police <- tibble(coef=full_plot_est$coef, 
                         se=full_plot_est$se, 
                         pv=full_plot_est$pv) %>%
  mutate(sample='Latino',
         outcome='Police')

full_plot_est_discrim <- rdrobust(latinos$discrimination_blacks,latinos$day_running,
                                  p=1,kernel = 'triangular')
latinos_discrim <- tibble(coef=full_plot_est_discrim$coef, 
                          se=full_plot_est_discrim$se, 
                          pv=full_plot_est_discrim$pv) %>%
  mutate(sample='Latino',
         outcome='Discrim') 

# Asian Respondents Only
full_plot_est <- rdrobust(asians$group_favorability_the_police,asians$day_running,
                          p=1,kernel = 'triangular')
asians_police <- tibble(coef=full_plot_est$coef, 
                        se=full_plot_est$se, 
                        pv=full_plot_est$pv) %>%
  mutate(sample='Asian American',
         outcome='Police')

full_plot_est_discrim <- rdrobust(asians$discrimination_blacks,asians$day_running,
                                  p=1,kernel = 'triangular')
asians_discrim <- tibble(coef=full_plot_est_discrim$coef, 
                         se=full_plot_est_discrim$se, 
                         pv=full_plot_est_discrim$pv) %>%
  mutate(sample='Asian American',
         outcome='Discrim') 

obj <- bind_rows(
  full_police, whites_police, blacks_police,latinos_police,asians_police,
  full_discrim, whites_discrim, blacks_discrim, latinos_discrim, asians_discrim
) %>% 
  as.data.frame() 

obj[ , 1:3] <- apply(obj[ , 1:3], 2, \(col){
  col <- unlist(col)
  col <- as.numeric(col)
  col <- round(col, 2)
}
)

obj %>% write_csv(file = 'replication_figures_new/tableb1.csv')

#######
# Table B.2 ---- 
#######

res <- rdrobust(df$pid7,df$day_running, p=1,kernel = 'triangular')
r1 <- tibble(coef=res$coef, se=res$se, pv=res$pv) %>%
  mutate(pct = round(coef[,"Coeff"] / sd(df$pid7, na.rm=T)*100,1))
r1


res2 <- rdrobust(df$group_fav_white_black,df$day_running, p=1,kernel = 'triangular')
r2 <- tibble(coef=res2$coef, se=res2$se, pv=res2$pv) %>%
  mutate(pct = round(coef[,"Coeff"] / sd(df$group_fav_white_black, na.rm=T)*100,1))
r2


res3 <- rdrobust(df$racial_attitudes_generations,df$day_running, p=1,kernel = 'triangular')
r3 <- tibble(coef=res3$coef, se=res3$se, pv=res3$pv)%>%
  mutate(pct = round(coef[,"Coeff"] / sd(df$racial_attitudes_generations, na.rm=T)*100,1))
r3

results <- bind_rows(r1, r2, r3)
results <- apply(results, 2, unlist)
results <- as.data.frame(results)

results %>%
  mutate(outcomes = c(
    'pid7','pid7','pid7',
    'group_fav_white_black','group_fav_white_black','group_fav_white_black',
    'racial_attitudes_generations','racial_attitudes_generations','racial_attitudes_generations'
  ))%>%
  write_csv(file = 'replication_figures_new/tableb2.csv')


####
# Table B.3 ---
####

# clustered standard errors by week
full_plot_est <- rdrobust(df$group_favorability_the_police,df$day_running,
                          p=1,kernel = 'triangular',
                          cluster = df$wave,
                          h = fav_bws$bws[1,1],
                          b = fav_bws$bws[2,1])
t1 <- tibble(coef=full_plot_est$coef, 
       se=full_plot_est$se, 
       pv=full_plot_est$pv)

full_plot_est_discrim <- rdrobust(df$discrimination_blacks,df$day_running,
                                  p=1,kernel = 'triangular',
                                  cluster=df$wave,
                                  h = dis_bws$bws[1,1],
                                  b = dis_bws$bws[2,1])
t2 <- tibble(coef=full_plot_est_discrim$coef, 
       se=full_plot_est_discrim$se, 
       pv=full_plot_est_discrim$pv)


# clustered standard errors by day
full_plot_est <- rdrobust(df$group_favorability_the_police,df$day_running,
                          p=1,kernel = 'triangular',
                          cluster = df$day,
                          h = fav_bws$bws[1,1],
                          b = fav_bws$bws[2,1])
t3 <- tibble(coef=full_plot_est$coef, 
       se=full_plot_est$se, 
       pv=full_plot_est$pv) 

full_plot_est_discrim <- rdrobust(df$discrimination_blacks,df$day_running,
                                  p=1,kernel = 'triangular',
                                  cluster=df$day,
                                  h = dis_bws$bws[1,1],
                                  b = dis_bws$bws[2,1])
t4 <- tibble(coef=full_plot_est_discrim$coef, 
       se=full_plot_est_discrim$se, 
       pv=full_plot_est_discrim$pv)

results2 <- apply(bind_rows(t1, t2, t3, t4), 2, unlist)
results2 <- as.data.frame(results2)

results2 %>%
  write_csv('~/replication_figures_new/tableb3.csv')



#####
# Figure 3: Coefficient and Predicted Value Plots for Prejudice and Partisanship ----
#####


p.out1 <- vector('list',60)
for(i in 1:60){
  res <- polr(factor(group_favorability_the_police) ~ group_fav_white_black_flip + college + female + age + race_white + household_income + pid7 + ideo5, 
              data=df[df$wave == i,], 
              Hess = T)
  p.out1[[i]] <- tidy(res) %>%
    dplyr::mutate(wave = i)
  print(i)
}

coef_black_white <- bind_rows(p.out1) %>% 
  filter(term=='group_fav_white_black_flip') %>%
  mutate(y=estimate,l=estimate-1.96*std.error,u=estimate+1.96*std.error) %>%
  ggplot(aes(wave, y,ymin=l,ymax=u)) +
  geom_vline(xintercept=45.86,linetype=2,color='black') +
  geom_errorbar(width=0.1, position=position_dodge(width=0.3), size=0.5) +
  geom_point(position=position_dodge(width=0.3),fill='white') +
  theme_bw() +
  labs(y='Coefficient White-Black Fav',x='', title='(A)') +
  scale_shape_manual(values=c(21,24), name='') +
  scale_color_brewer(palette='Set2',name='') +
  scale_x_continuous(breaks=1:60,labels=conversion$day) +
  theme(axis.text.x = element_text(angle=45,hjust=1,size=4.5)) +
  geom_hline(yintercept=0, linetype=2, color='red') +
  theme(plot.title = element_text(hjust=0.5)) 

coef_pid <- bind_rows(p.out1) %>% 
  filter(term=='pid7') %>%
  mutate(estimate = estimate*-1) %>%
  mutate(y=estimate,l=estimate-1.96*std.error,u=estimate+1.96*std.error) %>%
  ggplot(aes(wave, y,ymin=l,ymax=u)) +
  geom_vline(xintercept=45.86,linetype=2,color='black') +
  geom_errorbar(width=0.1, position=position_dodge(width=0.3), size=0.5) +
  geom_point(position=position_dodge(width=0.3),fill='white') +
  theme_bw() +
  labs(y='Party ID (R to D)',x='') +
  scale_shape_manual(values=c(21,24), name='') +
  scale_color_brewer(palette='Set2',name='') +
  scale_x_continuous(breaks=1:60,labels=conversion$day) +
  theme(axis.text.x = element_text(angle=45,hjust=1,size=4.5)) +
  geom_hline(yintercept=0, linetype=2, color='red') 

simOutPrej1 <- function(lm.out){
  form <- as.formula(lm.out$call$formula)
  simbetas <- MASS::mvrnorm(1000, c(coef(lm.out),lm.out$zeta),vcov(lm.out))
  tempdat <- extractdata(form, df,na.rm = T)
  mm <- cfMake(form, tempdat, nscen=2)
  mm <- cfChange(mm, 'group_fav_white_black', x=-3, scen=1)
  mm <- cfChange(mm, 'group_fav_white_black', x=3, scen=2)
  out <- oprobitsimev(mm, simbetas, constant=NA, cat=4)
  out <- data.frame(x=c('Low Prejudice','High Prejudice'),
                    y=out$pe[,4],
                    l=out$lower[,4],
                    u=out$upper[,4])
  return(out)
}


p.out1 <- vector('list',60)
for(i in 1:60){
  res <- polr(factor(group_favorability_the_police) ~ group_fav_white_black + college + female + age + race_white + household_income + pid7 + ideo5,
              data=df[df$wave == i,], 
              Hess = T)
  p.out1[[i]] <- simOutPrej1(res) %>%
    dplyr::mutate(wave = i)
  print(i)
}

pp_prej <- bind_rows(p.out1) %>% 
  mutate(x=factor(x, levels=c(
    'Low Prejudice','High Prejudice'
  ))) %>%
  ggplot(aes(wave, y,ymin=l,ymax=u, color=x, shape=x)) +
  geom_vline(xintercept=45.86,linetype=2,color='black') +
  geom_errorbar(width=0.1, position=position_dodge(width=0.3), size=0.5) +
  geom_point(position=position_dodge(width=0.3),fill='white') +
  theme_bw() +
  labs(y='Pr(Very Unfavorable)',x='', title='(B)') +
  scale_shape_manual(values=c(21,24), name='') +
  scale_color_brewer(palette='Set1',name='',direction = -1) +
  scale_x_continuous(breaks=1:60,labels=conversion$day) +
  theme(axis.text.x = element_text(angle=45,hjust=1,size=4.5)) +
  theme(plot.title = element_text(hjust=0.5))
pp_prej

simOutPID <- function(lm.out){
  form <- as.formula(lm.out$call$formula)
  simbetas <- MASS::mvrnorm(1000, c(coef(lm.out),lm.out$zeta),vcov(lm.out))
  tempdat <- extractdata(form, df,na.rm = T)
  mm <- cfMake(form, tempdat, nscen=2)
  mm <- cfChange(mm, 'pid7', x=1, scen=1)
  mm <- cfChange(mm, 'pid7', x=7, scen=2)
  out <- oprobitsimev(mm, simbetas, constant=NA, cat=4)
  out <- data.frame(x=c('Strong Democrat','Strong Republican'),
                    y=out$pe[,4],
                    l=out$lower[,4],
                    u=out$upper[,4])
  return(out)
}

p.out1 <- vector('list',60)
for(i in 1:60){
  res <- polr(factor(group_favorability_the_police) ~ group_fav_white_black + college + female + age + race_white + household_income + pid7 + ideo5,
              data=df[df$wave == i,], 
              Hess = T)
  p.out1[[i]] <- simOutPID(res) %>%
    dplyr::mutate(wave = i)
  print(i)
}

pp_pid <- bind_rows(p.out1) %>% 
  ggplot(aes(wave, y,ymin=l,ymax=u, color=x, shape=x)) +
  geom_vline(xintercept=45.86,linetype=2,color='black') +
  geom_errorbar(width=0.1, position=position_dodge(width=0.3), size=0.5) +
  geom_point(position=position_dodge(width=0.3),fill='white') +
  theme_bw() +
  labs(y='Pr(Very Unfavorable)',x='') +
  scale_shape_manual(values=c(21,24), name='') +
  scale_color_brewer(palette='Set1',name='',direction = -1) +
  scale_x_continuous(breaks=1:60,labels=conversion$day) +
  theme(axis.text.x = element_text(angle=45,hjust=1,size=4.5))
pp_pid

p.out1 <- vector('list',60)
for(i in 1:60){
  res <- polr(factor(group_favorability_the_police) ~ racial_attitudes_marry + college + female + age + race_white + household_income + pid7 + ideo5,
              data=df[df$wave == i,], 
              Hess = T)
  p.out1[[i]] <- tidy(res) %>%
    dplyr::mutate(wave = i)
  print(i)
}

coef_prej2 <- bind_rows(p.out1) %>% 
  filter(term=='racial_attitudes_marry') %>%
  mutate(y=estimate,
         l=estimate-1.96*std.error,
         u=estimate+1.96*std.error) %>%
  ggplot(aes(wave, y,ymin=l,ymax=u)) +
  geom_vline(xintercept=45.86,linetype=2,color='black') +
  geom_hline(yintercept=0, linetype=2, color='red')+
  geom_errorbar(width=0.1, position=position_dodge(width=0.3), size=0.5) +
  geom_point(position=position_dodge(width=0.3),fill='white') +
  theme_bw() +
  labs(y='Coefficient Interracial Marriage',x='') +
  scale_x_continuous(breaks=1:60,labels=conversion$day) +
  theme(axis.text.x = element_text(angle=45,hjust=1,size=4.5)) 

simOutPrej2 <- function(lm.out){
  form <- as.formula(lm.out$call$formula)
  simbetas <- MASS::mvrnorm(1000, c(coef(lm.out),lm.out$zeta),vcov(lm.out))
  tempdat <- extractdata(form, df,na.rm = T)
  mm <- cfMake(form, tempdat, nscen=2)
  mm <- cfChange(mm, 'racial_attitudes_marry', x=5, scen=1)
  mm <- cfChange(mm, 'racial_attitudes_marry', x=1, scen=2)
  out <- oprobitsimev(mm, simbetas, constant=NA, cat=4)
  out <- data.frame(x=c('Low Prejudice','High Prejudice'),
                    y=out$pe[,4],
                    l=out$lower[,4],
                    u=out$upper[,4])
  return(out)
}


p.out2 <- vector('list',60)
for(i in 1:60){
  res <- polr(factor(group_favorability_the_police) ~ racial_attitudes_marry + college + female + age + race_white + household_income + pid7 + ideo5,
              data=df[df$wave == i,], 
              Hess = T)
  p.out2[[i]] <- simOutPrej2(res) %>%
    dplyr::mutate(wave = i)
  print(i)
}

pp_prej2 <- bind_rows(p.out2) %>% 
  mutate(x=factor(x, levels=c(
    'Low Prejudice','High Prejudice'
  ))) %>%
  ggplot(aes(wave, y,ymin=l,ymax=u, color=x, shape=x)) +
  geom_vline(xintercept=45.86,linetype=2,color='black') +
  geom_errorbar(width=0.1, position=position_dodge(width=0.3), size=0.5) +
  geom_point(position=position_dodge(width=0.3),fill='white') +
  theme_bw() +
  labs(y='Pr(Very Unfavorable)',x='') +
  scale_shape_manual(values=c(21,24), name='') +
  scale_color_brewer(palette='Set1',name='', direction=-1) +
  scale_x_continuous(breaks=1:60,labels=conversion$day) +
  theme(axis.text.x = element_text(angle=45,hjust=1,size=4.5)) 
pp_prej2

p.out3 <- vector('list',60)
for(i in 1:60){
  res <- polr(factor(group_favorability_the_police) ~ racial_attitudes_generations_flip + college + female + age + race_white + household_income + pid7 + ideo5,
              data=df[df$wave == i,], 
              Hess = T)
  p.out3[[i]] <- tidy(res) %>%
    dplyr::mutate(wave = i)
  print(i)
}

coef_gen <- bind_rows(p.out3) %>% 
  filter(term=='racial_attitudes_generations_flip') %>%
  mutate(y=estimate,
         l=estimate-1.96*std.error,
         u=estimate+1.96*std.error) %>%
  ggplot(aes(wave, y,ymin=l,ymax=u)) +
  geom_vline(xintercept=45.86,linetype=2,color='black') +
  geom_hline(yintercept=0, linetype=2, color='red')+
  geom_errorbar(width=0.1, position=position_dodge(width=0.3), size=0.5) +
  geom_point(position=position_dodge(width=0.3),fill='white') +
  theme_bw() +
  labs(y='Coefficient RR Generations',x='') +
  scale_x_continuous(breaks=1:60,labels=conversion$day) +
  theme(axis.text.x = element_text(angle=45,hjust=1,size=4.5)) 

simOutPrej3 <- function(lm.out){
  form <- as.formula(lm.out$call$formula)
  simbetas <- MASS::mvrnorm(1000, c(coef(lm.out),lm.out$zeta),vcov(lm.out))
  tempdat <- extractdata(form, df,na.rm = T)
  mm <- cfMake(form, tempdat, nscen=2)
  mm <- cfChange(mm, 'racial_attitudes_generations', x=1, scen=1)
  mm <- cfChange(mm, 'racial_attitudes_generations', x=5, scen=2)
  out <- oprobitsimev(mm, simbetas, constant=NA, cat=4)
  out <- data.frame(x=c('Low Prejudice','High Prejudice'),
                    y=out$pe[,4],
                    l=out$lower[,4],
                    u=out$upper[,4])
  return(out)
}

p.out3 <- vector('list',60)
for(i in 1:60){
  res <- polr(factor(group_favorability_the_police) ~ racial_attitudes_generations + college + female + age + race_white + household_income + pid7 + ideo5,
              data=df[df$wave == i,], 
              Hess = T)
  p.out3[[i]] <- simOutPrej3(res) %>%
    dplyr::mutate(wave = i)
  print(i)
}

pp_gen <- bind_rows(p.out3) %>% 
  mutate(x=factor(x, levels=c(
    'Low Prejudice','High Prejudice'
  ))) %>%
  ggplot(aes(wave, y,ymin=l,ymax=u, color=x, shape=x)) +
  geom_vline(xintercept=45.86,linetype=2,color='black') +
  geom_errorbar(width=0.1, position=position_dodge(width=0.3), size=0.5) +
  geom_point(position=position_dodge(width=0.3),fill='white') +
  theme_bw() +
  labs(y='Pr(Very Unfavorable)',x='') +
  scale_shape_manual(values=c(21,24), name='') +
  scale_color_brewer(palette='Set1',name='',direction=-1) +
  scale_x_continuous(breaks=1:60,labels=conversion$day) +
  theme(axis.text.x = element_text(angle=45,hjust=1,size=4.5)) 
pp_gen

grobz <- gridExtra::arrangeGrob(coef_black_white,pp_prej,
                                coef_gen,pp_gen,
                                coef_pid,pp_pid,
                                ncol=2)
grobz
ggsave(grobz,width=14, height=9, 
       filename = 'replication_figures_new/figure_3_full.tiff')



#######
# Figure B.1 ----
# same analysis but discrimination as outcome
#######

p.out1 <- vector('list',60)
for(i in 1:60){
  res <- polr(factor(discrimination_blacks) ~ group_fav_white_black_flip + college + female + age + race_white + household_income + pid7 + ideo5, 
              data=df[df$wave == i,], 
              Hess = T)
  p.out1[[i]] <- tidy(res) %>%
    dplyr::mutate(wave = i)
  print(i)
}

coef_black_white <- bind_rows(p.out1) %>% 
  filter(term=='group_fav_white_black_flip') %>%
  mutate(y=estimate,l=estimate-1.96*std.error,u=estimate+1.96*std.error) %>%
  ggplot(aes(wave, y,ymin=l,ymax=u)) +
  geom_vline(xintercept=45.86,linetype=2,color='black') +
  geom_errorbar(width=0.1, position=position_dodge(width=0.3), size=0.5) +
  geom_point(position=position_dodge(width=0.3),fill='white') +
  theme_bw() +
  labs(y='Coefficient White-Black Fav',x='') +
  scale_shape_manual(values=c(21,24), name='') +
  scale_color_brewer(palette='Set2',name='') +
  scale_x_continuous(breaks=1:60,labels=conversion$day) +
  theme(axis.text.x = element_text(angle=45,hjust=1,size=5.5)) +
  geom_hline(yintercept=0, linetype=2, color='red')

coef_pid <- bind_rows(p.out1) %>% 
  filter(term=='pid7') %>%
  mutate(estimate = estimate*-1) %>%
  mutate(y=estimate,l=estimate-1.96*std.error,u=estimate+1.96*std.error) %>%
  ggplot(aes(wave, y,ymin=l,ymax=u)) +
  geom_vline(xintercept=45.86,linetype=2,color='black') +
  geom_errorbar(width=0.1, position=position_dodge(width=0.3), size=0.5) +
  geom_point(position=position_dodge(width=0.3),fill='white') +
  theme_bw() +
  labs(y='Party ID (R to D)',x='') +
  scale_shape_manual(values=c(21,24), name='') +
  scale_color_brewer(palette='Set2',name='') +
  scale_x_continuous(breaks=1:60,labels=conversion$day) +
  theme(axis.text.x = element_text(angle=45,hjust=1,size=5.5)) +
  geom_hline(yintercept=0, linetype=2, color='red') 

simOutPrej1 <- function(lm.out){
  form <- as.formula(lm.out$call$formula)
  simbetas <- MASS::mvrnorm(1000, c(coef(lm.out),lm.out$zeta),vcov(lm.out))
  tempdat <- extractdata(form, df,na.rm = T)
  mm <- cfMake(form, tempdat, nscen=2)
  mm <- cfChange(mm, 'group_fav_white_black', x=-3, scen=1)
  mm <- cfChange(mm, 'group_fav_white_black', x=3, scen=2)
  out <- oprobitsimev(mm, simbetas, constant=NA, cat=5)
  out <- data.frame(x=c('Low Prejudice','High Prejudice'),
                    y=out$pe[,5],
                    l=out$lower[,5],
                    u=out$upper[,5])
  return(out)
}

p.out1 <- vector('list',60)
for(i in 1:60){
  res <- polr(factor(discrimination_blacks) ~ group_fav_white_black + college + female + age + race_white + household_income + pid7 + ideo5, 
              data=df[df$wave == i,], 
              Hess = T)
  p.out1[[i]] <- simOutPrej1(res) %>%
    dplyr::mutate(wave = i)
  print(i)
}

pp_prej <- bind_rows(p.out1) %>% 
  ggplot(aes(wave, y,ymin=l,ymax=u, color=x, shape=x)) +
  geom_vline(xintercept=45.86,linetype=2,color='black') +
  geom_errorbar(width=0.1, position=position_dodge(width=0.3), size=0.5) +
  geom_point(position=position_dodge(width=0.3),fill='white') +
  theme_bw() +
  labs(y='Pr(A Great Deal)',x='') +
  scale_shape_manual(values=c(21,24), name='') +
  scale_color_brewer(palette='Set1',name='') +
  scale_x_continuous(breaks=1:60,labels=conversion$day) +
  theme(axis.text.x = element_text(angle=45,hjust=1,size=5.5)) 

simOutPID <- function(lm.out){
  form <- as.formula(lm.out$call$formula)
  simbetas <- MASS::mvrnorm(1000, c(coef(lm.out),lm.out$zeta),vcov(lm.out))
  tempdat <- extractdata(form, df,na.rm = T)
  mm <- cfMake(form, tempdat, nscen=2)
  mm <- cfChange(mm, 'pid7', x=1, scen=1)
  mm <- cfChange(mm, 'pid7', x=7, scen=2)
  out <- oprobitsimev(mm, simbetas, constant=NA, cat=5)
  out <- data.frame(x=c('Strong Democrat','Strong Republican'),
                    y=out$pe[,5],
                    l=out$lower[,5],
                    u=out$upper[,5])
  return(out)
}

p.out1 <- vector('list',60)
for(i in 1:60){
  res <- polr(factor(discrimination_blacks) ~ group_fav_white_black + college + female + age + race_white + household_income + pid7 + ideo5, 
              data=df[df$wave == i,], 
              Hess = T)
  p.out1[[i]] <- simOutPID(res) %>%
    dplyr::mutate(wave = i)
  print(i)
}

pp_pid <- bind_rows(p.out1) %>% 
  ggplot(aes(wave, y,ymin=l,ymax=u, color=x, shape=x)) +
  geom_vline(xintercept=45.86,linetype=2,color='black') +
  geom_errorbar(width=0.1, position=position_dodge(width=0.3), size=0.5) +
  geom_point(position=position_dodge(width=0.3),fill='white') +
  theme_bw() +
  labs(y='Pr(A Great Deal)',x='') +
  scale_shape_manual(values=c(21,24), name='') +
  scale_color_brewer(palette='Set1',direction=-1,name='') +
  scale_x_continuous(breaks=1:60,labels=conversion$day) +
  theme(axis.text.x = element_text(angle=45,hjust=1,size=5.5)) 

p.out3 <- vector('list',60)
for(i in 1:60){
  res <- polr(factor(discrimination_blacks) ~ racial_attitudes_generations_flip + college + female + age + race_white + household_income + pid7 + ideo5, 
              data=df[df$wave == i,], 
              Hess = T)
  p.out3[[i]] <- tidy(res) %>%
    dplyr::mutate(wave = i)
  print(i)
}

coef_gen <- bind_rows(p.out3) %>% 
  filter(term=='racial_attitudes_generations_flip') %>%
  mutate(y=estimate,
         l=estimate-1.96*std.error,
         u=estimate+1.96*std.error) %>%
  ggplot(aes(wave, y,ymin=l,ymax=u)) +
  geom_vline(xintercept=45.86,linetype=2,color='black') +
  geom_hline(yintercept=0, linetype=2, color='red')+
  geom_errorbar(width=0.1, position=position_dodge(width=0.3), size=0.5) +
  geom_point(position=position_dodge(width=0.3),fill='white') +
  theme_bw() +
  labs(y='Coefficient RR Generations',x='') +
  scale_x_continuous(breaks=1:60,labels=conversion$day) +
  theme(axis.text.x = element_text(angle=45,hjust=1,size=5.5)) 

simOutPrej3 <- function(lm.out){
  form <- as.formula(lm.out$call$formula)
  simbetas <- MASS::mvrnorm(1000, c(coef(lm.out),lm.out$zeta),vcov(lm.out))
  tempdat <- extractdata(form, df,na.rm = T)
  mm <- cfMake(form, tempdat, nscen=2)
  mm <- cfChange(mm, 'racial_attitudes_generations', x=1, scen=1)
  mm <- cfChange(mm, 'racial_attitudes_generations', x=5, scen=2)
  out <- oprobitsimev(mm, simbetas, constant=NA, cat=5)
  out <- data.frame(x=c('Low Prejudice','High Prejudice'),
                    y=out$pe[,5],
                    l=out$lower[,5],
                    u=out$upper[,5])
  return(out)
}


p.out3 <- vector('list',60)
for(i in 1:60){
  res <- polr(factor(discrimination_blacks) ~ racial_attitudes_generations + college + female + age + race_white + household_income + pid7 + ideo5, 
              data=df[df$wave == i,], 
              Hess = T)
  p.out3[[i]] <- simOutPrej3(res) %>%
    dplyr::mutate(wave = i)
  print(i)
}

pp_gen <- bind_rows(p.out3) %>% 
  ggplot(aes(wave, y,ymin=l,ymax=u, color=x, shape=x)) +
  geom_vline(xintercept=45.86,linetype=2,color='black') +
  geom_errorbar(width=0.1, position=position_dodge(width=0.3), size=0.5) +
  geom_point(position=position_dodge(width=0.3),fill='white') +
  theme_bw() +
  labs(y='Pr(A Great Deal)',x='') +
  scale_shape_manual(values=c(21,24), name='') +
  scale_color_brewer(palette='Set1',name='') +
  scale_x_continuous(breaks=1:60,labels=conversion$day) +
  theme(axis.text.x = element_text(angle=45,hjust=1,size=5.5)) 

grobz <- gridExtra::arrangeGrob(coef_black_white,pp_prej,
                                coef_gen,pp_gen,
                                coef_pid,pp_pid,
                                ncol=2)
ggsave(grobz,width=14, height=9, 
       filename = 'replication_figures_new/figure_B1.tiff')


#####
# Figure 4: media and protest ----
#####


estRd <- function(df, sample,moderator){
  full_plot_est <- rdrobust(df$group_favorability_the_police,
                            df$day_running,
                            p=1,
                            kernel = 'triangular')
  out <- data.frame(coef=full_plot_est$coef[1,1], 
                    se=full_plot_est$se[1,1]) %>%
    mutate(sample=sample,moderator=moderator)
  return(out)
}


a <- estRd(df, sample='Baseline','Full\nSample')
b <- estRd(df[df$interest==4,], sample='Hardly at all','Follow Politics')
c <- estRd(df[df$interest==3,], sample='Only now and then','Follow Politics')
d <- estRd(df[df$interest==2,], sample='Some of the time','Follow Politics')
e <- estRd(df[df$interest==1,], sample='Most of the time','Follow Politics')

f <- estRd(df[df$cable == 1,], sample='Cable TV', "Media Consumption")
g <- estRd(df[df$network_tv ==1,], sample='Network TV', "Media Consumption")
h <- estRd(df[df$newspapers ==1,], sample='Newspapers', "Media Consumption")
i <- estRd(df[df$radio == 1,], sample='Radio', "Media Consumption")
j <- estRd(df[df$social_media == 1,], sample='Social Media', "Media Consumption")
k <- estRd(df[df$media_scale == 0,], sample='No Media', "Media Consumption")

w <- estRd(df[df$protest==0,], sample='No Protests','Protest')
x <- estRd(df[df$protest==1,], sample='Any Protests','Protest')
y <- estRd(df[df$n_protests>1,], sample='More than 1 Protest','Protest')
z <- estRd(df[df$n_protests>2,], sample='More than 2 Protests','Protest')

a$moderator <- ""
out_all <- bind_rows(a,b,c,d,e,f,g,h,i,j,k,w,x,y,z)

plot1b <- out_all %>%
  mutate(moderator = factor(moderator, levels=rev(c(
    'Media Consumption','Follow Politics','Protest',''
  ))), sample=factor(sample, levels=c(
    'Baseline','Most of the time','Some of the time','Only now and then','Hardly at all',
    'Social Media','Radio','Newspapers','Network TV','Cable TV', 'No Media',
    'More than 2 Protests','More than 1 Protest','Any Protests','No Protests'
  ))) %>%
  mutate(u = coef + 1.96 * se,
         l = coef - 1.96 * se) %>%
  ggplot(aes(x=sample, y=coef, ymin=l,ymax=u)) +
  geom_hline(yintercept=0) +
  geom_errorbar(width=0.1) +
  geom_point() + 
  coord_flip() +
  theme(legend.position = 'bottom')+
  facet_grid(moderator~.,switch = 'y',scales = 'free_y',space='free_y') +
  labs(x='',y='RDiT Estimate',title='(A)')+
  theme(plot.title = element_text(hjust=0.5)) +
  theme_bw() 


simOutMedia <- function(lm.out){
  form <- as.formula(lm.out$call$formula)
  simbetas <- MASS::mvrnorm(1000, c(coef(lm.out),lm.out$zeta),vcov(lm.out))
  tempdat <- extractdata(form, df,na.rm = T)
  mm <- cfMake(form, tempdat, nscen=2)
  mm <- cfChange(mm, 'fox_only', x=0, scen=1)
  mm <- cfChange(mm, 'fox_only', x=1, scen=2)
  out <- oprobitsimev(mm, simbetas, constant=NA, cat=4)
  out <- data.frame(x=c('MSNBC','Fox'),
                    y=out$pe[,4],
                    l=out$lower[,4],
                    u=out$upper[,4])
  return(out)
}
p.out <- vector('list',60)
for(i in 1:60){
  res <- polr(factor(group_favorability_the_police) ~ fox_only + racial_attitudes_generations_flip + college + female + age + race_white + household_income + pid7 + ideo5, 
              data=df[df$wave == i,], 
              Hess = T)
  p.out[[i]] <- simOutMedia(res) %>%
    dplyr::mutate(wave = i)
  print(i)
}

plot2 <- bind_rows(p.out) %>%
  filter(wave>31) %>%
  ggplot(aes(wave, y,ymin=l,ymax=u, color=x, shape=x)) +
  geom_vline(xintercept=45.86,linetype=2,color='black') +
  geom_errorbar(width=0.1, position=position_dodge(width=0.3), size=0.5) +
  geom_point(position=position_dodge(width=0.3),fill='white') +
  theme_bw() +
  labs(y='Pr(Very Unfavorable)',x='', title='(B)') +
  scale_shape_manual(values=c(21,24), name='') +
  scale_color_brewer(palette='Set1',name='') +
  scale_x_continuous(breaks=1:60,labels=conversion$day) +
  theme(axis.text.x = element_text(angle=45,hjust=1,size=5.5)) +
  theme(plot.title = element_text(hjust=0.5)) +
  theme(legend.position='bottom',
        legend.margin=margin(t=-0.5, r=0, b=0, l=0, unit="cm")) 


#combine plots
library(ggpubr)
plots <- ggarrange(plot1b, NULL, plot2,
                   heights = c(6,6, 6),
                   widths = c(4,0.25,6),
                   ncol = 3, nrow = 1)
ggsave(plots,width=8,height=4,
       filename = 'replication_figures_new/figure_4.tiff')




####
# Balance Tables ----
####

# Figure A1
counts <- df %>%
  filter(day_running %in% c(-10:10)) %>% 
  group_by(day_running) %>% 
  summarise(n=n()) 
counts %>% 
  ggplot(aes(x=day_running, y=n)) + 
  geom_bar(stat='identity', fill='white', color='black') +
  theme_bw() +
  labs(x='Day',y='Number of Interviews') +
  geom_vline(xintercept=0, color='red', linetype=2) 
  ggsave(width=3,height=3, file='replication_figures_new/figure_A1.tiff')
t.test(counts$n[counts$day_running < 0],counts$n[counts$day_running >= 0])

# Table A.1
count <- df %>%
  filter(day_running %in% c(-10:10)) %>% 
  group_by(post = day_running >= 0) %>% 
  summarise(age = weighted.mean(age, na.rm=T, w=weight),
            female = weighted.mean(female==1, na.rm=T, w=weight),
            white = weighted.mean(race_white, na.rm=T, w=weight),
            black = weighted.mean(race_black, na.rm=T, w=weight),
            college = weighted.mean(college==1, na.rm=T, w=weight),
            income = weighted.mean(household_income,na.rm=T, w=weight),
            republican = weighted.mean(pid7,na.rm=T, w=weight),
            ideo = weighted.mean(ideo5,na.rm=T,w=weight),
            clinton = weighted.mean(vote_clinton==1,na.rm=T,w=weight)) %>%
  t() %>%
  round(2) %>%
  as.data.frame() %>%
  mutate(diff = abs(V1 - V2))

wtdt <- function(var, var_name){
  require(weights)
  out <- wtd.t.test(var[df$day_running < 0 & df$day_running >=-10],
                    var[df$day_running >= 0 & df$day_running <= 10],
                    weight=df$weight[df$day_running < 0 & df$day_running >=-10],
                    weighty=df$weight[df$day_running >= 0 & df$day_running <= 10])
  return(data.frame(diff=out$additional[1],p=out$coefficients[3], var=var_name))
}
p <- bind_rows(wtdt(df$age, 'Age'),
          wtdt(df$female, 'Female'),
          wtdt(df$race_white, 'White'),
          wtdt(df$race_black, 'Black'),
          wtdt(df$college, 'College'),
          wtdt(df$household_income, 'Income'),
          #wtdt(df$democrat, 'Democrat'),
          #wtdt(df$republican, 'Republican'),
          wtdt(df$pid7, 'PID7'),
          wtdt(df$ideo5, 'Ideology'),
          wtdt(df$vote_clinton, 'Vote Clinton')
)
count %>% mutate(p=c(NA,round(p$p, 2))) %>%
  slice(-1) %>%
  rename(pre=V1, post=V2) %>%
  write_csv('replication_figures_new/tablea1.csv')

df %>%
  filter(day_running %in% c(-10:10)) %>% 
  group_by(day_running) %>%
  summarise(days = n()) %>%
  group_by(post = day_running >= 0) %>%
  summarise(mean(days))

######
# Figure A.2 ----
######

# cutpoints
smalldat <- df[df$day_running >= -36,]
c_list <- -7:7
results <- vector('list',length(c_list))
for(i in 1:length(c_list)){
  out <- rdrobust(df$group_favorability_the_police,df$day_running,
                  p=1,kernel = 'triangular',c=c_list[i])
  results[[i]] <- tibble(coef=out$coef,
                         se=out$se, 
                         pv=out$pv,
                         c=c_list[i])
  print(i)
}
out <- do.call('rbind',results)
out$l <- out$coef - 1.96*out$se
out$u <- out$coef + 1.96*out$se
out$estimate <- rep(c('Traditional','Bias-Adjusted','Robust'),nrow(out)/3)
out %>%
  filter(estimate=='Traditional' & c %in% c(-7:7)) %>%
  ggplot(aes(c, coef, ymin=l, ymax=u)) +
  theme_bw() +
  geom_hline(yintercept=0, linetype=2, color='red') +
  geom_errorbar(width=0.1) +
  geom_point(shape=21, size=3, fill='white') +
  geom_point(data=out[out$c==0 & out$estimate == "Traditional",],
             shape=21, size=3, fill='red') +
  scale_x_continuous(breaks=-7:7) +
  labs(x='Day',y='LATE') 
  ggsave(width=5, height=5, 
         file='replication_figures_new/figure_a2_1.tiff')


# poynomials
smalldat <- df[df$day_running >= -36,]
p_list <- 1:5
results <- vector('list',length(p_list))
for(i in 1:length(c_list)){
  out <- rdrobust(df$group_favorability_the_police,df$day_running,
                  p=p_list[i],kernel = 'triangular',c=0)
  results[[i]] <- tibble(coef=out$coef,
                         se=out$se, 
                         pv=out$pv,
                         p=p_list[i])
  print(i)
}
out <- do.call('rbind',results)
out$l <- out$coef - 1.96*out$se
out$u <- out$coef + 1.96*out$se
out$estimate <- rep(c('Traditional','Bias-Adjusted','Robust'),nrow(out)/3)
out %>%
  filter(estimate=='Traditional') %>%
  ggplot(aes(p, coef, ymin=l, ymax=u)) +
  theme_bw() +
  geom_hline(yintercept=0, linetype=2, color='red') +
  geom_errorbar(width=0.1) +
  geom_point(shape=21, size=3, fill='white') +
  labs(x='Order of Polynomial',y='LATE') +
  ggsave(width=5, height=5, 
         file='replication_figures_new/figure_a2_2.tiff')


####
# Table B.4
####


rdGroupEst <- function(y,x,outcome,group){
  out <- rdrobust(y,x,p=1,kernel = 'triangular')
  out <- tibble(coef=out$coef, 
                se=out$se, 
                pv=out$pv) %>%
    mutate(sample=group, outcome=outcome) 
  return(out)
}

p1 <- bind_rows(
  rdGroupEst(y=df$group_favorability_the_police[df$pid7==7],
             x=df$day_running[df$pid7==7],
             outcome='Police',group='Strong Rep'),
  rdGroupEst(y=df$group_favorability_the_police[df$pid7==1],
             x=df$day_running[df$pid7==1],
             outcome='Police',group='Strong dem')
) %>% 
  slice(c(1,4))

p2 <- bind_rows(
  rdGroupEst(y=df$group_favorability_the_police[df$group_fav_white_black==3],
             x=df$day_running[df$group_fav_white_black==3],
             outcome='Police',group='Racist'),
  rdGroupEst(y=df$group_favorability_the_police[df$group_fav_white_black==-3],
             x=df$day_running[df$group_fav_white_black==-3],
             outcome='Police',group='Not RacisT')
)%>% 
  slice(c(1,4))

p3 <- bind_rows(
  rdGroupEst(y=df$group_favorability_the_police[df$racial_attitudes_generations==5],
             x=df$day_running[df$racial_attitudes_generations==5],
             outcome='Police',group='Racist'),
  rdGroupEst(y=df$group_favorability_the_police[df$racial_attitudes_generations==1],
             x=df$day_running[df$racial_attitudes_generations==1],
             outcome='Police',group='Not Racism')
)%>% 
  slice(c(1,4))



p4 <- bind_rows(
  rdGroupEst(y=df$discrimination_blacks[df$pid7==7],
             x=df$day_running[df$pid7==7],
             outcome='Discrimination',group='Strong Rep'),
  rdGroupEst(y=df$discrimination_blacks[df$pid7==1],
             x=df$day_running[df$pid7==1],
             outcome='Discrimination',group='Strong dem')
)%>% 
  slice(c(1,4))

p5 <- bind_rows(
  rdGroupEst(y=df$discrimination_blacks[df$group_fav_white_black==3],
             x=df$day_running[df$group_fav_white_black==3],
             outcome='Discrimination',group='Racist'),
  rdGroupEst(y=df$discrimination_blacks[df$group_fav_white_black==-3],
             x=df$day_running[df$group_fav_white_black==-3],
             outcome='Discrimination',group='Not Racist')
)%>% 
  slice(c(1,4))

p6 <- bind_rows(
  rdGroupEst(y=df$discrimination_blacks[df$racial_attitudes_generations==5],
             x=df$day_running[df$racial_attitudes_generations==5],
             outcome='Discrimination',group='Racist'),
  rdGroupEst(y=df$discrimination_blacks[df$racial_attitudes_generations==1],
             x=df$day_running[df$racial_attitudes_generations==1],
             outcome='Discrimination',group='Not Racist')
)%>% 
  slice(c(1,4))

bind_rows(p1,p2,p3,p4,p5,p6) %>%
  write_csv('replication_figures_new/tablec4.csv')

######
# Table C.1 Irrelevant Outcomes -----
######

rdplot(df$cand_favorability_obama,df$day_running,
       p=1, kernel='triangular')
obama <- rdrobust(df$cand_favorability_obama,df$day_running,
                  p=1,kernel = 'triangular')
full_obama <- tibble(coef=obama$coef, 
                     se=obama$se, 
                     pv=obama$pv)

rdplot(df$group_favorability_jews,df$day_running,
       p=1, kernel='triangular')
jews <- rdrobust(df$group_favorability_jews,df$day_running,
                 p=1,kernel = 'triangular')
full_jews <- tibble(coef=jews$coef, 
                    se=jews$se, 
                    pv=jews$pv)

whites <- rdrobust(df$group_favorability_whites,df$day_running,
                   p=1,kernel = 'triangular')
full_whites <- tibble(coef=whites$coef, 
                      se=whites$se, 
                      pv=whites$pv)

evangelicals <- rdrobust(df$group_favorability_evangelicals,df$day_running,
                         p=1,kernel = 'triangular')
full_evangelicals <- tibble(coef=evangelicals$coef, 
                            se=evangelicals$se, 
                            pv=evangelicals$pv)

socialists <- rdrobust(df$group_favorability_socialists,df$day_running,
                       p=1,kernel = 'triangular',c=0)
full_socialists <- tibble(coef=socialists$coef, 
                          se=socialists$se, 
                          pv=socialists$pv)

bind_rows(
  full_jews,
  full_evangelicals,
  full_socialists,
  full_whites,
  full_obama
) %>% as.data.frame() %>%
  mutate_all(function(x) round(x,3)) %>%
  mutate(group = c(
    "jews","jews","jews",
    "evangelicals","evangelicals","evangelicals",
    "socialists","socialists","socialists",
    "whites","whites","whites",
    "obama","obama","obama"
  ), pct_sd = c(pull(full_jews[,1]/sd(df$group_favorability_jews, na.rm=T)),
                pull(full_evangelicals[,1]/sd(df$group_favorability_evangelicals, na.rm=T)),
                pull(full_whites[,1]/sd(df$group_favorability_whites, na.rm=T)),
                pull(full_socialists[,1]/sd(df$group_favorability_socialists, na.rm=T)),
                pull(full_obama[,1]/sd(df$cand_favorability_obama, na.rm=T))
  )) %>% write_csv('replication_figures_new/tableC1.csv')



######
# Figure C.1 Other Police Killings -----
######

full_plot <- rdplot(df$group_favorability_the_police,df$day_running,
                    p=1, kernel = 'uniform')
shootings <- read_csv('other_shootings_washpo.csv')
shootings$day <- ymd(shootings$date)
shootings <- shootings %>% filter(day >= ymd('2019-07-18'))
shootings <- shootings %>%
  group_by(day) %>%
  filter(armed =='unarmed' & race=='B') %>%
  summarise(n_total=n(),Name=name) 
shootings %>% as.data.frame()
shootings <- left_join(shootings, unique(data.frame(day=df$day, day_running=df$day_running)), by='day')
returnPlot(full_plot, 
           yaxis='Police Unfavorability',
           title='') +
  geom_vline(data=shootings, aes(xintercept=day_running), linetype=3, color='blue') +
  geom_text_repel(data=shootings, aes(x=day_running, y=2.2, label=Name), size=2) +
  ggsave(width=6, height=3, 
         filename = 'replication_figures_new/figure_C1.tiff')

# descriptive statistics
descrips <- function(var){
  return(
    data.frame(
      mean = weighted.mean(var, na.rm=T, w=df$weight),
      sd = sd(var, na.rm=T)
    ) %>% mutate_all(function(x) 
      round(x,2))
  )
}
descrips(df$group_favorability_the_police)
descrips(df$discrimination_blacks)
descrips(df$n_protests);median(df$n_protests, na.rm=T);range(df$n_protests, na.rm=T)
descrips(df$group_favorability_blacks)
descrips(df$group_favorability_whites)
descrips(df$group_fav_white_black)
descrips(df$racial_attitudes_generations)
descrips(df$interest)
descrips(df$pid7)
descrips(df$college)
descrips(df$female)
descrips(df$age)
descrips(df$race_white)
descrips(df$household_income)
descrips(df$ideo5)

