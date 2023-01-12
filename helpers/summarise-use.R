# download metrics from shinyapps.io, summarise and plot cumulative number of connections throught time

library(tidyverse)
library(lubridate)

df <- rsconnect::showMetrics("container_status",
                             c("connect_count", 
                               "connect_procs"),
                             appName="blue-forests-app",
                             account="global-wetlands",
                             server="shinyapps.io",
                             from = as.numeric(as.POSIXct("2022-07-15 0:00:00 "))
                             #interval = "900s" # fifteen minuntes
                             ) 

df2 <- data.frame(date = as.numeric(df$connect_procs),
                  connections = as.numeric(df$timestamp)) %>% 
  mutate(date = as_datetime(date)) %>% 
  mutate(n_count=cumsum(connections),
         new_connect=case_when(
           connections>lag(connections,1) ~ connections-lag(connections,1), TRUE ~ 0),
         n_connect=cumsum(new_connect) # approximate
         )

# ccumulative connections through time

ggplot(df2) +
  aes(x = date, y = n_count) +
  geom_point() +
  ylab('Number of cumulative connections') +
  xlab('Date') +
  #geom_vline(xintercept = as.POSIXct('2022-10-05 14:05:00')) +
  theme_classic()

ggsave(paste0('use-data/plots/cumulative-connections_', month(df2[1,1], label = T),'-', day(df2[1,1]), '-', year(df2[1,1]), 
              '_' , month(df2[nrow(df2),1], label = T),'-', day(df2[nrow(df2),1]), '-', year(df2[nrow(df2),1]), '.png'),
       width = 6, height = 4)

ggplot(df2) +
  aes(x = date, y = n_connect) +
  geom_point() +
  theme_classic()

# save

write.csv(df, paste0('use-data/raw/metrics_raw_', month(df2[1,1], label = T),'-', day(df2[1,1]), '-', year(df2[1,1]), 
                     '_' , month(df2[nrow(df2),1], label = T),'-', day(df2[nrow(df2),1]), '-', year(df2[nrow(df2),1]), '.csv'),
          row.names = F)

write.csv(df2, paste0('use-data/wrangled/metrics_', month(df2[1,1], label = T),'-', day(df2[1,1]), '-', year(df2[1,1]), 
                     '_' , month(df2[nrow(df2),1], label = T),'-', day(df2[nrow(df2),1]), '-', year(df2[nrow(df2),1]), '.csv'),
          row.names = F)
