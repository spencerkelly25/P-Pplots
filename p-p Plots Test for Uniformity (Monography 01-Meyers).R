library(ggplot2)
library(dplyr)
library(gridExtra)
library(sn)

#Uniform 
n <- 100                                                            #Setting up datasets and dataframes
values1 <- runif(n, 0, 100) %>% data.frame()
names(values1)[1] <- 'values'
c_density <- list(1:100) %>% data.frame()
names(c_density)[1] <- 'index'
c_density$density <- knots(ecdf(values1$values))

hist_plot <- ggplot(data = values1, aes(x = values)) +              #Creating histogram plot 
  geom_histogram() +
  ggtitle('Uniform') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab('Frequency') + 
  xlab('')
cd_plot <- ggplot(data = c_density, aes(x = density, y = index)) +  #Creating cumulative density plot 
  geom_point() + 
  geom_abline() + 
  geom_abline(intercept = 13.6) + 
  geom_abline(intercept = -13.6) +
  ggtitle('Uniform') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab('Predicted') + 
  xlab('Expected')

grid.arrange(hist_plot, cd_plot, nrow = 1)                          #Putting it all together 


#Light Tailed 
values2 <- rbeta(n, .50, .50) %>% data.frame()                      #Setting up datasets and dataframes
names(values2)[1] <- 'values'
values2$values <- values2$values * 100
c_density2 <- list(1:100) %>% data.frame()
names(c_density2)[1] <- 'index'
c_density2$density <- knots(ecdf(values2$values))

hist_plot2 <- ggplot(data = values2, aes(x = values)) +             #Creating histogram plot 
  geom_histogram() +
  ggtitle('Light Tailed') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab('Frequency') + 
  xlab('')
cd_plot2 <- ggplot(data = c_density2, aes(x = density, y = index)) +#Creating cumulative density plot 
  geom_point() + 
  geom_abline() + 
  geom_abline(intercept = 13.6) + 
  geom_abline(intercept = -13.6) +
  ggtitle('Light Tailed') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab('Predicted') + 
  xlab('Expected')

grid.arrange(hist_plot2, cd_plot2, nrow = 1)                        #Putting it all together 


#Heavy Tailed 
mean = 50
sd = mean * (1/3)
values3 <- rnorm(n = n, mean = mean, sd = sd) %>% data.frame()      #Setting up datasets and dataframes
names(values3)[1] <- 'values'
c_density3 <- list(1:100) %>% data.frame()
names(c_density3)[1] <- 'index'
c_density3$density <- knots(ecdf(values3$values))

hist_plot3 <- ggplot(data = values3, aes(x = values)) +             #Creating histogram plot 
  geom_histogram() +
  ggtitle('Heavy Tailed') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab('Frequency') + 
  xlab('')
cd_plot3 <- ggplot(data = c_density3, aes(x = density, y = index)) +#Creating cumulative density plot 
  geom_point() + 
  geom_abline() + 
  geom_abline(intercept = 13.6) + 
  geom_abline(intercept = -13.6) +
  ggtitle('Heavy Tailed') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab('Predicted') + 
  xlab('Expected')

grid.arrange(hist_plot3, cd_plot3, nrow = 1)                        #Putting it all together 


#Biased High 
values4 <- rbeta(n, 1, 30) %>% data.frame()
names(values4)[1] <- 'values'
values4$values <- values4$values * 700
c_density4 <- list(1:100) %>% data.frame()
names(c_density4)[1] <- 'index'
c_density4$density <- knots(ecdf(values4$values))

hist_plot4 <- ggplot(data = values4, aes(x = values)) +             #Creating histogram plot 
  geom_histogram() +
  ggtitle('Biased High') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab('Frequency') + 
  xlab('')
cd_plot4 <- ggplot(data = c_density4, aes(x = density, y = index)) +#Creating cumulative density plot 
  geom_point() + 
  geom_abline() + 
  geom_abline(intercept = 13.6) + 
  geom_abline(intercept = -13.6) +
  ggtitle('Biased High') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab('Predicted') + 
  xlab('Expected')

grid.arrange(hist_plot4, cd_plot4, nrow = 1)                        #Putting it all together 


#Biased Low 
values5 <- rbeta(n, 2, 1) %>% data.frame()
names(values5)[1] <- 'values'
values5$values <- values5$values * 100
c_density5 <- list(1:100) %>% data.frame()
names(c_density5)[1] <- 'index'
c_density5$density <- knots(ecdf(values5$values))

hist_plot5 <- ggplot(data = values5, aes(x = values)) +             #Creating histogram plot 
  geom_histogram() +
  ggtitle('Biased Low') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab('Frequency') + 
  xlab('')
cd_plot5 <- ggplot(data = c_density5, aes(x = density, y = index)) +#Creating cumulative density plot 
  geom_point() + 
  geom_abline() + 
  geom_abline(intercept = 13.6) + 
  geom_abline(intercept = -13.6) +
  ggtitle('Biased Low') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab('Predicted') + 
  xlab('Expected')

grid.arrange(hist_plot5, cd_plot5, nrow = 1)                        #Putting it all together 

