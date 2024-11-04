# load libraries

library(ggplot2)

# import files

test_stats_EA1 <- read.csv('group112_EA1/EA1_5_Tests_stats.csv')
test_stats_EA2 <- read.csv('group112_EA2/EA2_5_Tests_stats.csv')

# preparing data for boxplots

IG <- c(test_stats_EA1$X3, test_stats_EA1$X2, test_stats_EA1$X1,
        test_stats_EA2$X3, test_stats_EA2$X2, test_stats_EA2$X1)
test <- factor(c(
  rep('EA1 v En.3', 10), rep('EA1 v En.2', 10),
  rep('EA1 v En.1', 10), rep('EA2 v En.3', 10),
  rep('EA2 v En.2', 10), rep('EA2 v En.1', 10)),
  levels=c('EA1 v En.3', 'EA2 v En.3', 'EA1 v En.2', 'EA2 v En.2', 'EA1 v En.1', 'EA2 v En.1'))
data <- data.frame(IG, test)


p <- ggplot(data, aes(x = test, y = IG, fill = test))+geom_boxplot(outlier.shape=1)+labs(x=NULL, y = "Individual Gain")
p+scale_fill_brewer(palette="Dark2")+theme(legend.position="none")+scale_y_continuous(breaks=seq(-20,100,20))


# two sided t-test for the mean for Enemy 3
t.test(test_stats_EA1$X3, test_stats_EA2$X3)

# two sided t-test for the mean for Enemy 2
t.test(test_stats_EA1$X2, test_stats_EA2$X2)

# two sided t-test for the mean for Enemy 1
t.test(test_stats_EA1$X1, test_stats_EA2$X1)

# one sided t-test for the mean, Enemy 3
#t.test(test_stats_EA2$X3, test_stats_EA1$X3, alternative = 'greater')
