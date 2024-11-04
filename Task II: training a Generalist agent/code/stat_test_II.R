
# retrieve data from csv files
gain_multi_3en <- read.csv('results/test_results/boxplot_gains_multi_set_of_3.csv', header = FALSE)
gain_multi_4en <- read.csv('results/test_results/boxplot_gains_multi_set_of_4.csv', header = FALSE)
gain_single_3en <- read.csv('results/test_results/boxplot_gains_single_set_of_3.csv', header = FALSE)
gain_single_4en <- read.csv('results/test_results/boxplot_gains_single_set_of_4.csv', header = FALSE)

# assemble data in one dataframe
values <- c(gain_multi_3en$V1, gain_multi_4en$V1, gain_single_3en$V1, gain_single_4en$V1)
group <- factor(c(
  rep('multi_3en', 10), rep('multi_4en', 10),
  rep('single_3en', 10), rep('single_4en', 10)),
  levels=c('multi_3en', 'multi_4en', 'single_3en', 'single_4en'))
data <- data.frame(values, group)


# install library
library(dplyr)

# view summary statistics
group_by(data, group) %>%
  summarise(
    count = n(),
    mean = mean(values, na.rm = TRUE),
    sd = sd(values, na.rm = TRUE),
    median = median(values, na.rm = TRUE),
    IQR = IQR(values, na.rm = TRUE)
  )

# Kruskal Wallis test
kruskal.test(values ~ group, data = data)
# p_value < 0.05 -> there is significant difference between the groups

# pairwise Wilcox test to test pairwise relations
pairwise.wilcox.test(data$values, data$group, p.adjust.method = "BH")
# paired p_values > 0.05 -> no significant difference can be derived






