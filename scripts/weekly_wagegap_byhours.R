library(ggplot2)

setwd("../Desktop/ASHE/")

week6.1a <- read.csv("data/t6.1a_weeklypaygross2019.csv", header = T, sep = ",",
                     fileEncoding = "UTF-8-BOM") #fileEncoding stops weird i appendage
# colnames(week6.1a)[1] <- "age_group"
week6.1a <- week6.1a[!(week6.1a$age_group == "all"),] #remove erroneous All rows
week6.1a$sex <- toupper(week6.1a$sex)

female <- subset(week6.1a, week6.1a$sex == "F")
male<- subset(week6.1a, week6.1a$sex == "M")

pt <- subset(week6.1a, week6.1a$hours == "PT")
ft <- subset(week6.1a, week6.1a$hours == "FT")

ggplot(pt, aes(x = age_group, y = median, fill = sex)) +
  geom_col(position = "dodge2", width = 0.7) +
  labs(fill = "Sex") +
  xlab("Age Group") +
  ylab("Median Weekly Gross Pay (£)") +
  theme(axis.text.x = element_text(angle = 90),
        text = element_text(size = 8)) +
  ggtitle("Gross Weekly Pay for Part-Time Workers\nby Age Group")

ggplot(ft, aes(x = age_group, y = median, fill = sex)) +
  geom_col(position = "dodge2", width = 0.7) +
  labs(fill = "Sex") +
  xlab("Age Group") +
  ylab("Median Weekly Gross Pay (£)") +
  theme(axis.text.x = element_text(angle = 90),
        text = element_text(size = 8)) +
  ggtitle("Gross Weekly Pay for Full-Time Workers\nby Age Group")

f_pt <- subset(pt, pt$sex == "F")
m_pt <- subset(pt, pt$sex == "M")
pt_wagegap <- data.frame(age_group = f_pt$age_group,
                         gap_m.f = (m_pt$median - f_pt$median))
#in part-time work, 16-30 males earn a little more, then females earn
#substantially more, and from 60+ males start earning more again.

f_ft <- subset(ft, ft$sex == "F")
m_ft <- subset(ft, ft$sex == "M")
ft_wagegap <- data.frame(age_group = f_ft$age_group,
                         gap_m.f = (m_ft$median - f_ft$median))

wagegap <- data.frame(age_group = rep(c("16-17", "18-21", "22-29", "30-39", "40-49", "50-59", "60+"), 2),
                      hours = c(rep("FT", 7), rep("PT", 7)),
                      gap_m.f = c((m_ft$median - f_ft$median),
                                      (m_pt$median - f_pt$median)))

ggplot(wagegap, aes(x = age_group, y = gap_m.f, fill = hours)) +
  geom_col(position = "dodge2", width = 0.7) +
  coord_flip() +
  scale_x_discrete(name="", limits = rev(levels(wagegap$age_group))) +
  labs(fill = "Hours") +
  xlab("Age Group") +
  ylab("Wage Gap (Male - Female) (£)") +
  theme(text = element_text(size = 8)) +
  geom_hline(aes(yintercept=0), color = "black") +
  ggtitle("Weekly Pay Difference by Gender\nfor FT and PT Workers")


## Test 
mood.ft <- mood.test(m_ft$median, f_ft$median, alternative = "g")
mood.pt <- mood.test(m_pt$median, f_pt$median, alternative = "g")
wilcox.test(m_ft$median, f_ft$median)
wilcox.test(m_pt$median, f_pt$median)
# differences in pay are not statistically significant, but that doesn't
# mean they're not socially significant either (link to IQ study to illustrate).
# also as we are dealing with medians, there isn't a plethora of good tests
# we can turn to.


## Compare percentile wages

ft_a <- ft[,-c(3:8)] #abridge to cols of interest
ft_a <- ft_a[,c(1:3, 6, 9, 12)]

ft_resh <- reshape(ft_a,
                   direction = "long",
                   varying = names(ft_a[,3:6]),
                   v.names = "median",
                   idvar = c("sex","age_group"),
                   times = names(ft_a[,3:6]))

ft_resh$time <- gsub("p10", "10th", ft_resh$time)
ft_resh$time <- gsub("p30", "30th", ft_resh$time)
ft_resh$time <- gsub("p70", "70th", ft_resh$time)
ft_resh$time <- gsub("p90", "90th", ft_resh$time)


# ggplot(ft_resh, aes(x = age_group, y = median, fill = sex)) +
#   geom_col(position = "dodge2", width = 0.8) +
#   labs(fill = "Sex") +
#   xlab("Age Group") +
#   ylab("Median Weekly Gross Pay (£)") +
#   theme(axis.text.x = element_text(angle = 90),
#         text = element_text(size = 8)) +
#   ggtitle("Gross Weekly Pay for Full-Time Workers by Age Group and Percentile")

ggplot(ft_resh, aes(x = age_group, y = median, fill = time, group = sex, alpha = sex)) +
  geom_col(position = "dodge2", width = 0.8) +
  scale_alpha_manual(values=c(0.4, 1)) +
  labs(fill = "Percentile", alpha = "Sex") +
  xlab("Age Group") +
  ylab("Median Weekly Gross Pay (£)") +
  theme(axis.text.x = element_text(angle = 90),
        text = element_text(size = 8)) +
  ggtitle("Gross Weekly Pay for Full-Time Workers by Age Group, Gender, and Percentile")

