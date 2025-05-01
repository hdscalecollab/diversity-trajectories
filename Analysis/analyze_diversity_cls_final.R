library(sf)
library(ggplot2)
library(tidyverse)


setwd(dirname(rstudioapi::getSourceEditorContext()$path))
getwd()

fattr = read.csv("race_diversity_cls.csv")



#### ------------------------  EStd  ------------------------------------------

#percent of Whites 
p <- fattr %>% select("NHW1990", "NHW2000", "NHW2010", "NHW2020", "TS_EStd")  %>%
  drop_na()  %>%
  pivot_longer( # then we collapse the columns for each side of the brain into a single column, with a second column holding size values
    cols = c("NHW1990", "NHW2000", "NHW2010", "NHW2020"),
    names_to = "YEAR",
    values_to = "DIV"
  ) %>% # then we plot and give it a title
  ggplot(aes(x = as.factor(TS_EStd), y = DIV, fill = YEAR)) +
  labs(y = "Percent in Census Tract", x = "Clusters", title = "White") + 
  geom_boxplot(
    outlier.shape=NA,
    outlier.size = 0.2
  ) + 
  theme_light() +
  #customized styles
  theme(
    plot.margin = margin(t = 1.5, r = 5, b = 1, l = 5),
    plot.title = element_text(size=14,face="bold"),
    axis.text=element_text(size=14),
    axis.title=element_text(size=14),
    )+
  theme(
    legend.position="bottom",
    legend.text = element_text(size=12),
    legend.title= element_text(size=14), 
    legend.box.spacing = margin(0.3)
    )+
  # Edit legend title and labels
  scale_fill_discrete(name = "Year", labels = c("1990", "2000", "2010","2020"))+
  scale_y_continuous(labels = scales::percent_format(scale = 100))+
  list()
p

file_name = "Pct_White_EStd_v4.png"
ggsave(filename = file_name, width = 6.5, height = 4, device='png', dpi=600)



#percent of Blacks 
p <- fattr %>% select("NHB1990", "NHB2000", "NHB2010", "NHB2020", "TS_EStd")  %>%
  drop_na()  %>%
  pivot_longer( # then we collapse the columns for each side of the brain into a single column, with a second column holding size values
    cols = c("NHB1990", "NHB2000", "NHB2010", "NHB2020"),
    names_to = "YEAR",
    values_to = "DIV"
  ) %>% # then we plot and give it a title
  ggplot(aes(x = as.factor(TS_EStd), y = DIV, fill = YEAR)) +
  labs(y = "Percent in Census Tract", x = "Clusters", title = "Black or African American") + 
  geom_boxplot(
    outlier.shape=NA,
    outlier.size = 0.2
  ) + 
  theme_light() +
  #customized styles
  theme(
    plot.margin = margin(t = 1.5, r = 5, b = 1, l = 5),
    plot.title = element_text(size=14,face="bold"),
    axis.text=element_text(size=14),
    axis.title=element_text(size=14),
  )+
  theme(
    legend.position="bottom",
    legend.text = element_text(size=12),
    legend.title= element_text(size=14), 
    legend.box.spacing = margin(0.3)
  )+
  # Edit legend title and labels
  scale_fill_discrete(name = "Year", labels = c("1990", "2000", "2010","2020"))+
  scale_y_continuous(labels = scales::percent_format(scale = 100))+
  list()
p

file_name = "Pct_Black_EStd_v4.png"
ggsave(filename = file_name, width = 6.5, height = 4, device='png', dpi=600)



#percent of Hispanics
p <- fattr %>% select("HISP1990", "HISP2000", "HISP2010", "HISP2020", "TS_EStd")  %>%
  drop_na()  %>%
  pivot_longer( # then we collapse the columns for each side of the brain into a single column, with a second column holding size values
    cols = c("HISP1990", "HISP2000", "HISP2010", "HISP2020"),
    names_to = "YEAR",
    values_to = "DIV"
  ) %>% # then we plot and give it a title
  ggplot(aes(x = as.factor(TS_EStd), y = DIV, fill = YEAR)) +
  labs(y = "Percent in Census Tract", x = "Clusters", title = "Hispanic or Latino") + 
  geom_boxplot(
    outlier.shape=NA,
    outlier.size = 0.2
  ) + 
  theme_light() +
  #customized styles
  theme(
    plot.margin = margin(t = 1.5, r = 5, b = 1, l = 5),
    plot.title = element_text(size=14,face="bold"),
    axis.text=element_text(size=14),
    axis.title=element_text(size=14),
  )+
  theme(
    legend.position="bottom",
    legend.text = element_text(size=12),
    legend.title= element_text(size=14), 
    legend.box.spacing = margin(0.3)
  )+
  # Edit legend title and labels
  scale_fill_discrete(name = "Year", labels = c("1990", "2000", "2010","2020"))+
  scale_y_continuous(labels = scales::percent_format(scale = 100))+
  list()
p

file_name = "Pct_Hispanic_EStd_v4.png"
ggsave(filename = file_name, width = 6.5, height = 4, device='png', dpi=600)




#percent of Asians
p <- fattr %>% select("NHAS1990", "NHAS2000", "NHAS2010", "NHAS2020", "TS_EStd")  %>%
  drop_na()  %>%
  pivot_longer( # then we collapse the columns for each side of the brain into a single column, with a second column holding size values
    cols = c("NHAS1990", "NHAS2000", "NHAS2010", "NHAS2020"),
    names_to = "YEAR",
    values_to = "DIV"
  ) %>% # then we plot and give it a title
  ggplot(aes(x = as.factor(TS_EStd), y = DIV, fill = YEAR)) +
  labs(y = "Percent in Census Tract", x = "Clusters", title = "Asian and Pacific Islander") + 
  geom_boxplot(
    outlier.shape=NA,
    outlier.size = 0.2
  ) + 
  theme_light() +
  #customized style
  theme(
    plot.margin = margin(t = 1.5, r = 5, b = 1, l = 5),
    plot.title = element_text(size=14,face="bold"),
    axis.text=element_text(size=14),
    axis.title=element_text(size=14),
  )+
  theme(
    legend.position="bottom",
    legend.text = element_text(size=12),
    legend.title= element_text(size=14), 
    legend.box.spacing = margin(0.3)
  )+
  # Edit legend title and labels
  scale_fill_discrete(name = "Year", labels = c("1990", "2000", "2010","2020"))+
  scale_y_continuous(labels = scales::percent_format(scale = 100))+
  list()
p

file_name = "Pct_Asian_EStd_v4.png"
ggsave(filename = file_name, width = 6.5, height = 4, device='png', dpi=600)





#percent of American Indian and Alaska Native
p <- fattr %>% select("NHAM1990", "NHAM2000", "NHAM2010", "NHAM2020", "TS_EStd")  %>%
  drop_na()  %>%
  pivot_longer( # then we collapse the columns for each side of the brain into a single column, with a second column holding size values
    cols = c("NHAM1990", "NHAM2000", "NHAM2010", "NHAM2020"),
    names_to = "YEAR",
    values_to = "DIV"
  ) %>% # then we plot and give it a title
  ggplot(aes(x = as.factor(TS_EStd), y = DIV, fill = YEAR)) +
  labs(y = "Percent in Census Tract", x = "Clusters", title = "American Indian and Alaska Native") + 
  geom_boxplot(
    outlier.shape=NA,
    outlier.size = 0.2
  ) + 
  theme_light() +
  #customized style
  theme(
    plot.margin = margin(t = 1.5, r = 5, b = 1, l = 5),
    plot.title = element_text(size=14,face="bold"),
    axis.text=element_text(size=14),
    axis.title=element_text(size=14),
  )+
  theme(
    legend.position="bottom",
    legend.text = element_text(size=12),
    legend.title= element_text(size=14), 
    legend.box.spacing = margin(0.3)
  )+
  # Edit legend title and labels
  scale_fill_discrete(name = "Year", labels = c("1990", "2000", "2010","2020"))+
  scale_y_continuous(labels = scales::percent_format(scale = 100))+
  list()
p

file_name = "Pct_AmIndianAK_EStd_v4.png"
ggsave(filename = file_name, width = 6.5, height = 4, device='png', dpi=600)




#percent of combines two categories
p <- fattr %>% select("NHOT1990", "NHOT2000", "NHOT2010", "NHOT2020", "TS_EStd")  %>%
  drop_na()  %>%
  pivot_longer( # then we collapse the columns for each side of the brain into a single column, with a second column holding size values
    cols = c("NHOT1990", "NHOT2000", "NHOT2010", "NHOT2020"),
    names_to = "YEAR",
    values_to = "DIV"
  ) %>% # then we plot and give it a title
  ggplot(aes(x = as.factor(TS_EStd), y = DIV, fill = YEAR)) +
  labs(y = "Percent in Census Tract", x = "Clusters", title = "Combination of Two Categories") + 
  geom_boxplot(
    outlier.shape=NA,
    outlier.size = 0.2
  ) + 
  theme_light() +
  #customized style
  theme(
    plot.margin = margin(t = 1.5, r = 5, b = 1, l = 5),
    plot.title = element_text(size=14,face="bold"),
    axis.text=element_text(size=14),
    axis.title=element_text(size=14),
  )+
  theme(
    legend.position="bottom",
    legend.text = element_text(size=12),
    legend.title= element_text(size=14), 
    legend.box.spacing = margin(0.3)
  )+
  # Edit legend title and labels
  scale_fill_discrete(name = "Year", labels = c("1990", "2000", "2010","2020"))+
  scale_y_continuous(labels = scales::percent_format(scale = 100))+
  list()
p

file_name = "Pct_Combo_EStd_v4.png"
ggsave(filename = file_name, width = 6.5, height = 4, device='png', dpi=600)





#### ------------------------  HILL  ------------------------------------------

#percent of Whites 
p <- fattr %>% select("NHW1990", "NHW2000", "NHW2010", "NHW2020", "TS_Hill")  %>%
  drop_na()  %>%
  pivot_longer( # then we collapse the columns for each side of the brain into a single column, with a second column holding size values
    cols = c("NHW1990", "NHW2000", "NHW2010", "NHW2020"),
    names_to = "YEAR",
    values_to = "DIV"
  ) %>% # then we plot and give it a title
  ggplot(aes(x = as.factor(TS_Hill), y = DIV, fill = YEAR)) +
  labs(y = "Percent in Census Tract", x = "Clusters", title = "White") + 
  geom_boxplot(
    outlier.shape=NA,
    outlier.size = 0.2
  ) + 
  theme_light() +
  #customized styles
  theme(
    plot.margin = margin(t = 1.5, r = 5, b = 1, l = 5),
    plot.title = element_text(size=14,face="bold"),
    axis.text=element_text(size=14),
    axis.title=element_text(size=14),
  )+
  theme(
    legend.position="bottom",
    legend.text = element_text(size=12),
    legend.title= element_text(size=14), 
    legend.box.spacing = margin(0.3)
  )+
  # Edit legend title and labels
  scale_fill_discrete(name = "Year", labels = c("1990", "2000", "2010","2020"))+
  list()
p

file_name = "Pct_White_Hill_v4.png"
ggsave(filename = file_name, width = 6.5, height = 4, device='png', dpi=600)


#percent of Blacks 
p <- fattr %>% select("NHB1990", "NHB2000", "NHB2010", "NHB2020", "TS_Hill")  %>%
  drop_na()  %>%
  pivot_longer( # then we collapse the columns for each side of the brain into a single column, with a second column holding size values
    cols = c("NHB1990", "NHB2000", "NHB2010", "NHB2020"),
    names_to = "YEAR",
    values_to = "DIV"
  ) %>% # then we plot and give it a title
  ggplot(aes(x = as.factor(TS_Hill), y = DIV, fill = YEAR)) +
  labs(y = "Percent in Census Tract", x = "Clusters", title = "Black or African American") + 
  geom_boxplot(
    outlier.shape=NA,
    outlier.size = 0.2
  ) + 
  theme_light() +
  #customized styles
  theme(
    plot.margin = margin(t = 1.5, r = 5, b = 1, l = 5),
    plot.title = element_text(size=14,face="bold"),
    axis.text=element_text(size=14),
    axis.title=element_text(size=14),
  )+
  theme(
    legend.position="bottom",
    legend.text = element_text(size=12),
    legend.title= element_text(size=14), 
    legend.box.spacing = margin(0.3)
  )+
  # Edit legend title and labels
  scale_fill_discrete(name = "Year", labels = c("1990", "2000", "2010","2020"))+
  list()
p

file_name = "Pct_Black_Hill_v4.png"
ggsave(filename = file_name, width = 6.5, height = 4, device='png', dpi=600)



#percent of Hispanics
p <- fattr %>% select("HISP1990", "HISP2000", "HISP2010", "HISP2020", "TS_Hill")  %>%
  drop_na()  %>%
  pivot_longer( # then we collapse the columns for each side of the brain into a single column, with a second column holding size values
    cols = c("HISP1990", "HISP2000", "HISP2010", "HISP2020"),
    names_to = "YEAR",
    values_to = "DIV"
  ) %>% # then we plot and give it a title
  ggplot(aes(x = as.factor(TS_Hill), y = DIV, fill = YEAR)) +
  labs(y = "Percent in Census Tract", x = "Clusters", title = "Hispanic or Latino") + 
  geom_boxplot(
    outlier.shape=NA,
    outlier.size = 0.2
  ) + 
  theme_light() +
  #customized style
  theme(
    plot.margin = margin(t = 1.5, r = 5, b = 1, l = 5),
    plot.title = element_text(size=14,face="bold"),
    axis.text=element_text(size=14),
    axis.title=element_text(size=14),
  )+
  theme(
    legend.position="bottom",
    legend.text = element_text(size=12),
    legend.title= element_text(size=14), 
    legend.box.spacing = margin(0.3)
  )+
  # Edit legend title and labels
  scale_fill_discrete(name = "Year", labels = c("1990", "2000", "2010","2020"))+
  list()
p

file_name = "Pct_Hispanic_Hill_v4.png"
ggsave(filename = file_name, width = 6.5, height = 4, device='png', dpi=600)




#percent of Asians
p <- fattr %>% select("NHAS1990", "NHAS2000", "NHAS2010", "NHAS2020", "TS_Hill")  %>%
  drop_na()  %>%
  pivot_longer( # then we collapse the columns for each side of the brain into a single column, with a second column holding size values
    cols = c("NHAS1990", "NHAS2000", "NHAS2010", "NHAS2020"),
    names_to = "YEAR",
    values_to = "DIV"
  ) %>% # then we plot and give it a title
  ggplot(aes(x = as.factor(TS_Hill), y = DIV, fill = YEAR)) +
  labs(y = "Percent in Census Tract", x = "Clusters", title = "Asian and Pacific Islander") + 
  geom_boxplot(
    outlier.shape=NA,
    outlier.size = 0.2
  ) + 
  theme_light() +
  #customized style
  theme(
    plot.margin = margin(t = 1.5, r = 5, b = 1, l = 5),
    plot.title = element_text(size=14,face="bold"),
    axis.text=element_text(size=14),
    axis.title=element_text(size=14),
  )+
  theme(
    legend.position="bottom",
    legend.text = element_text(size=12),
    legend.title= element_text(size=14), 
    legend.box.spacing = margin(0.3)
  )+
  # Edit legend title and labels
  scale_fill_discrete(name = "Year", labels = c("1990", "2000", "2010","2020"))+
  list()
p

file_name = "Pct_Asian_Hill_v4.png"
ggsave(filename = file_name, width = 6.5, height = 4, device='png', dpi=600)




#percent of American Indian and Alaska Native
p <- fattr %>% select("NHAM1990", "NHAM2000", "NHAM2010", "NHAM2020", "TS_Hill")  %>%
  drop_na()  %>%
  pivot_longer( # then we collapse the columns for each side of the brain into a single column, with a second column holding size values
    cols = c("NHAM1990", "NHAM2000", "NHAM2010", "NHAM2020"),
    names_to = "YEAR",
    values_to = "DIV"
  ) %>% # then we plot and give it a title
  ggplot(aes(x = as.factor(TS_Hill), y = DIV, fill = YEAR)) +
  labs(y = "Percent in Census Tract", x = "Clusters", title = "American Indian and Alaska Native") + 
  geom_boxplot(
    outlier.shape=NA,
    outlier.size = 0.2
  ) + 
  theme_light() +
  #customized style
  theme(
    plot.margin = margin(t = 1.5, r = 5, b = 1, l = 5),
    plot.title = element_text(size=14,face="bold"),
    axis.text=element_text(size=14),
    axis.title=element_text(size=14),
  )+
  theme(
    legend.position="bottom",
    legend.text = element_text(size=12),
    legend.title= element_text(size=14), 
    legend.box.spacing = margin(0.3)
  )+
  # Edit legend title and labels
  scale_fill_discrete(name = "Year", labels = c("1990", "2000", "2010","2020"))+
  scale_y_continuous(labels = scales::percent_format(scale = 100))+
  list()
p

file_name = "Pct_AmIndianAK_Hill_v4.png"
ggsave(filename = file_name, width = 6.5, height = 4, device='png', dpi=600)




#percent of combines two categories
p <- fattr %>% select("NHOT1990", "NHOT2000", "NHOT2010", "NHOT2020", "TS_Hill")  %>%
  drop_na()  %>%
  pivot_longer( # then we collapse the columns for each side of the brain into a single column, with a second column holding size values
    cols = c("NHOT1990", "NHOT2000", "NHOT2010", "NHOT2020"),
    names_to = "YEAR",
    values_to = "DIV"
  ) %>% # then we plot and give it a title
  ggplot(aes(x = as.factor(TS_Hill), y = DIV, fill = YEAR)) +
  labs(y = "Percent in Census Tract", x = "Clusters", title = "Combination of Two Categories") + 
  geom_boxplot(
    outlier.shape=NA,
    outlier.size = 0.2
  ) + 
  theme_light() +
  #customized style
  theme(
    plot.margin = margin(t = 1.5, r = 5, b = 1, l = 5),
    plot.title = element_text(size=14,face="bold"),
    axis.text=element_text(size=14),
    axis.title=element_text(size=14),
  )+
  theme(
    legend.position="bottom",
    legend.text = element_text(size=12),
    legend.title= element_text(size=14), 
    legend.box.spacing = margin(0.3)
  )+
  # Edit legend title and labels
  scale_fill_discrete(name = "Year", labels = c("1990", "2000", "2010","2020"))+
  scale_y_continuous(labels = scales::percent_format(scale = 100))+
  list()
p

file_name = "Pct_Combo_Hill_v4.png"
ggsave(filename = file_name, width = 6.5, height = 4, device='png', dpi=600)
