#rm(list = ls())
Pacotes <- c("dplyr", "ggplot2", "readxl", "tidyr",
             "gridExtra", "magrittr", "readstata13")
Novos.Pacotes <- Pacotes[!(Pacotes %in% installed.packages()[,"Package"])]
if(length(Novos.Pacotes)) install.packages(Novos.Pacotes)
lapply(Pacotes, require, character.only = TRUE)


#--------------------------------Aggregate Decomposition ----------------------------------------------
y <- data.frame(Region = factor(rep(c("Overall", "ER", "ED-HT"), each = 6),#                |  
                               levels = c("Overall", "ER", "ED-HT")),#                      |
               Type = factor(rep(c("Exposure", "Suceptibility", "Adaptative Capacity"), times = 6),#  |
                             levels = c("Adaptative Capacity", "Suceptibility", "Exposure")),#        |
               Morb = factor(rep(c("Low", "High"), times = 9), levels = c("Low", "High")),#           |
               Value = c(22.3, 32.5, 46, 27.2, 31.7, 40.3, 29, 30.9, 42.2, 19.9, 
                         28.8, 49.2, 14.2, 33.8, 50.5, 33.8, 35.4, 32.4))#                                        |
#-----------------------------------------------------------------------------------------------------#

#------------------------------Saving the Picture---------------------------------------
tiff(file="Figures/decomp_aggregate.tiff",#                                            |
     width=6, height=6, units="in", res=300)#                                          |
ggplot(y, aes(x = Morb, y = Value, fill = Type)) +#                                    |
  geom_bar(position = "fill", stat = "identity") +#                                    |
  facet_wrap(~Region) +#                                                               |
  scale_y_continuous(labels = scales::percent_format()) +#                             |
  scale_fill_grey(name = "", start = 0.3) +#                                                    |
  geom_text(aes(y = Value/100, label = scales::percent(Value/100, accuracy = 0.1)),#   |
            position = position_fill(vjust = 0.5),#                                    |                
            size = 4,#                                                                 |                            
            show.legend = FALSE) +#                                                    |
  theme_minimal() +#                                                                   |
  theme(legend.position = "bottom", axis.title.x = element_blank())#                   |
dev.off()#                                                                             |
#--------------------------------------------------------------------------------------#

#-----------------------Reading the data  ------------------------- 
x <- readxl::read_excel(path="Datasets/decomp.xlsx", sheet = "Planilha1")
x %<>% mutate(Region = case_when(
  Region == "Dry" ~ "ED-HT",
  Region == "Wet" ~ "ER",
  TRUE ~ Region
))# |
#-----------------------------------------------------------------#

#------------------Installing packages required for visualization------------------
myPackages <- c("RColorBrewer", "dplyr", "magrittr", "cowplot", "ggplot2")#       |
new.packages <- myPackages[!(myPackages %in% installed.packages()[,"Package"])]#  |
if(length(new.packages)) install.packages(new.packages)#                          |
lapply(myPackages, require, character.only = TRUE)#                               |
#---------------------------------------------------------------------------------#

#-------------------Data Manipulation-----------------------------
x$Indicator %<>% factor(levels = unique(x$Indicator))#           |
x$Percent <- round(c(x$Value[1:18]/sum(x$Value[1:18]),#          | 
        x$Value[19:36]/sum(x$Value[19:36]),#                     |   
        x$Value[37:54]/sum(x$Value[37:54])), #                   |
      digits = 3)#                                               |
#----------------------------------------------------------------#

#-------------------Pallete for color-----------------------------
My_palette <- setNames(c(brewer.pal(7, "Blues"), #               |
                         brewer.pal(5, "Reds"), #                |
                         brewer.pal(6, "Greens")),#              | 
                       levels(x$Indicator))#                     |
#----------------------------------------------------------------#

#--------------------------Main Plot----------------------------------------------
full_plot <- ggplot(x, aes(x = Region, y = Value, fill = Indicator)) + 
  labs(x = "Homoclimatic Zone") + #                                              |
  geom_bar(position = "fill",stat = "identity") + #                              |
  scale_y_continuous(labels = scales::percent_format()) + #                      |
  scale_fill_manual(values = My_palette) + #                                     |
  theme(legend.position = "none") + #                                            |
  geom_text(aes(y = Percent, label = scales::percent(Percent, accuracy = 0.1)),# |
            position = position_fill(vjust = 0.5), #                             |
            size = 3, #                                                          |
            show.legend = FALSE) #                                               |
#--------------------------------------------------------------------------------#

#----------------Gambiarra para Criar a Legenda----------------------------------
first_legend <- x %>% #                                                          |
  filter(`Indicator` %in% c("TXx", "TNx", "TX90p", #                             |
                            "TN90p", "DTR", "Cdd", "R99p")) %>% #                |
  ggplot(aes(x = Region, y = Value, fill = Indicator)) + #                       |
  geom_bar(position = "fill",stat = "identity") + #                              |
  scale_y_continuous(labels = scales::percent_format()) + #                      |
  scale_fill_manual(values = My_palette, name = "Exposure") #                    |
                                                            #                    |
second_legend <- x %>% #                                                         |
  filter(`Indicator` %in% c("Elderly", "Children", #                             |
                            "Income", "Literate", "Poor")) %>%#                  |
  ggplot(aes(x = Region, y = Value, fill = Indicator)) + #                       |
  geom_bar(position = "fill",stat = "identity") + #                              |
  scale_y_continuous(labels = scales::percent_format()) + #                      |
  scale_fill_manual(values = My_palette, name = "Susceptibility") #              |
                                                            #                    |
third_legend <- x %>% #                                                          |
  filter(`Indicator` %in% c("Sewage", "Water", "Garbage", "Urbanization", #      | 
                            "Primary Care", "Hospital Beds")) %>% #              |
  ggplot(aes(x = Region, y = Value, fill = Indicator)) + #                       |
  geom_bar(position = "fill",stat = "identity") + #                              |
  scale_y_continuous(labels = scales::percent_format()) + #                      |
  scale_fill_manual(values = My_palette, name = "Adaptive Capacity")#            |

tiff(file="Figures/decomp.tiff",
     width=6, height=8, units="in", res=300)                                                         #                       |
plot_grid( #                                                                     |
  full_plot, #                                                                   |
  plot_grid( #                                                                   |
    get_legend(first_legend), #                                                  |
    get_legend(second_legend), #                                                 |
    get_legend(third_legend), #                                                  |
    nrow = 1 #                                                                   |
  ) #                                                                            |
  , nrow = 2 #                                                                   |
  , rel_heights = c(6,2) #                                                       |
) #                                                                              |
dev.off()
#--------------------------------------------------------------------------------#


gom_boxdata <- read.dta13("C:\\Users\\Jeferson\\Desktop\\CEVI\\Kenya\\alpha2OLD.dta")


gom_boxdata1 <- gom_boxdata %>% 
  pivot_longer(cols = c(num_range("lambda_1_", 1:5), 
                        num_range("lambda_2_", 1:5)), 
               names_to = "Names", 
               values_to = "Value") %>%
  group_by(Names) %>%
  mutate(median = median(Value, na.rm = T))

gom_boxdata1$CDD <- factor(gom_boxdata1$Names, 
                              levels = c("lambda_1_1", "lambda_1_2", "lambda_1_3", "lambda_1_4", "lambda_1_5",
                                         "lambda_2_1", "lambda_2_2", "lambda_2_3", "lambda_2_4", "lambda_2_5"),
                              labels = c("Profile 1 - Q1", "Profile 1 - Q2", "Profile 1 - Q3", "Profile 1 - Q4",
                                         "Profile 1 - Q5", "Profile 2 - Q1", "Profile 2 - Q2", "Profile 2 - Q3",
                                         "Profile 2 - Q4", "Profile 2 - Q5"))
labels <- gom_boxdata1 %>% 
  group_by(Names, CDD) %>% 
  summarise(median = min(median), .groups = "drop") %>% 
  select(median, CDD) %>% 
  arrange(median) %>% 
  select(CDD) %>% 
  pull() %>%
  as.character()
tiff(file="Figures/Box1.tiff",
     width=6, height=8, units="in", res=300)
ggplot(gom_boxdata1, aes(y = Value, fill = factor(median), group = CDD)) +
  geom_boxplot() +
  scale_fill_grey(start = 0.9, end = 0.4, name = "Legend:", labels = labels) +
  geom_abline(intercept = 0.25, slope = 0) + 
  theme(legend.position = "bottom") +
  labs(y = "CDD")
ggplot(gom_boxdata1, aes(y = Value, fill = factor(median), group = CDD)) +
  geom_boxplot() +
  geom_abline(intercept = 0.25, slope = 0) + 
  theme(legend.position = "bottom") +
  labs(y = "CDD")
dev.off()
boxplot(gom_boxdata$lambda_1_1, angle=c(45, 0, 45, 0), density=seq(10,30,15))


gom_boxdata2 <- gom_boxdata %>% 
  pivot_longer(cols = c(num_range("lambda_1_", 6:10), 
                        num_range("lambda_2_", 6:10)), 
               names_to = "Names", 
               values_to = "Value") %>%
  group_by(Names) %>%
  mutate(median = median(Value, na.rm = T))
gom_boxdata2$DTR <- factor(gom_boxdata2$Names, 
                          levels = c("lambda_1_6", "lambda_1_7", "lambda_1_8", "lambda_1_9", "lambda_1_10",
                                     "lambda_2_6", "lambda_2_7", "lambda_2_8", "lambda_2_9", "lambda_2_10"),
                          labels = c("Profile 1 - Q1", "Profile 1 - Q2", "Profile 1 - Q3", "Profile 1 - Q4",
                                     "Profile 1 - Q5", "Profile 2 - Q1", "Profile 2 - Q2", "Profile 2 - Q3",
                                     "Profile 2 - Q4", "Profile 2 - Q5"))
labels <- gom_boxdata2 %>% 
  group_by(Names, DTR) %>% 
  summarise(median = min(median), .groups = "drop") %>% 
  select(median, DTR) %>% 
  arrange(median) %>% 
  select(DTR) %>% 
  pull() %>%
  as.character()
tiff(file="Figures/Box2.tiff",
     width=6, height=8, units="in", res=300)
ggplot(gom_boxdata2, aes(y = Value, fill = factor(median), group = DTR)) +
  geom_boxplot() +
  scale_fill_grey(start = 0.9, end = 0.4, labels = labels,
                  name = "Legend:") +
  geom_abline(intercept = 0.25, slope = 0) + 
  theme(legend.position = "bottom") +
  labs(y = "DTR")
dev.off()


gom_boxdata3 <- gom_boxdata %>% 
  pivot_longer(cols = c(num_range("lambda_1_", 11:15), 
                        num_range("lambda_2_", 11:15)), 
               names_to = "Names", 
               values_to = "Value") %>%
  group_by(Names) %>%
  mutate(median = median(Value, na.rm = T))
gom_boxdata3$R99P <- factor(gom_boxdata3$Names, 
                           levels = c("lambda_1_11", "lambda_1_12", "lambda_1_13", "lambda_1_14", "lambda_1_15",
                                      "lambda_2_11", "lambda_2_12", "lambda_2_13", "lambda_2_14", "lambda_2_15"),
                           labels = c("Profile 1 - Q1", "Profile 1 - Q2", "Profile 1 - Q3", "Profile 1 - Q4",
                                      "Profile 1 - Q5", "Profile 2 - Q1", "Profile 2 - Q2", "Profile 2 - Q3",
                                      "Profile 2 - Q4", "Profile 2 - Q5"))
labels <- gom_boxdata3 %>% 
  group_by(Names, R99P) %>% 
  summarise(median = min(median), .groups = "drop") %>% 
  select(median, R99P) %>% 
  arrange(median) %>% 
  select(R99P) %>% 
  pull() %>%
  as.character()
tiff(file="Figures/Box3.tiff",
     width=6, height=8, units="in", res=300)
ggplot(gom_boxdata3, aes(y = Value, fill = factor(median), group = R99P)) +
  geom_boxplot() +
  scale_fill_grey(start = 0.9, end = 0.4, labels = labels,
                  name = "Legend:") +
  geom_abline(intercept = 0.25, slope = 0) + 
  theme(legend.position = "bottom") +
  labs(y = "R99P")
dev.off()

gom_boxdata4 <- gom_boxdata %>% 
  pivot_longer(cols = c(num_range("lambda_1_", 16:20), 
                        num_range("lambda_2_", 16:20)), 
               names_to = "Names", 
               values_to = "Value") %>%
  group_by(Names) %>%
  mutate(median = median(Value, na.rm = T))
gom_boxdata4$TN90P <- factor(gom_boxdata4$Names, 
                            levels = c("lambda_1_16", "lambda_1_17", "lambda_1_18", "lambda_1_19", "lambda_1_20",
                                       "lambda_2_16", "lambda_2_17", "lambda_2_18", "lambda_2_19", "lambda_2_20"),
                            labels = c("Profile 1 - Q1", "Profile 1 - Q2", "Profile 1 - Q3", "Profile 1 - Q4",
                                       "Profile 1 - Q5", "Profile 2 - Q1", "Profile 2 - Q2", "Profile 2 - Q3",
                                       "Profile 2 - Q4", "Profile 2 - Q5"))
labels <- gom_boxdata4 %>% 
  group_by(Names, TN90P) %>% 
  summarise(median = min(median), .groups = "drop") %>% 
  select(median, TN90P) %>% 
  arrange(median) %>% 
  select(TN90P) %>% 
  pull() %>%
  as.character()
tiff(file="Figures/Box4.tiff",
     width=6, height=8, units="in", res=300)
ggplot(gom_boxdata4, aes(y = Value, fill = factor(median), group = TN90P)) +
  geom_boxplot() +
  scale_fill_grey(start = 0.9, end = 0.4, labels = unique(gom_boxdata4$TN90P),
                  name = "Legend") +
  geom_abline(intercept = 0.25, slope = 0) + 
  theme(legend.position = "bottom") +
  labs(y = "TN90P")
dev.off()

gom_boxdata5 <- gom_boxdata %>% 
  pivot_longer(cols = c(num_range("lambda_1_", 21:25), 
                        num_range("lambda_2_", 21:25)), 
               names_to = "Names", 
               values_to = "Value") %>%
  group_by(Names) %>%
  mutate(median = median(Value, na.rm = T))
gom_boxdata5$TNX <- factor(gom_boxdata5$Names, 
                             levels = c("lambda_1_21", "lambda_1_22", "lambda_1_23", "lambda_1_24", "lambda_1_25",
                                        "lambda_2_21", "lambda_2_22", "lambda_2_23", "lambda_2_24", "lambda_2_25"),
                             labels = c("Profile 1 - Q1", "Profile 1 - Q2", "Profile 1 - Q3", "Profile 1 - Q4",
                                        "Profile 1 - Q5", "Profile 2 - Q1", "Profile 2 - Q2", "Profile 2 - Q3",
                                        "Profile 2 - Q4", "Profile 2 - Q5"))
labels <- gom_boxdata5 %>% 
  group_by(Names, TNX) %>% 
  summarise(median = min(median), .groups = "drop") %>% 
  select(median, TNX) %>% 
  arrange(median) %>% 
  select(TNX) %>% 
  pull() %>%
  as.character()
tiff(file="Figures/Box5.tiff",
     width=6, height=8, units="in", res=300)
ggplot(gom_boxdata5, aes(y = Value, fill = factor(median), group = TNX)) +
  geom_boxplot() +
  scale_fill_grey(start = 0.9, end = 0.4, labels = labels,
                  name = "Legend:") +
  geom_abline(intercept = 0.25, slope = 0) + 
  theme(legend.position = "bottom") +
  labs(y = "TNX")
dev.off()

gom_boxdata6 <- gom_boxdata %>% 
  pivot_longer(cols = c(num_range("lambda_1_", 26:30), 
                        num_range("lambda_2_", 26:30)), 
               names_to = "Names", 
               values_to = "Value") %>%
  group_by(Names) %>%
  mutate(median = median(Value, na.rm = T))
gom_boxdata6$TX90P <- factor(gom_boxdata6$Names, 
                           levels = c("lambda_1_26", "lambda_1_27", "lambda_1_28", "lambda_1_29", "lambda_1_30",
                                      "lambda_2_26", "lambda_2_27", "lambda_2_28", "lambda_2_29", "lambda_2_30"),
                           labels = c("Profile 1 - Q1", "Profile 1 - Q2", "Profile 1 - Q3", "Profile 1 - Q4",
                                      "Profile 1 - Q5", "Profile 2 - Q1", "Profile 2 - Q2", "Profile 2 - Q3",
                                      "Profile 2 - Q4", "Profile 2 - Q5"))
labels <- gom_boxdata6 %>% 
  group_by(Names, TX90P) %>% 
  summarise(median = min(median), .groups = "drop") %>% 
  select(median, TX90P) %>% 
  arrange(median) %>% 
  select(TX90P) %>% 
  pull() %>%
  as.character()
tiff(file="Figures/Box6.tiff",
     width=6, height=8, units="in", res=300)
ggplot(gom_boxdata6, aes(y = Value, fill = factor(median), group = TX90P)) +
  geom_boxplot() +
  scale_fill_grey(start = 0.9, end = 0.4, labels = labels,
                  name = "Legend:") +
  geom_abline(intercept = 0.25, slope = 0) + 
  theme(legend.position = "bottom") + 
  labs(y = "TX90P")
dev.off()

gom_boxdata7 <- gom_boxdata %>% 
  pivot_longer(cols = c(num_range("lambda_1_", 31:35), 
                        num_range("lambda_2_", 31:35)), 
               names_to = "Names", 
               values_to = "Value") %>%
  group_by(Names) %>%
  mutate(median = median(Value, na.rm = T))
gom_boxdata7$TXX <- factor(gom_boxdata7$Names, 
                             levels = c("lambda_1_31", "lambda_1_32", "lambda_1_33", "lambda_1_34", "lambda_1_35",
                                        "lambda_2_31", "lambda_2_32", "lambda_2_33", "lambda_2_34", "lambda_2_35"),
                             labels = c("Profile 1 - Q1", "Profile 1 - Q2", "Profile 1 - Q3", "Profile 1 - Q4",
                                        "Profile 1 - Q5", "Profile 2 - Q1", "Profile 2 - Q2", "Profile 2 - Q3",
                                        "Profile 2 - Q4", "Profile 2 - Q5"))
labels <- gom_boxdata7 %>% 
  group_by(Names, TXX) %>% 
  summarise(median = min(median), .groups = "drop") %>% 
  select(median, TXX) %>% 
  arrange(median) %>% 
  select(TXX) %>% 
  pull() %>%
  as.character()
tiff(file="Figures/Box7.tiff",
     width=6, height=8, units="in", res=300)
ggplot(gom_boxdata7, aes(y = Value, fill = factor(median), group = TXX)) +
  geom_boxplot() +
  scale_fill_grey(start = 0.9, end = 0.4, labels = labels,
                  name = "Legend:") +
  geom_abline(intercept = 0.25, slope = 0) + 
  theme(legend.position = "bottom") + 
  labs(y = "TXX")
dev.off()
