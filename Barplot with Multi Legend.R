#rm(list = ls())


#--------------------------------Aggregate Decomposition ----------------------------------------------
y <- data.frame(Region = factor(rep(c("Overall", "Amazonia", "Northeast"), each = 6),#                |  
                               levels = c("Overall", "Amazonia", "Northeast")),#                      |
               Type = factor(rep(c("Exposure", "Suceptibility", "Adaptative Capacity"), times = 6),#  |
                             levels = c("Adaptative Capacity", "Suceptibility", "Exposure")),#        |
               Morb = factor(rep(c("Low", "High"), times = 9), levels = c("Low", "High")),#           |
               Value = c(9.6, 31.2, 48.4, 23.3, 42.0, 45.5,#                                          |
                         8.1, 32.3, 53.0, 19.9, 38.9, 47.8,#                                          |
                         16.0, 28.3, 28.0, 32.6, 56.0, 39.1))#                                        |
#-----------------------------------------------------------------------------------------------------#

#------------------------------Saving the Picture---------------------------------------
tiff(file="Figures/decomp_aggregate.tiff",#                                            |
     width=6, height=6, units="in", res=300)#                                          |
ggplot(y, aes(x = Morb, y = Value, fill = Type)) +#                                    |
  geom_bar(position = "fill", stat = "identity") +#                                    |
  facet_wrap(~Region) +#                                                               |
  scale_y_continuous(labels = scales::percent_format()) +#                             |
  scale_fill_discrete(name = "") +#                                                    |
  geom_text(aes(y = Value/100, label = scales::percent(Value/100, accuracy = 0.1)),#   |
            position = position_fill(vjust = 0.5),#                                    |                
            size = 4,#                                                                 |                            
            show.legend = FALSE) +#                                                    |
  theme_minimal() +#                                                                   |
  theme(legend.position = "bottom", axis.title.x = element_blank())#                   |
dev.off()#                                                                             |
#--------------------------------------------------------------------------------------#

#-----------------------Reading the data  ------------------------- 
x <- readxl::read_excel(path="decomp.xlsx", sheet = "Planilha1")# |
#-----------------------------------------------------------------#

#------------------Installing packages required for visualization------------------
myPackages <- c("RColorBrewer", "dplyr", "magrittr", "cowplot", "ggplot2")#       |
new.packages <- myPackages[!(myPackages %in% installed.packages()[,"Package"])]#  |
if(length(new.packages)) install.packages(new.packages)#                          |
lapply(myPackages, require, character.only = TRUE)#                               |
#---------------------------------------------------------------------------------#

#-------------------Data Manipulation-----------------------------
x$Indicator %<>% factor(levels = unique(x$Indicator))#           |
x$Percent <- round(c(x$Value[1:15]/sum(x$Value[1:15]),#          | 
        x$Value[16:30]/sum(x$Value[16:30]),#                     |   
        x$Value[31:45]/sum(x$Value[31:45])), #                   |
      digits = 3)#                                               |
#----------------------------------------------------------------#

#-------------------Pallete for color-----------------------------
My_palette <- setNames(c(brewer.pal(7, "Blues"), #               |
                         brewer.pal(4, "Reds"), #                |
                         brewer.pal(4, "Greens")),#              | 
                       levels(x$Indicator))#                     |
#----------------------------------------------------------------#

#--------------------------Main Plot----------------------------------------------
full_plot <- ggplot(x, aes(x = Region, y = Value, fill = Indicator)) + #         |
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
  filter(`Indicator` %in% c("Elderly", "Children", "Income", "Literate")) %>%# |
  ggplot(aes(x = Region, y = Value, fill = Indicator)) + #                       |
  geom_bar(position = "fill",stat = "identity") + #                              |
  scale_y_continuous(labels = scales::percent_format()) + #                      |
  scale_fill_manual(values = My_palette, name = "Susceptibility") #              |
                                                            #                    |
third_legend <- x %>% #                                                          |
  filter(`Indicator` %in% c("Sewage", "Water", "Garbage", "Urbanization")) %>% #          |
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



