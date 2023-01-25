
library(readxl)
library(tidyverse)
library(sysfonts)
library(gt)
library(showtext)
library(showtext)
library(ggplot2)
library(ggalt)
library(scales)
library(stringr)

# Load the data
df <- tibble(read.csv("Data/DIABETES.csv"))

# load fonts
font_add_google(name = "Poppins", family = "Poppins")
font_add_google(name = "Alegreya Sans", family = "Alegreya Sans")
showtext_auto()

# prep data
df <- na.omit(df)
plot_diabetes <- df %>% arrange(desc(MEAN))

# Wrap the subtitle text to a width of 40 characters
subtitle_text <- "The disparity in the cost of treating Diabetes can have a significant impact on society, as those living in areas with higher costs may struggle to afford the necessary treatments leading to poorer health outcomes."
subtitle_text <- str_wrap(subtitle_text, width = 100)


# plot
d <- ggplot(plot_diabetes, aes(x= reorder(COUNTRY, MEAN), y= MEAN)) +
     geom_point(size = 2,  color = "#efa599") +
     geom_segment(aes(x = COUNTRY, y = 0, xend = COUNTRY, yend = MEAN -46), size = 0.7, linetype = "dotted",  colour = alpha("#4c5ea1", 0.7)) +
     coord_flip()+
     labs(x = "", y = "", 
      title = "Mean anual expenditure for patients with Diabetes (in USD)", 
       subtitle = subtitle_text,
       caption= "Tamara-Angie|Data from Diabetes Atlas"
      )  + 
     theme(plot.title = element_text(colour = "#4c5ea1", family = "Poppins", size = 68, hjust = 0, lineheight = 0.7), 
           plot.subtitle = element_text(colour = "#4c5ea1", family = "Alegreya Sans", size = 46, hjust = 0, lineheight = 0.3),
           plot.caption = element_text(color= "#4c5ea1",  family = "Alegreya Sans", face = "italic",size = 25, hjust = 0),
        panel.background = element_rect(fill = "aliceblue", colour = "aliceblue"),
        plot.background = element_rect(fill = 'aliceblue', colour = 'aliceblue'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title.position = "panel",               
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text.y = element_text(colour = "#4c5ea1", family = "Alegreya Sans", size = 30, hjust = 1),
        axis.text.x = element_text(colour = "#4c5ea1", family = "Alegreya Sans", size = 30),
        plot.margin = unit(c(0.5, 2.5, 0.5, 1), "cm"))


##add arrow to the plot

d1 <- d + geom_curve(aes(x = 24, y = 11600, xend = 30, yend = 12195),
             size = 1, color = "#8AB0D4", angle = 80, stat = "unique",
             arrow = arrow(length = unit(0.03, "npc"))) +
    geom_text(aes(x = 24, y = 9350, label = "The United States of America has the highest \ntotal expenditure (around 3 billion USD/year)"),
              family = "Alegreya Sans", size = 12, color = "#4c5ea1", stat = "unique", lineheight = 0.3)


##add arrow and annotations (Mexico)

d2 <- d1 + geom_curve(aes(x = 20, y = 2200, xend = 18, yend = 1060),
                     size = 1, color = "#8AB0D4", angle = 50, stat = "unique",
                     arrow = arrow(length = unit(0.03, "npc"))) +
  geom_text(aes(x = 19, y = 4900, label = "14% of adults (over 18) in Mexico have been diagnosed with Diabetes"),
            family = "Alegreya Sans", size = 12, color = "#4c5ea1", stat = "unique")

d2


##add arrow and annotations (Mexico)


d3 <- d2 + geom_curve(aes(x = 8, y = 540, xend = 7, yend = 3600),
                      size = 1, color = "#8AB0D4",  angle = 260, stat = "unique",
                      arrow = arrow(length = unit(0.03, "npc"))) +
  geom_text(aes(x = 8.5, y = 5200, label = "The prevalence of diabetes in Belize is estimated to be around 10.5% \nwith an estimated 28,000 people living with the disease in the country."),
            family = "Alegreya Sans", size = 12, color = "#4c5ea1", stat = "unique", lineheight = 0.3)

d3

ggsave(d3, filename = "Diabetes/vizA4.jpg", height = 21, width = 29.7, units = "cm", dpi = 300)

