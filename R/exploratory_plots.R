library(tidyverse)
library(ggpattern)
askl;fndAS:Dnf;sdnfl;jk
# define the plotting theme to be used in subsequent ggplots
luke_theme <- 
  theme_bw() +
  theme(
    # text = element_text(family = "Franklin Gothic Book"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    axis.title.x = element_text(size = 10),
    axis.text.x  = element_text(size = 8), 
    axis.title.y = element_text(size = 10),
    axis.text.y = element_text(size = 8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(linewidth = 0.25, lineend = "round", colour = "grey60"),
    axis.ticks.length = unit(0.1, "cm"),
    panel.border = element_rect(linetype = "solid", colour = "grey")
  )

bashar_data <-
  read.csv(file = "data/26-June_meeting.csv") %>% 
  mutate(date = paste(year, ifelse(nchar(date) == 3,
                                   substring(date, first = 2, last = 3),
                                   substring(date, first = 3, last = 4)),
                      ifelse(nchar(date) == 3,
                             substring(date, first = 1, last = 1),
                             substring(date, first = 1, last = 2)),
                      sep = "-") %>% as.Date())

ggplot(data = bashar_data) +
  geom_area_pattern(aes(x = date, y = moult),
                    pattern = "gradient",
                    pattern_fill = "white",
                    pattern_fill2 = "#cc4c02", pattern_angle = 0, color = "white", size = 0.3) +
  geom_bar(aes(x = date, y = moult), stat='identity') +
  facet_wrap(. ~ bird, ncol = 1, strip.position = "right") +
  ylab("breeding plumage proportion") +
  xlab("date") +
  luke_theme +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(size = 10, face = "italic"),
        plot.title = element_text(hjust = 0.5, size = 11),
        plot.subtitle = element_text(hjust = 0.5, size = 9, face = "italic"),
        # axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text.y.right = element_text(angle = 0),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        panel.spacing = unit(0, "cm", data = NULL)) #+
  scale_x_date(date_labels = "%B", expand = c(0.01, 0.01), 
               date_breaks = "1 month", 
               limits = c(as.Date("2021-07-05"), as.Date("2022-05-01"))) +
  scale_color_manual(values = c("#cc4c02", "#000000"))
  