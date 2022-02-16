# Dubois Challenge 2022 - Challenge 5 
# Cory Sauve 
# 2022-02-15

# Libs 
library(tidyverse)
library(here)
library(showtext)
library(cowplot)

# Load data 
df <- read_csv(here("data", "c5_2022.csv"))

# Add font 
font_add_google("Public Sans", "Public Sans")
showtext_auto()

# Define for secondary axis 
year_lines <- seq(1790, 1860, 10)
year_labs <- c("1.3%", "1.7", "1.7", "1.2", "0.8", "0.9", "0.7", "0.8", "100%")

# Create figure 
p_c5 <- 
  df %>%  
  # Data frame as missing year from original portrait. Add 1863 to data ...
  add_row(Year = 1863, Slave = 97, Free = 3) %>% 
  pivot_longer(Slave:Free) %>% 
  ggplot(aes(x = Year, y = value)) + 
  geom_area(aes(fill = name), color = "#EAD1B9", size = 0.5, show.legend = FALSE, outline.type = "full") +
  geom_vline(xintercept = year_lines, color = "#EAD1B9", size = 0.5) +
  coord_flip(ylim = c(97, 100)) + 
  scale_x_reverse(
    "", 
    breaks = seq(1790, 1870, 10), 
    expand = c(0, 0), 
    sec.axis = sec_axis(~ ., breaks = df$Year, labels = year_labs)
  ) +
  scale_y_continuous(
    "",
    breaks = c(97, 98, 99, 100), 
    labels = c("3%", "2%", "1%", ""), 
    position = "right"
  ) + 
  scale_fill_manual(
    values = c("#DC143c", "#000000")
  ) +
  labs(
    title = "SLAVES AND FREE NEGROES.", 
    caption = "Data: Anthony Starks | Design: Cory Sauve"
  ) +
  theme(
    axis.ticks = element_blank(),
    axis.text.x.top = element_text(size = 20),
    axis.text.y = element_text(hjust = -10, size = 20),
    panel.grid.major = element_line(color = "#EAD1B9"),
    panel.grid.minor = element_line(color = "#EAD1B9"),
    panel.background = element_rect(fill = "#EAD1B9"),
    plot.title = element_text(hjust = 0.5, vjust = 1, face = "bold", size = 32),
    plot.caption = element_text(vjust = -6, hjust = 4.5, size = 16),
    plot.background = element_rect(fill = "#EAD1B9", color = NA),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  )

# Draw label of secondary axis 
plot_grid(p_c5) + 
  draw_text("PERCENT", x = 0.82, y = 0.87, size = 16) +
  draw_text("OF", x = 0.82, y = 0.85, size = 16) +
  draw_text("FREE NEGROES", x = 0.82, y = 0.83, size = 16)

ggsave(last_plot(), filename = here("figures", "c5_2022.png"), height = 5, width = 3)
