# Load libraries
library(causaldata)
library(modelsummary)
library(tidyr)
library(tidyverse)

# Load data

# Specify your own folder
folder_name <- "/Users/charlottederogis/Desktop/Stage/CSV\ populations/"
# Specify your dataset
dataset_name <- "table_snds_garcons.csv"
file_name <- paste0(folder_name,dataset_name)

# SNDS data for boys and men
df <- read.csv(file_name)

# Date of public policy (time zero)
CUTOFF_DATE <- "2016-05-02"
CUTOFF <- as.Date(CUTOFF_DATE)

# Length of time interval pre- and post-policy (2, 3, or 4 years)
BANDWIDTH <- 365 * 3

# Filter original dataset to select the population of interest
# Focus on a specific time period
# Create the variables required to fit the model
df <- df %>%
  mutate(
    Date    = as.Date(Date),
    t       = as.integer(Date - as.Date(CUTOFF)),
    treated = as.integer(Date >= CUTOFF)
  ) %>% 
  filter(abs(t) <= BANDWIDTH)

# Kink design: change in slope, but no change in level around time zero
m_kink <- lm(n ~ t + t:treated, data = df)

# Fitted values
df$fitted_m_kink <- m_kink$fitted.values

# Transform the dataset into a long format
df_long <- df %>% 
  pivot_longer(cols = c(n, fitted_m_kink), 
               names_to = "model", 
               values_to = "fitted_value")
df_long$model_f <- factor(df_long$model,
                              levels=c("n","fitted_m_kink"))

# Model summary

# Formatting, using x decimal digits (by default: x=2)
number_decimal_digits <- 2
msummary(list('Coefficients' = m_kink),
         stars = c('*' = .1, '**' = .05, '***' = .01), 
         vcov = 'robust',
         statistic = c("conf.int", "p.value"),
         gof_map = c("r.squared", "adj.r.squared"),
         fmt = number_decimal_digits)

# Data visualization

# Figure parameters
x_axis_label <- "Number of years before/after time zero"
y_axis_label <- "Monthly number of reimbursed HPV vaccine dispensations"

pre_policy_position_x <- -1.5
pre_policy_position_y <- 750

post_policy_position_x <- 1.5
post_policy_position_y <- 750

policy_font_size <- 4.5
policy_color <- "black"

time_zero_position_x <- -0.2
time_zero_position_y <- 1190
time_zero_font_size <- 4
time_zero_color <- "black"

legend_position <- c(0.16,0.86)
legend_title_font_size <- 11.5
legend_text_font_size <- 11

color_x_ticks <- "black"
color_y_ticks <- "black"

color_x_axis_label <- "black"
color_y_axis_label <- "black"

x_ticks_font_size <- 12
y_ticks_font_size <- 12

x_axis_label_font_size <- 12.5
y_axis_label_font_size <- 12.5

# Figure
p <- ggplot(df_long, aes(x = t/365.25, y = fitted_value, group = model_f)) +
  geom_line(aes(color = model_f)) +
  geom_point(aes(color = model_f)) +
  
  # x-axis and y-axis labels
  xlab(x_axis_label) +
  ylab(y_axis_label) +
  
  # Legend title
  labs(color = "Time series") +
  
  # Legend labels
  scale_color_discrete(labels = c("fitted_m_kink" = "Fitted",
                                  "n" = "Observed")) +
  geom_vline(xintercept = 0, lty = 2) +
  
  # Text annotations
  annotate("text", x = pre_policy_position_x, y = pre_policy_position_y, label = "Pre-policy",
           colour = policy_color, fontface='italic', size = policy_font_size) +
  annotate("text", x = post_policy_position_x, y = post_policy_position_y, label = "Post-policy",
           colour = policy_color, fontface='italic', size = policy_font_size) +
  annotate("text", x = time_zero_position_x, y = time_zero_position_y, label = "Time zero",
           colour = time_zero_color, size = time_zero_font_size, angle = 90, vjust = 1) +
  
  # Black and white background
  theme_bw() + 
  theme(# Legend position (inside the figure to save space)
        legend.position=legend_position, 
        # Legend title: bold if you'd like, font size can be adjusted
        legend.title = element_text(face = "bold", size = legend_title_font_size), 
        # Legend labels: font size can be adjusted
        legend.text = element_text(size = legend_text_font_size),
        # Legend background (none by default, but you can fill the rectangle with a color of your choice and put a line around it as well)
        legend.background = element_rect(fill = NA, color = NA),
        # x-ticks: you can adjust the color and font size
        axis.text.x = element_text(color = color_x_ticks, size = x_ticks_font_size),
        # y-ticks: you can adjust the color and font size
        axis.text.y=element_text(color = color_y_ticks, size = y_ticks_font_size),
        # x-axis label: you can adjust the color and font size
        axis.title.x=element_text(color = color_x_axis_label, size = x_axis_label_font_size),
        # y-axis label: you can adjust the color and font size
        axis.title.y=element_text(color = color_y_axis_label, size = y_axis_label_font_size))
p
