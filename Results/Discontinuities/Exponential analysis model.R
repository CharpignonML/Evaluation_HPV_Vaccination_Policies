# Load libraries
library(minpack.lm)
library(ggplot2)
library(dplyr)

save_plot <- function(plot, filename, width = 10, height = 6, dpi = 300) {
  ggsave(
    filename = filename,
    plot     = plot,
    width    = width,
    height   = height,
    units    = "in",
    dpi      = dpi
  )
}

# Specify your own folder
folder_name <- "/Users/charlottederogis/Desktop/Stage/CSV\ populations/GT/"
# Specify your dataset
dataset_name <- "28022023.csv"
dataset_name <- "02102023.csv"
dataset_name <- "16122019.csv"

file_name <- paste0(folder_name,dataset_name)
df <- read.csv2(file_name)

# Transform dates and outcome values
df$Date <- as.Date(df$Date)
df$n <- as.numeric(df$n)
t0 <- as.Date("2023-02-28")

# x semaines avant et après
df <- df %>%
  filter(Date >= t0 - 21, Date <= t0 + 21)

mediane_avant <- median(df$n[df$Date < t0], na.rm = TRUE)

# Préparation des données pour le fit 
post <- df[df$Date >= t0, ]
post$t <- post$Date - t0
post$n_centré <- post$n - mediane_avant
post$t <- as.numeric(post$t)

# Fit du modèle exponentiel décroissant
A_fixe <- 100 - mediane_avant
fit_exp <- nlsLM(
  n_centré ~ A_fixe * exp(-t / tau),
  data    = post,
  start   = list(tau = 3),        # ~ quelques jours
  lower   = c(tau = 0.01),
  upper   = c(tau = 100),
  control = nls.lm.control(maxiter = 200)
)

# Calcul du R²
# R² = 1 - (somme des carrés des résidus / somme des carrés totale)
residus       <- residuals(fit_exp)
SS_res        <- sum(residus^2)
SS_tot        <- sum((post$n_centré - mean(post$n_centré))^2)
R2            <- 1 - (SS_res / SS_tot)
cat("R² du modèle exponentiel :", round(R2, 4), "\n")

# --- Nombre de jours pour revenir à la baseline ---
coefs <- coef(fit_exp)
tau_est  <- coefs["tau"]
jours_retour_baseline <- -tau_est * log(0.05)
cat("Nombre de jours pour revenir à la baseline (seuil", 0.05*100, "%) :",
    round(jours_retour_baseline, 1), "jours\n")

# Visualisation
t_grid <- seq(0, max(post$t), length.out = 200)
df_fit <- data.frame(
  Date = t_grid + t0,
  n    = mediane_avant + predict(fit_exp, newdata = data.frame(t = t_grid))
)

# Option 1
plot1<- ggplot(df, aes(x = Date, y = n)) +
  geom_hline(
    yintercept = mediane_avant,
    linetype = "dashed",
    color = "gray40"
  ) +
  geom_vline(
    xintercept = t0,
    color = "firebrick"
  ) +
  geom_point(size = 2) +
  geom_line(
    data = df_fit,
    aes(x = Date, y = n),
    color = "steelblue",
    linewidth = 1
  ) +
  annotate(
    "text",
    x = min(df$Date),
    y = mediane_avant + 3,
    label = paste(
      "Pre-period median value =",
      round(mediane_avant, 1)
    ),
    hjust = -0.18,
    vjust = -1.5,
    color = "gray40",
    size = 4
  ) +
  annotate(
    "text",
    x = t0 - 0.75,
    y = 100,
    label = "Ann. of school vax campaigns",
    angle = 90,
    hjust = 0.98,
    vjust = 0,
    color = "firebrick",
    size = 4
  ) +
  scale_x_date(
    date_breaks = "3 days",
    date_labels = "%b %d"
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = c(0, 20, 40, 60, 80, 100)
  ) +
  labs(
    x = "Day",
    y = "Intensity of HPV-related Google searches"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      size = 12,
      angle = 90,
      vjust = 0.5
    ),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )
plot1

save_plot(
  plot1,
  "/Users/charlottederogis/Desktop/Stage/VF analyses/validés pour de vrai/Weibull/exp 28022023 3s.png"
)
