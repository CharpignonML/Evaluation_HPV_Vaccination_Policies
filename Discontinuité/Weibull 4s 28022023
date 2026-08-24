library(minpack.lm)
library(ggplot2)
library(dplyr)

df <- read.csv2("/Users/charlottederogis/Desktop/Stage/R/discontinuités/googletrends/28022023.csv")
df$Date <- as.Date(df$Date)
df$n <- as.numeric(df$n)
t0 <- as.Date("2023-02-28")

# 4 semaines avant et après
df <- df %>%
  filter(Date >= t0 - 28, Date <= t0 + 28)

mediane_avant <- median(df$n[df$Date < t0], na.rm = TRUE)

# Préparation des données pour le fit 

post <- df[df$Date >= as.Date("2023-02-28"), ]
post$t          <- post$Date -  as.Date("2023-02-28")
post$n_centré <- post$n - mediane_avant

post$t <- as.numeric(post$t)

# Fit du modèle Weibull 

A_fixe <- 100 - mediane_avant

fit_weibull <- nlsLM(
  n_centré ~ A_fixe * exp(-(t / eta)^beta),
  data    = post,
  start   = list(eta  = 3,      # ~ quelques jours
                 beta = 1),      # commence à exponentielle
  lower   = c(eta = 0.01, beta = 0.1),
  upper   = c(eta = 100,  beta = 5),
  control = nls.lm.control(maxiter = 200)
)

# Calcul du R²
# R² = 1 - (somme des carrés des résidus / somme des carrés totale)
residus       <- residuals(fit_weibull)
SS_res        <- sum(residus^2)
SS_tot        <- sum((post$n_centré - mean(post$n_centré))^2)
R2            <- 1 - (SS_res / SS_tot)
cat("R² du modèle Weibull :", round(R2, 4), "\n")

# --- Nombre de jours pour revenir à la baseline ---

coefs <- coef(fit_weibull)
eta_est  <- coefs["eta"]
beta_est <- coefs["beta"]

jours_retour_baseline <- eta_est * (-log(0.05))^(1 / beta_est)
cat("Nombre de jours pour revenir à la baseline (seuil", 0.05*100, "%) :",
    round(jours_retour_baseline, 1), "jours\n")


# Visualisation

t_grid <- seq(0, max(post$t), length.out = 200)
df_fit <- data.frame(
  Date = t_grid + t0,
  n    = mediane_avant + predict(fit_weibull, newdata = data.frame(t = t_grid))
)

ggplot(df, aes(x = Date, y = n)) +
  geom_hline(yintercept = mediane_avant, linetype = "dotted", color = "gray40") +
  geom_vline(xintercept = t0, linetype = "dashed", color = "firebrick") +
  geom_point(size = 2) +
  geom_line(data = df_fit, aes(x = Date, y = n),
            color = "steelblue", linewidth = 1) +
  annotate("text", x = min(df$Date), y = mediane_avant + 3,
           label = paste("Médiane avant =", round(mediane_avant, 1)),
           hjust = 0, color = "gray40", size = 5) +
  annotate("text", x = t0 - 1, y =  max(df$n),
           label = paste("28 février 2023"),
           hjust = 1, color = "firebrick", size = 5) +
  scale_x_date(date_breaks = "3 days", date_labels = "%d %b") +
  labs(title = "Données + fit Weibull",
       x = "Jour", y = "Intensité") +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(size = 12, angle = 0, hjust = 1),
    axis.text.y  = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    plot.title   = element_text(size = 16, face = "bold")
  )
