# ─────────────────────────────────────────────────────────────
#  Juventus Case Study · Analisi & Dashboard (2011-2024)
#  Autore : Manuel Lomartire · maggio 2025
# ─────────────────────────────────────────────────────────────
library(tidyverse)
library(janitor)
library(scales)
library(ggtext)
library(ggthemes)
library(patchwork)
library(here)

# 1. Import ---------------------------------------------------
# 1. Import: Leggi tutto come testo puro
raw <- read_csv(here("juventus.csv"),
                col_types = cols(.default = "c"))

# 2. Pulizia e conversione robusta
tidy <- raw %>% 
  rename(variabile = `Colonna 1`) %>% 
  filter(variabile != "Colonna 1") %>% 
  pivot_longer(-variabile,
               names_to = "stagione",
               values_to = "valore") %>% 
  pivot_wider(names_from = variabile, values_from = valore) %>% 
  clean_names() %>% 
  mutate(
    stagione = factor(stagione, levels = unique(stagione), ordered = TRUE)
  ) %>% 
  mutate(across(-stagione, ~ parse_number(.x, locale = locale(decimal_mark = ",", grouping_mark = "."))))

  


# Esclude stagione vuota 24/25
tidy_no_na <- tidy %>% filter(!is.na(punti))

# Colonna era
tidy_no_na <- tidy_no_na %>% 
  mutate(era = if_else(as.numeric(substr(stagione, 1, 4)) <= 2019,
                       "Scudetti", "Post-2020"))

# 3. Tema -----------------------------------------------------
theme_juve <- theme_minimal(base_family = "Helvetica") +
  theme(
    plot.title  = element_markdown(face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
theme_set(theme_juve)

# 4. Grafici --------------------------------------------------
## 4.1 Trend punti
g_punti <- ggplot(tidy_no_na, aes(stagione, punti, group = 1)) +
  geom_line(colour = "#0B60B0", linewidth = 1.2) +
  geom_point(size = 3, colour = "#0B60B0") +
  geom_text(aes(label = if_else(stagione == "2022/2023", "-10", "")),
            vjust = -1, colour = "red") +
  labs(title = "**Trend punti**", x = NULL, y = "Punti")

## 4.2 Valore rosa vs Punti
g_valore <- ggplot(tidy_no_na, aes(valore_rosa, punti, colour = era)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed",
              colour = "grey40") +
  geom_text(aes(label = stagione), vjust = -0.7, size = 3,
            show.legend = FALSE) +
  scale_colour_manual(values = c("Scudetti" = "#0B60B0",
                                 "Post-2020" = "#CC0000")) +
  labs(title = "**Valore rosa vs Punti**",
       x = "Valore rosa (mln €)", y = "Punti", colour = NULL) +
  scale_x_continuous(labels = label_number(big.mark = ".", decimal.mark = ","))

## 4.3 Lollipop posizione
g_pos <- ggplot(tidy_no_na, aes(stagione, posizione_finale_in_classifica)) +
  geom_segment(aes(xend = stagione, y = 1,
                   yend = posizione_finale_in_classifica, colour = era),
               linewidth = 1.1, show.legend = FALSE) +
  geom_point(aes(colour = era), size = 4, show.legend = FALSE) +
  geom_text(aes(label = posizione_finale_in_classifica),
            colour = "white", fontface = "bold", size = 3) +
  scale_y_reverse(breaks = 1:10, limits = c(10.5, .5)) +
  scale_colour_manual(values = c("#0B60B0", "#CC0000")) +
  labs(title = "**Posizione finale**", x = NULL, y = "Posizione")

## 4.4 Età media
g_eta <- ggplot(tidy_no_na, aes(stagione, eta_media, group = 1)) +
  geom_line(colour = "#888888", linewidth = 1.1) +
  geom_point(size = 3, colour = "#888888") +
  labs(title = "**Età media rosa**", x = NULL, y = "Età (anni)")

## 4.5 Mercato
g_mercato <- ggplot(tidy_no_na, aes(stagione)) +
  geom_col(aes(y = -spese,   fill = "Spese"),   width = 0.6) +
  geom_col(aes(y = entrate, fill = "Entrate"), width = 0.6) +
  geom_line(aes(y = netto_spesa_guadagno_calciomercato),
            colour = "black", linewidth = 1.1, group = 1) +
  geom_point(aes(y = netto_spesa_guadagno_calciomercato),
             colour = "black", size = 2) +
  scale_y_continuous(
    labels  = label_number(big.mark = ".", decimal.mark = ","),   # ← qui
    sec.axis = dup_axis(name = "Net-spend")
  ) +
  scale_fill_manual(values = c("Spese" = "#CC0000", "Entrate" = "#0B60B0")) +
  labs(title = "**Mercato: spese vs entrate**",
       x = NULL, y = "€ milioni", fill = NULL)


# 5. Dashboard patchwork -------------------------------------
col_sx <- g_punti / g_valore + plot_layout(heights = c(2, 1))
col_dx <- (g_pos / g_eta / g_mercato) + plot_layout(ncol = 1)

dashboard <- col_sx | col_dx +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# 6. Output ---------------------------------------------------
dir.create(here("output"), showWarnings = FALSE)
write_csv(tidy, here("output", "juve_clean.csv"))
ggsave(here("output", "juve_dashboard_v2.png"),
       dashboard, width = 14, height = 9, dpi = 300)

message("✓ Tutto pulito: numeri con virgola corretti e nuova dashboard salvata.")
