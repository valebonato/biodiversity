#VIOLIN BOXPLOT
# Carica librerie necessarie
library(dplyr)
library(ggplot2)

# Percorso del file ESH storico
hist_esh_file <- './fishsuit-africa/proc/w5e5/hist_existing/ESH_tab_hist.csv'

# Carica il file ESH storico
tab <- read.csv(hist_esh_file) %>%
  dplyr::select(id_no, ESH_all) %>%  # Seleziona solo le colonne necessarie
  rename(ESH = ESH_all) %>%          # Rinomina `ESH_all` in `ESH` per semplificare
  mutate(configuration = 'Historical existing dams')  # Aggiungi una colonna con etichetta fissa per asse x

# Genera il grafico con violin plot e boxplot
p <- ggplot(tab, aes(x = configuration, y = ESH, fill = configuration)) +
  geom_violin(width = 0.2, color = "transparent", alpha = 0.5) + # Aggiunge il violin plot
  geom_boxplot(fill = "white", color = "black", outlier.color = NA, 
               outlier.shape = NA, width = 0.1, lwd = 0.5, coef = 0, notch = TRUE) + # Boxplot più stretto
  
  # Color scales
  scale_fill_manual(values = viridis::viridis(10, option = "C")[5]) +
  scale_color_manual(values = viridis::viridis(10, option = "C")[5]) +
  
  stat_summary(fun = mean, geom = "point", 
               shape = 23, size = 2, color = "black", fill = viridis::viridis(10, option = "C")[5],
               position = position_dodge(width = 0.9), show.legend = FALSE) +
  
  # Asse y ristretto a 0-50%
  scale_y_reverse(breaks = seq(0, 50, by = 5), limits = c(50, 0), labels = paste0(seq(0, 50, by = 5), "%")) +
  ylab(label = "Percentage of range threatened") +
  xlab("") +  # Rimuove l'etichetta dell'asse x
  
  # Tema e personalizzazioni
  theme_bw() +
  coord_cartesian(expand = FALSE) +
  theme(
    legend.position = "none",   # Rimuove la legenda
    panel.grid.major.y = element_line(linetype = "dashed", color = "black"),  # Linee tratteggiate per asse y
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_blank(),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.text.x = element_blank(),  # Rimuove il testo dei valori sull'asse x
    axis.text.y = element_text(size = 10, color = "black"),
    axis.title.x = element_blank(),  # Rimuove completamente il titolo dell'asse x
    axis.title.y = element_text(size = 12),
    text = element_text(size = 10),
    strip.background = element_blank(), 
    strip.text.x = element_blank()  # Rimuove la doppia etichetta sull'asse x
  )

# Visualizza il grafico
print(p)

# Salvataggio del grafico
ggsave(paste0("./fishsuit-africa/figs/hist_ESH_violin_0_50.jpg"), p, width = 89, height = 120, units = "mm", dpi = 600)
ggsave(paste0("./fishsuit-africa/figs/hist_ESH_violin_0_50.pdf"), p, width = 89, height = 120, units = "mm")

#----VISUALIZZA MEDIA E MEDIANA e quante specie superano 10%
# Genera il grafico con violin plot e boxplot, aggiungendo etichette di media e mediana 
# Conta quante specie superano il 10% di ESH
species_above_10 <- tab %>% 
  filter(ESH > 10) %>%    # Filtra le specie con ESH maggiore di 10%
  nrow()                  # Conta il numero di righe che soddisfano la condizione

# Stampa il risultato
cat("Numero di specie con ESH superiore al 10%:", species_above_10, "\n")


#stampa media mediana di fianco al grafico
# Calcola i valori di media e mediana per posizionare le etichette
summary_values <- tab %>% 
  group_by(configuration) %>% 
  summarise(mean_val = mean(ESH, na.rm = TRUE),
            median_val = median(ESH, na.rm = TRUE))

# Crea il grafico
p <- ggplot(tab, aes(x = configuration, y = ESH, fill = configuration)) +
  geom_violin(width = 0.2, color = 'transparent', alpha = 0.5) + # Aggiunge il violin plot
  geom_boxplot(fill = 'white', color = "black", outlier.color = NA, 
               outlier.shape = NA, width = 0.1, lwd = 0.5, coef = 0, notch = 1) + # Boxplot più stretto
  
  # Color scales
  scale_fill_manual(values = viridis::viridis(10, option = 'C')[5]) +
  scale_color_manual(values = viridis::viridis(10, option = 'C')[5]) +
  
  # Media come punto
  stat_summary(fun = mean, geom = "point", 
               shape = 23, size = 2, color = 'black', fill = viridis::viridis(10, option = 'C')[5],
               position = position_dodge(width = 0.9), show.legend = FALSE) +
  
  # Annotazioni per media e mediana attaccate al lato destro del grafico
  annotate("text", x = 1, y = summary_values$mean_val[1], label = paste0("Mean: ", round(summary_values$mean_val[1], 1)), 
           hjust = -0.1, color = "black", size = 3) +
  annotate("text", x = 1, y = summary_values$median_val[1], label = paste0("Median: ", round(summary_values$median_val[1], 1)), 
           hjust = -0.1, color = "blue", size = 3) +
  annotate("text", x = 2, y = summary_values$mean_val[2], label = paste0("Mean: ", round(summary_values$mean_val[2], 1)), 
           hjust = -0.1, color = "black", size = 3) +
  annotate("text", x = 2, y = summary_values$median_val[2], label = paste0("Median: ", round(summary_values$median_val[2], 1)), 
           hjust = -0.1, color = "blue", size = 3) +
  
  # Asse y ristretto a 0-50%
  scale_y_reverse(breaks = seq(0, 50, by = 5), limits = c(50, 0), labels = paste0(seq(0, 50, by = 5), '%')) +
  ylab(label = 'Percentage of range threatened') +
  xlab("Historical existing dams") +
  
  # Tema e personalizzazioni
  theme_bw() +
  coord_cartesian(expand = FALSE, clip = "off") + # Disattiva il clipping per le annotazioni
  theme(
    legend.position = "none",   # Rimuove la legenda
    panel.grid.major.y = element_line(linetype = 'dashed', color = 'black'),  # Linee tratteggiate per asse y
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_blank(),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.text.x = element_text(size = 12, color = 'black', vjust = 3, margin = margin(t = 5)),
    axis.text.y = element_text(size = 10, color = 'black'),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    text = element_text(size = 10),
    strip.background = element_blank(), 
    strip.text.x = element_blank()  # Rimuove la doppia etichetta sull'asse x
  )

# Visualizza il grafico
print(p)

# Salvataggio del grafico
ggsave(paste0('./fishsuit-africa/figs/histex_ESH_violin_0_50_stampa.jpg'), p, width = 89, height = 120, units = 'mm', dpi = 600)
ggsave(paste0('./fishsuit-africa/figs/histex_ESH_violin_0_50_stampa.pdf'), p, width = 89, height = 120, units = 'mm')



#-----------OLD STAMPA MEDIA MEDIANA
p <- ggplot(tab, aes(x = configuration, y = ESH, fill = configuration)) +
  geom_violin(width = 0.2, color = 'transparent', alpha = 0.5) + # Aggiunge il violin plot
  geom_boxplot(fill = 'white', color = "black", outlier.color = NA, 
               outlier.shape = NA, width = 0.1, lwd = 0.5, coef = 0, notch = 1) + # Boxplot più stretto
  
  # Color scales
  scale_fill_manual(values = viridis::viridis(10, option = 'C')[5]) +
  scale_color_manual(values = viridis::viridis(10, option = 'C')[5]) +
  
  # Media come punto
  stat_summary(fun = mean, geom = "point", 
               shape = 23, size = 2, color = 'black', fill = viridis::viridis(10, option = 'C')[5],
               position = position_dodge(width = 0.9), show.legend = FALSE) +
  
  # Aggiungi etichetta per la media
  stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 1)), 
               color = "black", vjust = -0.5, size = 3) +
  
  # Aggiungi etichetta per la mediana
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)), 
               color = "blue", vjust = 1.5, size = 3) +
  
  # Asse y ristretto a 0-50%
  scale_y_reverse(breaks = seq(0, 50, by = 5), limits = c(50, 0), labels = paste0(seq(0, 50, by = 5), '%')) +
  ylab(label = 'Percentage of range threatened') +
  xlab("Historical existing dams") +
  
  # Tema e personalizzazioni
  theme_bw() +
  coord_cartesian(expand = FALSE) +
  theme(
    legend.position = "none",   # Rimuove la legenda
    panel.grid.major.y = element_line(linetype = 'dashed', color = 'black'),  # Linee tratteggiate per asse y
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_blank(),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.text.x = element_text(size = 12, color = 'black', vjust = 3, margin = margin(t = 5)),
    axis.text.y = element_text(size = 10, color = 'black'),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    text = element_text(size = 10),
    strip.background = element_blank(), 
    strip.text.x = element_blank()  # Rimuove la doppia etichetta sull'asse x
  )

# Visualizza il grafico
print(p)

# Salvataggio del grafico
ggsave(paste0('./fishsuit-africa/figs/histex_ESH_violin_0_50_stampe.jpg'), p, width = 89, height = 120, units = 'mm', dpi = 600)
ggsave(paste0('./fishsuit-africa/figs/histex_ESH_violin_0_50_stampe.pdf'), p, width = 89, height = 120, units = 'mm')

#------BOXPLOT ONLY
# Carica librerie necessarie
library(dplyr)
library(ggplot2)

# Percorso del file ESH storico
hist_esh_file <- './fishsuit-africa/proc/w5e5/hist_existing/ESH_tab_hist.csv'

# Carica il file ESH storico
tab <- read.csv(hist_esh_file) %>%
  dplyr::select(id_no, ESH_all) %>%  # Seleziona solo le colonne necessarie
  rename(ESH = ESH_all) %>%           # Rinomina `ESH_all` in `ESH` per semplificare
  mutate(configuration = 'Historical existing dams')  # Aggiungi una colonna con etichetta fissa per asse x

# Genera il boxplot con un intervallo y tra 0% e 50%
p <- ggplot(tab, aes(x = configuration, y = ESH, fill = configuration)) +
  geom_boxplot(fill = 'white', color = "black", outlier.color = NA, 
               outlier.shape = NA, width = 0.3, lwd = 0.5, coef = 0, notch = 1) +
  
  # Color scales
  scale_fill_manual(values = viridis::viridis(10, option = 'C')[5]) +
  scale_color_manual(values = viridis::viridis(10, option = 'C')[5]) +
  
  stat_summary(fun = mean, geom = "point", 
               shape = 23, size = 2, color = 'black', fill = viridis::viridis(10, option = 'C')[5],
               position = position_dodge(width = 0.9), show.legend = FALSE) +
  
  # Asse y ristretto a 0-50%
  scale_y_reverse(breaks = seq(0, 50, by = 5), limits = c(50, 0), labels = paste0(seq(0, 50, by = 5), '%')) +
  ylab(label = 'Percentage of range threatened') +
  xlab("Historical existing dams") +
  
  # Tema e personalizzazioni
  theme_bw() +
  coord_cartesian(expand = FALSE) +
  theme(
    legend.position = "none",   # Rimuove la legenda
    panel.grid.major.y = element_line(linetype = 'dashed', color = 'black'),  # Linee tratteggiate per asse y
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_blank(),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.text.x = element_text(size = 12, color = 'black', vjust = 3, margin = margin(t = 5)),
    axis.text.y = element_text(size = 10, color = 'black'),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    text = element_text(size = 10),
    strip.background = element_blank(), 
    strip.text.x = element_blank()  # Rimuove la doppia etichetta sull'asse x
  )

# Visualizza il grafico
print(p)

# Salvataggio del grafico
ggsave(paste0('./fishsuit-africa/figs/hist_ESH_boxplot_0_50.jpg'), p, width = 89, height = 120, units = 'mm', dpi = 600)
ggsave(paste0('./fishsuit-africa/figs/hist_ESH_boxplot_0_50.pdf'), p, width = 89, height = 120, units = 'mm')

