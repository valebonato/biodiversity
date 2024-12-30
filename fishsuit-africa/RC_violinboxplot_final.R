
# Carica le librerie
source('africa_config.R') # Carica le configurazioni
library(foreach); library(dplyr); library(ggplot2); library(RColorBrewer); library(sf)

# Parametri dei modelli climatici, scenari e configurazioni
climate_models <- c('GFDL', 'IPSL', 'MPI', 'MRI', 'UKESM')
scen <- c(1, 3, 5)
conf <- c("ex", "fut")
years <- c("2020_2029", "2030_2039", "2040_2049")

# Genera combinazioni di scenari
combinations <- expand.grid(
  scen = scen,
  conf = conf,
  year = years
)
hist_conf <- 'hist_existing'
comboscen <- apply(combinations[, c("scen", "conf", "year")], 1, function(x) paste(x, collapse = "_"))
print(comboscen)
# Funzione per costruire la tabella
build_tab <- function() {  
  return(
    foreach(clmod = climate_models, .combine = 'rbind') %do% {
      read.csv(paste0('./fishsuit-africa/proc/', hist_conf, '/', clmod, '/ESH_tab_nonat.csv')) %>%
        reshape2::melt(measure.vars = c('ESH_all', 'ESH_Q', 'ESH_T', 'ESH_QnT')) %>%
        rename(ESH_type = variable, ESH = value) %>%
        mutate(ESH_type = forcats::fct_recode(ESH_type, Total = 'ESH_all', Q = 'ESH_Q', Tw = 'ESH_T', 'Q&Tw' = 'ESH_QnT'),
               GCM = clmod)
    } %>%
      droplevels() %>%
      mutate(GCM = factor(GCM)) %>%
      cbind(., as.data.frame(do.call('rbind', strsplit(as.character(.[,'comboscen']), '_')))) %>%
      as_tibble() %>%
      rename(SSP = V1, configuration = V2, year = V3) %>%
      mutate(SSP = forcats::fct_recode(SSP, 'SSP1-2.6' = '1', 'SSP3-7.0' = '3', 'SSP5-8.5' = '5')) %>%
      mutate(GCM = forcats::fct_recode(GCM, 'GFDL' = "GFDL", "IPSL" = "IPSL", 'MPI' = 'MPI', "MRI" = "MRI", "UKESM" = "UKESM"))
  )
}

# Calcola la mediana per ciascun scenario
compute_median <- function(tab) {
  return(
    foreach(sp = unique(tab$id_no), .combine = 'rbind') %do% {
      tsp <- droplevels(tab[tab$id_no == sp,])
      foreach(scen = unique(tsp$SSP), .combine = 'rbind') %do% {
        tsc <- droplevels(tsp[tsp$SSP == scen,])
        foreach(conf = unique(tsc$configuration), .combine = 'rbind') %do% {  
          tc <- droplevels(tsc[tsc$configuration == conf,])
          foreach(y = unique(tc$year), .combine = 'rbind') %do% {
            ty <- droplevels(tc[tc$year == y,])
            return(data.frame(id_no = sp, ESH_mean = mean(ty$ESH, na.rm = TRUE), SSP = scen, configuration = conf, year = y))
          }
        }
      }  
    } %>% as_tibble()
  )
}

# Costruisce la tabella principale
tab <- rbind(
  build_tab() %>% dplyr::select(id_no, ESH_type, ESH, GCM, SSP, configuration, year) %>%
    filter(ESH_type == 'Total') %>% 
    droplevels() %>% compute_median()
) 

# Ensure 'year' column shows ranges instead of simplified years
#tab <- tab %>%
#  mutate(year = recode(year,
#                       "2020" = "2020_2029",
#                       "2030" = "2030_2039",
#                       "2040" = "2040_2049"))


# Stampa le medie rappresentate dai diamanti nella console. You will see year 2020, 2030, 2040 cause it didn't register the last part after _ (ex:_2029) 
print(tab)

# Configura le etichette della legenda
tab$configuration <- factor(tab$configuration, levels = c('ex', 'fut'), labels = c('Existing dams', 'Future dams'))

# COMPACT LABELS
p <- ggplot(tab, aes(x = factor(year), y = ESH_mean, fill = configuration)) + 
  geom_violin(aes(fill = configuration), color = NA, scale = "width", alpha = 0.3, width = 0.4, position = position_dodge(0.8)) + 
  geom_boxplot(aes(color = configuration), fill = 'white', outlier.color = NA, outlier.shape = NA, width = 0.2, lwd = 1, coef = 0, notch = TRUE, position = position_dodge(0.8)) +
  
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, position = position_dodge(width = 0.8), show.legend = FALSE) +
  
  scale_fill_manual(values = viridis::viridis(10, option = 'C')[c(5, 8)]) +
  scale_color_manual(values = viridis::viridis(10, option = 'C')[c(5, 8)]) +
  
  # Modifica etichette dell'asse x
  scale_x_discrete(labels = c('2020\n-\n2029', '2030\n-\n2039', '2040\n-\n2049')) +
  
  # Limita l'asse y senza continuazione sopra il 100%
  scale_y_reverse(breaks = c(0, 20, 40, 60, 80, 100), limits = c(100, 0), labels = paste0(c(0, 20, 40, 60, 80, 100), '%')) +
  
  ylab(label = 'Percentage of range threatened') +
  xlab("") +
  
  theme_bw() +
  coord_cartesian(expand = FALSE) +
  theme(
    legend.position = "bottom", # Legenda sotto
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),  # Testo legenda più piccolo
    legend.key.size = unit(0.8, "cm"),      # Riduzione dimensione chiavi legenda
    panel.grid = element_blank(),
    panel.border = element_blank(),
    panel.grid.major.y = element_line(linetype = 'dashed', color = 'black'),
    axis.ticks.x = element_blank(),
    # Etichette compatte con riduzione dello spazio tra le linee
    axis.text.x = element_text(color = 'black', vjust = 1, size = 12, lineheight = 0.6), # lineheight ridotto
    axis.text.y = element_text(color = 'black', angle = 90, hjust = 0.5, vjust = 1, size = 12),
    axis.line.y = element_line(color = 'black'),
    axis.title.y = element_text(size = 16),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent"),
    strip.background = element_rect('white'),
    strip.background.x = element_blank(),
    strip.background.y = element_blank(),
    strip.text = element_text(size = 16),
    plot.margin = margin(5, 5, 5, 5) # Margini ridotti per avvicinare legenda
  ) +
  
  facet_grid(~ SSP)

# Visualizza il grafico
print(p)

# Salva il grafico
ggsave(paste0('./fishsuit-africa/figs/average_violins_updated_RC_compact_labels_example.jpg'), p, width = 200, height = 150, dpi = 600, units = 'mm')
ggsave(paste0('./fishsuit-africa/figs/average_violins_updated_RC_compact_labels_example.pdf'), p, width = 200, height = 150, units = 'mm')


#assi che vanno da 0 a 100 dal basso all'alto
# COMPACT LABELS
p <- ggplot(tab, aes(x = factor(year), y = ESH_mean, fill = configuration)) + 
  geom_violin(aes(fill = configuration), color = NA, scale = "width", alpha = 0.3, width = 0.4, position = position_dodge(0.8)) + 
  geom_boxplot(aes(color = configuration), fill = 'white', outlier.color = NA, outlier.shape = NA, width = 0.2, lwd = 1, coef = 0, notch = TRUE, position = position_dodge(0.8)) +
  
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, position = position_dodge(width = 0.8), show.legend = FALSE) +
  
  scale_fill_manual(values = viridis::viridis(10, option = 'C')[c(5, 8)]) +
  scale_color_manual(values = viridis::viridis(10, option = 'C')[c(5, 8)]) +
  
  # Modifica etichette dell'asse x
  scale_x_discrete(labels = c('2020\n-\n2029', '2030\n-\n2039', '2040\n-\n2049')) +
  
  # Limita l'asse y dal basso verso l'alto (0-100%)
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100), labels = paste0(c(0, 20, 40, 60, 80, 100), '%')) +
  
  ylab(label = 'Percentage of range threatened') +
  xlab("") +
  
  theme_bw() +
  coord_cartesian(expand = FALSE) +
  theme(
    legend.position = "bottom", # Legenda sotto
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),  # Testo legenda più piccolo
    legend.key.size = unit(0.8, "cm"),      # Riduzione dimensione chiavi legenda
    panel.grid = element_blank(),
    panel.border = element_blank(),
    panel.grid.major.y = element_line(linetype = 'dashed', color = 'black'),
    axis.ticks.x = element_blank(),
    # Etichette compatte con riduzione dello spazio tra le linee
    axis.text.x = element_text(color = 'black', vjust = 1, size = 12, lineheight = 0.6), # lineheight ridotto
    axis.text.y = element_text(color = 'black', angle = 90, hjust = 0.5, vjust = 1, size = 12),
    axis.line.y = element_line(color = 'black'),
    axis.title.y = element_text(size = 16),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent"),
    strip.background = element_rect('white'),
    strip.background.x = element_blank(),
    strip.background.y = element_blank(),
    strip.text = element_text(size = 16),
    plot.margin = margin(5, 5, 5, 5) # Margini ridotti per avvicinare legenda
  ) +
  
  facet_grid(~ SSP)

# Visualizza il grafico
print(p)

# Salva il grafico
ggsave(paste0('./fishsuit-africa/figs/average_violins_updated_RC_compact_labels_example.jpg'), p, width = 200, height = 150, dpi = 600, units = 'mm')
ggsave(paste0('./fishsuit-africa/figs/average_violins_updated_RC_compact_labels_example.pdf'), p, width = 200, height = 150, units = 'mm')

#LABELS 2030s 2040s 2050s
p <- ggplot(tab, aes(x = factor(year), y = ESH_mean, fill = configuration)) + 
  geom_violin(aes(fill = configuration), color = NA, scale = "width", alpha = 0.3, width = 0.4, position = position_dodge(0.8)) + 
  geom_boxplot(aes(color = configuration), fill = 'white', outlier.color = NA, outlier.shape = NA, width = 0.2, lwd = 1, coef = 0, notch = TRUE, position = position_dodge(0.8)) +
  
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, position = position_dodge(width = 0.8), show.legend = FALSE) +
  
  scale_fill_manual(values = viridis::viridis(10, option = 'C')[c(5, 8)]) +
  scale_color_manual(values = viridis::viridis(10, option = 'C')[c(5, 8)]) +
  
  # Modifica etichette dell'asse x
  scale_x_discrete(labels = c('2030s', '2040s', '2050s')) +
  
  # Limita l'asse y senza continuazione sopra il 100%
  scale_y_reverse(breaks = c(0, 20, 40, 60, 80, 100), limits = c(100, 0), labels = paste0(c(0, 20, 40, 60, 80, 100), '%')) +
  
  ylab(label = 'Percentage of range threatened') +
  xlab("") +
  
  theme_bw() +
  coord_cartesian(expand = FALSE) +
  theme(
    legend.position = "bottom", # Legenda sotto
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),  # Testo legenda più piccolo
    legend.key.size = unit(0.8, "cm"),      # Riduzione dimensione chiavi legenda
    panel.grid = element_blank(),
    panel.border = element_blank(),
    panel.grid.major.y = element_line(linetype = 'dashed', color = 'black'),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(color = 'black', vjust = 1, size = 14), # Etichette anni chiare e dritte
    axis.text.y = element_text(color = 'black', angle = 90, hjust = 0.5, vjust = 1, size = 12),
    axis.line.y = element_line(color = 'black'),
    axis.title.y = element_text(size = 16),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent"),
    strip.background = element_rect('white'),
    strip.background.x = element_blank(),
    strip.background.y = element_blank(),
    strip.text = element_text(size = 16),
    plot.margin = margin(5, 5, 5, 5) # Margini ridotti per avvicinare legenda
  ) +
  
  facet_grid(~ SSP)

# Visualizza il grafico
print(p)

# Salva il grafico
ggsave(paste0('./fishsuit-africa/figs/average_violins_updated_RC_decades.jpg'), p, width = 200, height = 150, dpi = 600, units = 'mm')
ggsave(paste0('./fishsuit-africa/figs/average_violins_updated_RC_decades.pdf'), p, width = 200, height = 150, units = 'mm')




#BIG GRAPH
p <- ggplot(tab, aes(x = factor(year), y = ESH_mean, fill = configuration)) + 
  geom_violin(aes(fill = configuration), color = NA, scale = "width", alpha = 0.3, width = 0.4, position = position_dodge(0.8)) + 
  geom_boxplot(aes(color = configuration), fill = 'white', outlier.color = NA, outlier.shape = NA, width = 0.2, lwd = 1, coef = 0, notch = TRUE, position = position_dodge(0.8)) +
  
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, position = position_dodge(width = 0.8), show.legend = FALSE) +
  
  scale_fill_manual(values = viridis::viridis(10, option = 'C')[c(5, 8)]) +
  scale_color_manual(values = viridis::viridis(10, option = 'C')[c(5, 8)]) +
  
  scale_x_discrete(labels = c('2030s', '2040s', '2050s')) +
  scale_y_reverse(breaks = c(0, 25, 50, 75, 100), limits = c(100, 0), labels = paste0(c(0, 25, 50, 75, 100), '%')) +
  ylab(label = 'Percentage of range threatened') +
  xlab("") +
  
  theme_bw() +
  coord_cartesian(expand = FALSE) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),  # Dimensione testo legenda
    legend.key.size = unit(1, "cm"),       # Dimensione delle chiavi legenda
    panel.grid = element_blank(),
    panel.border = element_blank(),
    panel.grid.major.y = element_line(linetype = 'dashed', color = 'black'),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(color = 'black', vjust = 3, size = 14),
    axis.text.y = element_text(color = 'black', angle = 90, hjust = 0.5, vjust = 1, size = 12),
    axis.line.y = element_line(color = 'black'),
    axis.title.y = element_text(size = 16), # Dimensione titolo asse y
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent"),
    strip.background = element_rect('white'),
    strip.background.x = element_blank(),
    strip.background.y = element_blank(),
    strip.text = element_text(size = 16)
  ) +
  
  facet_grid(~ SSP)

# Visualizza il grafico
print(p)

# Salva il grafico
ggsave(paste0('./fishsuit-africa/figs/average_violins_overall_RC_grande2.jpg'), p, width = 200, height = 100, dpi = 600, units = 'mm')
ggsave(paste0('./fishsuit-africa/figs/average_violins_overall_RC_grande2.pdf'), p, width = 200, height = 100, units = 'mm')


#BIG AND LONG GRAPH
p <- ggplot(tab, aes(x = factor(year), y = ESH_mean, fill = configuration)) + 
  geom_violin(aes(fill = configuration), color = NA, scale = "width", alpha = 0.3, width = 0.4, position = position_dodge(0.8)) + 
  geom_boxplot(aes(color = configuration), fill = 'white', outlier.color = NA, outlier.shape = NA, width = 0.2, lwd = 1, coef = 0, notch = TRUE, position = position_dodge(0.8)) +
  
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, position = position_dodge(width = 0.8), show.legend = FALSE) +
  
  scale_fill_manual(values = viridis::viridis(10, option = 'C')[c(5, 8)]) +
  scale_color_manual(values = viridis::viridis(10, option = 'C')[c(5, 8)]) +
  
  scale_x_discrete(labels = c('2020-2029', '2030-2039', '2040-2049')) +
  scale_y_reverse(breaks = c(0, 20, 40, 60, 80, 100), limits = c(110, 0), labels = paste0(c(0, 20, 40, 60, 80, 100), '%')) +
  ylab(label = 'Percentage of range threatened') +
  xlab("") +
  
  theme_bw() +
  coord_cartesian(expand = FALSE) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.key.size = unit(1, "cm"),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    panel.grid.major.y = element_line(linetype = 'dashed', color = 'black'),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(color = 'black', vjust = 3, size = 14),
    axis.text.y = element_text(color = 'black', angle = 90, hjust = 0.5, vjust = 1, size = 12),
    axis.line.y = element_line(color = 'black'),
    axis.title.y = element_text(size = 16),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent"),
    strip.background = element_rect('white'),
    strip.background.x = element_blank(),
    strip.background.y = element_blank(),
    strip.text = element_text(size = 16)
  ) +
  
  facet_grid(~ SSP)

# Visualizza il grafico
print(p)

# Salva il grafico con altezza aumentata
ggsave(paste0('./fishsuit-africa/figs/average_violins_overall_RC_grandeallungato.jpg'), p, width = 200, height = 150, dpi = 600, units = 'mm')
ggsave(paste0('./fishsuit-africa/figs/average_violins_overall_RC_grandeallungato.pdf'), p, width = 200, height = 150, units = 'mm')

#VIOLIN BOXPLOT FIGURE

p <- ggplot(tab, aes(x = factor(year), y = ESH_mean, fill = configuration)) + 
  geom_violin(aes(fill = configuration), color = NA, scale = "width", alpha = 0.3, width = 0.2, position = position_dodge(0.8)) + 
  geom_boxplot(aes(color = configuration), fill = 'white', outlier.color = NA, outlier.shape = NA, width = 0.1, lwd = 0.5, coef = 0, notch = TRUE, position = position_dodge(0.8)) +
  
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2, position = position_dodge(width = 0.8), show.legend = FALSE) +
  
  scale_fill_manual(values = viridis::viridis(10, option = 'C')[c(5, 8)]) +
  scale_color_manual(values = viridis::viridis(10, option = 'C')[c(5, 8)]) +
  
  scale_x_discrete(labels = c('2030', '2040', '2050')) +
  scale_y_reverse(breaks = c(0, 25, 50, 75, 100), limits = c(100, 0), labels = paste0(c(0, 25, 50, 75, 100), '%')) +
  ylab(label = 'Percentage of range threatened') +
  xlab("") +
  
  theme_bw() +
  coord_cartesian(expand = FALSE) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    panel.grid.major.y = element_line(linetype = 'dashed', color = 'black'),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(color = 'black', vjust = 3, size = 12),
    axis.text.y = element_text(color = 'black', angle = 90, hjust = 0.5, vjust = 1),
    axis.line.y = element_line(color = 'black'),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent"),
    strip.background = element_rect('white'),
    strip.background.x = element_blank(),
    strip.background.y = element_blank(),
    axis.title.y = element_text(size = 9),
    strip.text = element_text(size = 16)
  ) +
  
  facet_grid(~ SSP)

# Visualizza il grafico
print(p)

# Salva il grafico
ggsave(paste0('./fishsuit-africa/figs/average_violins_overall_RC.jpg'), p, width = 200, height = 100, dpi = 600, units = 'mm')
ggsave(paste0('./fishsuit-africa/figs/average_violins_overall_RC.pdf'), p, width = 200, height = 100, units = 'mm')

# Filter species with ESH_mean greater than 50% (percentage of range threatened > 50%). Questo guarda al violin plot per vedere distribuzione specie
species_above_50 <- tab %>%
  filter(ESH_mean > 50) %>%
  group_by(SSP, configuration, year) %>%
  summarise(count = n(), .groups = 'drop')

# Calculate the total species count for each scenario and year
species_total <- tab %>%
  group_by(SSP, configuration, year) %>%
  summarise(total_count = n(), .groups = 'drop')

# Merge to get the proportion
species_above_50 <- species_above_50 %>%
  left_join(species_total, by = c("SSP", "configuration", "year")) %>%
  mutate(percentage_above_50 = (count / total_count) * 100)

# Print the results
print(species_above_50)


# BOXPLOTS BY RCP AND GCM ------------------------------------------------------------------------------
#avarage on years
tab <- rbind(
  build_tab() %>% dplyr::select(id_no,ESH_type,ESH,GCM,SSP,configuration,year) %>% filter(ESH_type == 'Total') %>% 
    droplevels() 
) 

# one figure for GCM-RCP
d <- rbind(
  tab %>% filter(ESH_type == 'Total') %>% mutate(g = 'GCM') %>% mutate(f = factor(GCM)),
  tab %>% filter(ESH_type == 'Total') %>% mutate(g = 'SSP') %>% mutate(f = factor(SSP))
) %>% 
  mutate(g = factor(g)) %>%
  droplevels()
# Modifica per rinominare i livelli di `configuration` nel dataframe `d`
d <- d %>%
  mutate(configuration = recode(configuration, 
                                'ex' = 'Existing dams', 
                                'fut' = 'Future dams'))

# Aggiorna il grafico con i nuovi livelli
p <- ggplot(d, aes(x = configuration, y = ESH)) +
  geom_boxplot(aes(fill = f), varwidth = FALSE, width = 0.5, outlier.size = 0.3, outlier.alpha = 0.3, outlier.colour = 'Grey') +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_discrete(labels = levels(d$configuration)) +
  scale_fill_manual(values = c(brewer.pal(n = 5, 'Blues'), brewer.pal(4, 'Reds'))) +
  scale_y_reverse() +
  xlab(label = ' ') +
  ylab(label = 'Percentage of range threatened [%]') +
  facet_grid(g ~ configuration) +
  theme_bw() +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(),
    axis.ticks.x = element_blank(),
    strip.background = element_blank(),
    text = element_text(size = 20),
    axis.text.x = element_text(color = 'black'),
    axis.text.y = element_text(color = 'black'),
    legend.title = element_blank()
  )

p

# Salvataggio del grafico
ggsave(paste0('./fishsuit-africa/figs/boxplot_RCP_and_GCM.jpg'), p, width = 170, height = 170, units = 'mm', dpi = 600, scale = 1.3)

# BOXPLOTS BY RCP AND GCM ONLY FOR LAST DECADE
# Filter `tab` for the last decade (2040_2049). It is written 2040 since it skipped the _2049 after.
tab_2050s <- tab %>%
  filter(year == "2040") %>% 
  droplevels()

# Check if `tab_2050s` has any data after filtering
if (nrow(tab_2050s) == 0) {
  stop("No data available for the last decade (2040_2049) in `tab_2050s`. Please check the input data or filtering conditions.")
}

# Set configuration labels in `tab_2050s`
tab_2050s$configuration <- factor(tab_2050s$configuration, levels = c('ex', 'fut'), labels = c('Existing dams', 'Future dams'))

# Create `d` with the filtered `tab_2050s` data for faceting by `GCM` and `SSP`
d <- rbind(
  tab_2050s %>% mutate(g = 'GCM', f = factor(GCM)),
  tab_2050s %>% mutate(g = 'SSP', f = factor(SSP))
) %>%
  mutate(g = factor(g)) %>%
  droplevels()

# Rename levels of `configuration` in `d`
d <- d %>%
  mutate(configuration = recode(configuration, 
                                'ex' = 'Existing dams', 
                                'fut' = 'Future dams'))

# Create the plot
p <- ggplot(d, aes(x = configuration, y = ESH)) +
  geom_boxplot(aes(fill = f), varwidth = FALSE, width = 0.5, outlier.size = 0.3, outlier.alpha = 0.3, outlier.colour = 'Grey') +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_discrete(labels = levels(d$configuration)) +
  scale_fill_manual(values = c(brewer.pal(n = 5, 'Blues'), brewer.pal(4, 'Reds'))) +
  scale_y_reverse() +
  xlab(label = ' ') +
  ylab(label = 'Percentage of range threatened [%]') +
  facet_grid(g ~ configuration) +
  theme_bw() +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(),
    axis.ticks.x = element_blank(),
    strip.background = element_blank(),
    text = element_text(size = 20),
    axis.text.x = element_text(color = 'black'),
    axis.text.y = element_text(color = 'black'),
    legend.title = element_blank()
  )

# Display the plot
print(p)

# Save the plot
ggsave(paste0('./fishsuit-africa/figs/boxplot_RCP_and_GCM_2050s.jpg'), p, width = 170, height = 170, units = 'mm', dpi = 600, scale = 1.3)

# Ensure 'year' column shows ranges with hyphens instead of underscores
tab <- tab %>%
  mutate(year = recode(year,
                       "2020" = "2020-2029",
                       "2030" = "2030-2039",
                       "2040" = "2040-2049"))

# Print the updated `tab` to confirm the changes
print(tab)

# Configure the legend labels for `configuration`
tab$configuration <- factor(tab$configuration, levels = c('ex', 'fut'), labels = c('Existing dams', 'Future dams'))

# Get the number of rows in `tab`
n_rows <- nrow(tab)

# Create a repeating pattern of "Existing dams" (3 times) and "Future dams" (3 times)
pattern <- rep(c(rep("Existing dams", 3), rep("Future dams", 3)), length.out = n_rows)

# Assign this pattern to the `configuration` column in `tab`
tab$configuration <- pattern

# Convert `configuration` to a factor if needed
tab$configuration <- factor(tab$configuration, levels = c("Existing dams", "Future dams"))

# Check the result
print(head(tab, 12))  # Check the first 12 rows to confirm the pattern


# VIOLIN PLOT WITH 2020_2029 2030_2039 2040_2049 labels
p <- ggplot(tab, aes(x = factor(year), y = ESH_mean, fill = configuration)) + 
  geom_violin(aes(fill = configuration), color = NA, scale = "width", alpha = 0.3, width = 0.2, position = position_dodge(0.8)) + 
  geom_boxplot(aes(color = configuration), fill = "white", outlier.color = NA, outlier.shape = NA, width = 0.1, lwd = 0.5, coef = 0, notch = TRUE, position = position_dodge(0.8)) +
  
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2, position = position_dodge(width = 0.8), show.legend = FALSE) +
  
  scale_fill_manual(values = viridis::viridis(10, option = 'C')[c(5, 8)], name = "Configuration") +
  scale_color_manual(values = viridis::viridis(10, option = 'C')[c(5, 8)], name = "Configuration") +
  
  scale_x_discrete(labels = c('2020-2029', '2030-2039', '2040-2049')) +  # Set x-axis labels with hyphens
  scale_y_reverse(breaks = c(0, 25, 50, 75, 100), limits = c(100, 0), labels = paste0(c(0, 25, 50, 75, 100), '%')) +
  ylab(label = 'Percentage of range threatened') +
  xlab("") +
  
  theme_bw() +
  coord_cartesian(expand = FALSE) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    panel.grid.major.y = element_line(linetype = 'dashed', color = 'black'),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(size = 8, color = 'black', vjust = 3),  # Smaller x-axis text
    axis.text.y = element_text(color = 'black', angle = 90, hjust = 0.5, vjust = 1),
    axis.line.y = element_line(color = 'black'),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent"),
    strip.background = element_rect('white'),
    strip.background.x = element_blank(),
    strip.background.y = element_blank(),
    axis.title.y = element_text(size = 9),
    strip.text = element_text(size = 16)
  ) +
  
  facet_grid(~ SSP)

# Display the plot
print(p)

# Save the plot
ggsave(paste0('./fishsuit-africa/figs/average_violins_overall_RC_prova4.jpg'), p, width = 200, height = 100, dpi = 600, units = 'mm')
ggsave(paste0('./fishsuit-africa/figs/average_violins_overall_RC_prova4.pdf'), p, width = 200, height = 100, units = 'mm')


