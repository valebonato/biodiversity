
#For guidance, Nature's standard figure sizes are 89 mm wide (single column) and 183 mm wide (double column). 
#The full depth of a Nature page is 247 mm. Figures can also be a column-and-a-half where necessary (120–136 mm).

source('africa_config.R'); # always load as functions are loaded within this script

library(raster); library(foreach); library(sf); library(dplyr); library(matrixStats); library(ggplot2); library(rnaturalearth)

#> FUNCTIONS AND BASE LAYERS -------------------------------------------------------------------------------------

# template points
temp <- readRDS('./fishsuit-africa/proc/ssp/points_template.rds')

library(sf)
template <- temp %>% dplyr::select(row_no,geometry)
#temp_coords <- cbind(temp, st_coordinates(temp$geometry))
#ggplot(temp_coords, aes(x = X, y = Y)) +
#  geom_point() +
#  theme_minimal() +
#  ggtitle("Distribuzione dei punti nella griglia")
#str(template)

scen <-  c(1,3,5) #scenarios[scenarios != 'hist']
conf <- c("ex", "fut") #conf[conf != 'nat']
years = c("2020_2029")
conf_hist <- 'hist_existing'
# Generate combinations using expand.grid
combinations <- expand.grid(
  scen = scen,
  conf = conf,
  year = years
)
# View the resulting data frame
comboscen <- apply(combinations[, c("scen", "conf", "year")], 1, function(x) paste(x, collapse = "_"))
print(comboscen)

rasterize_rel_losses <- function(comboscen, var, clmod, sc, conf_hist = 'hist_existing') {
  template <- temp %>% dplyr::select(row_no, geometry)
  
  all_results_ex_list <- list()
  all_results_fut_list <- list()
  
  # Itera su ciascun modello climatico
  for (c in clmod) {
    cat("Processing climate model:", c, "\n")
    
    # Itera su ciascun SSP specificato
    for (scenario_label in sc) {
      cat("Processing SSP", scenario_label, "\n")
      
      model_results_ex <- data.frame(row_no = template$row_no)
      model_results_fut <- data.frame(row_no = template$row_no)
      
      # Itera su ciascuna combinazione di scenario
      for (sc_comb in comboscen) {
        if (grepl(paste0("^", scenario_label, "_"), sc_comb)) {
          file_path <- paste0('./fishsuit-africa/proc/', conf_hist, '/', c, '/SR_tab_', sc_comb, '.rds')
          
          if (file.exists(file_path)) {
            t <- readRDS(file_path)
            cat("Number of rows in 't':", nrow(t), "\n")  # Debug
            t$val <- (t$occ - t[, var]) / t$occ
            
            # Stampa di debug per t$val
            cat("Debug: Valori di t$val:\n")
            print(head(t$val))
            
            if (grepl("_ex_", sc_comb)) {
              model_results_ex[[paste0("val_", c, "_", scenario_label)]] <- t$val
              cat("Added 'ex' values for model:", c, "- Scenario:", scenario_label, "\n")
              
              # Stampa di debug per model_results_ex
              cat("Debug: model_results_ex per scenario '", scenario_label, "' e modello '", c, "':\n")
              print(head(model_results_ex))
            } else if (grepl("_fut_", sc_comb)) {
              model_results_fut[[paste0("val_", c, "_", scenario_label)]] <- t$val
              cat("Added 'fut' values for model:", c, "- Scenario:", scenario_label, "\n")
              
              # Stampa di debug per model_results_fut
              cat("Debug: model_results_fut per scenario '", scenario_label, "' e modello '", c, "':\n")
              print(head(model_results_fut))
            }
          }
        }
      }
      
      # Accumula i risultati per il calcolo delle mediane
      if (ncol(model_results_ex) > 1) {
        all_results_ex_list[[paste0("ex_", scenario_label)]] <- cbind(all_results_ex_list[[paste0("ex_", scenario_label)]], model_results_ex[,-1])
        
        # Stampa di debug per all_results_ex_list
        cat("Debug: all_results_ex_list per SSP", scenario_label, ":\n")
        print(head(all_results_ex_list[[paste0("ex_", scenario_label)]]))
      }
      if (ncol(model_results_fut) > 1) {
        all_results_fut_list[[paste0("fut_", scenario_label)]] <- cbind(all_results_fut_list[[paste0("fut_", scenario_label)]], model_results_fut[,-1])
        
        # Stampa di debug per all_results_fut_list
        cat("Debug: all_results_fut_list per SSP", scenario_label, ":\n")
        print(head(all_results_fut_list[[paste0("fut_", scenario_label)]]))
      }
      
      cat("Completed processing for climate model:", c, "in SSP", scenario_label, "\n")  # Debug
    }
  }
  
  # Calcola la mediana per ciascun SSP sui modelli climatici
  median_results_ex <- list()
  median_results_fut <- list()
  
  for (scenario_label in sc) {
    if (!is.null(all_results_ex_list[[paste0("ex_", scenario_label)]])) {
      # Calcolo della mediana come facevi prima
      template[[paste0("val_ex_", scenario_label)]] <- apply(all_results_ex_list[[paste0("ex_", scenario_label)]], 1, median, na.rm = TRUE)
      median_results_ex[[paste0("ex_", scenario_label)]] <- raster(as(as_Spatial(template[, paste0("val_ex_", scenario_label)]), "SpatialPixelsDataFrame"))
      
      # Stampa di debug per template$val_ex
      cat("Debug: Prime righe di template$val_ex_", scenario_label, ":\n")
      print(head(template[[paste0("val_ex_", scenario_label)]]))
      cat("Summary of template$val_ex_", scenario_label, ":\n")
      print(summary(template[[paste0("val_ex_", scenario_label)]]))
    }
    
    if (!is.null(all_results_fut_list[[paste0("fut_", scenario_label)]])) {
      # Calcolo della mediana come facevi prima
      template[[paste0("val_fut_", scenario_label)]] <- apply(all_results_fut_list[[paste0("fut_", scenario_label)]], 1, median, na.rm = TRUE)
      median_results_fut[[paste0("fut_", scenario_label)]] <- raster(as(as_Spatial(template[, paste0("val_fut_", scenario_label)]), "SpatialPixelsDataFrame"))
      
      # Stampa di debug per template$val_fut
      cat("Debug: Prime righe di template$val_fut_", scenario_label, ":\n")
      print(head(template[[paste0("val_fut_", scenario_label)]]))
      cat("Summary of template$val_fut_", scenario_label, ":\n")
      print(summary(template[[paste0("val_fut_", scenario_label)]]))
    }
  }
  
  return(list(ex = median_results_ex, fut = median_results_fut))
}

result <- rasterize_rel_losses(
  comboscen = comboscen,
  var = 'all',
  clmod = c('MPI', 'GFDL', 'IPSL', 'MRI', 'UKESM'),
  sc = c(1, 3, 5),
  conf_hist = 'hist_existing'
)

# Estrai i raster per 'ex' e 'fut' da result
raster_ex_list <- result$ex
raster_fut_list <- result$fut

# Conta i valori diversi da zero in df per ciascun SSP, il max min e la media
for (scenario_label in unique(df$ssp)) {
  # Filtra i dati per scenario specifico
  scenario_data <- df$value[df$ssp == scenario_label]
  
  # Calcola il numero di punti diversi da zero e uguali a zero
  num_nonzero <- sum(scenario_data != 0, na.rm = TRUE)
  num_zero <- sum(scenario_data == 0, na.rm = TRUE)
  
  # Calcola i valori minimo, massimo, media e deviazione standard
  min_value <- min(scenario_data, na.rm = TRUE)
  max_value <- max(scenario_data, na.rm = TRUE)
  mean_value <- mean(scenario_data, na.rm = TRUE)
  sd_value <- sd(scenario_data, na.rm = TRUE)
  
  # Stampa i risultati
  cat("Scenario:", scenario_label, "\n")
  cat("  Numero di punti diversi da zero:", num_nonzero, "\n")
  cat("  Numero di punti uguali a zero:", num_zero, "\n")
  cat("  Valore minimo:", min_value, "\n")
  cat("  Valore massimo:", max_value, "\n")
  cat("  Media:", mean_value, "\n")
  cat("  Deviazione standard:", sd_value, "\n\n")
}


# Definisci il CRS personalizzato con proiezione equirettangolare
crs_custom <- "+proj=longlat +datum=WGS84 +no_defs"

# Scarica i confini dei paesi africani e trasforma al CRS personalizzato
africa <- rnaturalearth::ne_countries(returnclass = "sf", continent = "Africa") %>%
  st_transform(crs_custom)

# Crea una bounding box per l'Africa e convertila in geometria semplice
africa_bounds <- st_bbox(africa)
bb_africa <- st_as_sfc(africa_bounds, crs = crs_custom)
bb_africa_sp <- as(bb_africa, "Spatial")

# Crea i graticoli per l'Africa con intervalli di 30 gradi e trasforma al CRS personalizzato
graticules <- rnaturalearth::ne_download(type = "graticules_30", category = "physical", returnclass = "sf") %>%
  st_transform(crs_custom) %>%
  st_intersection(st_as_sf(bb_africa))  # Mantieni solo i graticoli che intersecano l'Africa

# Calcola la differenza tra fut e ex e prepara i dati per il grafico
df_list <- list()

for (scenario_label in c("1", "3", "5")) {
  # Estrai i risultati per 'ex' e 'fut'
  raster_ex <- raster_ex_list[[paste0("ex_", scenario_label)]]
  raster_fut <- raster_fut_list[[paste0("fut_", scenario_label)]]
  
  # Calcola la differenza (fut - ex)
  raster_diff <- raster_fut - raster_ex
  
  # Trasforma il raster in CRS personalizzato e maschera per l'Africa
  raster_diff <- projectRaster(raster_diff, crs = crs_custom)
  raster_diff <- mask(raster_diff, bb_africa_sp)
  
  # Converti il raster delle differenze in data frame per ggplot2
  df_diff <- as(raster_diff, "SpatialPixelsDataFrame") %>% as.data.frame()
  
  # Rinomina esplicitamente la colonna dei valori
  colnames(df_diff)[1] <- "value"  # Rinominare la prima colonna in 'value' per ggplot
  
  df_diff$ssp <- paste0("SSP", scenario_label)
  df_diff$config <- "Difference"  # Configurazione differenza
  
  # Aggiungi il data frame alla lista
  df_list[[paste0("diff_", scenario_label)]] <- df_diff
}

# Combina tutti i data frame in uno solo per ggplot
df <- do.call(rbind, df_list)

# ----- final plot with logaritmic scale

# Ordina i livelli di 'ssp' e 'config' per rappresentazione grafica
df$ssp <- factor(df$ssp, levels = c("SSP1", "SSP3", "SSP5"), labels = c("SSP1-2.6", "SSP3-7.0", "SSP5-8.5"))
df$config <- factor(df$config, levels = c("Difference"), labels = c("Difference"))


df <- df %>%
  mutate(scaled_value = sign(value) * log1p(abs(value)))  # Scala logaritmica per enfatizzare le piccole differenze

p <- ggplot() +
  geom_sf(data = africa, fill = "grey90") +
  geom_tile(data = df, aes(x = x, y = y, fill = scaled_value)) +
  scale_fill_gradientn(
    colors = c("darkblue", "lightblue", "white", "salmon", "darkred"),
    values = scales::rescale(c(-0.1, 0, 0.1)),  # Mostra solo valori estremi e zero
    limits = c(-0.1, 0.1),  # Limita la scala da -0.1 a 0.1
    oob = scales::squish,  # Mantiene i valori entro il range specificato
    name = "log PAF difference"  # Nome della scala
  ) +
  facet_wrap(~ ssp, ncol = 1) +  # Un pannello per ogni scenario
  theme_minimal() +
  theme(
    legend.position = 'bottom',
    legend.key.width = unit(6, 'line'),
    axis.text = element_blank(),      # Rimuove le etichette dei gradi
    axis.title = element_blank(),     # Rimuove i titoli degli assi
    axis.ticks = element_blank(),     # Rimuove le tacche sugli assi
    panel.grid.major = element_blank(),  # Rimuove il reticolo
    strip.background = element_blank(),
    strip.text = element_text(size = 12)
  )
# Stampa del grafico
print(p)

# Salva il grafico come un singolo file
ggsave(
  filename = './fishsuit-africa/figs/diff_ex_fut_logaritmic_2030_median.jpg',
  plot = p,
  width = 210, height = 229.51, dpi = 600, units = 'mm'
)

#-----OLD PLOT LOGARITMIC
# Visualizzazione della mappa con ggplot2, scala di colori che enfatizza piccoli cambiamenti
p <- ggplot() +
  geom_sf(data = africa, fill = "grey90") +
  geom_tile(data = df, aes(x = x, y = y, fill = scaled_value)) +
  scale_fill_gradientn(
    colors = c("darkblue", "lightblue", "white", "salmon", "darkred"),
    values = scales::rescale(c(-0.1, -0.01, 0, 0.01, 0.1)),
    limits = c(-max(abs(df$scaled_value), na.rm = TRUE), max(abs(df$scaled_value), na.rm = TRUE)),
    oob = scales::squish  # Mantiene i valori entro il range
  ) +
  facet_wrap(~ ssp, ncol = 1) +  # Un pannello per ogni scenario, tutti nello stesso grafico
  theme_minimal() +
  theme(
    legend.position = 'bottom',
    legend.key.width = unit(6, 'line'),
    axis.text = element_blank(),      # Rimuove le etichette dei gradi
    axis.title = element_blank(),     # Rimuove i titoli degli assi
    axis.ticks = element_blank(),     # Rimuove le tacche sugli assi
    panel.grid.major = element_blank(),  # Rimuove il reticolo
    strip.background = element_blank(),
    strip.text = element_text(size = 12)
  )

# Stampa del grafico
print(p)

# Salva il grafico come un singolo file
ggsave(
  filename = './fishsuit-africa/figs/diff_ex_fut_logaritmic_2050_median.jpg',
  plot = p,
  width = 210, height = 229.51, dpi = 600, units = 'mm'
)


# ------- PLOT WITHOUT LOGARITMIC SCALE
# Visualizzazione della mappa con ggplot2, zero in bianco e valori estremi in rosso/blu scuro
p <- ggplot() +
  geom_sf(data = africa, fill = "grey90") +
  geom_tile(data = df, aes(x = x, y = y, fill = value)) +
  scale_fill_gradient2(
    low = "darkblue", mid = "white", high = "darkred",
    midpoint = 0,  # Zero è rappresentato come bianco
    limits = c(-0.05, 0.32),  # Limita i colori ai valori min e max del dataset
    oob = scales::squish  # Valori fuori dai limiti vengono mantenuti entro il range
  ) +
  facet_wrap(~ ssp, ncol = 1) +  # Un pannello per ogni scenario, tutti nello stesso grafico
  theme_minimal() +
  theme(
    legend.position = 'bottom',
    legend.key.width = unit(6, 'line'),
    axis.text = element_blank(),  # Rimuove le etichette dei gradi
    axis.ticks = element_blank(),  # Rimuove le tacche sugli assi
    panel.grid.major = element_blank(),  # Rimuove il reticolo
    strip.background = element_blank(),
    strip.text = element_text(size = 12)
  )

# Stampa del grafico
print(p)

# Salva il grafico come un singolo file
ggsave(
  filename = './fishsuit-africa/figs/diff_ex_fut_2050.jpg',
  plot = p,
  width = 210, height = 229.51, dpi = 600, units = 'mm'
)

