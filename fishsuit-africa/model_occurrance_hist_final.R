library(raster)
library(dplyr)
library(sf)
library(parallel)

# Set paths
conf_hist <- 'hist_existing'
dir_proc <- './fishsuit-africa/proc/w5e5/'
dir_merged <- paste0(dir_proc, conf_hist, '/pcrglobwb_processed/merged/')
dir_out <- paste0(dir_proc, conf_hist, '/')

# Load species thresholds
niche <- read.csv(paste0(dir_proc, conf_hist, "/niches/niches_filtered.csv"))

# Variabili di interesse
vars <- c("Qmi", "Qma", "Tma", "Tmi", "Qzf")

# --------- ESH hist calculation for each species
calculate_ESH_hist <- function(species_id) {
  # Carica i dati di occorrenza storica per la specie
  result <- readRDS(paste0("./modelled_occurrence/", species_id, ".rds"))
  result <- result[complete.cases(result), ]  # Rimuove righe con NAs
  
  # Inizializza il dataframe per i risultati ESH
  d <- data.frame(
    id_no = species_id,
    no_cells = nrow(result),
    area_total_km2 = sum(result$area, na.rm = TRUE),
    ESH_all = NA,
    ESH_Q = NA,
    ESH_T = NA,
    ESH_QnT = NA
  )
  
  # Calcola le metriche ESH
  all_vars_hist <- paste0(vars, '_hist')
  
  # ESH complessivo
  d$ESH_all <- sum(result[!apply(result[, all_vars_hist], 1, all), 'area'], na.rm = TRUE) / d$area_total_km2 * 100
  
  # ESH per le variabili di flusso
  d$ESH_Q <- sum(result[!apply(result[, paste0(c('Qmi_hist', 'Qzf_hist', 'Qma_hist'))], 1, all), 'area'], na.rm = TRUE) / d$area_total_km2 * 100
  
  # ESH per le variabili di temperatura
  d$ESH_T <- sum(result[!apply(result[, paste0(c('Tmi_hist', 'Tma_hist'))], 1, all), 'area'], na.rm = TRUE) / d$area_total_km2 * 100
  
  # ESH combinato per flusso e temperatura
  d$ESH_QnT <- sum(result[!apply(cbind(
    apply(result[, paste0(c('Qmi_hist', 'Qzf_hist', 'Qma_hist'))], 1, all),
    apply(result[, paste0(c('Tmi_hist', 'Tma_hist'))], 1, all)
  ), 1, any), 'area'], na.rm = TRUE) / d$area_total_km2 * 100
  
  # Calcolo ESH per ogni variabile individuale
  for (v in vars) {
    d[, paste0('ESH_', v)] <- sum(result[!result[, paste0(v, '_hist')], 'area'], na.rm = TRUE) / d$area_total_km2 * 100
  }
  
  return(d)
}

# Elenco degli ID delle specie
species_ids <- unique(niche$id_no)

# Esegui `calculate_ESH_hist` per tutte le specie in parallelo e aggrega i risultati
#ncores <- 1  # Imposta il numero di core
ESH_tab <- do.call(rbind, parallel::mcmapply(calculate_ESH_hist, species_ids, SIMPLIFY = FALSE, mc.cores = ncores))
row.names(ESH_tab) <- NULL
# Salva i risultati in un file CSV
write.csv(ESH_tab, paste0(dir_out, "ESH_tab_hist.csv"), row.names = FALSE)
cat("ESH storico calcolato e salvato con successo in:", paste0(dir_out, "ESH_tab_hist.csv"), "\n")

#----------SR calculation for each species and grid cells
# Carica il template dei punti
tab <- readRDS('./fishsuit-africa/proc/ssp/points_template.rds') %>%
  as.data.frame() %>%
  dplyr::select(-geometry)

# colonne di output da aggiungere alla tabella
additional_cols <- c("Q_all", "T_all", "any", "all", "both_QT", "occ")
for (v in c(vars, additional_cols)) tab[, v] <- NA

# Lista degli ID delle specie
species_ids <- unique(read.csv(paste0(dir_proc, conf_hist, "/niches/niches_filtered.csv"))$id_no)

# Calcola gli indicatori SR basati sui dati storici
for (i in species_ids) {
  
  # Carica i dati di occorrenza per la specie attuale
  t <- readRDS(paste0("./modelled_occurrence/",i, ".rds"))
  t$X <- as.integer(row.names(t)) # Mappa l'indice di riga
  
  # Filtra i dati storici validi
  th <- t[, paste0(vars, '_hist')]  # Dati storici per le variabili
  t <- t[which(apply(th, 1, all)), ]  # Mantieni solo le righe dove tutte le condizioni storiche sono soddisfatte
  
  # Calcola gli indicatori e aggiorna la tabella `tab`
  tab$occ[t$X] <- apply(cbind(tab$occ[t$X], rep(1, nrow(t))), 1, function(x) sum(x, na.rm = TRUE))
  
  tab[t$X, 'all'] <- apply(cbind(tab[t$X, 'all'], as.integer(apply(t[, paste0(c('Qmi', 'Qzf', 'Qma', 'Tma', 'Tmi'), '_hist')], 1, all))), 1, function(x) sum(x, na.rm = TRUE))
  
  tab[t$X, 'any'] <- apply(cbind(tab[t$X, 'any'], as.integer(apply(t[, paste0(c('Qmi', 'Qzf', 'Qma', 'Tma', 'Tmi'), '_hist')], 1, any))), 1, function(x) sum(x, na.rm = TRUE))
  
  tab[t$X, 'Q_all'] <- apply(cbind(tab[t$X, 'Q_all'], as.integer(apply(t[, paste0(c('Qmi', 'Qzf', 'Qma'), '_hist')], 1, all))), 1, function(x) sum(x, na.rm = TRUE))
  
  tab[t$X, 'T_all'] <- apply(cbind(tab[t$X, 'T_all'], as.integer(apply(t[, paste0(c('Tmi', 'Tma'), '_hist')], 1, all))), 1, function(x) sum(x, na.rm = TRUE))
  
  tab[t$X, 'both_QT'] <- apply(cbind(
    tab[t$X, 'both_QT'],
    as.integer(apply(cbind(
      apply(t[, paste0(c('Qmi', 'Qzf', 'Qma'), '_hist')], 1, all),
      apply(t[, paste0(c('Tmi', 'Tma'), '_hist')], 1, all)
    ), 1, any))
  ), 1, function(x) sum(x, na.rm = TRUE))
  
  # Aggiorna la tabella `tab` con i risultati per ogni variabile storica individuale
  for (v in vars) {
    tab[t$X, v] <- apply(cbind(tab[t$X, v], as.integer(t[, paste0(v, '_hist')])), 1, function(x) sum(x, na.rm = TRUE))
  }
}

# Salva i risultati finali in un file RDS
saveRDS(tab, paste0(output_dir, 'SR_tab_hist.rds'))
cat('\n\nCalcolo SR storico completato e salvato in:', paste0(output_dir, 'SR_tab_hist.rds'), '\n')
cat(paste0(rep('-', 30)), '\n\n')

