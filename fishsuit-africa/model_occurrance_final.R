source('africa_config.R'); # always load as functions are loaded within this script

libinv(c('raster','foreach','dplyr','sf'))

clmod = c('UKESM')
scen <- c(1, 3, 5) #scenarios[scenarios != 'hist']
conf <- c('ex', 'fut', 'nat')
years <- c(2030, 2040, 2050)
conf_hist <- 'hist_existing'

# Stampa i valori per confermare
cat("Modello climatico:", clmod, "\n")
cat("Scenario:", scen, "\n")
cat("Configurazione:", conf, "\n")
cat("Anno:", years, "\n")

# Generate combinations using expand.grid
combinations <- expand.grid(
  clmod = clmod,
  scen = scen,
  conf = conf,
  year = years
)

# View the resulting data frame
comboscen <- apply(combinations[, c("scen", "conf", "year")], 1, function(x) paste(x, collapse = "_"))
print(comboscen)

cat(paste0(rep('-',30)),'\n')
cat('thresholds used:\n')
cat(paste(vars,parse=' '))
cat('\n')
cat(paste(thresholds,parse=' '))
cat('\n',paste0(rep('-',30)),'\n\n')

# make one table per GCM
niche_list <- lapply(
  seq_along(vars),
  function(j){
    
    d <- readRDS(paste0('/scratch-shared/vbonato1/fishsuit/proc/', 'w5e5/', conf_hist, '/niches/', vars[j], '.rds')) %>%        # per noi è solo w5e5
      dplyr::select(id_no = ID,paste0(thresholds[j],'%')) %>% as_tibble()
    colnames(d)[2] <- paste0(vars[j],'_',thresholds[j],'%')
    
    return(d)
  }
)

# make sure all ids correspond properly
niche <- niche_list[[1]] %>% as_tibble()
for(i in seq_along(vars)[-1]){
  niche <- inner_join(niche,niche_list[[i]])
}

niche <- niche %>% 
  mutate(id_no = as.character(id_no)) %>%
  filter(id_no %in% read.csv('/scratch-shared/vbonato1/fishsuit/proc/thresholds_average_filtered_ex.csv')$id_no) #CAMBIA CON _NAT SE E' HIST_NATURAL!!

# filter based on overall species from threshold_selection.R
write.csv(niche, paste0("/scratch-shared/vbonato1/fishsuit/proc/w5e5/", conf_hist, "/niches/niches_filtered.csv"))

cat(paste0(rep('-',30)),'\n\n')

#----------------------------------------------------------------------------------------------------

cat(paste0(rep('-',30)),'\n')
cat('modeling HABITAT SUITABILITY of each species..\n')

# make tab niches
nich <- niche %>% as.data.frame()
colnames(nich) <- c('id_no',paste0(vars,'_th'))

dir_out <- dir_(paste0('/scratch-shared/vbonato1/fishsuit/proc/', conf_hist, '/', clmod,'/modelled_occurrence/'))
print(paste("Directory dir_out:", dir_out))

# function that takes in input a species and calculates the suitability 
# of each cell based on the thresholds

build_table <- function(n) {
  
  sp <- nich[n, 'id_no']  # ID for the species
  
  pts <- readRDS(paste0('/scratch-shared/vbonato1/fishsuit/proc/ssp/single_points/', sp, '.rds'))
  
  # Combine areas and coordinates, check if correctly formed
  tab <- cbind(
    cbind(pts %>% as.data.frame() %>% dplyr::select(-geometry), st_coordinates(pts))[, c('area', 'X', 'Y')],  # Cell information
    
    # Loop through each variable in vars
    foreach(v = 1:length(vars), .combine = 'cbind') %do% {
      
      # Check historical data
      r <- raster(paste0('/scratch-shared/vbonato1/fishsuit/proc/w5e5/', conf_hist, '/pcrglobwb_processed/merged/', vars[v], '_hist.tif'))
      if (!file.exists(r@file@name)) {
        print(paste("Raster file does not exist:", r@file@name))
        return(NA)
      }
      
      # Extract values, handle length mismatch errors
      val <- extract(r, pts)
      if (length(val) != nrow(pts)) {
        stop(paste("Length mismatch for", vars[v], "in historical data: Expected", nrow(pts), "but got", length(val)))
      }
      
      # Apply thresholds based on variable type
      if (vars[v] == 'Tma') {return(val <= nich[n, paste0(vars[v], '_th')])}
      if (vars[v] == 'Tmi') {return(val >= nich[n, paste0(vars[v], '_th')])}
      if (vars[v] == 'Qma') {return(val <= nich[n, paste0(vars[v], '_th')])}
      if (vars[v] == 'Qzf') {return(val <= nich[n, paste0(vars[v], '_th')])}
      if (vars[v] == 'Qmi') {return(val >= nich[n, paste0(vars[v], '_th')])}
      else {return(NA)}  # Default return in case of unrecognized variable
    },
    
    # Loop through each combination in comboscen for each variable
    foreach(v = seq_along(vars), .combine = 'cbind') %do% {
      foreach(s = seq_along(comboscen), .combine = 'cbind') %do% {
        
        # Check scenario data
        r <- raster(paste0('/scratch-shared/vbonato1/fishsuit/proc/', conf_hist, '/', clmod, '/pcrglobwb_processed/masked/years/', vars[v], '_', comboscen[s], '.tif'))
        if (!file.exists(r@file@name)) {
          print(paste("Raster file does not exist:", r@file@name))
          return(NA)
        }
        
        # Extract values, handle length mismatch errors
        val <- extract(r, pts)
        if (length(val) != nrow(pts)) {
          stop(paste("Length mismatch for", vars[v], "in scenario", comboscen[s], ": Expected", nrow(pts), "but got", length(val)))
        }
        
        # Apply thresholds
        if (vars[v] == 'Tma') {return(val <= nich[n, paste0(vars[v], '_th')])}
        if (vars[v] == 'Tmi') {return(val >= nich[n, paste0(vars[v], '_th')])}
        if (vars[v] == 'Qma') {return(val <= nich[n, paste0(vars[v], '_th')])}
        if (vars[v] == 'Qzf') {return(val <= nich[n, paste0(vars[v], '_th')])}
        if (vars[v] == 'Qmi') {return(val >= nich[n, paste0(vars[v], '_th')])}
        else {return(NA)}
      }
    }
  )
  
  # Assign column names to the table
  colnames(tab) <- c('area', 'x', 'y',
                     paste0(vars, '_hist'),
                     paste0(rep(vars, each = length(comboscen)), '_', comboscen))
  
  # Save the results
  write.csv(tab, paste0(dir_out, sp, '.csv'), row.names = TRUE)
  saveRDS(tab, paste0(dir_out, sp, '.rds'))
}

n_rows <- nrow(nich)
if (n_rows == 0) stop("nich has zero rows.")
print(paste("Total rows to process:", n_rows))

invisible(parallel::mcmapply(build_table,1:nrow(nich),mc.silent = TRUE,mc.cores = ncores))

cat('..ended!\n')
cat(paste0(rep('-',30)),'\n\n')

#-------------------------------------------------------------------------------------------------------
# ESH_tab

cat(paste0(rep('-',30)),'\n')
cat('Build ESH_tab\n')

tabs <- list.files(dir_out,pattern='.rds')
ids <- as.character(
  sapply(list.files(dir_out,pattern='.rds'),function(x) strsplit(strsplit(x,'_')[[1]][1],'\\.')[[1]][1])
)


# function to calculate ESH for each species-tab
esh_calc <- function(sp){ #,sn
  # read the table
  t <- readRDS(paste0(dir_out,sp,'.rds'))
  t <- t[complete.cases(t),] #remove rows that contain one or more NAs
  
  d <- data.frame(
    id_no = rep(sp,length(comboscen)),
    no_cells = nrow(t),
    area_total_km2 = sum(t$area),
    ESH_all = NA,
    ESH_Q = NA,
    ESH_T = NA,
    ESH_QnT = NA,
    comboscen = comboscen #sn is the looped scen
  )
  for(v in vars) d[,paste0('ESH_',v)] <- NA
  
  for(y in 1:nrow(d)){
    
    sn = d$comboscen[y]
    all_vars_hist <- paste0(vars,'_hist')
    all_vars_scen <- paste0(vars,'_', sn)
    
    #filter out rows of ts that are already excluded in th due to the quantile threshold
    t <- t[which(apply(t[,all_vars_hist],1,all)),]
    
    d$area_adjusted_km2 <- sum(t$area)
    
    # need to count the number of false and calculate the associated area per grid-cell
    
    d$ESH_all[y] <- sum(t[which(!apply(t[,all_vars_scen],1,all)),'area'])/d$area_adjusted_km2[y]*100
    
    d$ESH_Q[y] <- sum(t[which(!apply(t[,paste0(c('Qmi','Qzf','Qma'),'_',sn)],1,all)),'area'])/d$area_adjusted_km2[y]*100
    
    d$ESH_T[y] <- sum(t[which(!apply(t[,paste0(c('Tmi','Tma'),'_',sn)],1,all)),'area'])/d$area_adjusted_km2[y]*100
    
    # <<<<<<< NEED TO FIX THIS
    d$ESH_QnT[y] <- sum(t[which(!apply(cbind(apply(t[,paste0(c('Qmi','Qzf','Qma'),'_',sn)],1,all),apply(t[,paste0(c('Tmi','Tma'),'_',sn)],1,all)),1,any)),'area'])/d$area_adjusted_km2[y]*100
    
    for(v in vars) d[y,paste0('ESH_',v)] <- sum(t[which(!t[,paste0(v,'_',sn)]),'area'])/d$area_adjusted_km2[y]*100
    
  }
  return(d)
}

# calculate ESH for each scenario and year span
ESH_tab <- do.call('rbind', parallel::mcmapply(esh_calc,sp = ids, SIMPLIFY = F,mc.cores = ncores))

row.names(ESH_tab) <- NULL

ESH_tab$comboscen <- as.factor(ESH_tab$comboscen)

#tryCatch({
#  cat("Running esh_calc mcmapply\n")
#  ESH_tab <- do.call('rbind', parallel::mcmapply(esh_calc, sp = ids, SIMPLIFY = FALSE, mc.silent = FALSE, mc.cores = ncores))
#  cat("Completed esh_calc mcmapply\n")
#}, error = function(e) {
#  cat("Error in esh_calc mcmapply:", e$message, "\n")
#})

#if (is.data.frame(ESH_tab)) {
#    row.names(ESH_tab) <- NULL
#} else {
#    warning("`ESH_tab` is not a data frame or is NULL. Check data creation steps.")
#}
#ESH_tab$comboscen <- as.factor(ESH_tab$comboscen)


out <- paste0('/scratch-shared/vbonato1/fishsuit/proc/', conf_hist, '/', clmod, '/ESH_tab.csv')

write.csv(ESH_tab, out, row.names = F)

cat('successfully written to ',out,'\n')
cat(paste0(rep('-',30)),'\n\n')

#----------------------------------------------------------------------------------------------------
# SUMMARIZE at the cell-level

cat(paste0(rep('-',30)),'\n')
cat('Build SR_tab\n')

for(sc in comboscen){
  
  tab <- readRDS('/scratch-shared/vbonato1/fishsuit/proc/ssp/points_template.rds') %>% as.data.frame() %>% dplyr::select(-geometry)
  for(v in c(vars,'Q_all','T_all','any','all','both_QT','occ')) tab[,v] <- NA
  
  # <<<<<<<< need to parallelize this
  for(i in ids){
    
    t <- readRDS(paste0(dir_out,i,'.rds'))
    t$X <- as.integer(row.names(t)) #the way it was coded based on csv
    th <- t[, paste0(vars, '_hist')] #historical
    
    t <- t[which(apply(th, 1, all)), ]
    
    #all
    tab$occ[t$X] <- apply(cbind(tab$occ[t$X],rep(1,nrow(t))),1,function(x) sum(x,na.rm=T)) 
    
    tab[t$X,'all'] <- apply(cbind(tab[t$X,'all'],as.integer(apply(t[,paste0(c('Qmi','Qzf','Qma','Tma','Tmi'),'_',sc)],1,all))),1,function(x) sum(x,na.rm=T))
    
    
    tab[t$X,'any'] <- apply(cbind(tab[t$X,'any'],as.integer(apply(t[,paste0(c('Qmi','Qzf','Qma','Tma','Tmi'),'_',sc)],1,any))),1,function(x) sum(x,na.rm=T))
    
    
    tab[t$X,'Q_all'] <- apply(cbind(tab[t$X,'Q_all'],as.integer(apply(t[,paste0(c('Qmi','Qzf','Qma'),'_',sc)],1,all))),1,function(x) sum(x,na.rm=T))
    
    
    tab[t$X,'T_all'] <- apply(cbind(tab[t$X,'T_all'],as.integer(apply(t[,paste0(c('Tmi','Tma'),'_',sc)],1,all))),1,function(x) sum(x,na.rm=T))
    
    
    tab[t$X,'both_QT'] <- apply(cbind(tab[t$X,'both_QT'],as.integer(apply(cbind(
      apply(t[,paste0(c('Qmi','Qzf','Qma'),'_',sc)],1,all),
      apply(t[,paste0(c('Tmi','Tma'),'_',sc)],1,all)
    ),1,any)))
    ,1,function(x) sum(x,na.rm=T))
    
    
    for(v in vars) tab[t$X,v] <- apply(cbind(tab[t$X,v],as.integer(t[,paste0(v,'_',sc)])),1,function(x) sum(x,na.rm=T))
    
  }
  
  saveRDS(tab,paste0('/scratch-shared/vbonato1/fishsuit/proc/',conf_hist, '/',clmod,'/SR_tab_',sc,'.rds'))
  
}


cat('\n\n..done!\n')
cat(paste0(rep('-',30)),'\n\n')
