
source('./africa_config.R') # always load as functions are loaded within this script
source('./generic.R')
# override, memory problems..
ncores <- 1

libinv(c('raster','foreach','dplyr','sf'))

# read the species ranges
cat('Reading ranges shapefile..\n')
ranges <- read_sf('./proc/Africa_clipped_species_ranges_merged.gpkg')

# split to parallelize the run
cat('Splitting the ranges shapefile for parallelized runs..\n')

# create ids groups
ids_split <- ranges %>% 
  pull(id_no) %>%
  unique %>%
  split(.,cut(1:length(.),40,labels=F))

# split the ranges
ranges_split <- lapply(ids_split,function(x) ranges %>% filter(id_no %in% x))

# crop global template 
data_points <- readRDS('proc/ssp/points_template.rds')
africa_box <- st_bbox(c(xmin=-18.9583,xmax=59.9583,ymin=-34.9583,ymax=37.9583), crs=st_crs(data_points)) #define lat/lon for African continent
africa_data <- st_crop(data_points, africa_box)
saveRDS(africa_data, 'proc/ssp/points_template_africa.rds')

cat('Run algorithm..\n')
source('./fun/range2table.R')
parallel::mcmapply(range2table,
                   in_shapefile_ranges = ranges_split,
                   out_shapefile_multipoints = paste0('sp_points_',1:length(ranges_split))%>% as.list,
                   out_dir_shapefile_multipoints = 'proc/ssp/',
                   mask_lyr = 'data/ldd.asc',
                   template_points = c(list('proc/ssp/points_template_africa.rds'),rep(list(NULL),length(ranges_split)-1) ),
                   dir_single_ranges = 'proc/ssp/single_points/',
                   mc.cores = ncores, SIMPLIFY = F
)

# # merge the multipoint shapefiles together
# cat('Binding together and saving MULTIPOINT shapefiles..\n')
# files <- list.files('proc/ssp',pattern = '.gpkg',full.names = T)
# points_merged <- lapply(files,read_sf) %>% do.call('rbind',.)
# st_write(points_merged,'proc/ssp/sp_points.gpkg')
# 
# lapply(files,function(x) system(paste0('rm -r ',x)))

# # diagnostics
# range2table(in_shapefile_ranges = ranges_split[[1]],
#             out_shapefile_multipoints = paste0('sp_points_',1:length(ranges_split))[1],
#             out_dir_shapefile_multipoints = 'proc/ssp/',
#             mask_lyr = 'data/ldd.asc',
#             template_points = 'proc/ssp/points_template.rds',
#             dir_single_ranges = 'proc/ssp/single_points/'
# )
