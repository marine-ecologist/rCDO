


library(httr)
library(rerddap)
library(tictoc)



tic()
NOAA_subset <- griddap(datasetx='NOAA_DHW',
                       time=c('2018-01-01', '2019-12-31'),
                       latitude = c(-23.5, -23.4),
                       longitude = c(153.4, 153.5),
                       fields = c('CRW_SST'),
                       fmt = "nc"
)

toc()

base::saveRDS(NOAA_subset, "/Users/rof011/cdoR/datasets/OISST_subset.Rds")



tic()
OISST_march <- griddap(datasetx='ncdcOisst21NrtAgg_LonPM180',
                           time=c('2024-03-01', '2024-03-01'),
                           latitude = c(-89.875, 89.875),
                           longitude = c(-179.875, 179.875),
                           fmt = "nc")
toc()

base::saveRDS(NOAA_subset, "/Users/rof011/GBR-dhw/datasets/NOAA_subset.Rds")


tic()
NOAA_timeseries_nc <- griddap(datasetx='NOAA_DHW',
                              time=c('1985-06-01', '2024-04-31'),
                              latitude = c(-25, -10.5),
                              longitude = c(140, 155),
                              fields = c('CRW_SST', 'CRW_SSTANOMALY', 'CRW_HOTSPOT', 'CRW_DHW'),
                              fmt = "nc"
                              )

toc()

base::saveRDS(NOAA_timeseries_nc, "/Users/rof011/GBR-dhw/datasets/NOAA_timeseries_nc.Rds")
