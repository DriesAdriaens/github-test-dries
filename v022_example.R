options(stringsAsFactors = FALSE)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

library(remotes)

install_github("inbo/n2khab",
               build_vignettes = TRUE,
               upgrade = TRUE)

library(n2khab)

n2khab_data_path <- fileman_up("n2khab_data")

soilmap_simple_path <- file.path(n2khab_data_path, "20_processed/soilmap_simple")
dir.create(soilmap_simple_path)
download_zenodo(doi = "10.5281/zenodo.3732903",
                path = soilmap_simple_path)

library(knitr)
library(tidyverse)
library(sf)
library(mapview)
library(plotly)
library(units)

?read_soilmap

sm_simple <- read_soilmap()
sm_simple
#> Simple feature collection with 270550 features and 10 fields
#> geometry type:  MULTIPOLYGON
#> dimension:      XY
#> bbox:           xmin: 22265.45 ymin: 153062.6 xmax: 258872.2 ymax: 244027.9
#> epsg (SRID):    31370
#> proj4string:    +proj=lcc +lat_1=51.16666723333333 +lat_2=49.8333339 +lat_0=90 +lon_0=4.367486666666666 +x_0=150000.013 +y_0=5400088.438 +ellps=intl +towgs84=-106.8686,52.2978,-103.7239,0.3366,-0.457,1.8422,-1.2747 +units=m +no_defs
#> # A tibble: 270,550 x 11
#>    bsm_poly_id bsm_region bsm_ge_coastalp… bsm_mo_soilunit… bsm_mo_substr
#>  *       <dbl> <fct>      <lgl>            <fct>            <fct>        
#>  1      165740 Kunstmati… TRUE             OB               <NA>         
#>  2      176046 Kunstmati… TRUE             OB               <NA>         
#>  3      185239 Zandleems… FALSE            Ldc              <NA>         
#>  4      162400 Kunstmati… TRUE             OB               <NA>         
#>  5      173971 Kunstmati… TRUE             OB               <NA>         
#>  6      173087 Zandleems… FALSE            Ldp              <NA>         
#>  7      199453 Zandleems… FALSE            Lep              <NA>         
#>  8      176922 Zandleems… FALSE            Ldc              <NA>         
#>  9      227861 Zandleems… FALSE            Abp(c)           <NA>         
#> 10      185390 Zandleems… FALSE            Lca              <NA>         
#> # … with 270,540 more rows, and 6 more variables: bsm_mo_tex <fct>,
#> #   bsm_mo_drain <fct>, bsm_mo_prof <fct>, bsm_mo_parentmat <fct>,
#> #   bsm_mo_profvar <fct>, geom <MULTIPOLYGON [m]>

glimpse(sm_simple)
#> Observations: 270,550
#> Variables: 11
#> $ bsm_poly_id         <dbl> 165740, 176046, 185239, 162400, 173971, 173087, 1…
#> $ bsm_region          <fct> Kunstmatige gronden, Kunstmatige gronden, Zandlee…
#> $ bsm_ge_coastalplain <lgl> TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALS…
#> $ bsm_mo_soilunitype  <fct> OB, OB, Ldc, OB, OB, Ldp, Lep, Ldc, Abp(c), Lca, …
#> $ bsm_mo_substr       <fct> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ bsm_mo_tex          <fct> NA, NA, L, NA, NA, L, L, L, A, L, NA, L, L, NA, N…
#> $ bsm_mo_drain        <fct> NA, NA, d, NA, NA, d, e, d, b, c, NA, d, c, NA, N…
#> $ bsm_mo_prof         <fct> NA, NA, c, NA, NA, p, p, c, p, a, NA, c, a, NA, N…
#> $ bsm_mo_parentmat    <fct> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ bsm_mo_profvar      <fct> NA, NA, NA, NA, NA, NA, NA, NA, (c), NA, NA, NA, …
#> $ geom                <MULTIPOLYGON [m]> MULTIPOLYGON (((204667.9 19..., MULT…

sm_simple %>%
    st_drop_geometry %>%
    count(bsm_region)

sm_simple %>%
    mutate(area = st_area(.)) %>%
    st_drop_geometry %>%
    group_by(bsm_region) %>%
    summarise(mean_area = mean(area))

zwin_map <-
    sm_simple %>%
    filter(bsm_region == "Zwin") %>%
    ggplot(aes(fill = bsm_mo_tex)) +
    geom_sf()

zwin_map

zwin_map + coord_sf(datum = st_crs(31370))

zwin_map %>% ggplotly()

sm_simple %>%
  filter(bsm_region == "Zwin") %>%
  mutate(bsm_mo_tex = as.character(bsm_mo_tex)) %>%
  mapview(zcol = "bsm_mo_tex",
          alpha.region = 0.8
          # map.types = c("OpenStreetMap", "OpenTopoMap")
          )

soilmap_path <- file.path(n2khab_data_path, "10_raw/soilmap")
dir.create(soilmap_path)
download_zenodo(doi = "10.5281/zenodo.3387008",
                path = soilmap_path,
                parallel = TRUE)

sm <- read_soilmap(use_processed = FALSE)
sm
#> Simple feature collection with 270550 features and 37 fields
#> geometry type:  MULTIPOLYGON
#> dimension:      XY
#> bbox:           xmin: 22265.45 ymin: 153062.6 xmax: 258872.2 ymax: 244027.9
#> epsg (SRID):    31370
#> proj4string:    +proj=lcc +lat_1=51.16666723333333 +lat_2=49.8333339 +lat_0=90 +lon_0=4.367486666666666 +x_0=150000.013 +y_0=5400088.438 +ellps=intl +towgs84=-106.8686,52.2978,-103.7239,0.3366,-0.457,1.8422,-1.2747 +units=m +no_defs
#> # A tibble: 270,550 x 38
#>    bsm_poly_id bsm_map_id bsm_region bsm_ge_coastalp… bsm_ge_region bsm_legend
#>  *       <dbl> <fct>      <fct>      <lgl>            <fct>         <fct>     
#>  1      165740 61E        Kunstmati… TRUE             <NA>          Antropoge…
#>  2      176046 78W        Kunstmati… TRUE             <NA>          Antropoge…
#>  3      185239 95W        Zandleems… FALSE            <NA>          Vochtig z…
#>  4      162400 75E        Kunstmati… TRUE             <NA>          Antropoge…
#>  5      173971 63W        Kunstmati… TRUE             <NA>          Antropoge…
#>  6      173087 64W        Zandleems… FALSE            <NA>          Vochtig z…
#>  7      199453 98E        Zandleems… FALSE            <NA>          Nat zandl…
#>  8      176922 81W        Zandleems… FALSE            <NA>          Vochtig z…
#>  9      227861 88W        Zandleems… FALSE            <NA>          Droge leem
#> 10      185390 95W        Zandleems… FALSE            <NA>          Vochtig z…
#> # … with 270,540 more rows, and 32 more variables: bsm_legend_title <fct>,
#> #   bsm_legend_explan <fct>, bsm_soiltype_id <dbl>, bsm_soiltype <fct>,
#> #   bsm_soiltype_region <fct>, bsm_soilseries <fct>,
#> #   bsm_soilseries_explan <fct>, bsm_mo_soilunitype <fct>, bsm_mo_substr <fct>,
#> #   bsm_mo_substr_explan <fct>, bsm_mo_tex <fct>, bsm_mo_tex_explan <fct>,
#> #   bsm_mo_drain <fct>, bsm_mo_drain_explan <fct>, bsm_mo_prof <fct>,
#> #   bsm_mo_prof_explan <fct>, bsm_mo_parentmat <fct>,
#> #   bsm_mo_parentmat_explan <fct>, bsm_mo_profvar <fct>,
#> #   bsm_mo_profvar_explan <fct>, bsm_mo_phase <fct>, bsm_ge_substr <fct>,
#> #   bsm_ge_substr_explan <fct>, bsm_ge_series <fct>,
#> #   bsm_ge_series_explan <fct>, bsm_ge_subseries <fct>,
#> #   bsm_ge_subseries_explan <fct>, bsm_map_url <fct>, bsm_book_url <fct>,
#> #   bsm_detailmap_url <fct>, bsm_profloc_url <fct>, geometry <MULTIPOLYGON [m]>

glimpse(sm)

sm_mp <-
    sm %>%
    filter(bsm_region == "Middellandpolders")
dim(sm_mp)
#> [1] 3991   38

sm_mp %>%
    mutate(bsm_ge_series = as.character(bsm_ge_series)) %>%
    mapview(zcol = "bsm_ge_series",
            alpha.region = 0.2,
            map.types = "Wikimedia",
            alpha = 0)

sm_mp %>%
    mutate(area = st_area(.) %>% set_units("ha")) %>%
    st_drop_geometry %>%
    group_by(bsm_ge_series, bsm_ge_series_explan) %>%
    summarise(area = sum(area) %>% round(2),
              nr_polygons = n()) %>%
    arrange(desc(area)) %>%
    kable

sm_mp %>%
    filter(bsm_ge_series == "D") %>%
    mutate(area = st_area(.) %>% set_units("ha")) %>%
    st_drop_geometry %>%
    group_by(bsm_soilseries, bsm_soilseries_explan) %>%
    summarise(area = sum(area) %>% round(2),
              nr_polygons = n()) %>%
    arrange(desc(area)) %>%
    kable

sm_mp %>%
    filter(bsm_soilseries == "m.D5") %>%
    mapview(color = "red",
            alpha = 1,
            alpha.region = 0)
