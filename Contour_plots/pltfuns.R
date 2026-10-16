plt_contour <- function(df, 
              xlab="u", 
              ylab="h",
              xlims= c(-2,2),
              ylims= c(-2,2),
              subtitle="",
              legend_title=" ",
              color_option = "plasma",
              xbrks = c(-2,0,2),
              ybrks = c(-2,0,2),
              n_col=1,
              legend_height=4,
              text_size=30) {
require(ggplot2)
require(scales)
require(viridis)

# check if df contains s, t, cor
vars = c("s", "t", "cor")
missing = vars %in% names(df)

missing <- setdiff(vars, names(df))
if (length(missing) > 0) {
  stop(sprintf("Missing variables: %s", paste(missing, collapse = ", ")))
}


base_theme <-  
  theme(
    #plot.margin = margin(0,0,0,0),
    #axis.title = element_blank(),
    #strip.text = element_blank(),
    text = element_text(size=text_size),
    legend.title = element_blank(),
    legend.key.height = unit(0.3 * legend_height / n_col, "cm")
  ) 

brks <- pretty(range(df$cor, na.rm = TRUE), n = 5)

df$group = "1"  
p = df %>% ggplot() + 
  geom_tile(aes(x=s,y=t, fill=cor)) + 
  scale_x_continuous(breaks=xbrks) + 
  scale_y_continuous(breaks=ybrks) + 
  geom_contour(aes(x=s,y=t,z=cor), breaks=brks) + 
  facet_wrap(~group, 
    labeller = as_labeller(subtitle),
    ncol=n_col) + 
  scale_fill_gradientn(
    #colors=viridis::viridis(256, option = "plasma")) + 
    colors=c("white", "red")) + 
  labs(x = xlab, y = ylab, fill = legend_title) + 
  coord_equal(expand=TRUE,xlim = xlims, ylim = ylims) + 
   base_theme
  
  
  return(p)
}





plt2_contour <- function(df, 
              subtitle=" ",
              xlab=" ", 
              ylab=" ",
              xlims= c(-2,2),
              ylims=c(-2,2),
              legend_title=" ",
              color_option = "plasma",
              xbrks = c(-2,0,2),
              ybrks = c(-2,0,2),
              n_col=1,
              legend_height=4,
              text_size=30) {
require(ggplot2)
require(scales)
require(viridis)

# check if df contains s, t, cor
vars = c("s", "t", "cor")
missing = vars %in% names(df)

missing <- setdiff(vars, names(df))
if (length(missing) > 0) {
  stop(sprintf("Missing variables: %s", paste(missing, collapse = ", ")))
}


base_theme <- #theme_minimal() + 
  theme(
    #plot.margin = margin(0,0,0,0),
    #axis.title = element_blank(),
    #strip.text = element_blank(),
    text = element_text(size=text_size),
    legend.title = element_blank(),
    legend.key.height = unit(0.8 * legend_height / n_col, "cm")
  ) 

brks <- pretty(range(df$cor, na.rm = TRUE), n = 5)

p = df %>% ggplot(aes(x=s,y=t)) + 
  geom_tile(aes(fill=cor)) + 
  scale_x_continuous(breaks=xbrks) + 
  scale_y_continuous(breaks=ybrks) + 
  geom_contour(aes(z=cor), breaks=brks) + 
  facet_wrap(
    ~ group, 
    ncol=n_col, 
    labeller = as_labeller(subtitle),
    as.table=TRUE) + 
  scale_fill_gradientn(
    #colors=viridis::viridis(256, option = "plasma")) + 
    colors=c("white", "red")) + 
  labs(x = xlab, y = ylab, fill = legend_title) + 
  coord_equal(expand=TRUE,xlim = xlims, ylim = ylims) + 
   base_theme
  
  
  return(p)
}


plt_map = function(
  df, 
 zlim = NULL, 
 breaks = NULL,
 mainTitle = NULL, 
 legendTitle = ""
  ){
  require(sf)
  require(ggplot2)
  require(scales)
  require(viridis)

  df = df %>% drop_na()

  varnames = names(df)
  if(! ("long" %in% varnames) || !("lat" %in% varnames) ){
    stop("`df` must have columns: long, lat")
  } 


  data_sf <- st_as_sf(df, coords = c("long", "lat"),
   crs = 4326)
# Extract numeric X/Y from sf points
xy <- cbind(data_sf, st_coordinates(data_sf)) %>% 
  as.data.frame()  # columns X, Y
maxabs <- max(abs(xy$y), na.rm = TRUE)


  # --- bbox from data_sf ---
  bb <- st_bbox(data_sf)
  xlim <- c(bb["xmin"], bb["xmax"])
  ylim <- c(bb["ymin"], bb["ymax"])
  bbox_sfc <- st_as_sfc(bb)  # polygon for cropping
  
  bx = pretty(xlim, n = 3)
  by = pretty(ylim, n = 3)

  if(is.null(zlim)){
    zlim = c(min(df$y), max(abs(df$y)) )
  }

  g = ggplot(data=data_sf) + 
  geom_sf(aes(geometry=geometry, color=y)) + 
  scale_color_gradientn(
      colours = c("#0000FFFF", "#FFFFFFFF", "#FF0000FF"),
      name = legendTitle,
      limits = zlim
  ) +
    # scale_fill_viridis_c(
    #   option = "plasma",
    #   oob=scales::squish,
    #   #breaks = breaks,
    #   guide=guide_colorbar(
    #         title=legendTitle)
    # ) + 
    #theme_minimal() +
    theme(
      legend.position = "right",   # <- force legend on
      legend.title = element_text(size = 12),
      legend.text  = element_text(size = 10)
    ) + 
    scale_x_continuous(breaks = bx) +
    scale_y_continuous(breaks = by)
  

  return(g)
}


plt_usamap = function(
  df, 
 zlim = NULL, 
 breaks = NULL,
 mainTitle = NULL, 
 legendTitle = "ppb"
  ){
  require(sf)
  require(ggplot2)
  require(scales)
  require(viridis)

  df = df %>% drop_na()

  varnames = names(df)
  if(! ("long" %in% varnames) || !("lat" %in% varnames) ){
    stop("`df` must have columns: long, lat")
  } 


  data_sf <- st_as_sf(df, coords = c("long", "lat"),
   crs = 4326)
# Extract numeric X/Y from sf points
xy <- cbind(data_sf, st_coordinates(data_sf)) %>% 
  as.data.frame()  # columns X, Y
maxabs <- max(abs(xy$y), na.rm = TRUE)

us_states = ne_states(country = "United States of America", returnclass = "sf")

  # --- bbox from data_sf ---
  # bb <- st_bbox(data_sf)

  # xlim <- c(bb["xmin"], bb["xmax"])
  # ylim <- c(bb["ymin"], bb["ymax"])
  # bbox_sfc <- st_as_sfc(bb)  # polygon for cropping
  
  bbox <- list(xmin=-125, xmax=-66, ymin=24.5, ymax=49.5)
  xlim = c(bbox$xmin, bbox$xmax)
  ylim = c(bbox$ymin, bbox$ymax)

  bx = pretty(xlim, n = 3)
  by = pretty(ylim, n = 3)

  if(is.null(zlim)){
    zlim = c(min(df$y), max(df$y) )
  }

  g = ggplot(data=data_sf) + 
  geom_sf(aes(geometry=geometry, color=y)) + 
  scale_color_gradientn(
      colours = c("#0000FFFF", "#FFFFFFFF", "#FF0000FF"),
      limits = zlim,
      name = legendTitle
  ) +
  geom_sf(data = us_states, 
          fill = NA, 
          color = "black", 
          linewidth = 0.1) + 
  coord_sf(xlim=c(bbox$xmin, bbox$xmax), 
           ylim=c(bbox$ymin, bbox$ymax) ) + 
    #theme_minimal() +
    theme(
      legend.position = "right",   # <- force legend on
      legend.title = element_text(size = 12),
      legend.text  = element_text(size = 10)
    ) + 
    scale_x_continuous(breaks = bx) +
    scale_y_continuous(breaks = by) + 
    ggtitle(mainTitle)

  

  return(g)
}



plt_camap = function(
  df, 
 zlim = NULL, 
 breaks = NULL,
 mainTitle = NULL, 
 legendTitle = "ppb"
  ){
  require(sf)
  require(ggplot2)
  require(scales)
  require(viridis)

  df = df %>% drop_na()

  varnames = names(df)
  if(! ("long" %in% varnames) || !("lat" %in% varnames) ){
    stop("`df` must have columns: long, lat")
  } 


  data_sf <- st_as_sf(df, coords = c("long", "lat"),
   crs = 4326)
# Extract numeric X/Y from sf points
xy <- cbind(data_sf, st_coordinates(data_sf)) %>% 
  as.data.frame()  # columns X, Y
maxabs <- max(abs(xy$y), na.rm = TRUE)

#us_states = ne_states(country = "United States of America", returnclass = "sf")
if (!requireNamespace("tigris", quietly = TRUE)) {
  stop("Install the tigris package to draw California county boundaries.")
}
old_options <- options(tigris_use_cache = TRUE)
on.exit(options(old_options), add = TRUE)
ca_counties <- tigris::counties(state = "CA", cb = TRUE, year = 2024,
                              class = "sf")
ca_state <- tigris::states(cb = TRUE, year = 2024, class = "sf") |>
  subset(STUSPS == "CA")

  # --- bbox from data_sf ---
  # bb <- st_bbox(data_sf)

  # xlim <- c(bb["xmin"], bb["xmax"])
  # ylim <- c(bb["ymin"], bb["ymax"])
  # bbox_sfc <- st_as_sfc(bb)  # polygon for cropping
  
  bbox <- list(xmin=-125, xmax=-113, ymin=32, ymax=43)
  xlim = c(bbox$xmin, bbox$xmax)
  ylim = c(bbox$ymin, bbox$ymax)

  bx = pretty(xlim, n = 3)
  by = pretty(ylim, n = 3)

  if(is.null(zlim)){
    zlim = c(min(df$y), max(df$y) )
  }

  g = ggplot(data=data_sf) + 
  geom_sf(aes(geometry=geometry, color=y), size=1) + 
  #geom_tile(aes(geometry=geometry,fill=y), stat = "sf_coordinates") + 
  scale_color_gradientn(
      colours = c("#0000FFFF", "#FFFFFFFF", "#FF0000FF"),
      limits = zlim,
      name = legendTitle
  ) +
  geom_sf(data = ca_state, 
          fill = NA, 
          color = "black", 
          linewidth = 0.1) + 
  geom_sf(data = ca_counties, fill = NA, 
          color = "gray40", linewidth = 0.1) +
  coord_sf(xlim=c(bbox$xmin, bbox$xmax), 
           ylim=c(bbox$ymin, bbox$ymax) ) + 
    #theme_minimal() +
    theme(
      legend.position = "right",   # <- force legend on
      legend.title = element_text(size = 12),
      legend.text  = element_text(size = 10)
    ) + 
    scale_x_continuous(breaks = bx) +
    scale_y_continuous(breaks = by) + 
    ggtitle(mainTitle)

  

  return(g)
}



plt_cnmap = function(
  df, 
 zlim = NULL, 
 breaks = NULL,
 mainTitle = NULL, 
 legendTitle = ""
  ){
  require(sf)
  require(ggplot2)
  require(scales)
  require(viridis)
  require(ggmapcn)

  df = df %>% drop_na()

  varnames = names(df)
  if(! ("long" %in% varnames) || !("lat" %in% varnames) ){
    stop("`df` must have columns: long, lat")
  } 


  data_sf <- st_as_sf(df, coords = c("long", "lat"),
   crs = 4326)
# Extract numeric X/Y from sf points
xy <- cbind(data_sf, st_coordinates(data_sf)) %>% 
  as.data.frame()  # columns X, Y
maxabs <- max(abs(xy$y), na.rm = TRUE)

# region = ne_states(country = "China", returnclass = "sf")
# region <- geom_mapcn(admin_level = "province", return_sf = TRUE)

  # --- bbox from data_sf ---
  # bb <- st_bbox(data_sf)

  # xlim <- c(bb["xmin"], bb["xmax"])
  # ylim <- c(bb["ymin"], bb["ymax"])
  # bbox_sfc <- st_as_sfc(bb)  # polygon for cropping
  
  # bbox <- list(xmin=73, xmax=134, ymin=20, ymax=54)
  # xlim = c(bbox$xmin, bbox$xmax)
  # ylim = c(bbox$ymin, bbox$ymax)

  # bx = pretty(xlim, n = 3)
  # by = pretty(ylim, n = 3)

  if(is.null(zlim)){
    zlim = c(min(df$y), max(abs(df$y)) )
  }

  g = ggplot(data=data_sf) + 
  geom_sf(aes(geometry=geometry, color=y)) + 
  scale_color_gradientn(
      colours = c("#0000FFFF", "#FFFFFFFF", "#FF0000FF"),
      limits = zlim,
      name = legendTitle
  ) +
  # geom_sf(data = region, 
  #         fill = NA, 
  #         color = "black", 
  #         linewidth = 0.1) + 
  geom_mapcn(data = region,  admin_level = "province", 
    fill = NA, color = "black", linewidth = 0.1) +
  #geom_boundary_cn() + 
  coord_sf() + 
  # coord_sf(xlim=c(bbox$xmin, bbox$xmax), 
  #          ylim=c(bbox$ymin, bbox$ymax) ) + 
    #theme_minimal() +
    theme(
      legend.position = "right",   # <- force legend on
      legend.title = element_text(size = 12),
      legend.text  = element_text(size = 10)
    ) + 
    scale_x_continuous() +
    scale_y_continuous() + 
    ggtitle(mainTitle)

  

  return(g)
}



### spatial variograms

is_sfc = function(longlat){
  if(is.data.frame(longlat)){
    sp_pts = st_as_sf(longlat, coords=c("long", "lat"), crs=4326) 
  }else if(is.matrix(longlat)){
      df = data.frame(long=longlat[,1], lat=longlat[,2]) 
      sp_pts = st_as_sf(df, coords=c("long", "lat"), crs=4326) 
  }

  if(inherits(longlat)){

  }

  return(sp_pts)

}

plt_vg = function(longlat, y, uvec, maxdist=NULL, omni=TRUE, km=TRUE){

  df = longlat 

  if(!inherits(df, "sfc")){


    if(is.data.frame(df)){
      varnames = names(df)
      if(! ("long" %in% varnames) || !("lat" %in% varnames) ){
        stop("`longlat` must have columns: long, lat")
      } 

      sp_pts = st_as_sf(df, coords=c("long", "lat"), crs=4326) 
    }else if(is.matrix(df)){
        df = data.frame(long=longlat[,1], lat=longlat[,2]) 
        sp_pts = st_as_sf(df, coords=c("long", "lat"), crs=4326) 
    }else{
      stop("`longlat` must be in one of the following types: `sfc`, `data.frame`, `matrix`")
    }

  }

  if(km){
    sp_pts = sp_pts %>% 
             st_transform(crs=5070)    # transform to meters from degrees 
  }

  ns = length(sp_pts$geometry) 
  if(length(y)!=ns){
    stop("y must has the same dimenon in longlat")
  }
  
  ind.na = is.na(y)
  df1 = as.data.frame(st_coordinates(sp_pts))

  df1 = df1[!ind.na,]
  
  xy_km = as.matrix(df1) / 1000      # make a matrix [x_m, y_m] in km 
  
  
  geodat = list(coords=xy_km, data=y[!ind.na])
  class(geodat) = "geodata"
  

  if(is.null(maxdist)){
    maxdist = max(uvec)
  }
  if(!omni){
    vg = geoR::variog4(
      geodat, 
      uvec = uvec,
      max.dist = maxdist,
      na.action="na.omit"
    )  
  }else{
    vg = geoR::variog(
      geodat, 
      uvec = uvec,
      max.dist = maxdist,
      na.action="na.omit"
    ) 
    
  }
  
  return(vg)

}



###### Plot temporal variograms
make_tvg_df <- function(i, j, lags, tvg_mat) {
  data.frame(day = lags, 
             variograms = tvg_mat[i, j, ], 
             series = sprintf("(%d,%d)", i, j))
}

plt_tvg = function(tvg_mat, pairs=list(c(1,1)), lags=seq(1,7,by=1), legend=TRUE){

  tvg_all = do.call(dplyr::bind_rows, 
      lapply(pairs, 
      \(ij) make_tvg_df(ij[1], ij[2], lags, tvg_mat))
      )

  g = ggplot(tvg_all, 
         aes(day, variograms, color = series)) +
    geom_point() + 
    geom_line() +
    theme_minimal() + 
    labs(x="Day", 
         y="Temporal variogram", 
         color="Locations") + 
    scale_x_continuous(breaks = lags, 
      limits = c(min(lags), max(lags))
      )

  if(!legend){
    g = g + theme(legend.position = "none") 
  }

  return(g)

}



plt_d_tvg = function(obs, lonlat, tvg_mat=NULL, ind.y, lags = c(1,2,3,4)){

  if(is.matrix(obs)){
    ns = nrow(obs) 
    nt = ncol(obs)
  }else{
    stop("obs must be in a matrix form")
  }

  if(ns!=nrow(lonlat)){
    stop("The number of rows in `obs` must be the same as the number of spatial locations in `lonlat`.")
  }


  if(is.null(tvg_mat)){
    tvg_mat = get_tvg(obs, lags=lags)
  }
  
  nlag = length(lags)
  ns = dim(tvg_mat)[1]
  dg = array(NA, dim=c(ns, ns, nlag))
  for(k in 1:nlag){
    M = tvg_mat[,,k]
    dg[,,k] = M - t(M)
  }


  plots_lon <- list()
  plots_lat <- list()

  for (k in seq_len(dim(dg)[3])) {
    
    # --- longitude difference plot ---
    g1 <- data.frame(
      x = lonlat[,1] - lonlat[ind.y,1],
      variograms = dg[, ind.y, k]
    ) %>% drop_na() %>%
      ggplot(aes(x = x, y = variograms)) +
      geom_point() +
      theme_minimal() +
      labs(
        x = "Longitude difference (deg)",
        y = "Asymmetry",
        title = paste("Lag =", lags[k])
      )
    
    # --- latitude difference plot ---
    g2 <- data.frame(
      x = lonlat[,2] - lonlat[ind.y,2],
      variograms = dg[, ind.y, k]
    ) %>% drop_na() %>%
      ggplot(aes(x = x, y = variograms)) +
      geom_point() +
      theme_minimal() +
      labs(
        x = "Latitude difference (deg)",
        y = "Asymmetry",
        title = paste("Lag =", lags[k])
      )
    
    # store them
    plots_lon[[k]] <- g1
    plots_lat[[k]] <- g2
  }




  return(list(plots_lon,plots_lat))


}


plt_avgd_tvg = function(obs, lonlat, tvg_mat=NULL, lags = seq(1,4,by=1)){

  if(is.matrix(obs)){
    ns = nrow(obs) 
    nt = ncol(obs)
  }else{
    stop("obs must be in a matrix form")
  }

  if(ns!=nrow(lonlat)){
    stop("The number of rows in `obs` must be the same as the number of spatial locations in `lonlat`.")
  }

  if(is.null(tvg_mat)){
    tvg_mat = get_tvg(obs, lags=lags)
  }

  nlag = length(lags)
  ns = dim(tvg_mat)[1]
  dg = array(NA, dim=c(ns, ns, nlag))
  for(k in 1:nlag){
    M = tvg_mat[,,k]
    dg[,,k] = M - t(M)
  }

  dg_bar = apply(dg, c(2,3), mean)

  plots_lon <- list()
  plots_lat <- list()

  for (k in seq_len(dim(dg)[3])) {
    
    # --- longitude difference plot ---
    g1 <- data.frame(
      x = lonlat[,1],
      variograms = dg_bar[, k]
    ) %>% drop_na() %>%
      ggplot(aes(x = x, y = variograms)) +
      geom_point() +
      theme_minimal() +
      labs(
        x = "Longitude (deg)",
        y = "Averaged asymmetry",
        title = paste("Lag =", lags[k])
      )
    
    # --- latitude difference plot ---
    g2 <- data.frame(
      x = lonlat[,2],
      variograms = dg_bar[, k]
    ) %>% drop_na() %>%
      ggplot(aes(x = x, y = variograms)) +
      geom_point() +
      theme_minimal() +
      labs(
        x = "Latitude (deg)",
        y = "Averaged asymmetry",
        title = paste("Lag =", lags[k])
      )
    
    # store them
    plots_lon[[k]] <- g1
    plots_lat[[k]] <- g2
  }


  return(list(plots_lon,plots_lat))

}




### data format conversion
st_as_stfdf = function(df, time=NULL){
  is_there = c("long", "lat", "time", "var") %in% colnames(df)
  if(sum(is_there)!=4){
    stop("df must contain four variables named: `long`, `lat`, `time`, `var`")
  }
  
  df_w = stl_to_stw(df)
  
  if(is.null(time)){
    time = colnames(df_w)[-c(1:2)]
    if(!is.Date(time)){
      stop("The column names of the time variable must be Dates.")
    }
  }

  if(!is.Date(time)){
    stop("time must be a Date variable.")
  }
  std = stw_to_stfdf(df_w, time=time)
  
  return(std)
  
}
stl_to_stw = function(df){
  is_there = c("long", "lat", "time", "var") %in% colnames(df)
  if(sum(is_there)!=4){
    stop("df must contain four variables named: `long`, `lat`, `time`, `var`")
  }
  
  Y_wide = df %>% 
  pivot_wider(
    id_cols = c(long, lat),
    names_from = time,
    values_from = var
  ) 
  return(Y_wide)
}

stw_to_stfdf = function(df_w, time=NULL){
  if(is.null(time)){
    time = colnames(df_w)[-c(1:2)]
    if(!lubridate::is.Date(time)){
      stop("The column names of the time variable must be Dates.")
    }
  }
  
  if(!lubridate::is.Date(time)){
    stop("time must be a Date variable.")
  }
  
  # create sf coordinates
  sp_sf = st_as_sf(df_w, 
                coords=c("long", "lat"), 
                crs=4326) %>% 
    st_geometry() 
  # transform to SpatialPoints format
  sp_pts = st_transform(sp_sf, 5070) %>% as_Spatial()
  
  stmat = df_w %>%
    select(-long, -lat) %>%
    t()
  
  out = STFDF(sp=sp_pts, time=time, data=data.frame(as.vector(stmat)))
  return(out)
}


