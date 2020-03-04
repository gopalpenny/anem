
library(magick)

aa <- import_app_rds("shiny-app/example_scenarios/groundwater_agency_scenario2.rds",gen_well_images = FALSE)

wells <- aa$wells %>% sf::st_set_geometry(NULL)

aquifer <- aa$aquifer
aquifer$recharge <- NULL
aquifer$bounds <- aquifer$bounds %>% sf::st_set_geometry(NULL)

Ksat_range <- c(1e-4,2e-3)
z0_range <- c(15,25)
n_range <- c(0.35,0.4)
t_roi <- 3600*24*365*t_ROI_years
R_max <- get_ROI(method="cooper-jacob",Tr=max(Ksat_range)*max(z0_range),t=t_roi,S=min(n_range))
well_images <- wells %>%
  dplyr::filter(wID %in% wID_target) %>%
  dplyr::mutate(R = R_max,
                Q = 1) %>% # set Q = 1 for unit drawdown
  generate_image_wells(aa$aquifer,include_image_columns = T)
loc <- wells %>% dplyr::filter(well_image=="Actual") %>% dplyr::select(x,y,wID)

set.seed(100)
N <- 1000
params_df <- tibble::tibble(Ksat=runif(N,min=Ksat_range[1],max=Ksat_range[2]),
                            z0=runif(N,min=z0_range[1],max=z0_range[2]),
                            n=runif(N,min=n_range[1],max=n_range[2])) %>%
  dplyr::mutate(R=get_ROI(method="cooper-jacob",Tr=Ksat*z0,t=t_roi,S=params_list$n),
                i = dplyr::row_number())


well_head_random <- lapply(split(params_df,1:nrow(params_df)),get_well_heads,loc=loc,well_images=well_images,aquifer=aquifer) %>%
  dplyr::bind_rows()
well_head_drawdown <- well_head_random %>%
  dplyr::filter(wID != 3) %>%
  dplyr::group_by(i) %>%
  dplyr::summarize(head_unit=min(head)) %>%
  dplyr::mutate(drawdown_unit=50-head_unit) %>%
  tidyr::crossing(Q=seq(-0.1,-0.5,by=-0.01)) %>%
  dplyr::mutate(drawdown = drawdown_unit * Q,
                head=50-drawdown,
                selected=Q %in% seq(-0.1,-0.5,by=-0.1),
                Q_name=paste0("Q = ",Q)) %>%
  dplyr::group_by(Q,Q_name) %>%
  dplyr::arrange(dplyr::desc(head)) %>%
  dplyr::mutate(pct_reduction=(50-head)/50*100,
                ecdf=dplyr::row_number()/dplyr::n()) %>%
  dplyr::group_by()

q_colors <- colorspace::heat_hcl(5,h=c(90,350),l=c(80,40),c=c(100,80))
# colorspace::swatchplot(q_colors)
drawdown_quantiles <- well_head_drawdown %>%
  dplyr::group_by(Q,Q_name,selected) %>%
  dplyr::summarize(median=quantile(pct_reduction,0.5))
drawdown_percentiles <- well_head_drawdown %>%
  dplyr::group_by(Q,Q_name,selected) %>%
  dplyr::summarize(pct5=sum((pct_reduction < 5)*1)/dplyr::n())
p_density <- ggplot() +
  geom_density(data=well_head_drawdown %>% dplyr::filter(selected),aes(pct_reduction,color=Q_name),size=1) +
  coord_cartesian(xlim=c(0,15)) +
  scale_color_manual("Pumping rate",values = q_colors) +
  labs(x="Percent reduction",y="Density") +
  theme_bw() %+replace% theme(panel.grid = element_blank(),
                              legend.position = 'none')# c(0.7,0.7))
p_ecdf <- ggplot() +
  geom_line(data=well_head_drawdown %>% dplyr::filter(selected),aes(pct_reduction,ecdf,color=Q_name),size=1) +
  geom_segment(data=drawdown_percentiles %>% dplyr::filter(selected),aes(x=0,xend=5,y=pct5,yend=pct5,color=Q_name),linetype="dashed",size=0.75) +
  geom_point(data=drawdown_percentiles %>% dplyr::filter(selected),aes(x=5,y=pct5,color=Q_name),show.legend = FALSE,size=2) +
  geom_point(data=drawdown_percentiles %>% dplyr::filter(selected),aes(x=0,y=pct5,color=Q_name),show.legend = FALSE,size=2) +
  scale_color_manual("Pumping rate",values=q_colors) +
  xlab("Percent reduction") + ylab("ECDF") +
  coord_cartesian(xlim=c(0,15)) +
  theme_bw() %+replace% theme(panel.grid = element_blank(),
                              # legend.margin = margin(1,0,1,0,unit="cm"),
                              legend.justification = c(0,1))

gw_image <- image_read("shiny-app/screenshots/groundwater_agency_application.png")
tiff_file <- tempfile()
image_write(gw_image,path=tiff_file,format="tiff")
gw_raster <- raster::brick(tiff_file)
p <- raster::plotRGB(gw_raster)
gg_gw <- RStoolbox::ggRGB(gw_raster,r=1,g=2,b=3) +
  xlim(15,310) + ylim(20,390) +
  theme(axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.border = element_blank())

pdf(file.path("vignettes","images","groundwater_agency.pdf"),width=6.5,height=3)
gg_gw + p_density / p_ecdf +
  patchwork::plot_annotation(tag_levels = "a")
dev.off()

png(file.path("vignettes","images","groundwater_agency.png"),width=600,height=300)
gg_gw + p_density / p_ecdf +
  patchwork::plot_annotation(tag_levels = "a")
dev.off()

# for (i in 1:1000)
# randomly generate Ksat, z0, n
# set aquifer scenario
# set R & filter wells
# calculate hydraulic head wells


params_list <- params_df[1,]

get_well_heads <- function(loc,well_images,aquifer,params_list,t_ROI_years = 10) {
  aquifer_random <- aquifer
  aquifer_random$Ksat <- params_list$Ksat
  aquifer_random$z0 <- params_list$z0
  aquifer_random$n <- params_list$n
  well_images_random <- well_images %>%
    dplyr::mutate(R = params_list$R) %>%
    dplyr::filter(R > dist)
  head_wi_pumping <- get_hydraulic_head(loc,well_images_random,aquifer_random)
  # well_images_random_no_pumping <- well_images_random
  # well_images_random_no_pumping$Q[well_images_random_no_pumping$orig_wID %in% wID_target] <- 0
  # head_no_pumping <- get_hydraulic_head(loc,well_images_random_no_pumping,aquifer_random)
  out_df <- tidyr::crossing(i=params_list$i,tidyr::nesting(wID=loc$wID,head=head_wi_pumping))
  return(out_df)
}

# set aquifer scenario
# set R & filter wells
# calculate hydraulic head wells


head <-

probability of drawdown for given pumping rate
probability of exceeding 5% threshold vs pumping rate
