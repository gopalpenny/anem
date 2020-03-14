
library(magick)
library(anem)

# function for getting well heads given paramers
get_well_heads <- function(loc,well_images,aquifer,params_list) {
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

# Import data from the app
aa <- import_app_rds("shiny-app/example_scenarios/groundwater_agency_scenario2.rds",gen_well_images = FALSE)

wells <- aa$wells %>% sf::st_set_geometry(NULL)

aquifer <- aa$aquifer
aquifer$recharge <- NULL
aquifer$bounds <- aquifer$bounds %>% sf::st_set_geometry(NULL)

get_hydraulic_head(wells %>% dplyr::filter(well_type=="Actual"),wells,aquifer)
# Set uniform distributions
Ksat_range <- c(1e-5,2e-4)
z0_range <- c(15,25)
n_range <- c(0.3,0.4)
t_ROI_years <- 10
t_roi <- 3600*24*365*t_ROI_years

# Get max ROI
R_max <- get_ROI(method="cooper-jacob",Tr=max(Ksat_range)*max(z0_range),t=t_roi,S=min(n_range))
wID_target <- 17
well_images <- wells %>%
  dplyr::filter(wID %in% wID_target) %>%
  dplyr::mutate(R = R_max,
                Q = 1) %>% # set Q = 1 for unit drawdown
  generate_image_wells(aa$aquifer,include_image_columns = T)
loc <- wells %>% dplyr::filter(well_image=="Actual") %>% dplyr::select(x,y,wID)

# get random parameter data.frame (N=1000)
set.seed(200)
N <- 1000
params_df <- tibble::tibble(Ksat=runif(N,min=Ksat_range[1],max=Ksat_range[2]),
                            z0=runif(N,min=z0_range[1],max=z0_range[2]),
                            n=runif(N,min=n_range[1],max=n_range[2])) %>%
  dplyr::mutate(R=get_ROI(method="cooper-jacob",Tr=Ksat*z0,t=t_roi,S=n),
                i = dplyr::row_number())

# get change in head at other wells due to unit injection (Q=1) at the new well
well_head_random <- lapply(split(params_df,1:nrow(params_df)),get_well_heads,loc=loc,well_images=well_images,aquifer=aquifer) %>%
  dplyr::bind_rows() %>%
  dplyr::filter(wID != 17) %>%
  dplyr::mutate(unit_change_in_head_m=head-aquifer$h0)

ggplot(well_head_random) + geom_histogram(aes(unit_change_in_head_m),bins=100)
#
well_head_drawdown <- well_head_random  %>%
  dplyr::group_by(i) %>%
  dplyr::summarize(drawdown_unit=max(unit_change_in_head_m)) %>%
  # dplyr::mutate(drawdown_unit=50-head_unit) %>%
  tidyr::crossing(Q=seq(-0.002,-0.02,by=-0.001)) %>%
  # tidyr::crossing(Q=seq(-0.1,-0.5,by=-0.01)) %>%
  dplyr::mutate(change_in_head = drawdown_unit * Q,
                head=aquifer$h0+change_in_head,
                selected=Q %in% seq(-0.004,-0.016,by=-0.004),
                Q_name=factor(paste0("Q = ",Q*1e3),
                              levels=paste0("Q = ",sort(unique(Q),decreasing = TRUE)*1e3))) %>%
  dplyr::group_by(Q,Q_name) %>%
  dplyr::arrange(dplyr::desc(head)) %>%
  dplyr::mutate(pct_reduction=(aquifer$h0-head)/aquifer$h0*100,
                ecdf=dplyr::row_number()/dplyr::n()) %>%
  dplyr::group_by()

q_colors <- colorspace::heat_hcl(4,h=c(90,320),l=c(80,40),c=c(100,70))
# colorspace::swatchplot(q_colors)
drawdown_quantiles <- well_head_drawdown %>%
  dplyr::group_by(Q,Q_name,selected) %>%
  dplyr::summarize(median=quantile(pct_reduction,0.5))
drawdown_percentiles <- well_head_drawdown %>%
  dplyr::group_by(Q,Q_name,selected) %>%
  dplyr::summarize(pct5=sum((pct_reduction < 5)*1)/dplyr::n())
p_density <- ggplot() +
  geom_density(data=well_head_drawdown %>% dplyr::filter(selected),aes(pct_reduction,color=Q_name),size=0.5) +
  coord_cartesian(xlim=c(0,15)) +
  scale_color_manual("Pumping\nrate (l/s)",values = q_colors) +
  labs(x="Percent reduction",y="Probability density",subtitle = "(b) Uncertainty analysis (R anem)") +
  theme_bw() %+replace% theme(panel.grid = element_blank(),
                              # axis.title.x=element_blank(),
                              legend.position = c(0.7,0.65))

# gg_gw + p_density + p_ecdf +
#   patchwork::plot_annotation(tag_levels = "a")

p_ecdf <- ggplot() +
  geom_line(data=well_head_drawdown %>% dplyr::filter(selected),aes(pct_reduction,ecdf,color=Q_name),size=0.5) +
  geom_segment(data=drawdown_percentiles %>% dplyr::filter(selected),aes(x=0,xend=5,y=pct5,yend=pct5,color=Q_name),linetype="dashed",size=0.5) +
  geom_point(data=drawdown_percentiles %>% dplyr::filter(selected),aes(x=5,y=pct5,color=Q_name),show.legend = FALSE,size=2) +
  geom_point(data=drawdown_percentiles %>% dplyr::filter(selected),aes(x=0,y=pct5,color=Q_name),show.legend = FALSE,size=2) +
  scale_color_manual("Pumping rate",values=q_colors) +
  xlab("Percent reduction") + ylab("ECDF") +
  coord_cartesian(xlim=c(0,15)) +
  labs(subtitle = "(c) Prob compliance (R anem)") +
  theme_bw() %+replace% theme(panel.grid = element_blank(),
                              # legend.margin = margin(1,0,1,0,unit="cm"),
                              legend.position="none",
                              legend.justification = c(0,1))

gw_image <- image_read("shiny-app/screenshots/groundwater_agency_application2.png")
tiff_file <- tempfile()
image_write(gw_image,path=tiff_file,format="tiff")
gw_raster <- raster::brick(tiff_file)
# p <- raster::plotRGB(gw_raster)
gg_gw <- RStoolbox::ggRGB(gw_raster,r=1,g=2,b=3) +
  coord_equal(xlim=round(gw_raster@ncols*c(0.047,0.953)),ylim=round(gw_raster@nrows*c(0.047,0.953))) +
  # xlim(15,310) + ylim(20,390) +
  labs(subtitle = "(a) Current situation (anem-app)") +
  theme(axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill=NA,color=NA))

pdf(file.path("vignettes","images","groundwater_agency_orig.pdf"),width=8.8,height=3)
# gg_gw + p_density / p_ecdf + patchwork::plot_annotation(tag_levels = "a")
ggpubr::ggarrange(gg_gw, p_density, p_ecdf, nrow=1, widths=c(0.34,0.33,0.33))
dev.off()

png(file.path("vignettes","images","groundwater_agency_orig.png"),width=600,height=300)
gg_gw + p_density / p_ecdf +
  patchwork::plot_annotation(tag_levels = "a")
dev.off()

# for (i in 1:1000)
# randomly generate Ksat, z0, n
# set aquifer scenario
# set R & filter wells
# calculate hydraulic head wells



# set aquifer scenario
# set R & filter wells
# calculate hydraulic head wells
