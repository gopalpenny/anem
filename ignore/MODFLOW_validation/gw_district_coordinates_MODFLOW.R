# This script is used to load and transform well coordinates for the Groundwater district scenario
library(tidyverse)
mod_path <- "ignore/MODFLOW_validation"

# functions:
get_intersection <- function(m1,b1,m2,b2) {
  x <- ifelse(m1==m2,NaN,
              ifelse(m1==Inf, b1,
                     ifelse(m2==Inf, b2,
                            (b2-b1)/(m1-m2))))
  y <- ifelse(m1==m2,NaN,
              ifelse(m1==Inf, m2*x + b2,
                     ifelse(m2==Inf, m1*x + b1,
                            m2*x + b2)))
  intersections <- data.frame(x=x,y=y)
  return(intersections)
}
get_nearest_point_on_line <- function(loc,m,b) {
  if (max(grepl("data.frame",class(loc)))) {
    x_loc <- loc$x
    y_loc <- loc$y
  } else {
    x_loc <- loc[1]
    y_loc <- loc[2]
  }
  if (!(length(m) %in% c(1,length(x_loc))) | !(length(b) %in%  c(1,length(x_loc)))) {
    stop("get_nearest_point_on_line: m has length ",length(m),
         ", b has length",length(b),". The length of each should be 1 or the number of points.")
  }
  if (length(m) == 1) {
    m <- rep(m,length(x_loc))
  } else if (length(m)!=length(x_loc)) {
    stop("get_nearest_point_on_line: m has length ",length(m),
         ", b has length",length(b),". The length of each should be 1 or the number of points.")
  }
  if (length(b) == 1) {
    b <- rep(b,length(x_loc))
  } else if (length(b)!=length(x_loc)) {
    stop("get_nearest_point_on_line: m has length ",length(m),
         ", b has length",length(b),". The length of each should be 1 or the number of points.")
  }

  x <- ifelse(m==Inf,b,
              ifelse(m==0, x_loc,
                     get_intersection(m,b,-1/m, y_loc  + x_loc/m)$x))
  y <- ifelse(m==Inf,y_loc,
              ifelse(m==0, b,
                     get_intersection(m,b,-1/m, y_loc + x_loc/m)$y))
  nearest_point <- data.frame(x=x,y=y)
  return(nearest_point)
}

# code:
gw_anem <- anem::import_app_rds(file.path(mod_path,"/gw_district_download.rds"))
# identical(anem::groundwater_district$aquifer,gw_anem$aquifer)

aquifer <- gw_anem$aquifer
aq_bounds <- gw_anem$aquifer$bounds %>% sf::st_set_geometry(NULL) %>% filter(bound_type!="PB")
wells <- gw_anem$wells %>% filter(well_image=="Actual") %>% select(wID,Q,R,diam,x,y) %>% sf::st_set_geometry(NULL)

wells_transformed <- wells %>% rename(x_well=x,y_well=y) %>% bind_cols(get_nearest_point_on_line(wells,aq_bounds$m[1],aq_bounds$b[1])) %>%
  mutate(y_new=sqrt((x-x_well)^2+(y-y_well)^2)) %>% select(-x,-y) %>% bind_cols(get_nearest_point_on_line(wells,aq_bounds$m[2],aq_bounds$b[2])) %>%
  mutate(x_new=-sqrt((x-x_well)^2+(y-y_well)^2)) %>% select(-x,-y)

ggplot(wells_transformed) +
  geom_point(aes(x_new,y_new)) +
  geom_point(aes(x_well-403015,y_well-3488056),color="blue") +
  geom_segment(data=aquifer$bounds,aes(x1,y1,xend=x2,yend=y2,color=bound_type)) +
  coord_equal()

bounds_transformed_df <- tibble(m=c(0,Inf,0,Inf),b=c(0,0,1000,800),bound_type=c("CH","PB","PB","NF"))
aquifer$bounds <- define_bounds(bounds_transformed_df)
aquifer$recharge <- NULL

wells_new <- wells_transformed %>% mutate(x = x_new + 800) %>% select(wID, Q, R, diam, x, y=y_new) %>%
  define_wells() %>% generate_image_wells(aquifer)

ggplot(wells_new) +
  geom_point(aes(x,y)) +
  geom_segment(data=aquifer$bounds,aes(x1,y1,xend=x2,yend=y2,color=bound_type)) +
  coord_equal()

modflow <- readxl::read_excel(file.path(mod_path,"modflow_model_results.xlsx"), col_names = c("id1","id2","layer","yg","xg","head")) %>%
  mutate(y= (1000 - yg) * 10, x = (xg - 920) * 10) %>%
  select(x,y,MODFLOW=head) %>% filter(x > 0, y < 1000)
model <- bind_cols(modflow,
  anem=get_hydraulic_head(modflow,wells=wells_new, aquifer = aquifer)) %>%
  mutate(error = MODFLOW - anem,
         mean_head = mean(anem))

NSE <- model %>% summarize(SSE = sum(error^2),
                           VAR = sum((anem - mean_head)^2)) %>%
  mutate(NSE = 1 - (SSE/VAR))
contours_MODFLOW <- model %>% rename(z=MODFLOW) %>%
  anem::get_contourlines(levels=seq(44,50,by=0.5))
contours_anem <- model %>% rename(z=anem) %>%
  anem::get_contourlines(levels=seq(44,50,by=0.5))

x_transects <- data.frame(y = c(250, 500, 750), id = c("c","b","a"), transect="x transect")
y_transects <- data.frame(x = c(400, 600), id = c("d","e"), transect="y transect")

p_head <- ggplot(model) +
  geom_raster(aes(x,y,fill=anem)) +
  scale_fill_viridis_c("Head, m") +
  geom_path(data=contours_MODFLOW,aes(x, y, group=line, linetype= "MODFLOW"),color="black") +
  geom_path(data=contours_anem,aes(x, y, group=line, linetype = "anem"),color="black") +
  geom_point(data=wells_new %>% filter(well_image=="Actual"),aes(x,y)) +
  geom_segment(data=x_transects,aes(0, y, xend=800, yend = y, color = as.factor(id)))+
  geom_segment(data=y_transects,aes(x, 0, xend=x, yend = 1000, color = as.factor(id)))+
  geom_text(data=x_transects,aes(750, y+20, label=id), color="white", vjust=0)+
  geom_text(data=y_transects,aes(x+20, 950, label=id), color="white", hjust=0)+
  scale_color_manual(values=rep("white",5),guide=FALSE) +
  scale_linetype_discrete("Model\ncontours") +
  guides(fill=guide_colorbar(barheight = unit(1,"in"))) +
  coord_equal() + ggp::t_manu()
p_head

p_error <- ggplot(model) +
  geom_contour_filled(aes(x,y,z=abs(error))) +
  coord_equal()
#
# p_head + p_error

df_plot <- bind_rows(inner_join(model, x_transects, by="y"),
                     inner_join(model, y_transects, by="x"))  %>%
  mutate(path=if_else(transect=="x transect",x/800,y/1000)) %>%
  pivot_longer(c("MODFLOW","anem"))
p_transects <- ggplot(df_plot) +
  geom_line(aes(path,value,color=id,linetype=name,group=interaction(id,name))) +
  labs(y="Head, m",x="Distance") +
  ggsci::scale_color_npg(palette="nrc",name="Transect") +
  scale_linetype("Model", guide=FALSE) +
  ggp::t_manu()
  # facet_wrap(~transect)
p_transects

# p_y

p_validation <- p_head + p_transects + patchwork::plot_annotation(tag_levels = "a")

ggsave("modflow_validation.pdf",p_validation,path="~/Projects/CNH/anem",width=8,height=3.5)
