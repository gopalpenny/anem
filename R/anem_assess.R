# anem_assess.R


get_segment_seq <- function(segment,length.out=10) {
  segment_seq <- tibble::tibble(
    x = seq(segment$x_1,segment$x_2,length.out = length.out),
    y = seq(segment$y_1,segment$y_2,length.out = length.out),
    sID = segment$sID
  )
  segment_length <- sqrt((segment$x_1-segment$x_2)^2+(segment$y_1-segment$y_2)^2)
  segment_seq <- segment_seq %>%
    mutate(dist=sqrt((x-segment$x_1)^2+(y-segment$y_1)^2),
           dist_rel=dist/segment_length)
  return(segment_seq)
}

#' Get hydraulic behavior along segments
#'
#' Get hydraulic head and flow on segments
#' @param segments A data.frame containing, m, b, sID, x_1, x_2, y_1, y_2
#' @param length.out The number of points to check on each segment
#' @inheritParams get_hydraulic_head
#' @return Returns a \code{data.frame}, with \code{length.out} rows for each bID.
#' Each row represents a point along sID and contains the head and flow as:
#'  dx = -Ksat * dh/dx, and
#'  dy = -Ksat * dh/dy
#' @importFrom magrittr %>%
#' @export
#' @examples
#' wells <- define_wells(x=c(50,10),y=c(25,2.5),Q=c(0.5,-0.2),diam=c(0.05,0.08)) %>%
#'   mutate(R=get_ROI(Ksat=0.0000005,h=10,t=630720000,n=0.5,method="aravin-numerov"))  # 630720000 - 20 years
#' bounds_df <- tibble(bound_type=c("CH","NF","NF","NF"), m=c(0.5,-2,0.5,-2),b=c(0,100,20,50), bID=as.numeric(1:4))
#' aquifer <- define_aquifer("unconfined",1e-4,bounds=bounds_df,h0=100)
#' well_images <- generate_image_wells(wells,bounds,num_levels=2) %>% filter(max_mirror_dist<R)
#' segments <- bounds[1:2,] %>% rename(sID=bID) %>% mutate(x_1=c(0,50),x_2=c(50,75),y_1=c(0,50),y_2=c(50,25)) %>%
#'   dplyr::mutate(x_unit_norm=sqrt(m^2/(1+m^2)),y_unit_norm=-sign(m)*sqrt(1/(1+m^2)))
#' segments_behavior <- get_segments_behavior(well_images,segments,aquifer) %>% as_tibble()
#' p_domain <- ggplot() +
#'   geom_point(data=well_images,aes(x0,y0,fill=rate),color="black",size=2,shape=21) +
#'   scale_fill_gradient2(low="blue",high="red",mid="gray")+
#'   geom_abline(data=bounds,aes(slope=m,intercept=b,linetype=bound_type,color=as.factor(bID))) +
#'   geom_segment(data=segments,aes(x_1,y_1,xend=x_2,yend=y_2),linetype="dashed") +
#'   coord_equal()
#' p_segments <- ggplot(segments_behavior %>% gather(var,val,head,flow_normal)) +
#'   geom_line(aes(dist,val,color=as.factor(sID))) + facet_grid(var~sID,scales="free")
#' gridExtra::grid.arrange(p_domain,p_segments,nrow=1)
get_segments_behavior <- function(wells,segments,aquifer,length.out=100,eps=1e-10) {

  segments_seq_list <- lapply(split(segments,segments$sID),get_segment_seq,length.out=length.out)

  segments_seq <- do.call(rbind,segments_seq_list) %>%
    dplyr::left_join(segments %>% dplyr::select(sID,x_unit_norm,y_unit_norm),by="sID")

  head <- get_hydraulic_head(segments_seq,wells,aquifer)
  flowdir <- get_flowdir(segments_seq,wells,aquifer,eps=eps)

  bounds_seq <- segments_seq %>%
    mutate(head=head,
           flow_x=aquifer$Ksat*flowdir$dx,
           flow_y=aquifer$Ksat*flowdir$dy,
           flow_normal=flow_x*x_unit_norm+flow_y*y_unit_norm,
           flow_mag=abs(flow_normal))

  return(bounds_seq)
}

#' Get hydraulic behavior on bounds
#'
#' Get hydraulic head and flow on boundaries
#' @inheritParams get_hydraulic_head
#' @param length.out The number of points to check on each boundary
#' @return Returns a \code{data.frame}, with \code{length.out} rows for each bID.
#' Each row represents a point along bID and contains the head and flow as:
#'  dx = -Ksat * dh/dx, and
#'  dy = -Ksat * dh/dy
#' @importFrom magrittr %>%
#' @export
#' @examples
#' wells <- define_wells(x=c(50,5),y=c(25,2.5),Q=c(0.5,-0.2),diam=c(0.05,0.08)) %>% as_tibble() %>%
#'   mutate(R=get_ROI(Ksat=0.00005,h=100,t=630720000,n=0.5,method="aravin-numerov"))  # 630720000 - 20 years
#' bounds_df <- tibble(bound_type=c("CH","NF","NF","NF"),
#'                  m=c(0.8,-1.25,0.8,-1.25),b=c(3,100,-25,1),
#'                  bID=as.numeric(1:4))
#' aquifer <- define_aquifer("unconfined",1e-4,bounds=bounds_df)
#' well_images <- generate_image_wells(wells,bounds,num_levels=6) %>% filter(max_mirror_dist<R)
#' ggplot() +
#'   geom_point(data=well_images,aes(x,y,fill=Q),color="black",size=2,shape=21) +
#'   scale_fill_gradient2(low="blue",high="red",mid="gray")+
#'   geom_abline(data=bounds,aes(slope=m,intercept=b,linetype=bound_type,color=as.factor(bID))) + coord_equal()
#' bound_behavior <- get_bounds_behavior(well_images,aquifer) %>%
#'   dplyr::mutate(im="images")
#' gg_list <- plot_bounds_behavior(wells_noimages,well_images,bounds=bounds,h0=1000,Ksat=1,z0=100,aquifer_type="confined",length.out=50)
#' gg_list$p_h
#' gg_list$p_f
get_bounds_behavior <- function(wells,aquifer,length.out=100) {
  if (max(grepl("aquifer",class(aquifer)))) {
    bounds <- aquifer$bounds
  } else {
    bounds <- aquifer
  }
  bcheck <- check_bounds(bounds)

  bounds_wide <- bounds %>%
    dplyr::left_join(get_quad_vertices_wide(bounds),by="bID") %>%
    dplyr::mutate(x_unit_norm=sqrt(m^2/(1+m^2)),
                  y_unit_norm=-sign(m)*sqrt(1/(1+m^2)))

  bounds_seq <- get_segments_behavior(wells,bounds_wide %>% rename(sID=bID),
                                      aquifer=aquifer,length.out=length.out) %>%
    rename(bID=sID)

  return(bounds_seq)
}

#' Plot behavior on boundaries
#'
#' Plot behavior on boundaries with and without well images
#' @inheritParams get_bounds_behavior
#' @return Two ggplot objects that show behavior on the boundaries, one for head and one for flow
#' @importFrom magrittr %>%
#' @export
#' @examples
#' wells1 <- define_wells(x0=c(5),y0=c(5),rate=c(-.0001),diam=c(.1)) %>% as_tibble() %>%
#'   mutate(R=get_ROI(Ksat=0.00000005,h=100,t=6.3e8,n=0.5,method="aravin-numerov"))  # 630720000 - 20 years
#' bounds_df1 <- tibble(bound_type=c("CH","NF","NF","NF"),
#'                   m=c(1,-1,1,-1),b=c(10,20,-10,1),
#'                   bID=as.numeric(1:4),bGroup=c(2,1,2,1))
#' aquifer1 <- define_aquifer("unconfined",Ksat=1,bounds=bounds_df1)
#' well_images1 <- generate_image_wells(wells1,aquifer1,num_levels=20) %>%
#'   filter(max_mirror_dist<R)
#' p_domain <- ggplot() +
#'   geom_point(data=well_images1,aes(x0,y0,fill=rate),color="black",size=2,shape=21,alpha=0.5) +
#'   scale_fill_gradient2(low="blue",high="red",mid="gray") +
#'   geom_abline(data=bounds1,aes(slope=m,intercept=b,linetype=bound_type,color=as.factor(bID))) +
#'   coord_equal()
#' gg_list <- plot_bounds_behavior(wells1,well_images1,bounds=bounds1,
#'                                 h0=100,Ksat=0.00000005,aquifer_type="unconfined",length.out=20)
#' gridExtra::grid.arrange(p_domain,gg_list$p_h,gg_list$p_f,nrow=1)
#'
#'
#' wells2 <- data.frame(x0=c(2,8),y0=c(5,5),rate=c(-.0007,.0005),diam=c(1,1)) %>% as_tibble() %>%
#'   mutate(roi=get_ROI(Ksat=0.00000005,h=100,t=6.3e8,n=0.5,method="aravin-numerov"))  # 630720000 - 20 years
#' bounds2 <- tibble(bound_type=c("CH","NF","NF","NF"),
#'                   m=c(1,-1,1,-1),b=c(10,20,-10,1),
#'                   bID=as.numeric(1:4),bGroup=c(2,1,2,1))
#' well_images2 <- generate_image_wells(wells2,bounds2,num_levels=20) %>%
#'   filter(max_mirror_dist<roi)
#' p_domain <- ggplot() +
#'   geom_point(data=well_images2,aes(x0,y0,fill=rate),color="black",size=2,shape=21) +
#'   scale_fill_gradient2(low="blue",high="red",mid="gray") +
#'   geom_abline(data=bounds2,aes(slope=m,intercept=b,linetype=bound_type,color=as.factor(bID))) +
#'   coord_equal()
#' gg_list <- plot_bounds_behavior(wells2,well_images2,bounds=bounds2,
#'                                 h0=100,Ksat=0.00000005,aquifer_type="unconfined",length.out=20)
#' gridExtra::grid.arrange(p_domain,gg_list$p_h,gg_list$p_f,nrow=1)
plot_bounds_behavior <- function(well_images,aquifer,length.out=100) {
  cAquifer <- check_aquifer(aquifer)

  if (max(grepl("sf",class(aquifer$bounds)))) {
    aquifer$bounds <- aquifer$bounds %>% sf::st_set_geometry(NULL)
  }

  wells_noimages <- well_images %>% dplyr::filter(wID==origin)

  bounds_behavior_noim <- get_bounds_behavior(wells_noimages,aquifer,length.out=length.out) %>%
    dplyr::mutate(im="no_images")
  bounds_behavior_wim <- get_bounds_behavior(well_images,aquifer,length.out=length.out) %>%
    dplyr::mutate(im="images")
  bounds_behavior <- dplyr::bind_rows(bounds_behavior_noim,bounds_behavior_wim) %>%
    dplyr::left_join(aquifer$bounds %>% dplyr::mutate(BT=paste(bID,bound_type)) %>%
                       dplyr::select(bID,BT),by="bID") %>%
    dplyr::mutate(im=factor(im,levels=c("no_images","images")))
  p_h <- ggplot2::ggplot(bounds_behavior) + ggplot2::geom_line(aes(dist_rel,head)) +
    ggplot2::facet_grid(BT~im,scales="free_x") + ggplot2::theme(axis.text.x=element_blank()) + ggtitle("Head along boundary")
  p_f <- ggplot2::ggplot(bounds_behavior) + ggplot2::geom_line(aes(dist_rel,flow_normal)) +
    ggplot2::facet_grid(BT~im,scales="free_x") + ggplot2::theme(axis.text.x=element_blank()) + ggtitle("Flow normal to boundary")

  return(list(p_h=p_h,p_f=p_f,bounds_behavior=bounds_behavior))
}

#' Check aquifer properties
#'
#' @examples
#' aquifer <- define_aquifer("confined",1e-4)
#' check_aquifer(aquifer)
#'
#' aquifer2 <- define_aquifer("unconfined",1e-4)
#' check_aquifer(aquifer2,c("bounds","Ksat"))
#' check_aquifer(aquifer2,c("Ksat"))
check_aquifer <- function(aquifer,standard_columns=NULL) {
  cAquifer <- "Good"

  if (is.null(standard_columns)) {
    standard_columns <- c("aquifer_type","Ksat","h0","z0")
  }
  if (aquifer$aquifer_type == "unconfined") {
    standard_columns <- standard_columns[standard_columns!="z0"]
  }

  missing_columns <- standard_columns[!(standard_columns %in% names(aquifer))]

  if (length(missing_columns) > 0) {
    cAquifer <- "Warning"
    warning("Aquifer missing fields: ",paste(missing_columns,collapse=", "))
  }
  return(cAquifer)
}



#' Check boundaries
#'
#' Check that boundaries have slopes that are normal to each other
#' @examples
#' check_bounds(tibble(m=c(1,1,-1,-1)))
#' check_bounds(tibble(m=c(1,0.9,-1,-1)))
check_bounds <- function(bounds) {
  slopes <- bounds$m
  slopes_unique <- unique(slopes)
  warn <- "Good"
  if (length(unique(round(slopes,10))) > 2) {
    warn <- "Warning"
    warning("check_bounds(): More than two slopes have been specified: ",paste(slopes_unique,collapse=", "))
  }
  if ((abs(slopes_unique[1]) == Inf & slopes_unique[2] == 0) | (slopes_unique[1] == 0 & abs(slopes_unique[2]) == Inf)) {
  } else if (abs(slopes_unique[1] + 1/slopes_unique[2]) > 1e-10) {
    warn <- "Warning"
    warning("check_bounds(): Slopes are not normal to each other (tolerance 1e-10). Slopes: ",paste(slopes_unique,collapse=", "))
  }
  return(warn)
}
