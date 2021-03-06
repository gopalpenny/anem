# anem_assess.R

#' Get segment sequence
#'
#' Get segment sequence
#' @param segment A list containing x1, y1, x2, y2, and sID
#' @keywords internal
#' @examples
#' \dontrun{
#' get_segment_seq(list(sID=1,x1=0,y1=0,x2=1,y2=1),length.out=5)
#' }
get_segment_seq <- function(segment,length.out=10) {
  segment_seq <- tibble::tibble(
    x = seq(segment$x1,segment$x2,length.out = length.out),
    y = seq(segment$y1,segment$y2,length.out = length.out),
    sID = segment$sID
  )
  segment_length <- sqrt((segment$x1-segment$x2)^2+(segment$y1-segment$y2)^2)
  segment_seq <- segment_seq %>%
    dplyr::mutate(dist=sqrt((x-segment$x1)^2+(y-segment$y1)^2),
           dist_rel=dist/segment_length)
  return(segment_seq)
}

#' Get hydraulic behavior along segments
#'
#' Get hydraulic head and flow on segments
#' @param segments Either: (a) A data.frame containing sID, x1, x2, y1, y2, or (2) a vector containing c(x1,y1,x2,y2)
#' @param length.out The number of points to evaluate on each segment
#' @inheritParams get_hydraulic_head
#' @return Returns a \code{data.frame}, with \code{length.out} rows for each sID.
#' Each row represents a point along sID and contains the head and flow as:
#'  dx = -Ksat * dh/dx, and
#'  dy = -Ksat * dh/dy
#' @importFrom magrittr %>%
#' @export
#' @examples
#' # define the aquifer
#' aquifer <- aquifer_confined_example
#' aquifer$Ksat <- 5e-4
#'
#' # define wells and images
#' wells_actual <- define_wells(data.frame(x=c(400,500,900),y=500,R=1500,Q=-0.1,diam=1))
#' wells <- generate_image_wells(wells_actual,aquifer)
#'
#' # set the segment
#' seg <- c(0,500,1000,500)
#'
#' # get segment for each well and all wells
#' seg_w1 <- get_segments_behavior(seg,wells[wells$orig_wID==1,],aquifer,1000)
#' seg_w1 <- data.frame(seg_w1[,c("x","y","head")],Wells="W1")
#' seg_w2 <- get_segments_behavior(seg,wells[wells$orig_wID==2,],aquifer,1000)
#' seg_w2 <- data.frame(seg_w2[,c("x","y","head")],Wells="W2")
#' seg_w3 <- get_segments_behavior(seg,wells[wells$orig_wID==3,],aquifer,1000)
#' seg_w3 <- data.frame(seg_w3[,c("x","y","head")],Wells="W3")
#' seg_all <- get_segments_behavior(seg,wells,aquifer,1000)
#' seg_all <- data.frame(seg_all[,c("x","y","head")],Wells="All")
#'
#' # plot results
#' library(ggplot2)
#' seg_behavior <- rbind(seg_all,seg_w1,seg_w2,seg_w3)
#' ggplot() +
#'   geom_line(data=seg_behavior,aes(x,head,linetype=Wells)) +
#'   geom_vline(data=data.frame(x=c(0,1000)),aes(xintercept=x),color="gray") +
#'   annotate(geom="text",x=40,y=90,label="Constant head boundary",angle=-90) +
#'   annotate(geom="text",x=1040,y=93,label="No flow boundary",angle=-90)
#'
#' # Example 2
#' segments <- data.frame(x1=c(0,300),y1=c(500,0),x2=c(1000,1000),y2=c(500,800),sID=1:2)
#' seg_results <- get_segments_behavior(segments,wells,aquifer,1000)
#' seg_results$sID <- as.factor(seg_results$sID)
#' p_domain <- ggplot() +
#'   scale_fill_gradient2(low="blue",high="red",mid="gray")+
#'   geom_segment(data=aquifer$bounds,aes(x1,y1,xend=x2,yend=y2,linetype=bound_type),color="black") +
#'   geom_path(data=seg_results,aes(x,y,color=sID)) +
#'   geom_point(data=wells[wells$well_image=="Actual",],aes(x,y),color="black",size=2,shape=21) +
#'   coord_equal()
#' p_segments <- ggplot(seg_results) +
#'   geom_line(aes(dist,head,color=sID)) + facet_wrap("sID",scales="free")
#' gridExtra::grid.arrange(p_domain,p_segments,nrow=2)
get_segments_behavior <- function(segments,wells,aquifer,length.out=100) {

  if (any(grep("data.frame",class(segments)))) {
    if (!any(names(segments)=="sID")) {
      segments$sID <- 1:nrow(segments)
    }
  } else if (length(segments)==4) {
    segments <- data.frame(sID=1,x1=segments[1],y1=segments[2],x2=segments[3],y2=segments[4])
  } else {
    stop("segments must be a data.frame or vector with length 4.")
  }
  if (!any(grep("x_unit_norm",names(segments))) | !any(grep("y_unit_norm",names(segments)))) {
    segments <- segments %>%
      dplyr::mutate(m=get_slope_intercept(x1,y1,x2,y2)$m,
                    x_unit_norm=get_unit_norm(m,"x"),
                    y_unit_norm=get_unit_norm(m,"y"))
    }
  segments_seq_list <- lapply(split(segments,segments$sID),get_segment_seq,length.out=length.out)

  segments_seq <- do.call(rbind,segments_seq_list) %>%
    dplyr::left_join(segments %>% dplyr::select(sID,x_unit_norm,y_unit_norm),by="sID")

  head <- get_hydraulic_head(segments_seq,wells,aquifer)
  flowdir <- get_flow_direction(segments_seq,wells,aquifer)

  bounds_seq <- segments_seq %>%
    dplyr::mutate(head=head,
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
#' @param length.out The number of points to evaluate on each boundary
#' @return Returns a \code{data.frame}, with \code{length.out} rows for each bID.
#' Each row represents a point along bID and contains the head and flow as:
#'  dx = -Ksat * dh/dx, and
#'  dy = -Ksat * dh/dy
#' @importFrom magrittr %>%
#' @export
#' @examples
#' # Example 1
#' wells <- define_wells(x=c(50,5),y=c(25,2.5),Q=c(0.5,-0.2),diam=c(0.05,0.08),R=100)
#' bounds_df <- data.frame(bound_type=c("CH","NF","NF","NF"),
#'                  m=c(0.8,-1.25,0.8,-1.25),b=c(3,100,-25,1),
#'                  bID=as.numeric(1:4))
#' aquifer <- define_aquifer("unconfined",1e-4,h0=100,bounds=bounds_df)
#' well_images <- generate_image_wells(wells,aquifer)
#'
#' library(ggplot2)
#' ggplot() +
#'   geom_point(data=well_images,aes(x,y,fill=Q),color="black",size=2,shape=21) +
#'   scale_fill_gradient2(low="blue",high="red",mid="gray")+
#'   geom_segment(data=aquifer$bounds,aes(x1,y1,xend=x2,yend=y2,
#'     linetype=bound_type,color=as.factor(bID))) + coord_equal()
#' bound_behavior_im <- get_bounds_behavior(well_images,aquifer)
#' bound_behavior_im$im <- "images"
#' bound_behavior_no_im <- get_bounds_behavior(wells,aquifer)
#' bound_behavior_no_im$im <- "no_images"
#'
#' # Example 2
#' bounds_df <- data.frame(bound_type=c("CH","NF","NF","NF"),m=c(Inf,0,Inf,0),b=c(0,1000,1000,0))
#' aquifer_unconfined <- define_aquifer("unconfined",1e-3,bounds=bounds_df,h0=100)
#'
#' library(dplyr)
#' set.seed(30)
#' wells_df <- data.frame(x=runif(8,0,1000),y=runif(8,0,1000),diam=1,R=1000) %>%
#'   mutate(country=factor(y>500,levels=c(FALSE,TRUE),labels=c("A","B"))) %>%
#'   group_by(country) %>%
#'   mutate(weights=1,Q=-1/n()) %>% group_by()
#' wells_actual <- define_wells(wells_df)
#' wells <- wells_actual %>% generate_image_wells(aquifer_unconfined)
#' bound_behavior_no_im <- get_bounds_behavior(wells_actual,aquifer_unconfined) %>%
#'   dplyr::mutate(im="unbounded")
#' bound_behavior_im <- get_bounds_behavior(wells,aquifer_unconfined) %>%
#'   dplyr::mutate(im="bounded")
#' bounds_behavior_summary <- bound_behavior_no_im %>% bind_rows(bound_behavior_im) %>%
#'   group_by(bID,bound_type,im) %>%
#'   summarize(`Head`=mean(head),
#'             `Flow`=mean(abs(flow_normal)))
get_bounds_behavior <- function(wells,aquifer,length.out=100) {
  if (max(grepl("aquifer",class(aquifer)))) {
    bounds <- aquifer$bounds
  } else {
    bounds <- aquifer
  }
  bcheck <- check_bounds(bounds)

  bounds_wide <- bounds %>%
    dplyr::left_join(get_quad_vertices_wide(bounds),by="bID") %>%
    dplyr::mutate(x_unit_norm=get_unit_norm(m,"x"),
                  y_unit_norm=get_unit_norm(m,"y"))

  bounds_seq <- get_segments_behavior(bounds_wide %>% dplyr::rename(sID=bID),
                                      wells,aquifer=aquifer,length.out=length.out) %>%
    dplyr::rename(bID=sID) %>%
    dplyr::left_join(bounds %>% dplyr::select(bID,bound_type),by="bID")

  return(bounds_seq)
}

get_unit_norm <- function(m,axis) {
  x_unit_norm <- dplyr::case_when(
    m==Inf~1,
    TRUE~sqrt(m^2/(1+m^2))
  )
  y_unit_norm <- dplyr::case_when(
    m==Inf~0,
    m==0~1,
    TRUE~-sign(m)*sqrt(1/(1+m^2))
  )
  if (axis=="x") {
    return(x_unit_norm)
  } else if (axis=="y") {
    return(y_unit_norm)
  } else {
    stop("specify axis as x or y")
  }
}

#' Plot behavior on boundaries
#'
#' Plot behavior on boundaries with and without well images
#' @inheritParams get_bounds_behavior
#' @return Two ggplot objects that show behavior on the boundaries, one for head and one for flow
#' @importFrom magrittr %>%
#' @export
#' @examples
#' aquifer <- aquifer_confined_example
#' wells <- generate_image_wells(define_wells(wells_example), aquifer)
#' gg_list <- plot_bounds_behavior(wells,aquifer,length.out=20)
#'
#' library(ggplot2)
#' p_domain <- ggplot() +
#'   geom_point(data=wells,aes(x,y,fill=Q),color="black",size=2,shape=21) +
#'   scale_fill_gradient2(low="blue",high="red",mid="gray") +
#'   geom_segment(data=aquifer$bounds,aes(x1,y1,xend=x2,yend=y2,linetype=bound_type)) +
#'   coord_equal()
#' gridExtra::grid.arrange(p_domain,gg_list$p_h,gg_list$p_f,nrow=1)
#' gg_list$table
#' gg_list$bounds_behavior
plot_bounds_behavior <- function(wells,aquifer,length.out=100) {
  dist_rel <- head <- flow_normal <- NULL

  cAquifer <- check_aquifer(aquifer)

  if (max(grepl("sf",class(aquifer$bounds)))) {
    aquifer$bounds <- aquifer$bounds %>% sf::st_set_geometry(NULL)
  }

  wells_noimages <- wells %>% dplyr::filter(wID==orig_wID)

  bounds_behavior_noim <- get_bounds_behavior(wells_noimages,aquifer,length.out=length.out)
  bounds_behavior_noim$im <- "no images"

  bounds_behavior_wim <- get_bounds_behavior(wells,aquifer,length.out=length.out)
  bounds_behavior_wim$im="images"

  bounds_BT <- aquifer$bounds
  bounds_BT$BT <- paste(bounds_BT$bID,bounds_BT$bound_type)
  bounds_behavior <- dplyr::bind_rows(bounds_behavior_noim,bounds_behavior_wim) %>%
    dplyr::left_join(bounds_BT[,c("bID","BT")],by="bID")
  bounds_behavior$im=factor(bounds_behavior$im,levels=c("no images","images"))

  p_h <- ggplot2::ggplot(bounds_behavior) + ggplot2::geom_line(ggplot2::aes(dist_rel,head)) +
    ggplot2::facet_grid(BT~im,scales="free_x") + ggplot2::theme(axis.text.x=ggplot2::element_blank()) + ggplot2::ggtitle("Head along boundary")
  p_f <- ggplot2::ggplot(bounds_behavior) + ggplot2::geom_line(ggplot2::aes(dist_rel,flow_normal)) +
    ggplot2::facet_grid(BT~im,scales="free_x") + ggplot2::theme(axis.text.x=ggplot2::element_blank()) + ggplot2::ggtitle("Flow normal to boundary")

  # summary of bounds behavior
  bounds_behavior_summary <- bounds_behavior %>%
    dplyr::group_by(bID,bound_type,im) %>%
    dplyr::summarize(`Mean head`=mean(head),
              `Mean flow`=mean(abs(flow_normal)))
  bounds_behavior_table <- bounds_behavior_summary %>% dplyr::select(-dplyr::ends_with("_sd")) %>%
    tidyr::gather(var,val,`Mean head`,`Mean flow`) %>%
    tidyr::unite(bound,bID,bound_type,sep=" ") %>% tidyr::unite(var,var,im,sep=", ") %>%
    tidyr::spread(var,val)

  return(list(p_h=p_h,p_f=p_f,table=bounds_behavior_table,bounds_behavior=bounds_behavior))
}

#' Check aquifer properties
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' aquifer <- define_aquifer("confined",1e-4)
#' check_aquifer(aquifer)
#'
#' aquifer2 <- define_aquifer("unconfined",1e-4)
#' check_aquifer(aquifer2,c("bounds","Ksat"))
#' check_aquifer(aquifer2,c("Ksat"))
#' }
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
#' @keywords internal
#' @examples
#' \dontrun{
#' check_bounds(tibble(m=c(1,1,-1,-1)))
#' check_bounds(tibble(m=c(1,0.9,-1,-1)))
#' }
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

#' Check rectangle
#'
#' Check rectangle -- ensure 4 edges connecting 3 vertices and rectilinear
#' @param rect rectangle
#' @keywords internal
check_rectangle <- function(rect) {
  return(NULL)
}

#' Check wells
#'
#' Check that wells have standard columns and that specified columns have no NA values
#' @keywords internal
#' @examples
#' \dontrun{
#' wells_df <- data.frame(R=1:10,Q=NA,x=10:1,y=1:10,image=NA,diam=NA)
#' wells <- define_wells(wells_df)
#' check_wells(wells)
#' check_wells(wells,c("R"))
#' check_wells(wells,c("R","Q","well_type"))
#' }
check_wells <- function(wells,columns=NULL) {
  standard_names <- c("wID","Q","R","diam","x","y","well_type","well_image")
  missing_columns <- standard_names[!(standard_names %in% names(wells))]
  cWells <- "Good"

  if (length(missing_columns) > 0) {
    cWells <- "Warning"
    warning("Wells missing fields: ",paste(missing_columns,collapse=", "))
  }

  if (!is.null(columns)) {
    wells_NA_check <- lapply(wells[,columns],function(x) (max(is.na(x))==1)) %>% as.data.frame() %>% tidyr::gather(var,has_NA)

    if (max(wells_NA_check$has_NA)) {
      columns_with_NA <- wells_NA_check$var[wells_NA_check$has_NA]
      cWells <- "Warning"
      warning("Wells columns (",paste(columns_with_NA,collapse=", "),") contain NA values.")
    }
  }

  return(cWells)
}
