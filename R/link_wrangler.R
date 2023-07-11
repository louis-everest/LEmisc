link_wrangler <- function(data, node,id,x,y,subset.col){
  data <- data[order(data[[id]], data[[x]]),]

  data_out <- data.frame("source"=0,"target"=0,"value" =0)
  pts <- unique(data[[id]])
  for(i in 1:length(pts)){
    pt <- pts[i]
    pt_temp <- data[which(pt ==data[[id]]),]
    max <- nrow(pt_temp)
    for(j in 1:max){
      if(j ==1){
        data_temp <- data.frame("source"= pt_temp[j,subset.col],
                                "target"=pt_temp$node_name[j],"value" =1)
        names(data_temp) <- c("source", "target", "value")
        data_out <- rbind(data_out, data_temp)
      }
      if(max == 1){
        data_temp <- data.frame("source"=pt_temp$node_name[j],
                                "target"=NA,"value" =1)
      }else{
        data_temp <- data.frame("source"=pt_temp$node_name[j],
                                "target"= pt_temp$node_name[j+1],"value" =1)
      }
      data_out <- rbind(data_out, data_temp)
      max <- max -1
    }}
  data_out <- data_out[-1,]
  data_out <- data_out %>%
    group_by(source, target) %>%
    summarise(value = n())
  data_out <- data_out[!is.na(data_out$target),]
  data_out$IDsource <- match(data_out$source, node[[1]])-1
  data_out$IDtarget <- match(data_out$target, node[[1]])-1
  return(as.data.frame(data_out))
}
