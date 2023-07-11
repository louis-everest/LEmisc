auto_sankey <- function(data,id,x,y,subset.col=F, subset.variable = F, help =F,
                        font.size = 18, height = 1000, width = 800,
                        save.png.name = F, labels = "all", get.legend = F,
                        color.palette = "category20"){

  if(help ==T){
    Example_Data <- data.frame("id.example" = c("id_1","id_1","id_1","id_2","id_3","id_3"),
                               "y.example" = c("apples","apples","oranges","apples","oranges","oranges"),
                               "x.example" = c(1,2,3,1,1,2),
                               "subset.example" = c("class_a","class_a","class_a","class_b","class_b","class_b"))
    return(Example_Data)}
  # Collect all domain values
  all_domain <- unique(data[,y])
  #Clean inputs
  if(subset.variable != F){
    data <- data[data[subset.col]==subset.variable,]
    data <- data[,c(id,x,y,subset.col)]
  }else{
    data <- data[,c(id,x,y)]
  }

  data <- data[!duplicated(data), ]
  data <- data[!is.na(data[[id]]), ]

  ## Create the node variables
  data$node_name <-
    paste(data[[y]],
          data[[x]],
          sep = "_")

  nodes <- merge(
    unique(data[[y]]),
    unique(data[[x]]))

  nodes$comb <- paste(nodes$x,
                      nodes$y,
                      sep = "_")

  # Add an "All" group
  if(subset.variable != F){
    nodes <-
      data.frame(name = c(unique(data[[subset.col]]), nodes$comb),
                 group = as.factor(c(
                   unique(data[[subset.col]]), nodes$x)))
    all_domain <- c(unique(data[[subset.col]]),all_domain)
  }else{
    subset.col <- "subset.col"
    data[subset.col] <- "All"
    nodes <-
      data.frame(name = c(unique(data$subset.col), nodes$comb),
                 group = as.factor(
                   c(unique(data$subset.col), nodes$x)))
    all_domain <- c("All",all_domain)
  }
  nodes <- nodes[!duplicated(nodes),]
  ## Define Colors
  ## Auto
  if(length(color.palette)>1){
    if(nrow(color.palette) < length(domain) | colnames(color.palette) != c("group","col")){
      return(print("Error: unexpected manual color palette. Manual color palette
                   must have column names `group` and `col`, and include all
                   domain level in the group column."))
    }else{
      domain <- unique(nodes$group)
      cols <- col_pal$col[match(domain,col_pal[,1])]
    }
  }else if(grepl("category", color.palette)){
    col_pal <- pal_d3(color.palette)(length(unlist(all_domain)))
  } else if(color.palette == "Liu"){
    col_pal <- c("#FFFF99", "#CAB2D6","#FF7F0EFF","#FFBB78FF",
                 "#98DF8AFF",  "#2CA02CFF", "#A6CEE3",  "#1F78B4" )
  } else if(color.palette %in% rownames(brewer.pal.info)){
    col_pal <- brewer.pal(lenth(unlist(all_domain)), color.palette)
  } else if(color.palette %in% gg_sci_pals ){
    color.palette <- paste0("pal_",color.palette,"()(",length(unlist(all_domain)),")")
    col_pal <- eval(parse(text = color.palette))
  }else{
    return(print("Error: unexpected color.palette value. Set color.palette to a palette from
                 RColorBrewer, ggsci (including pal_d3 palettes), or `Liu` (custom palette)"))
  }
  if(length(col_pal) < length(unlist(all_domain))){
    return(print("Error: Insufficient palette levels, try a different color.palette value."))
  }

  domain <- unique(nodes$group)
  cols <- col_pal[match(domain,unlist(all_domain))]
  ## Manual
  #col_pal <- data.frame(
  #  "group" = c(unique(data[[subset.col]]), "Chemo/Immuno_Combo_therapy","Chemotherapy", "Immunotherapy",
  #              "Trial_treatment","Targeted_treatment", "Osimertinib",  "Other_Osimertinib_Combinations"),
  #  "col" = c("#FFFF99", "#CAB2D6","#FF7F0EFF","#FFBB78FF", "#98DF8AFF",  "#2CA02CFF", "#A6CEE3",  "#1F78B4" )
  #)

  #domain <- unique(nodes$group)
  #cols <- col_pal$col[match(domain,col_pal[,1])]
  domain <- make_string(domain)
  cols <- make_string(cols)
  my_color <- paste0('d3.scaleOrdinal() .domain(',domain,') .range(',cols,')')
  #Wrangle Data to link format
  links <- link_wrangler(data, node = nodes, id=id, x=x,y=y, subset.col= subset.col)
  # Fix the names
  node_vals <- links %>%
    group_by(target) %>%
    summarise(val = sum(value))

  nodes$vals <- node_vals$val[match(nodes$name, node_vals$target)]
  nodes$vals <- ifelse(nodes$name == nodes$group,
                       length(unique(data[[id]])),nodes$vals)
  if(labels == "all"){
    nodes$name <- paste(nodes$group, nodes$vals, sep = " ")
    nodes$name <- ifelse(is.na(nodes$vals), "",nodes$name)
  }else if(labels == "names"){
    nodes$name <-  as.character(nodes$group)
    nodes$name <- ifelse(is.na(nodes$vals), "",nodes$name)
  }else if(labels == "counts"){
    nodes$name <- ifelse(is.na(nodes$vals), "",nodes$vals)
  }else if(labels == "none"){
    nodes$name <- ""
  }else{
    print("Error: labels variable must be one of: all, names, counts, none.")
    stop()
  }
  nodes$name <- gsub("_", " ",nodes$name)

  # Make the Network
  p <- sankeyNetwork(Links = links, Nodes = nodes,
                     Source = "IDsource", Target = "IDtarget",
                     Value = "value", NodeID = "name",
                     colourScale=my_color, NodeGroup="group",
                     width = width, height = height, sinksRight=FALSE,
                     fontSize = font.size)
  out <- list(p,nodes,links)

  #Save png
  if(save.png.name != F){
    html_name <- paste0(save.png.name, ".html")
    png_name <- paste0(save.png.name, ".png")
    saveNetwork(p, html_name)
    webshot(html_name,png_name, vwidth = width, vheight = height)
  }
  # Output legend
  if(get.legend){
    legend.data <- data.frame(domain =  gsub("_", " ", unlist(all_domain)),
                              col = col_pal[1:length(unlist(all_domain))] )
    legend.plot <- ggplot(legend.data, aes(y = 1, fill = domain)) +
      geom_bar() + scale_fill_manual(values = legend.data$col,
                                     labels = legend.data$domain) +
      guides(fill=guide_legend(title=NULL))
    legend.plot <- get_legend(legend.plot)
    out <- list(p,legend.plot,nodes,links)
  }


  return(out)
}
