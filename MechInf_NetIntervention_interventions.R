
remove_cross_edges <- function(SIM_school.net) {
  
  threshold = .25
  node.df = tibble(id = c(1:network.size(SIM_school.net)),
                   classroom = get.vertex.attribute(SIM_school.net, "classroom"))
              
  
  edge_remove.df = as.edgelist(SIM_school.net,output="tibble") %>% 
    left_join(node.df, by = c(".tail" = "id")) %>%
    rename(classroom_tail = classroom) %>%
    left_join(node.df, by = c(".head" = "id")) %>%
    rename(classroom_head = classroom) %>%
    mutate(class_cross_edge = !(classroom_tail==classroom_head),
           prob_remove = runif(n()),
           edge_remove = if_else(prob_remove < threshold, TRUE, FALSE)) 

  edge_remove_id = which(edge_remove.df$edge_remove == TRUE)
  
  SIM_school_1.net = SIM_school.net
  delete.edges(SIM_school_1.net,edge_remove_id)
  
  return(SIM_school_1.net)
}