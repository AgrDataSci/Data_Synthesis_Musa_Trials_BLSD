

node_2_data <-musa_plt_01[[2]]$data

node_3_data <-musa_plt_01[[3]]$data

nrow(node_2_data)

nrow(node_3_data)

hist(node_2_data$MLDS)
sort(node_2_data$MLDS)

hist(node_3_data$MLDS)
sort(node_3_data$MLDS)

hist(node_2_data$MLWS)
sort(node_2_data$MLWS)

hist(node_3_data$MLWS)
sort(node_3_data$MLWS)

hist(node_2_data$R10mm)

hist(node_3_data$R10mm)

hist(node_2_data$R20mm)

hist(node_3_data$R20mm)

hist(node_2_data$rhum_09)

sd(node_2_data$rhum_09)
sd(node_3_data$rhum_09)

t.test(node_2_data$rhum_09, node_3_data$rhum_09)

boxplot(node_2_data$rhum_09, node_3_data$rhum_09, main = "rhum_09", text = c("node 2", "node 3"))

boxplot(node_2_data$R10mm, node_3_data$R10mm)

rhum_09_node_2 <- data.frame("rhum_09" = musa_plt_01[[2]]$data$rhum_09)

rhum_09_node_2$node <- rep("node_2", nrow(rhum_09_node_2))

rhum_09_node_3 <- data.frame("rhum_09" = musa_plt_01[[3]]$data$rhum_09)

rhum_09_node_3$node <- rep("node_3", nrow(rhum_09_node_3))


rhum_09_nodes <- rbind(rhum_09_node_2, rhum_09_node_3)


rhum_09_p <- ggplot() + 
  geom_boxplot(data = rhum_09_nodes, aes(x = rhum_09, y = node))



MLDS_node_2 <- data.frame("MLDS" = musa_plt_01[[2]]$data$MLDS)

MLDS_node_2$node <- rep("node_2", nrow(MLDS_node_2))

MLDS_node_3 <- data.frame("MLDS" = musa_plt_01[[3]]$data$MLDS)

MLDS_node_3$node <- rep("node_3", nrow(MLDS_node_3))


MLDS_nodes <- rbind(MLDS_node_2, MLDS_node_3)


MLDS_p <- ggplot() + 
  geom_boxplot(data = MLDS_nodes, aes(x = node, y = MLDS)) + 
  
dplyr::arrange(MLDS_nodes, desc(MLDS))

t.test(MLDS_nodes[MLDS_nodes$MLDS <= 13, "MLDS"], 
       MLDS_nodes[MLDS_nodes$MLDS > 13, "MLDS"])


r10mm_node_2 <- data.frame("r10mm" = musa_plt_01[[2]]$data$R10mm)

r10mm_node_2$node <- rep("node_2", nrow(r10mm_node_2))

r10mm_node_3 <- data.frame("r10mm" = musa_plt_01[[3]]$data$R10mm)

r10mm_node_3$node <- rep("node_3", nrow(r10mm_node_3))


r10mm_nodes <- rbind(r10mm_node_2, r10mm_node_3)


r10mm_p <-  ggplot() + 
  geom_boxplot(data = r10mm_nodes, aes(x = r10mm, y = node))


Rtotal_node_2 <- data.frame("Rtotal" = musa_plt_01[[2]]$data$Rtotal)

Rtotal_node_2$node <- rep("node_2", nrow(Rtotal_node_2))

Rtotal_node_3 <- data.frame("Rtotal" = musa_plt_01[[3]]$data$Rtotal)

Rtotal_node_3$node <- rep("node_3", nrow(Rtotal_node_3))


Rtotal_nodes <- rbind(Rtotal_node_2, Rtotal_node_3)


rtotal_p <- ggplot() + 
  geom_boxplot(data = Rtotal_nodes, aes(x = Rtotal, y = node))


(rhum_09_p + MLDS_p) / (r10mm_p + rtotal_p)


t.test(node_2_data$MLDS, node_3_data$MLDS)


t.test(node_2_data$Rtotal, node_3_data$Rtotal)

t.test(node_2_data$R10mm, node_3_data$R10mm)

mean(node_3_data$rhum_09)

boxplot(node_2_data$rhum_09)

boxplot(node_3_data$rhum_09)
