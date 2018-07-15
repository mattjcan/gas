# EXPORT --------------------------------------------------------------------

png("img/p_wallum.png", width = 6, height = 3, units = "in", res = 300)
p_p3$Wallumbilla
dev.off() 

png("img/p_aust.png", width = 6, height = 3, units = "in", res = 300)
grid.arrange(p_aust$Sydney + scale_color_manual(values = c("grey", "#2166ac"), labels = c("North East Asia", "Sydney")), p_aust$Victoria + scale_color_manual(values = c("grey", "#2166ac"), labels = c("North East Asia", "Victoria")), ncol = 2)
dev.off() 

png("img/p_inter.png", width = 6, height = 6, units = "in", res = 300)
grid.arrange(p_p3_inter[[10]] + labs(title = "Henry Hub"), p_p3_inter[[11]], p_p3_inter[[13]] + labs(title = "Singapore Sling"), p_p3_inter[[12]] + labs(title = "Japan"), ncol = 2)
dev.off() 

png("img/p_gap.png", width = 6, height = 3, units = "in", res = 300)
grid.arrange(p_gap$vic_bris, p_gap$vic_asia, ncol = 2)
dev.off() 

png("img/p_net.png", width = 6, height = 3, units = "in", res = 300)
p_net
dev.off() 

png("img/p_net_12.png", width = 6, height = 3, units = "in", res = 300)
p_net_12
dev.off() 

png("img/p_net_jv.png", width = 6, height = 3, units = "in", res = 300)
p_net_jv
dev.off() 

png("img/p_net_jv_12.png", width = 6, height = 3, units = "in", res = 300)
p_net_jv_12
dev.off() 


