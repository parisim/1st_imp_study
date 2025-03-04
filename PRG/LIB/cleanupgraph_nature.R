### Note: I did not write this function. This function was found here: 
# https://osf.io/27d69

# Themes
cleanupgraph_nature = function(striptext, fontsize, fontsize_title){theme(panel.grid.major = element_line(color = "white"),
                                                                           panel.grid.minor = element_line(color = "white"),
                                                                           panel.background = element_blank(),
                                                                           axis.line = element_line(color = "black", size = 0.5),
                                                                           axis.ticks = element_line(color = "black", size = 0.5),
                                                                           axis.text = element_text(color = "black", size=fontsize),
                                                                           axis.title.x =  element_text(color = "black", size=fontsize),
                                                                           axis.title.y =  element_text(color = "black", size=fontsize),
                                                                           strip.background =element_rect(color="black", size = 0.5),
                                                                           strip.text.x = element_text(colour = striptext, face = "bold", size = fontsize),
                                                                           text = element_text(size=fontsize),
                                                                           plot.title = element_text(size=fontsize_title),
                                                                           legend.position="bottom",
                                                                           legend.text=element_text(size=fontsize),
                                                                           legend.title = element_text(size=fontsize),
                                                                           legend.key = element_blank())}

cleanupgraph_nature_l = cleanupgraph_nature("white", fontsize=25, fontsize_title=12) #large font option
cleanupgraph_nature_m = cleanupgraph_nature("white", 21, fontsize_title=12) #medium font option
cleanupgraph_nature_s = cleanupgraph_nature("white", 14, fontsize_title=10) #small font option
