library(hexSticker)
library(magick)
library(sysfonts)
library(tidyverse)

httk_logo <- image_read("httk-logo.png")
fonts <- font_files()

#font_add("myname", "FILENAME.TTF")

out <- sticker(
        subplot = httk_logo,
        package = "httk",
        s_width = 1.6,
        s_height = 1.6,
        s_x = 1.05,
        s_y = 0.8,
        p_size = 72,
        p_y=1.5,
        p_color = "black",
        h_fill = "lightblue",
        h_color = "red",
        h_size = 1.5,
        url ="https://CRAN.R-project.org/package=httk",
        u_size = 8,
        spotlight=T,
        l_y=1.2,
        l_x=1.5,
        l_width=3,
        l_height=3,
        l_alpha=0.4
        ) #%>% print()
ggsave("httk.png",dpi=300)        

        
