cave::createLogo("rawlogo.jpg",
                 package = "rubix",
                 p_family = "Righteous",
                 p_size = 12,
                 p_y = 1.5,
                 p_color = "#000000",
                 h_fill = "#FFFFFF",
                 h_color = "#FF0000",
                 file = "logo.png",
                 s_width = .9,
                 s_height = .9,
                 s_x = 1,
                 s_y = .8)



library(showtext)
font_add_google("Righteous")
showtext_auto()

cave::makeFavicons()
