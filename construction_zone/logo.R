easyBakeOven::create_medallion(sourceImg = "rawlogo.jpg",
                 package = "rubix",
                 p_family = "Righteous",
                 white_around_sticker = TRUE,
                 p_size = 12,
                 p_y = 1.3,
                 p_color = "#000000",
                 h_fill = "#FFFFFF",
                 h_color = "#FFFFFF",
                 file = "logo.png",
                 s_width = .7,
                 s_height = .7,
                 s_x = 1,
                 s_y = .8)



library(showtext)
font_add_google("Righteous")
showtext_auto()

cave::makeFavicons()
