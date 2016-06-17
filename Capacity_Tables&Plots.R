if (!require(c("RColorBrewer", "ggplot2","fields","caTools") )) {
    install.packages("RColorBrewer")
    install.packages("ggplot2")
    install.packages("fields")
    install.packages("caTools")
    library(RColorBrewer)
    library(ggplot2)
    library(fields) # for tim.colors
    library(caTools) # for write.gif
}



col.func <- brewer.pal( 11, "Greens" )





        to.plot <- data.frame(Cap <- listCap[,i], CF <- listCap[,j]) 
       
        ggout<- ggplot(list.Cap)
        
            geom_point(size=1) +
            theme_light() +
            theme(
                axis.text = element_text(family= "serif", size = 14),
                axis.title = element_text(family = "serif", size = 14 )
            ) +
            
            
            scale_x_continuous( trans= "reverse", 'Capacity MW') + 
            scale_y_continuous ( 'CF %') +
            scale_colour_brewer( type = "seq", palette = "Greens" ) #"Greens"

    
    
    to.plot <- data.frame(Cap, CF)
    
    ggplot(to.plot)
        
        geom_point(aes(x=listCap[,1], y=listCap[,2], size=1)) +
        geom_point(aes(x=listCap[,3], y=listCap[,4], size=1)) +
        theme_light() +
        theme(
            axis.text = element_text(family= "serif", size = 14),
            axis.title = element_text(family = "serif", size = 14 )
        ) +
        
        scale_x_continuous( trans= "reverse", "Capacity MW" ) + 
        scale_y_continuous ( 'CF %' ) +
        scale_colour_brewer(type = "seq", palette = "Blues")
    
    

    
    
    draw.data <- function(xy, obj = ggplot()){

        
        x.lab <- "Capacity, MW"
        y.lab <- "CF, %"
        
        my.data <- list(Cap
        
        my_labels <- parse( text = paste0("1E", seq(1, 7, 2)))
        
        # Find max, min and difference
        y.max <- max(my.data$)
        y.min <- min(my.data)
        
        y.max <- 1
        y.min <- 0
        
        diff <- y.max - y.min
        
        # Find percentage and apply to new column 
        my.data$y <- apply(my.data, 1, function(z) ((z["y"] - y.min)/diff)*100)
        
        ggp.temp <- obj +
            geom_line(data = my.data, aes(x = x, y = y)) +
            # Draw 2 lines at 50% and 90% through the y-axis
            geom_hline(yintercept = c(50, 90), linetype = "dotted") + 
            scale_x_continuous(x.lab, breaks = seq(-10, -4, 1), 
                               labels = my_labels) + 
            labs(title = "Graph", x = x.lab, y = y.lab)
        
        return (ggp.temp)
    }
    
    p1 <- draw.data(xy = dat1)
    p2 <- draw.data(xy = dat2, obj = p1)
    print(p2)
    

    
####### TEST GIFs #########
    
    library(fields) # for tim.colors
    library(caTools) # for write.gif
    m = 400 # grid size
    C = complex( real=rep(seq(-1.8,0.6, length.out=m), each=m ), imag=rep(seq(-1.2,1.2, length.out=m), m ) )
    C = matrix(C,m,m)
    
    Z = 0
    X = array(0, c(m,m,20))
    for (k in 1:20) {
        Z = Z^2+C
        X[,,k] = exp(-abs(Z))
    }
    
    image(X[,,k], col=tim.colors(256)) # show final image in R
    write.gif(X, "Mandelbrot_1.gif", col=tim.colors(256), delay=10)
    
    
    
    