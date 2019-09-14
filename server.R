library(shiny)
library(shinydashboard)
library(rgl)
library(lubridate)

function(input, output) {
  
  output$title <- renderText({
    switch (
      input$tabs,
      "rec" = "Imaginary Axis",
      "cyl" = "HSB Colour Space",
      "sph" = "Globe Coordinate")
  })
  
  # Rectangular Coordinate -----------------------------------------
  val <- reactiveValues(
    theta = 60,
    phi = 10
  )
  
  observeEvent(input$viewPointA, {
    val$theta <- 0
    val$phi <- -90
  })
  
  observeEvent(input$viewPointB, {
    val$theta <- 275
    val$phi <- 5
  })
  
  observeEvent(input$reset, {
    val$theta <- 60
    val$phi <- 10
  })
  
  output$surface <- renderRglwidget({
    
    open3d(useNULL = T)
    
    rgl.viewpoint(theta = val$theta, phi = val$phi, zoom = 1)
    
    rgl.bg(color = "white")
    
    long <- 3
    
    x <- seq(-long, long, length.out = 50)
    y <- seq(-long, long, length.out = 50)
    z <- outer(x,y, function(x,y) {
      x^2 + 2*x*y - y^2 + 1
    })
    
    persp3d(x,y,z, alpha = 0.6, xlab = "x", ylab = "j", zlab = "y", axes = F)
    box3d()
    
    lim <- function(x){c(-max(abs(x)), max(abs(x))) * 1.1}
    xlim <- lim(x); ylim <- lim(y); zlim <- lim(z)
    rgl.lines(xlim, c(0, 0), c(0, 0), color = "red")
    rgl.lines(c(0, 0), ylim, c(0, 0), color = "blue")
    rgl.lines(c(0, 0), c(0, 0), zlim, color = "green")
    rglwidget()
  })
  
  # Cylindrical Coordinate -----------------------------------------
  
  output$htmlColour <- renderValueBox({
    theta <- input$hue * pi / 180
    radius <- input$sat / 100
    high <- input$bright / 100
    
    colr <- hsv(theta / (2*pi), radius, high)
    
    hb <- theta > 5 * pi / 18 & theta < pi
    sb <- radius < 0.5
    bb <- high > 0.7
    
    fontColor <- ifelse(bb & (hb | (!hb & sb)),
                        "#000000", "#FFFFFF")
    
    tgs <- paste("font-weight: bold;font-size: 50px;text-align: center;color: ", fontColor)
    
    valueBox(
      tagList(tags$h3(style = tgs, HTML(paste0("HSB(", input$hue, "&deg;, ", input$sat, ", " , input$bright, ")"))),
              tags$style(".small-box.bg-black { background-color: ", colr, "!important; }")),
      "", width = 12, color = "black")
  })
  
  
  output$plot <- renderRglwidget({
    
    open3d(useNULL = T)
    rgl.viewpoint(theta = 0, phi = -80, zoom = 1)
    rgl.bg(color = "white")
    
    theta <- input$hue * pi / 180
    radius <- input$sat / 100
    high <- input$bright / 100
    
    xcoor <- radius * cos(theta)
    ycoor <- radius * sin(theta)
    zcoor <- high * 5
    
    sphColour <- hsv(theta / (2*pi), radius, high)
    
    x <- rep(0,6)
    y <- rep(0,6)
    z <- seq(0,5)
    cyl <- cylinder3d(cbind(x,y,z), sides = 20)
    plot3d(cyl, type = "wire", box = F, axes = F, xlab = "", ylab = "", zlab = "")
    spheres3d(xcoor, ycoor, zcoor, radius = 0.1, color = sphColour)
    rglwidget()
  })
  
  # Spherical Coordinate -----------------------------------------
  
  val1 <- reactiveValues(
    lat = 0,
    long = 0,
    dirLat = "N",
    dirLong = "W"
  )
  
  observeEvent(input$submit, {
    val1$lat <- ifelse(!is.na(as.numeric(input$lat)), as.numeric(input$lat), 0)
    val1$dirLat <- input$dirLat
    val1$long <- ifelse(!is.na(as.numeric(input$long)), as.numeric(input$long), 0)
    val1$dirLong <- input$dirLong
  })
  
  output$coor <- renderValueBox({
    
    greenwichTime <- as.POSIXct(Sys.Date()) + 4*60*60
    currentTime <- switch (
      val1$dirLong,
      "E" = greenwichTime + val1$long * 4 * 60,
      "W" = greenwichTime - val1$long * 4 * 60
    )
    currentTime <- ifelse(currentTime == Sys.Date() | currentTime == Sys.Date() + 1,
                          "00:00", 
                          gsub("^(.*\\s)|(:00)$", "", currentTime))
    
    clock24 <- as.numeric(gsub(":.*$", "", currentTime))
    
    dayNight12 <- ifelse(clock24 >= 12, "pm", "am")
    clock12 <- ifelse(clock24 > 12, clock24 - 12, clock24)
    currentTime <- paste0(clock12, gsub("^.*:", ":", currentTime), " ", dayNight12)
    
    bg <- "#000000"
    font <- "#FFFFFF"
    if(clock24 < 7) {
      # night
      bg <- "#0a1340"
      font <- "#6e73ae"
    } else if (clock24 < 11) {
      # morning
      bg <- "#1895fd"
      font <- "#a8fffd"
    } else if (clock24 < 14) {
      # noon
      bg <-"#ffd61b"
      font <- "#fcffb5"
    } else if (clock24 < 17) {
      # afternoon
      bg <- "#f8aa27"
      font <- "#fff8b6"
    } else if (clock24 < 19) {
      # evening
      bg <- "#4d136d"
      font <- "#d2a8ff"
    } else if (clock24 < 24) {
      # night
      bg <- "#0a1340"
      font <- "#6e73ae"
    }
    
    display <- HTML(paste0(val1$lat, "&deg; ", val1$dirLat, "  ", val1$long, "&deg; ", val1$dirLong, ", ", currentTime))
    style <- paste0("font-weight: bold;font-size: 50px;text-align: center;color: ", font)
    valueBox(tagList(tags$h3(style = style, display), tags$style(".small-box.bg-aqua { background-color: ", bg, "!important; }")),"", width = 12)
  })
  
  output$globe <- renderRglwidget({
    lat <- matrix(seq(90, -90, len = 50)*pi/180, 50, 50, byrow = TRUE)
    long <- matrix(seq(-180, 180, len = 50)*pi/180, 50, 50)
    
    r <- 6378.1 # radius of Earth in km
    x <- r*cos(lat)*cos(long)
    y <- r*cos(lat)*sin(long)
    z <- r*sin(lat)
    
    open3d(useNULL = T)
    rgl.viewpoint(theta = 23.5,phi = -70, zoom = 0.6)
    persp3d(x, y, z, col = "white", 
            texture = system.file("textures/world.png", package = "rgl"),
            type = "wire",
            specular = "black", axes = F, box = F, xlab = "", ylab = "", zlab = "",
            normal_x = x, normal_y = y, normal_z = z)
    
    longitude <- val1$long # [-180,180]
    latitude <- val1$lat # [-90, 90]
    directionLat <- val1$dirLat
    directionLong <- val1$dirLong
    
    phi <- switch (directionLat,
                   "N" = (90 - latitude) * pi / 180,
                   "S" = (90 + latitude) * pi / 180
    )
    rLat <- r * sin(phi)
    elevation <- switch (directionLat,
                         "N" = sqrt(r^2 - rLat^2),
                         "S" = -sqrt(r^2 - rLat^2)
    )
    th <- seq(0, 2*pi, len = 201)
    xr <- rLat * cos(th)
    yr <- rLat * sin(th)
    lines3d(xr, yr, elevation, col = "red", lwd = 2)
    
    theta  <- switch (directionLong,
                      "E" = longitude * pi / 180,
                      "W" = -(longitude * pi / 180)
    )
    
    fi <- seq(-pi/2, pi/2, len = 201)
    xr1 <- r * cos(fi) * cos(theta)
    yr1 <- r * cos(fi) * sin(theta)
    zr1 <- r * sin(fi)
    lines3d(xr1, yr1, zr1, col = "red", lwd = 2)
    
    rglwidget()
  })
}