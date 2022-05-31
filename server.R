library(shiny)

function(input, output) {

  library(ggplot2)
  
  debug=FALSE
  
  # angle in radians
  # draw 2*pi cycles with a point every 1/density radians
  calcCycles=100
  density =100
  defaultTheta <- 2 * pi * seq(0, calcCycles, by = 1/density)
  defaultPoints = length(defaultTheta)
  
  circle <- function(cx=0,cy=0,radius = 1, npoints = 100){
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- cx + radius * cos(tt)
    yy <- cy + radius * sin(tt)
    return(data.frame(x = xx, y = yy))
  }  
  
  update_plot <- reactive({
    ggplotcTrochoid_A(
      radius1=input$ring1,
      radius2=input$ring2,
      point=input$point,
      overlay=input$showRings,
      penColor=input$penColor,
      penLoc = input$penLoc,
      zoom=input$zoom)
    
  })
  
  ggoverlayCircles<-function(base.gg,radius1,radius2,px,py,cx,cy,penColor){
    
    c1<-geom_path(data=circle(0,0,radius1),aes(x,y),lwd=2,na.rm=TRUE)
    c2<-geom_path(data=circle(cx,cy,abs(radius2)),aes(x,y),lwd=2,na.rm=TRUE)
    l1<- geom_segment(x = cx, 
                      y = cy, 
                      xend = px, 
                      yend = py,
                      arrow = arrow(length = unit(0.5, "cm")),
                      color=penColor,lwd=2)
    
    return (base.gg + c1+c2+l1)
  }
  
  hypoTrochoid <-function(r1=100,r2=-10,penLoc=10,theta=defaultTheta)  {
    #cyclic Trochoid
    #http://en.wikipedia.org/wiki/Hypotrochoid
    # in the user interface we denote hypotrochoid by a negative r2 so flip this sign for the usual formulation
    r2 = -r2
    
    #cx,cy is center of moving ring, x,y is point to plot
    cx <-(r1-r2)*cos(theta)
    x <- cx + penLoc*cos((r1-r2)/r2*theta)
    cy <-(r1-r2)*sin(theta)
    y <- cy - penLoc*sin((r1-r2)/r2*theta)
    return(data.frame(x,y,cx,cy))
  }
  
  epiTrochoid <-function(r1=100,r2=10,penLoc=10,theta=defaultTheta)  {
    #cyclic Trochoid
    #http://en.wikipedia.org/wiki/epitrochoid
    
    cx <-(r1+r2)*cos(theta)
    x<- cx - penLoc*cos((r1+r2)/r2*theta)
    cy <-(r1+r2)*sin(theta)
    y<- cy - penLoc*sin((r1+r2)/r2*theta)
    return(data.frame(x,y,cx,cy))
  }
  
  
  #---------------------------------------------------------------------------
  compute_CTrochoid<-function(radius1=100,radius2=20, penLoc=5) {
    
    #figuretype is not used yet
    
    
    if (radius2>0) {
      # rotating ring goes outside stationary ring
      figureType ='Epitrochoid'
      if (radius1==radius2)  figureType="Epicycloid (stationary Ring=Rotating Ring)"
      if (penLoc==radius2)  figureType="Limaçon (Pen Length = Rotating Ring)"      
      if (penLoc==0)  figureType="Circle (Pen Length = 0)"      
      points<-epiTrochoid(radius1,radius2,penLoc,defaultTheta)
    }
    else {
      # rotating ring goes inside stationary ring
      figureType = 'Hypotrochoid'
      if (radius1==-2*radius2)  figureType="Ellipse (stationary Ring = 2 * Rotating Ring)"
      if (penLoc==-radius2)  figureType="Hypocycloid (Pen Size = Rotating Ring)"      
      points<-hypoTrochoid(radius1,radius2,penLoc,defaultTheta)
      
    } 
    
    
    return (list(points=points,figureType=figureType))
  } 
  
  # ----------------- old plotter ------------------
  ggplotcTrochoid_A<-function(radius1=100,radius2=20,
                              penLoc=5,point=defaultPoints,
                              overlay=TRUE,penColor="black",zoom=FALSE) {
    
    
    if (radius2>0) {
      # rotating ring goes outside stationary ring
      figureType ='Epitrochoid'
      if (radius1==radius2)  figureType="Epicycloid (stationary Ring=Rotating Ring)"
      if (penLoc==radius2)  figureType="Limaçon (Pen Size = Rotating Ring)"      
      if (penLoc==0)  figureType="Circle (Pen Size = 0)"      
      dat<-epiTrochoid(radius1,radius2,penLoc,defaultTheta)
    }
    else {
      # rotating ring goes inside stationary ring
      figureType = 'Hypotrochoid'
      if (radius1==radius2)  figureType="Ellipse (stationary Ring = 2 * Rotating Ring)"
      if (penLoc==radius2)  figureType="Hypocycloid (Pen Size = Rotating Ring)"      
      
      dat<-hypoTrochoid(radius1,radius2,penLoc,defaultTheta)
    } 
    # are these special cases?
    
    
    # zoom might clip ring visibility but make figure fill plot area
    if (zoom)
      maxRange= max(abs(dat))
    else
      maxRange=radius1+abs(radius2)+penLoc
    
    plotRange=c(-maxRange,maxRange)
    
    t.plot<-ggplot(data=dat[1:point,],aes(x=x,y=y))+geom_path(color=penColor)+xlim(plotRange)+ylim(plotRange)
    #plot rotating ring and 'pen' for last point
    if (overlay) t.plot <- ggoverlayCircles(t.plot,radius1,radius2,
                                            px =dat[point,]$x,
                                            py =dat[point,]$y,
                                            cx =dat[point,]$cx,
                                            cy =dat[point,]$cy,
                                            penColor)
    print(t.plot)
  } 
  
  #-------------------------------------------------------------------------------------------
  ggplotcTrochoid_B<-function(dat,radius1,radius2,point=defaultPoints,
                              overlay=TRUE,penColor="black",zoom=FALSE) {
    
    
    # zoom might clip ring visibility but make figure fill plot area
    if (zoom)
      maxRange= max(abs(dat$points))
    else
      maxRange=max(abs(dat$points))*1.3
    
    plotRange=c(-maxRange,maxRange)
    
    t.plot<-ggplot(data=dat$points[1:point,],aes(x=x,y=y))
    #plot the points
    t.plot<-t.plot+geom_path(color=penColor)+xlim(plotRange)+ylim(plotRange)+ggtitle(dat$figureType)
    #plot rotating ring and 'pen' for last point
    if (overlay) t.plot <- ggoverlayCircles(t.plot,radius1,radius2,
                                            px =dat$points[point,]$x,
                                            py =dat$points[point,]$y,
                                            cx =dat$points[point,]$cx,
                                            cy =dat$points[point,]$cy,
                                            penColor)
    return(t.plot)
  } 
  # --------------------------------------------------------------------------  
  # precompute all points of trochoid and center of rotating ring.
  # Don't recompute unless these inputs change
  trochoidPoints <- reactive({
    
    compute_CTrochoid(radius1=input$ring1,
                      radius2=input$ring2,
                      penLoc=input$penLoc
    )
  })
  update_plot <- reactive({
    ggplotcTrochoid_A(
      radius1=input$ring1,
      radius2=input$ring2,
      point=input$point,
      overlay=input$showRings,
      penColor=input$penColor,
      penLoc = input$penLoc,
      zoom=input$zoom)
    
  })

# ------------------------------------------------------
# plot the figure
  
  output$distPlot <- output$distPlot <- renderPlot({
    update_plot()
    
  })
}
  