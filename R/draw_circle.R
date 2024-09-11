# Functions from the orphaned (~11/2023) plotrix R package
# Authors:  	Jim Lemon, Ben Bolker, Sander Oom, Eduardo Klein, Barry Rowlingson,
# Hadley Wickham, Anupam Tyagi, Olivier Eterradossi, Gabor Grothendieck, Michael Toews, 
# John Kane, Rolf Turner, Carl Witthoft, Julian Stander, Thomas Petzoldt, Remko Duursma, 
# Elisa Biancotto, Ofir Levy, Christophe Dutang, Peter Solymos, Robby Engelmann, 
# Michael Hecker, Felix Steinbeck, Hans Borchers, Henrik Singmann, Ted Toal, 
# Derek Ogle, Darshan Baral, Ulrike Groemping, Bill Venables, The CRAN Team
# License GPL-3
# https://cran.r-project.org/package=plotrix
# https://github.com/plotrix/plotrix
draw.circle<-function(x,y,radius,nv=100,border=NULL,col=NA,
 lty=1,density=NULL,angle=45,lwd = 1) {

 xylim<-par("usr")
 plotdim<-par("pin")
 ymult<-getYmult()
 angle.inc<-2*pi/nv
 angles<-seq(0,2*pi-angle.inc,by=angle.inc)
 if(length(col)<length(radius)) 
  col<-rep(col,length.out=length(radius))
 for(circle in 1:length(radius)) {
  xv<-cos(angles)*radius[circle]+x
  yv<-sin(angles)*radius[circle]*ymult+y
  polygon(xv,yv,border=border,col=col[circle],lty=lty,
   density=density,angle=angle,lwd=lwd)
 }
 invisible(list(x=xv,y=yv))
}

getYmult<-function() {
 if(dev.cur() == 1) {
  warning("No graphics device open.")
  ymult<-1
 }
 else {
  # get the plot aspect ratio
  xyasp<-par("pin")
  # get the plot coordinate ratio
  xycr<-diff(par("usr"))[c(1,3)]
  ymult<-xyasp[1]/xyasp[2]*xycr[2]/xycr[1]
 }
 return(ymult)
}
