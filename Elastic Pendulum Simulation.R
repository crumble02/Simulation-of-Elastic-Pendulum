euler=function(n,t0,gamma,m,dt){
  
  tm=rep(0,n);
  x=rep(0,n);
  y=rep(0,n);
  vx=rep(0,n);
  vy=rep(0,n);
  ax=rep(0,n);
  ay=rep(0,n);
  
  tm[1]= t0;
  x[1]= 1;
  y[1]= -2;
  vx[1]=vy[1]=ax[1]=ay[1]= 0;
  l= 4;
  
  
  g=-9.8;
  
  for(i in 2:n){
    
    tm[i] = tm[i-1] + dt;
    
    x[i] = x[i-1] + vx[i-1]*dt;
    y[i] = y[i-1] + vy[i-1]*dt;
    
    vx[i] = vx[i-1] + ax[i-1]*dt;
    vy[i] = vy[i-1] + ay[i-1]*dt;
    
    ax[i] = (gamma/m)*x[i-1]*(1 - l/sqrt((x[i-1])^2 + (y[i-1])^2));
    ay[i] = (gamma/m)*y[i-1]*(1 - l/sqrt((x[i-1])^2 + (y[i-1])^2)) - g;
    
    plot(c(0,x[i]), c(0,y[i]), type="o", lwd=2, xlim=c(-8,8), ylim=c(-8,8))
      Sys.sleep(0.01)
  }
}
