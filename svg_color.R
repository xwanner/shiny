###### EXAMPLE
# color = set_color(list(r=0,g=164,b=214))
# s = set_solver(color)
# solve(s)$filter





#########" COLOR
# COLOR = list(r=,g=,b=) entre 0 et 255
  set_color<-function(color) {
    color$r = clamp(color$r);
    color$g = clamp(color$g);
    color$b = clamp(color$b);
    return(color)
  }
  
  hueRotate<-function(color,angle = 0) {
    angle = angle / 180 * pi;
    sin = sin(angle);
    cos = cos(angle);
    
    color = multiply(color,c(
      0.213 + cos * 0.787 - sin * 0.213, 0.715 - cos * 0.715 - sin * 0.715, 0.072 - cos * 0.072 + sin * 0.928,
      0.213 - cos * 0.213 + sin * 0.143, 0.715 + cos * 0.285 + sin * 0.140, 0.072 - cos * 0.072 - sin * 0.283,
      0.213 - cos * 0.213 - sin * 0.787, 0.715 - cos * 0.715 + sin * 0.715, 0.072 + cos * 0.928 + sin * 0.072
    ));
    return(color)
  }
  
  grayscale<-function(color,value = 1) {
    color = multiply(color,c(
      0.2126 + 0.7874 * (1 - value), 0.7152 - 0.7152 * (1 - value), 0.0722 - 0.0722 * (1 - value),
      0.2126 - 0.2126 * (1 - value), 0.7152 + 0.2848 * (1 - value), 0.0722 - 0.0722 * (1 - value),
      0.2126 - 0.2126 * (1 - value), 0.7152 - 0.7152 * (1 - value), 0.0722 + 0.9278 * (1 - value)
    ));
    return(color)
  }
  
  sepia<-function(color,value = 1) {
    color = multiply(color,c(
      0.393 + 0.607 * (1 - value), 0.769 - 0.769 * (1 - value), 0.189 - 0.189 * (1 - value),
      0.349 - 0.349 * (1 - value), 0.686 + 0.314 * (1 - value), 0.168 - 0.168 * (1 - value),
      0.272 - 0.272 * (1 - value), 0.534 - 0.534 * (1 - value), 0.131 + 0.869 * (1 - value)
    ));
    return(color)
  }
  
  saturate<-function(color,value = 1) {
    color = multiply(color,c(
      0.213 + 0.787 * value, 0.715 - 0.715 * value, 0.072 - 0.072 * value,
      0.213 - 0.213 * value, 0.715 + 0.285 * value, 0.072 - 0.072 * value,
      0.213 - 0.213 * value, 0.715 - 0.715 * value, 0.072 + 0.928 * value
    ));
    return(color)
  }
  
  multiply<-function(color,matrix) {
    newR = clamp(color$r * matrix[1] + color$g * matrix[2] + color$b * matrix[3]);
    newG = clamp(color$r * matrix[4] + color$g * matrix[5] + color$b * matrix[6]);
    newB = clamp(color$r * matrix[7] + color$g * matrix[8] + color$b * matrix[9]);
    color$r = newR; color$g = newG; color$b = newB;
    return(color)
  }
  
  
  brightness<-function(color,value = 1) { return(linear(color,value)) }
  contrast<-function(color,value = 1) { return(linear(color,value, -(0.5 * value) + 0.5)) }
  
  linear<-function(color,slope = 1, intercept = 0) {
    color$r = clamp(color$r * slope + intercept * 255);
    color$g = clamp(color$g * slope + intercept * 255);
    color$b = clamp(color$b * slope + intercept * 255);
    return(color)
  }
  
  invert<-function(color,value = 1) {
    color$r = clamp((value + (color$r / 255) * (1 - 2 * value)) * 255);
    color$g = clamp((value + (color$g / 255) * (1 - 2 * value)) * 255);
    color$b = clamp((value + (color$b / 255) * (1 - 2 * value)) * 255);
    return(color)
  }
  
  hsl<-function(color) { 
    r = color$r / 255
    g = color$g / 255
    b = color$b / 255
    max = max(r, g, b)
    min = min(r, g, b)
    h  = (max + min) / 2
    s =h
    l =h
    
    if(max == min) {
      h = s = 0;
    } else {
      d = max - min;
      if(l>0.5) s = d/(2 - max - min)
      else s = d/(max+min)
      
      if(max==r) {
        if(g<b) h = (g-b)/d+6
        else h = (g-b)/d
      }else if(max==g){
        h = (b - r) / d + 2
      }else if(max==b){
        h = (r - g) / d + 4
      }
      h = h/ 6;
    }
    
    return(list(h = h*100, s=s*100,l=l*100))
  }
  
  clamp<-function(value) {
    if(value > 255) { value = 255; }
    else if(value < 0) { value = 0; }
    return(value)
  }

  
##########" TARGET 
# target=list(r=,g=,b=) entre 0 et 255
  set_solver<-function(target){
    return(list(target = target, targetHSL = hsl(target),reusedColor = set_color(list(r=0,g=0,b=0))))
  } # returns a solver !

  solve<-function(solver) {
    result = solveNarrow(solver,solveWide(solver));
    return(list(
      values = result$values,
      loss = result$loss,
      filter = css(solver,result$values)
    ));
  }

  solveWide<-function(solver) {
    A = 5;
    c = 15;
    a = c(60, 180, 18000, 600, 1.2, 1.2);

    best = list(loss = Inf);
    i = 0
    while( best$loss > 25 && i < 3) {
      initial = c(50, 20, 3750, 50, 100, 100);
      result = spsa(solver,A, a, c, initial, 1000);
      if(result$loss < best$loss) { best = result; }
      i = i+1
    } 
    return(best);
  }

  solveNarrow<-function(solver,wide) {
    A = wide$loss;
    c = 2;
    A1 = A + 1;
    a = c(0.25 * A1, 0.25 * A1, A1, 0.25 * A1, 0.2 * A1, 0.2 * A1);
    return(spsa(solver,A, a, c, wide$values, 500))
  }

  spsa<-function(solver,A, a, c, values, iters){
    alpha = 1;
    gamma = 0.16666666666666666;

    best = NULL;
    bestLoss = Inf;
    deltas = c();
    highArgs = c();
    lowArgs = c();
    
    fix<-function(value, idx) {
      max = 100;
      if(idx == 3 ) { max = 7500; }
      else if(idx == 5  || idx == 6 ) { max = 200; }
      
      if(idx == 4 ) {
        if(value > max) { value = value %% max; }
        else if(value < 0) { value = max + value %% max; }
      } else if(value < 0) { value = 0; }
      else if(value > max) { value = max; }
      return(value);
    }

    
    for(k in 0:(iters-1)){
      ck = c / (k + 1)^gamma;
      for(i in 1:6) {
        if(runif(1)>0.5) deltas[i] = 1
        else deltas[i] = -1
        highArgs[i] = values[i] + ck * deltas[i];
        lowArgs[i]  = values[i] - ck * deltas[i];
      }

      lossDiff = loss(solver,highArgs) - loss(solver,lowArgs);
      for(i in 1:6) {
        g = lossDiff / (2 * ck) * deltas[i];
        ak = a[i] / (A + k + 1)^alpha
        values[i] = fix(values[i] - ak * g, i);
      }

      loss = loss(solver,values);
      if(loss < bestLoss) { best = values; bestLoss = loss; }
    } 
    return(list(values= best, loss= bestLoss ));


  }

  loss<-function(solver,filters) { # Argument is array of percentages.
    color = list(r=0,g=0,b=0);
    color = set_color(color);

    invert(color,filters[1] / 100);
    sepia(color,filters[2] / 100);
    saturate(color,filters[3] / 100);
    hueRotate(color,filters[4] * 3.6);
    brightness(color,filters[5] / 100);
    contrast(color,filters[6] / 100);

    colorHSL = hsl(color);
    return(abs(color$r - solver$target$r)
    + abs(color$g - solver$target$g)
    + abs(color$b - solver$target$b)
    + abs(colorHSL$h - solver$targetHSL$h)
    + abs(colorHSL$s - solver$targetHSL$s)
    + abs(colorHSL$l - solver$targetHSL$l));
  }

  css<-function(solver,filters) {
    fmt<-function(idx, multiplier = 1) { return(round(filters[idx] * multiplier)) }
    return(paste0("filter: invert(",fmt(1),"%) sepia(",fmt(2),"%) saturate(",fmt(3),"%) hue-rotate(",fmt(4, 3.6),"deg) brightness(",fmt(5),"%) contrast(",fmt(6),"%);"))
  }

