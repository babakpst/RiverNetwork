


def main(arg):
  import matplotlib.pyplot as plt
  import numpy as np
  import matplotlib.ticker as ticker
  import sys
  import os
  import string


  def bathymetry(x):

    a1 = 0.674202
    a2 = 21.7112
    a3 = 14.492
    a4 = 1.4305
    Gravity = 9.81

    if 0.0<= x and x <= 200.0/3.0:
      H = ((4.0/Gravity)**(1.0/3.0)) * ( 4.0/3.0 - x/100.0 ) - (9*x/1000.0) * ( x/100.0 - 2.0/3.0)
    else:
      H = ((4.0/Gravity)**(1.0/3.0))*( +a1 * (x/100.0 - 2.0/3.0)**4 +a1 * (x/100.0 - 2.0/3.0)**3 -a2 * (x/100.0 - 2.0/3.0)**2 +a3 * (x/100.0 - 2.0/3.0)  + a4 )

    return H


  Gravity = 9.81
  results =((4.0/Gravity)**(1.0/3.0))*(4.0/3.0)
  print("result is:", results)
  npoints = 200
  dx =0.5

  x = np.zeros (npoints, dtype=np.float)
  z = np.zeros (npoints, dtype=np.float)

  for ii in range(npoints):
    
    x[ii] = ii*dx
    z[ii] = bathymetry(x[ii])
    
  MaxX = np.amax(x)
  Range = MaxX/10.0

  xTick = np.arange(0, MaxX, Range)

  fig, ax = plt.subplots()
  ax.plot( x, z, label ="Domain" , color = "r", linewidth = 2.0)
    
  y_labels = ax.get_yticks()
  ax.yaxis.set_major_formatter(ticker.FormatStrFormatter('%10.4f'))

  plt.show() # <modify> See why the execution stops when the the command gets here. 

if __name__ == '__main__':
    import sys    
    main(sys.argv)

