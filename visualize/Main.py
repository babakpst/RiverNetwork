###################################################################################################
# Purpose: This code solves the 2D Shallow Water Equation
#
# Developed by: Babak Poursartip
# Supervised by: Clint Dawson
#
# The Institute for Computational Engineering and Sciences (ICES)
# The University of Texas at Austin
#
# ================================ V E R S I O N ==================================================
# V0.00: 02/16/2018  - Initiation.
# V0.01: 02/18/2018  - Compiled for the first time.
# V0.02: 02/23/2018  - Adding input modules
# V0.02: 02/24/2018  - Adding input modules
# V0.02: 02/26/2018  - Adding discretize module
#
# File version $Id $
#
# Last update: 02/22/2018
#
# ================================ Global   V A R I A B L E S =====================================
#  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
#
###################################################################################################


def main(arg):
  import matplotlib.pyplot as plt
  import numpy as np
  import matplotlib.ticker as ticker
  import sys
  import os
  import string

  print("")
  print(" ========== Plot the domain ==========")
  print(" Allocating memory ...")

  Input = open("EX3_Limiter.Domain","r")
#  Input = open("EX1.Domain","r")
  Temp = Input.readline().rstrip("\n")  # 1
  Temp = Input.readline().rstrip("\n")  # 1
  npoints = int(Input.readline().rstrip("\n"))  # 1

  print("{} {} ".format("Number of points:", npoints ))

  Temp = Input.readline().rstrip("\n")  # 1

  x = np.zeros (npoints, dtype=np.float)
  z = np.zeros (npoints, dtype=np.float)

  h = np.zeros (npoints, dtype=np.float)
  uh= np.zeros (npoints, dtype=np.float)

  for ii in range(npoints):

    Temp = Input.readline().rstrip("\n")  # 1
    numbers = string.split(Temp)

    x[ii] = float(numbers[1])
    z[ii] = float(numbers[2])

    #print(x[ii])
    #print(z[ii])


  fig, ax = plt.subplots()
  ax.plot( x, z, label ="Domain" , color = "r", linewidth = 2.0)
    
  y_labels = ax.get_yticks()
  ax.yaxis.set_major_formatter(ticker.FormatStrFormatter('%10.4f'))
  #plt.show()

  plt.show(block=False) # <modify> See why the execution stops when the the command gets here. 
  
  plt.savefig("domain.jpg")
  #savefig(fname, dpi=None, facecolor='w', edgecolor='w', orientation='portrait', papertype=None, format=None, transparent=False, bbox_inches=None, pad_inches=0.1, frameon=None)

  plt.close(fig)  
  

  for ii in range(FileName):

    2000 ="EX3_Case1/EX3_Limiter_" + str(ii*100 + 1) + ".Res"
    #FileName ="EX1_Case1/EX1_" + str(ii*100 + 1) + ".Res"
    File_Input = open(FileName,"r")
    #Temp = File_Input.readline().rstrip("\n")
    #Temp = File_Input.readline().rstrip("\n")
    #Temp = File_Input.readline().rstrip("\n")    
    #Temp = Temp.split()
    #npoints = 250 #int(Temp[0])
    #Temp = File_Input.readline().rstrip("\n")    

    for jj in range(npoints):
      Temp = File_Input.readline().rstrip("\n")    
      Temp = Temp.split()
      h[jj] = float(Temp[1])
      uh[jj] = float(Temp[2])

    fig = plt.figure()

    ax1 = fig.add_subplot(211)
    ax1.grid(True, color='k')   
    #ax1.plot(X_Arr, Q_Arr, label ="Water flow" , color = "c", linewidth = 2.0)

    ax1.fill_between (x, z[:], z[:] +h[:])
    #ax1.fill_between (x, z[:], h[:])
    #plt.fill_between ( x, z[:], h[:] )
    
    title_string = ( 'H(T) - Time = %8.2f' % ( ii*0.001 ) )
    plt.title(title_string, fontsize = 16)

    plt.xlabel ( 'X',  fontsize=12 )
    plt.ylabel ( 'H(X,T)',  fontsize=12 )


    #plt.axis ( [ 0.0, 2000, 0, 10 ] )
    #plt.fill_between ( x, z[:], z[:]+h[:] )

    ax2 = fig.add_subplot(212)
    ax2.grid(True, color='k')   
    #ax1.plot(X_Arr, Q_Arr, label ="Water flow" , color = "c", linewidth = 2.0)
    ax2.plot (x, uh[:], label ="Water flow" , color = "c", linewidth = 2.0)
    
    title_string = ( 'UH(T) - Time = %8.2f' % ( ii ) )
    plt.title(title_string, fontsize = 16)

    plt.xlabel ( 'X',  fontsize=12)
    plt.ylabel ( 'UH(X,T)',  fontsize=12)

    #mng = plt.get_current_fig_manager()
    #mng.resize(*mng.window.maxsize())


    #plt.show ( )
    plt.show(block=False) # <modify> See why the execution stops when the the command gets here. 

    FileName = os.path.join('Time_' +str(ii)+"_s" +'.jpg')
    print("this is the path: ", FileName)
    plt.savefig(FileName)
    #savefig(fname, dpi=None, facecolor='w', edgecolor='w', orientation='portrait', papertype=None, format=None, transparent=False, bbox_inches=None, pad_inches=0.1, frameon=None)
    plt.close(fig)  









if __name__ == '__main__':
    import sys    
    main(sys.argv)

