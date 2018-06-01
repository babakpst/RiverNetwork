
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
# V1.00: 03/25/2018  - Adding discretize module
# V2.00: 05/21/2018  - Modifying the code for parallel MPI
#
# File version $Id $
#
# Last update: 05/21/2018
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

  # Input section:
  InputName = "EX5_EX5_case1_s4.VisPy"
  Infofile = open(InputName,"r")

  Temp = Infofile.readline().rstrip("\n")  # 1
  Temp = Infofile.readline().rstrip("\n")  # 2
  Temp = Infofile.readline().rstrip("\n")  # 3

  Temp = Infofile.readline().rstrip("\n")  # 4
  fileName = Temp
  fileName = fileName.strip()

  Temp = Infofile.readline().rstrip("\n")  # 1
  Temp = Infofile.readline().rstrip("\n")  # 2
  Temp = Infofile.readline().rstrip("\n")  # 3
  analysisName = Temp
  analysisName = analysisName.strip()

  Temp = Infofile.readline().rstrip("\n")  # 1
  Temp = Infofile.readline().rstrip("\n")  # 2
  Temp = Infofile.readline().rstrip("\n")  # 3
  size = int(Temp)

  Temp = Infofile.readline().rstrip("\n")  # 1
  Temp = Infofile.readline().rstrip("\n")  # 2
  Temp = Infofile.readline().rstrip("\n")  # 3
  DT = float(Temp)

  Temp = Infofile.readline().rstrip("\n")  # 1
  Temp = Infofile.readline().rstrip("\n")  # 2
  Temp = Infofile.readline().rstrip("\n")  # 3
  nstep = int(Temp)

  Temp = Infofile.readline().rstrip("\n")  # 1
  Temp = Infofile.readline().rstrip("\n")  # 2
  Temp = Infofile.readline().rstrip("\n")  # 3
  dataFile = int(Temp)

  # Directories:
  fileNameDir = os.path.join("..", "output", fileName)
  fileName_domain = os.path.join(fileNameDir,fileName+ "_s"+ str(size)+".Domain")

  # Creating the output directory
  OutDir = os.path.join(fileName, analysisName + "_s"+ str(size),"" )
  directory = os.path.dirname(OutDir)
  if not os.path.exists(directory):
    print(" Creating the output dirctory ...")
    print("{} {}".format("The directory is: ",OutDir))
    os.makedirs(directory)

  Input = open(fileName_domain,"r")

  Temp = Input.readline().rstrip("\n")  # 1
  Temp = Input.readline().rstrip("\n")  # 1
  npoints = int(Input.readline().rstrip("\n"))  # 1

  #print("{} {} ".format(" Number of points:", npoints ))

  Temp = Input.readline().rstrip("\n")  # 1

  NCells = np.zeros (size, dtype=np.int32)

  x = np.zeros (npoints, dtype=np.float)
  z = np.zeros (npoints, dtype=np.float)

  h = np.zeros (npoints, dtype=np.float)
  uh= np.zeros (npoints, dtype=np.float)

  for ii in range(npoints):

    Temp = Input.readline().rstrip("\n")  # 1
    numbers = string.split(Temp)

    x[ii] = float(numbers[1])
    z[ii] = float(numbers[2])
    
  MaxX = np.amax(x)
  Range = MaxX/10.0

  xTick = np.arange(0, MaxX, Range)

  fig, ax = plt.subplots()
  ax.plot( x, z, label ="Domain" , color = "r", linewidth = 2.0)
    
  y_labels = ax.get_yticks()
  ax.yaxis.set_major_formatter(ticker.FormatStrFormatter('%10.4f'))

  plt.show(block=False) # <modify> See why the execution stops when the the command gets here. 
  
  PicName = os.path.join(OutDir,"domain.jpg")

  plt.savefig(PicName)
  #savefig(fname, dpi=None, facecolor='w', edgecolor='w', orientation='portrait', papertype=None, 
  #                format=None, transparent=False, bbox_inches=None, pad_inches=0.1, frameon=None)

  plt.close(fig)  

  for ii in range(1,nstep,dataFile):
    print("{:} {} {:} {}".format(" printing figure: ", ii, " out of: ", nstep))
    count = -1
    for kk in range (size):
      Files =  os.path.join(fileNameDir,analysisName+ "_s"+str(size),fileName+ "_s"+str(size)+"_p"
                            + str(kk)+"_" + str(ii) + ".Res")

      File_Input = open(Files,"r")

      Temp = File_Input.readline().rstrip("\n")    
      Temp = File_Input.readline().rstrip("\n")    
      Temp = File_Input.readline().rstrip("\n")    
      Temp = Temp.split()
      NCells[kk] = float(Temp[0])
      Temp = File_Input.readline().rstrip("\n")    
      #print(" no. cells in rank: ", kk, " is: ", NCells[kk])

      for jj in range(NCells[kk]):
        count = count + 1
        Temp = File_Input.readline().rstrip("\n")    
        Temp = Temp.split()
        h[count] = float(Temp[1])
        uh[count] = float(Temp[2])

    fig = plt.figure()

    ax1 = fig.add_subplot(211)
    ax1.grid(True, color='k')   
    #ax1.plot(X_Arr, Q_Arr, label ="Water flow" , color = "c", linewidth = 2.0)

    ax1.fill_between (x, z[:], z[:] +h[:])
    #ax1.fill_between (x, z[:], h[:])
    #plt.fill_between ( x, z[:], h[:] )
    
    title_string = ( 'H(T) - Time = %8.3f' % ( ii*DT ) )
    plt.title(title_string, fontsize = 16)

    plt.xlabel ( 'X',  fontsize=12 )
    plt.ylabel ( 'H(X,T)',  fontsize=12 )
    plt.xticks(xTick)

    #plt.axis ( [ 0.0, 2000, 0, 10 ] )
    #plt.fill_between ( x, z[:], z[:]+h[:] )

    ax2 = fig.add_subplot(212)
    ax2.grid(True, color='k')   
    #ax1.plot(X_Arr, Q_Arr, label ="Water flow" , color = "c", linewidth = 2.0)
    ax2.plot (x, uh[:], label ="Water flow" , color = "c", linewidth = 2.0)
    
    title_string = ( 'UH(T) - Time = %8.3f' % ( ii*DT ) )
    plt.title(title_string, fontsize = 16)

    plt.xlabel ( 'X',  fontsize=12)
    plt.ylabel ( 'UH(X,T)',  fontsize=12)
    plt.xticks(xTick)

    #mng = plt.get_current_fig_manager()
    #mng.resize(*mng.window.maxsize())


    #plt.show ( )
    plt.show(block=False) # <modify> See why the execution stops when the the command gets here. 

    PicName = os.path.join(OutDir,'Time_' +str(ii)+"_s" +'.jpg')

    plt.savefig(PicName)
    #savefig(fname, dpi=None, facecolor='w', edgecolor='w', orientation='portrait', papertype=None,
    #  format=None, transparent=False, bbox_inches=None, pad_inches=0.1, frameon=None)
    plt.close(fig)  

if __name__ == '__main__':
    import sys    
    main(sys.argv)

