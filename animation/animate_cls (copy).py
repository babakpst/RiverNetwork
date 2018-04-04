

###################################################################################################
# Purpose: This class animates the results.
#
# Developed by: Babak Poursartip
# 
# The Institute for Computational Engineering and Sciences (ICES)
# The University of Texas at Austin
#
# ================================ V E R S I O N ==================================================
# V0.0: 04/02/2018 - Class initiation.
#
# File version $Id
#
#
# ================================ L O C A L   V A R I A B L E S ==================================
# (Refer to the main code to see the list of imported variables)
#  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . . .
#
###################################################################################################


class animate_class():
  
  def __init__(self):
    import domain_cls

    print("initiated....")
    self.Domain = domain_cls.domain_cls()
    self.Domain.domain_def()
    self.Domain.PlotDomain_cls()
    print("initiated....")

  def animate_def(self):
    import matplotlib.pyplot as plt
    import numpy as np
    import matplotlib.ticker as ticker
    import sys
    import os
    import string

    import domain_cls

    h = np.zeros (self.Domain.npoints, dtype=np.float)
    uh= np.zeros (self.Domain.npoints, dtype=np.float)

    #for ii in range(2000):

    FileName ="EX3_Case1/EX3_Limiter_" + str(ii*100 + 1) + ".Res"
    #FileName ="EX1_Case1/EX1_" + str(ii*100 + 1) + ".Res"
    File_Input = open(FileName,"r")
    #Temp = File_Input.readline().rstrip("\n")
    #Temp = File_Input.readline().rstrip("\n")
    #Temp = File_Input.readline().rstrip("\n")    
    #Temp = Temp.split()
    #npoints = 250 #int(Temp[0])
    #Temp = File_Input.readline().rstrip("\n")    

    for jj in range(self.Domain.npoints):
      Temp = File_Input.readline().rstrip("\n")    
      Temp = Temp.split()
      h[jj] = float(Temp[1])
      uh[jj] = float(Temp[2])

    fig = plt.figure()

    ax1 = fig.add_subplot(211)
    ax1.grid(True, color='k')   
    #ax1.plot(X_Arr, Q_Arr, label ="Water flow" , color = "c", linewidth = 2.0)

    ax1.fill_between (self.Domain.x, self.Domain.z[:], self.Domain.z[:] +h[:])
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
    ax2.plot (self.Domain.x, uh[:], label ="Water flow" , color = "c", linewidth = 2.0)
    
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





