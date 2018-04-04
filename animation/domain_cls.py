###################################################################################################
# Purpose: This class reads the domain. 
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


class domain_cls:

  
  def __init__(self):
    pass



  def domain_def(self):

    import numpy as np
    import string

    print("")
    print(" ========== Reads domain form file... ==========")

    Input = open("EX1_Limiter.Domain","r")
  #  Input = open("EX1.Domain","r")
    Temp = Input.readline().rstrip("\n")  # 1
    Temp = Input.readline().rstrip("\n")  # 1
    self.npoints = int(Input.readline().rstrip("\n"))  # 1

    print("{} {} ".format("Number of points:", self.npoints ))

    Temp = Input.readline().rstrip("\n")  # 1

    self.x = np.zeros (self.npoints, dtype=np.float)
    self.z = np.zeros (self.npoints, dtype=np.float)

    for ii in range(self.npoints):
    
      Temp = Input.readline().rstrip("\n")  # 1
      numbers = string.split(Temp)

      self.x[ii] = float(numbers[1])
      self.z[ii] = float(numbers[2])

      #print(x[ii])
      #print(z[ii])


  def PlotDomain_cls(self):

    import matplotlib.pyplot as plt
    import numpy as np
    import matplotlib.ticker as ticker

    print("")
    print(" ========== Plots the domain ==========")

    fig, ax = plt.subplots()
    ax.plot( self.x, self.z, label ="Domain" , color = "r", linewidth = 2.0)
      
    y_labels = ax.get_yticks()
    ax.yaxis.set_major_formatter(ticker.FormatStrFormatter('%10.4f'))
    #plt.show()

    plt.show(block=False) # <modify> See why the execution stops when the the command gets here. 
    
    plt.savefig("domain.jpg")
    #savefig(fname, dpi=None, facecolor='w', edgecolor='w', orientation='portrait', papertype=None, format=None, transparent=False, bbox_inches=None, pad_inches=0.1, frameon=None)

    plt.close(fig)  