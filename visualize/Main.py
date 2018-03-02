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
  import string

  print("")
  print(" ========== Plot the domain ==========")
  print(" Allocating memory ...")

  Input = open("EX1.Domain","r")
  Temp = Input.readline().rstrip("\n")  # 1
  Temp = Input.readline().rstrip("\n")  # 1
  npoints = int(Input.readline().rstrip("\n"))  # 1

  print("{} {} ".format("Number of points:", npoints ))

  Temp = Input.readline().rstrip("\n")  # 1

  x = np.zeros (npoints, dtype=np.float)
  z = np.zeros (npoints, dtype=np.float)

  for ii in range(npoints):

    Temp = Input.readline().rstrip("\n")  # 1
    numbers = string.split(Temp)

    x[ii] = float(numbers[0])
    z[ii] = float(numbers[1])

    #print(x[ii])
    #print(z[ii])


  fig, ax = plt.subplots()
  ax.plot( x, z, label ="Domain" , color = "r", linewidth = 2.0)
    
  y_labels = ax.get_yticks()
  ax.yaxis.set_major_formatter(ticker.FormatStrFormatter('%10.4f'))
  plt.show()
  

if __name__ == '__main__':
    import sys    
    main(sys.argv)

