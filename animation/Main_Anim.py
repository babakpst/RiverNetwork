
###################################################################################################
# Purpose: This code animates the results of Shallow Water Equation.
#
# Developed by: Babak Poursartip
#
# The Institute for Computational Engineering and Sciences (ICES)
# The University of Texas at Austin
#
# ================================ V E R S I O N ==================================================
# V0.00: 04/02/2018  - Initiation.
# V0.01: 04/02/2018  - Compiled for the first time.
# V0.10: 04/02/2018  - Animation
#
# File version $Id $
#
# Last update: 04/02/2018
#
# ================================ Global   V A R I A B L E S =====================================
#  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
#
###################################################################################################


def main(arg):
  
  # Import built-in libraries ====================================================================
  import matplotlib.pyplot as plt
  import numpy as np
  import matplotlib.ticker as ticker
  import sys
  import os
  import string
  from matplotlib import animation as animation

  # Import user-defined modules ===================================================================
  import animate_cls


  # Code begins ==================================================================================
  print("")
  print("{:^80}".format("-------------------- Animate the Shallow Water Eqs -------------------"))
  print("{:^80}".format("---------- Developers:: Babak Poursartip/Clint Dawson ----------------"))
  print("")


  Animate = animate_cls.animate_class()
  Animate.animate_def()
  print("done")

  #anim = animation.FuncAnimation(fig, Animate.animate_def, init_func=init, frames=200, interval=20, blit=True)
  #anim.save('basic_animation.mp4', fps=30, extra_args=['-vcodec', 'libx264'])


if __name__ == '__main__':
    import sys    
    main(sys.argv)

