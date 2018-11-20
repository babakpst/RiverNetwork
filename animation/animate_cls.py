

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
    from matplotlib import animation as animation

    import domain_cls

    h = np.zeros (self.Domain.npoints, dtype=np.float)
    uh= np.zeros (self.Domain.npoints, dtype=np.float)
    xx= np.zeros (self.Domain.npoints, dtype=np.float)
    h = self.Domain.z+h

    fig, ax = plt.subplots()
    ax.grid(True, color='k')   
    ax = plt.axes(xlim=(0, 200), ylim=(0, 1.2))
    title_string = ( 'h(t)' )
    plt.title(title_string, fontsize = 16)

    plt.xlabel ( 'X',  fontsize=12 )
    plt.ylabel ( 'h(x,t)',  fontsize=12 )

    #line, = ax.plot(self.Domain.x, self.Domain.z+h, color='k')
    ax.plot(self.Domain.x, self.Domain.z, color='black', lw=2)
    line, = ax.plot(self.Domain.x, h, color='b', lw=2)
    #line, = ax.fill_between (self.Domain.x, self.Domain.z, self.Domain.z +h)

    def init():
      line.set_data([], [])
      return line,


    
    #def update(num, xx, uh, line):
    def update( num):

      xx = self.Domain.x
      nsize = 4
      total_in_rank = self.Domain.npoints / nsize

      for irank in range(nsize):
        FileName ="EX5_Case1_s4/EX5_s4_p"+str(irank) + "_"+ str(num*100 + 1) + ".Res"
        File_Input = open(FileName,"r")
        print(num,FileName)
        Temp = File_Input.readline().rstrip("\n")
        Temp = File_Input.readline().rstrip("\n")    
        Temp = File_Input.readline().rstrip("\n")        
        Temp = File_Input.readline().rstrip("\n")        
        for jj in range(irank*100, irank*100 + total_in_rank):
          Temp = File_Input.readline().rstrip("\n")    
          Temp = Temp.split()
          h[jj] =  float(Temp[1])+ self.Domain.z[jj]
          uh[jj] = float(Temp[2])

      line.set_data(xx, h)
      return line,

    #anim = animation.FuncAnimation(fig, update, len(xx), fargs=[xx, uh, line], interval=25, blit=True)
    anim = animation.FuncAnimation(fig, update, init_func=init,frames=200, interval=1, blit=True)
    #anim = animation.FuncAnimation(fig, update, init_func=init,frames=349, interval=1)

    anim.save('EX5_Dam.mp4', fps=3, extra_args=['-vcodec', 'libx264'])
    plt.show(block=False) # <modify> See why the execution stops when the the command gets here. 

