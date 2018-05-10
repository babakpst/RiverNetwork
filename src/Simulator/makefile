# =================================================================================================
#
# Developer    : Babak Poursartip
#                Institute for Computational Engineering and Sciences
#                The University of Texas at Austin, TX, USA
#
# Code         : 2D Shallow Water Equation
#
# Last Update  : 02/13/2018
#
# =================================================================================================

F90=ifort
#F90=ifort -pg
#F90= gfortran  # not working because of ifort module
#F90= gfortran -ffree-line-length-none#F90= gfortran -ffree-line-length-none


#FFLAGS=-O2 -qopenmp -g -debug all -debug-parameters all -traceback -check all -fp-stack-check -check bounds -check stack -gen-interfaces -warn interfaces -ftrapuv -fpe0 -CB
FFLAGS=-O2 -qopenmp
#FFLAGS=-O2

SOURCESF=	Parameters_mod.f90 \
					Timer_mod.f90 \
					Input_mod.f90 \
					Inf_mod.f90 \
					Results_mod.f90 \
					Limiter_mod.f90 \
					Driver_Simulation.f90

OBJSF=$(SOURCESF:%.F90=%.o)


main:$(OBJSF)
	@echo Linking...
	$(F90) $(FFLAGS) $(OBJSF) -o main
	@echo Done!

%.o   : %.F90
	@echo Compiling...
	$(F90) $(FFLAGS) -c $<



# clean rule for object files
cln  :
	@echo Clean Object files...
	rm -rf *.o main
	rm -rf *.mod main
	rm -rf fort.* main




