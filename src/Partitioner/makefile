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
METIS_DIR=/workspace/source/metis-5.1.0
#METIS_DIR=/workspace/source/metis-4.0.3

F90=ifort
#F90= gfortran -ffree-line-length-none
FFLAGS=-O2

SOURCESF=	Parameters_mod.f90 \
					messages_and_errors_mod.f90 \
					reading_address_file_mod.f90 \
					reading_address_file_smod.f90 \
					reading_model_mod.f90 \
					Inf_mod.f90 \
					Results_mod.f90 \
					discretize_the_network_mod.f90 \
					partition_the_network_mod.f90 \
					Driver_Partitioner.f90

OBJSF=$(SOURCESF:%.F90=%.o)

#-L$(METIS_DIR) -lmetis
# METIS 4
# $(METIS_DIR)/libmetis.a

# METIS 5
# $(METIS_DIR)/build/libmetis/libmetis.a

main:$(OBJSF)
	@echo Linking...
	$(F90) $(FFLAGS) $(OBJSF) -o -I$(METIS_DIR) main $(METIS_DIR)/build/libmetis/libmetis.a
	@echo Done!

%.o   : %.F90
	@echo Compiling...
	$(F90) $(FFLAGS) -c -cpp -I$(METIS_DIR)/include $<



# clean rule for object files
cln  :
	@echo Clean Object files...
	rm -rf *.o main
	rm -rf *.mod main
	rm -rf *.smod main
	rm -rf fort.* main




