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
#METIS_DIR=/workspace/source/metis-5.1.0
METIS_DIR=/workspace/source/metis-4.0.3

#F90=ifort   # without hdf
#F90= gfortran -ffree-line-length-none
F90=h5fc

FFLAGS=-O2

SOURCESF=	Parameters_mod.f90 \
					messages_and_errors_mod.f90 \
					reading_address_file_mod.f90 \
					reading_model_mod.f90 \
					Inf_mod.f90 \
					Results_mod.f90 \
					discretize_the_network_mod.f90 \
					partition_the_network_mod.f90 \
					Paraview_mod.f90 \
					Driver_Partitioner.f90

OBJSF=$(SOURCESF:%.F90=%.o)

#-L$(METIS_DIR) -lmetis
# METIS 4
# $(METIS_DIR)/libmetis.a

# METIS 5
# $(METIS_DIR)/build/libmetis/libmetis.a

#hdf5
#	h5fc -o Myexample_network Myexample_network.f90 -I${HDF5_INC} -L${HDF5_LIB} -lhdf5 -lz

main:$(OBJSF)
	@echo Linking...
	$(F90) $(FFLAGS) $(OBJSF) -o main -I$(METIS_DIR) $(METIS_DIR)/libmetis.a
	@echo Done!

%.o   : %.F90
	@echo Compiling...
	$(F90) $(FFLAGS) -c -cpp -I$(METIS_DIR)/include -L${HDF5_LIB} $<


# clean rule for object files
cln  :
	@echo Clean Object files...
	rm -rf *.o main
	rm -rf *.mod main
	rm -rf *.smod main
	rm -rf fort.* main




