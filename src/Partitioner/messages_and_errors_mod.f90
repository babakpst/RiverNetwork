
!##################################################################################################
! Purpose: This module prints out the some error messages.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.01: 06/05/2018 - Initiated: Compiled without error for the first time.
!
! File version $Id $
!
! Last update: 06/05/2018
!
! ================================ S U B R O U T I N E ============================================
!
! ================================ F U N C T I O N ================================================
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

module messages_and_errors_mod

use Parameters_mod

implicit none

!private

interface errorMessage
  procedure error_in_opening_a_file
end interface


contains

!##################################################################################################
! Purpose: This subroutine writes down the error message for opening an external file.
!
! Developed by: Babak Poursartip
! Supervised by:
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.01: 06/05/2018 - Initiated: Compiled without error for the first time.
!
! File version $Id $
!
! Last update: 06/05/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

subroutine error_in_opening_a_file(UnFile, IO_File)


! Libraries =======================================================================================

! User defined modules ============================================================================


implicit none

! Global variables ================================================================================
integer(kind=Smll):: UnFile     ! Holds Unit of a file for error message
integer(kind=Smll)::IO_File     ! For IOSTAT: Input Output status in OPEN command

! Local variables =================================================================================

! code ============================================================================================
if (IO_File > 0) then
        write(*, Fmt_Err1_OPEN) UnFile, IO_File; write(FileInfo, Fmt_Err1_OPEN) UnFile, IO_File;
        write(*, Fmt_FL); write(FileInfo, Fmt_FL);
        write(*, Fmt_End); read(*,*); stop;
      Else if (IO_File < 0) then
        write(*, Fmt_Err1_OPEN) UnFile, IO_File;
        write(FileInfo, Fmt_Err1_OPEN) UnFile, IO_File; write(*, Fmt_FL); write(FileInfo, Fmt_FL);
        write(*, Fmt_End);  read(*,*);   stop;
      end if

return
end subroutine error_in_opening_a_file

!##################################################################################################
! Purpose: This subroutine writes down the error message for closing an external file.
!
! Developed by: Babak Poursartip
! Supervised by:
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.01: 06/05/2018 - Initiated: Compiled without error for the first time.
!
! File version $Id $
!
! Last update: 06/05/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

subroutine error_in_closing_a_file(UnFile, IO_File)


! Libraries =======================================================================================

! User defined modules ============================================================================


implicit none

! Global variables ================================================================================
integer(kind=Smll):: UnFile     ! Holds Unit of a file for error message
integer(kind=Smll)::IO_File     ! For IOSTAT: Input Output status in OPEN command

! Local variables =================================================================================

! code ============================================================================================
if (IO_File > 0) then
        write(*, Fmt_Err1_Close) UnFile, IO_File; write(FileInfo, Fmt_Err1_Close) UnFile, IO_File;
        write(*, Fmt_FL); write(FileInfo, Fmt_FL);
        write(*, Fmt_End); read(*,*);   stop;
      end if

return
end subroutine error_in_closing_a_file



!##################################################################################################
! Purpose: This subroutine writes down the error message for allocation.
!
! Developed by: Babak Poursartip
! Supervised by:
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.01: 06/05/2018 - Initiated: Compiled without error for the first time.
!
! File version $Id $
!
! Last update: 06/05/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

subroutine error_in_allocation(ERR_Alloc)


! Libraries =======================================================================================

! User defined modules ============================================================================


implicit none

! Global variables ================================================================================
integer(kind=Smll) :: ERR_Alloc   ! Allocating and DeAllocating errors
integer(kind=Smll) :: ERR_DeAlloc ! Allocating and DeAllocating errors

! Local variables =================================================================================

! code ============================================================================================

write(*, Fmt_ALLCT) ERR_Alloc; write(FileInfo, Fmt_ALLCT) ERR_Alloc;
write(*, Fmt_FL); write(FileInfo, Fmt_FL); read(*, Fmt_End); stop;

return
end subroutine error_in_allocation

!##################################################################################################
! Purpose: This subroutine writes down the error message for allocation.
!
! Developed by: Babak Poursartip
! Supervised by:
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.01: 06/05/2018 - Initiated: Compiled without error for the first time.
!
! File version $Id $
!
! Last update: 06/05/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

subroutine error_in_deallocation(ERR_DeAlloc)


! Libraries =======================================================================================

! User defined modules ============================================================================


implicit none

! Global variables ================================================================================
integer(kind=Smll) :: ERR_DeAlloc ! Allocating and DeAllocating errors

! Local variables =================================================================================

! code ============================================================================================
write(*, Fmt_DEALLCT) ERR_DeAlloc; write(FileInfo, Fmt_DEALLCT) ERR_DeAlloc;
write(*, Fmt_FL); write(FileInfo, Fmt_FL);  write(*, Fmt_End);  read(*,*);   stop;

return
end subroutine error_in_deallocation


!##################################################################################################
! Purpose: This subroutine writes down the error message for write info in an external file.
!
! Developed by: Babak Poursartip
! Supervised by:
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.01: 06/05/2018 - Initiated: Compiled without error for the first time.
!
! File version $Id $
!
! Last update: 06/05/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

subroutine error_in_writing(UnFile, IO_write)


! Libraries =======================================================================================

! User defined modules ============================================================================


implicit none

! Global variables ================================================================================
integer(kind=Smll):: UnFile     ! Holds Unit of a file for error message
integer(kind=Smll):: IO_write ! Used for IOSTAT - Input Output Status - in the write command.

! Local variables =================================================================================

! code ============================================================================================
write(*, Fmt_write1) UnFile, IO_write; write(UnFile, Fmt_write1) UnFile, IO_write;
write(*, Fmt_FL); write(FileInfo, Fmt_FL); write(*, Fmt_End); read(*,*);  stop;

return
end subroutine error_in_writing


!##################################################################################################
! Purpose: This subroutine writes down the error message if there is an issue in reading data
!          from an external file.
!
! Developed by: Babak Poursartip
! Supervised by:
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.01: 06/05/2018 - Initiated: Compiled without error for the first time.
!
! File version $Id $
!
! Last update: 06/05/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

subroutine error_in_reading_data_from_file(UnFile, IO_read)


! Libraries =======================================================================================

! User defined modules ============================================================================


implicit none

! Global variables ================================================================================
integer(kind=Smll):: UnFile     ! Holds Unit of a file for error message
integer(kind=Smll):: IO_read  ! Holds error of read statements

! Local variables =================================================================================

! code ============================================================================================
write(*, Fmt_read1) UnFile, IO_read; write(UnFile, Fmt_read1) UnFile, IO_read;
write(*, Fmt_FL);  write(FileInfo, Fmt_FL); write(*, Fmt_End); read(*,*);  stop;

return
end subroutine error_in_reading_data_from_file


!##################################################################################################
! Purpose: This subroutine writes down the error message if there is an END-OF-FILE error.
!
! Developed by: Babak Poursartip
! Supervised by:
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.01: 06/05/2018 - Initiated: Compiled without error for the first time.
!
! File version $Id $
!
! Last update: 06/05/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

subroutine error_in_reading_data_from_file_EOF(UnFile, IO_read)


! Libraries =======================================================================================

! User defined modules ============================================================================


implicit none

! Global variables ================================================================================
integer(kind=Smll):: UnFile     ! Holds Unit of a file for error message
integer(kind=Smll):: IO_read  ! Holds error of read statements

! Local variables =================================================================================

! code ============================================================================================
write(*, Fmt_read1) UnFile, IO_read; write(UnFile, Fmt_read1) UnFile, IO_read;
write(*, Fmt_FL);  write(FileInfo, Fmt_FL); write(*, Fmt_End); read(*,*);  stop;

return
end subroutine error_in_reading_data_from_file_EOF


end module messages_and_errors_mod
