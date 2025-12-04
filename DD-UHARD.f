	module data_module
	  implicit none
	  real*8, allocatable :: total_yield_data(:)
	  real*8, allocatable :: total_gradient_data(:)
	  integer :: data_assignment = 0
	  
	contains
	  subroutine assign_data(file_path)
	    integer :: i, num_increments
		real*8 :: temp_value
		character(len=255) :: file_path
		
		character(len=255) :: Total_YIELD_path
	    character(len=255) :: Total_gradient_path
		
	    character(len=255) :: Total_YIELD_name
	    character(len=255) :: Total_gradient_name
		
		! User should replace these with actual data file names
	    Total_YIELD_name = 'your_Total_YIELD.txt'
	    Total_gradient_name = 'your_Total_gradient.txt'
		
	    Total_YIELD_path = trim(file_path)//Total_YIELD_name
	    Total_gradient_path = trim(file_path)//Total_gradient_name
		
		! User should replace this with the actual number of increments
		num_increments = your_num_inc     
		
		open(unit=10, file=Total_YIELD_path, action='read', status='old')
		allocate(total_yield_data(num_increments))
		
		do i = 1, num_increments 
		  read(10, *) temp_value
	      total_yield_data(i) = temp_value
		end do
		close(10)
		
		open(unit=20, file=Total_gradient_path, action='read', status='old')
		allocate(total_gradient_data(num_increments))
	  
		do i = 1, num_increments
		  read(20, *) temp_value
		  total_gradient_data(i) = temp_value
		end do
		close(20)
		
		data_assignment = 1
	  end subroutine assign_data
	end module data_module

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!     Subroutine UHARD
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
      SUBROUTINE UHARD(SYIELD,HARD,EQPLAS,EQPLASRT,
     + TIME,DTIME,TEMP,DTEMP,
     + NOEL,NPT,LAYER,KSPT,KSTEP,KINC,CMNAME,NSTATV,
     + STATEV,NUMFIELDV,PREDEF,DPRED,NUMPROPS,PROPS)
      USE data_module
	  INCLUDE 'ABA_PARAM.INC'

      DIMENSION HARD(3),STATEV(NSTATV),TIME(*),
     +          PREDEF(NUMFIELDV),DPRED(*),PROPS(*)
      CHARACTER*80 CMNAME

	  real*8, allocatable :: data(:)
	  
	  real :: delta_time_first_step     
	  real :: delta_time_second_step    
      real :: length_of_first_step         
	 
	  integer :: index_v
	  integer :: last_index_first_step
	  
	  character(len=255) :: file_path
	  
!-----------------------------------------------------------------------
!     Beginning of subroutine
!----------------------------------------------------------------------- 
	  if (data_assignment == 0) then
		! User should replace this with the actual directory path containing the data files
		file_path = 'your_path' 
		
		call assign_data(file_path)
	  endif
	  	  
	  ! User should replace these with the actual values for their simulation
	  delta_time_first_step = your_dt_first_step   ! For the first half cycle
	  delta_time_second_step = your_dt_second_step  ! For the remaining cycles
	  ! Note: If these two values are identical, separating the steps is unnecessary.
	  
	  ! User should replace these with the actual values for their simulation
	  length_of_first_step = your_length_of_first_step
	  last_index_first_step = your_last_index_first_step
	  
	  if (TIME(2) > length_of_first_step) then
		index_v = floor((TIME(2) - length_of_first_step)/ delta_time_second_step) + last_index_first_step + 1
		
	  else
	    index_v = floor(TIME(2) / delta_time_first_step) + 1
	  end if
	  
!-----------------------------------------------------------------------
!     Compute yield stress
!-----------------------------------------------------------------------
	  SYIELD = total_yield_data(index_v)
!-----------------------------------------------------------------------
!     Compute yield stress derivatives
!-----------------------------------------------------------------------
c     Derivative with respect to equivalent plastic strain
	  HARD(1) = total_gradient_data(index_v)
	  
c     Derivative with respect to equivalent plastic strain rate
      HARD(2) = 0.0
	  
c     Derivative with respect to temperature
      HARD(3) = 0.0
!-----------------------------------------------------------------------
      RETURN
      END
