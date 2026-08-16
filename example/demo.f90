program demo

  use, intrinsic :: iso_fortran_env

  use, non_intrinsic :: dancing_with_friends_and_enemies


  implicit none


  integer, parameter :: num_cycles = 5000
  integer, parameter :: save_steps =    5


  integer :: i_cycle

  character(17) :: file

  type(simulation_type) :: simulation


  call set_random_seed()

  call simulation%parameters%setup( &!
    interaction_update_threshold =   100          , &!
    num_dancers                  =  1000          , &!
    position_regularization      =  1.0e-2_real64 , &!
    to_origin                    =  0.995_real64  , &!
    to_friend                    =  0.020_real64  , &!
    to_enemy                     = -0.010_real64    &!
  )

  call simulation%setup()


  write(unit=file, fmt="(A,I6.6,A)") "result/", 0, ".dat"

  call simulation%save_dancer_position(file)


  do i_cycle = 1, num_cycles

    call simulation%update_dancers()

    if ( mod(i_cycle, save_steps) == 0 ) then

      write(unit=file, fmt="(A,I6.6,A)") "result/", i_cycle, ".dat"

      call simulation%save_dancer_position(file)

    end if

  end do


  call save_data_for_gnuplot


  print *, "END"



  contains



  subroutine save_data_for_gnuplot

    integer :: file_unit


    open( &!
      newunit = file_unit     , &!
      file    = "setting.plt" , &!
      action  = "write"         &!
    )

    write(file_unit, '(A,I0)') "num_cycles = ", num_cycles
    write(file_unit, '(A,I0)') "save_steps = ", save_steps

    close(file_unit)

  end subroutine save_data_for_gnuplot



  subroutine set_random_seed

    integer :: i

    integer :: seed_size

    integer, allocatable, dimension(:) :: seed


    call random_seed(size = seed_size)

    allocate( seed(1:seed_size) )

    do concurrent ( i = 1 : seed_size )
      seed(i) = i
    end do

    call random_seed(put=seed(:))

  end subroutine set_random_seed

end program demo
