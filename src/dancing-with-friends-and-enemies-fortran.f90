module dancing_with_friends_and_enemies
  !! Original implementation:
  !! https://community.wolfram.com/groups/-/m/t/122095
  !! First time seeing this code:
  !! https://x.com/IsaacKing314/status/2086721066106253347

  use, intrinsic :: iso_fortran_env, only: error_unit
  use, intrinsic :: iso_fortran_env, only: int64
  use, intrinsic :: iso_fortran_env, only: real64

  use, intrinsic :: ieee_arithmetic, only: ieee_signaling_nan
  use, intrinsic :: ieee_arithmetic, only: ieee_value



  implicit none



  private

  public :: parameters_type
  public :: simulation_type
  public :: write(formatted)


  integer, parameter :: friend_index = 1
  integer, parameter :: enemy_index  = 2

  integer, parameter :: initial_now_index = 1
  integer, parameter :: initial_old_index = 2

  integer, parameter :: x_index = 1
  integer, parameter :: y_index = 2

  integer, parameter :: len_msg = 256

  real(real64), parameter :: nan = transfer(source=-1.0_int64, mold=0.0_real64)



  type parameters_type

    integer, private :: num_dancers = -1

    integer, private :: interaction_update_threshold = -1

    real(real64), private :: position_regularization = nan

    real(real64), private :: to_origin = nan

    real(real64), private :: to_enemy = nan

    real(real64), private :: to_friend = nan


    contains


    procedure, pass, private :: setup_parameters
    procedure, pass, private :: validate_parameters

    generic, public :: setup    => setup_parameters
    generic, public :: validate => validate_parameters

  end type parameters_type



  type indexer_type

    integer, private :: now = initial_now_index

    integer, private :: old = initial_old_index


    contains


    procedure, pass, private :: exchange
    procedure, pass, private :: initialize_indexer

    generic, public :: initialize => initialize_indexer

  end type indexer_type



  type simulation_type

    integer, private, allocatable, dimension(:,:) :: interaction
    !! (friend:enemy, dancer)

    real(real64), private, allocatable, dimension(:,:,:) :: position
    !! (x:y, dancer, now or old)

    type(parameters_type), public :: parameters

    type(indexer_type), private :: indexer


    contains


    procedure, pass, private :: initialize_interaction
    procedure, pass, private :: initialize_position
    procedure, pass, private :: resize_interaction
    procedure, pass, private :: resize_position
    procedure, pass, public  :: save_dancer_position
    procedure, pass, private :: setup_simulation
    procedure, pass, public  :: update_dancers
    procedure, pass, private :: update_position
    procedure, pass, private :: update_interaction

    generic, public :: setup => setup_simulation

  end type simulation_type


  interface write(formatted)
    module procedure :: write_formatted_simulation
  end interface write(formatted)



  contains



  pure function calc_next_position(parameters, self_position, friend_position, enemy_position) result(next_position)

    type(parameters_type), intent(in) :: parameters

    real(real64), intent(in), dimension(x_index:y_index) :: self_position

    real(real64), intent(in), dimension(x_index:y_index) :: friend_position

    real(real64), intent(in), dimension(x_index:y_index) :: enemy_position


    real(real64) :: next_position(x_index:y_index)


    real(real64) :: offset_to_enemy(x_index:y_index)

    real(real64) :: offset_to_friend(x_index:y_index)


    offset_to_enemy(:) = &!
      position_offset_to_other( &!
        parameters     = parameters         , &!
        self_position  = self_position  (:) , &!
        other_position = enemy_position (:)   &!
      )

    offset_to_friend(:) = &!
      position_offset_to_other( &!
        parameters     = parameters          , &!
        self_position  = self_position   (:) , &!
        other_position = friend_position (:)   &!
      )

    next_position(:) = &!
      & parameters%to_origin * self_position    (:) &!
      + parameters%to_enemy  * offset_to_enemy  (:) &!
      + parameters%to_friend * offset_to_friend (:)

  end function



  pure function position_offset_to_other(parameters, self_position, other_position)

    type(parameters_type), intent(in) :: parameters

    real(real64), intent(in), dimension(x_index:y_index) :: &!
      self_position, other_position


    real(real64), dimension(x_index:y_index) :: position_offset_to_other


    real(real64), dimension(x_index:y_index) :: relative_position


    relative_position(:) = other_position(:) - self_position(:)

    position_offset_to_other(:) = &!
      & relative_position(:) &!
      / (parameters%position_regularization + norm2(relative_position(:)))

  end function position_offset_to_other



  integer function rand_dancer_index(num_dancers) result(dancer_index)

    integer, intent(in) :: num_dancers


    real(real64) :: harvest


    call random_number(harvest)

    dancer_index = 1 + int(harvest * num_dancers)

  end function rand_dancer_index



  subroutine exchange(indexer)

    class(indexer_type), intent(inout) :: indexer


    integer :: buffer


    buffer      = indexer%old
    indexer%old = indexer%now
    indexer%now = buffer

  end subroutine exchange



  subroutine handle_stat(stat, msg)

    integer, intent(in) :: stat

    character(*), intent(in) :: msg


    if (stat /= 0) then
      write( error_unit, "(A,I0)" ) "stat : " , stat
      write( error_unit, "(A,A)"  ) "errmsg :", trim(msg)
      error stop
    end if

  end subroutine handle_stat



  subroutine initialize_indexer(indexer)

    class(indexer_type), intent(out) :: indexer


    indexer%old = initial_old_index
    indexer%now = initial_now_index

  end subroutine initialize_indexer



  subroutine initialize_interaction(simulation)

    class(simulation_type), intent(inout) :: simulation


    integer :: dancer_index


    associate(num_dancers => simulation%parameters%num_dancers)

        do dancer_index = 1, num_dancers

        associate( interaction => simulation%interaction(:,dancer_index) )

            interaction( enemy_index  ) = rand_dancer_index(num_dancers)
            interaction( friend_index ) = rand_dancer_index(num_dancers)

        end associate

      end do

    end associate

  end subroutine initialize_interaction



  subroutine initialize_position(simulation)
    !! Initialize dancer positions uniformly in [-1, 1)^2.

    class(simulation_type), intent(inout) :: simulation


    associate( &!
      position => simulation%position(:,:,simulation%indexer%old) &!
    )

      call random_number(position(:,:))

      position(:,:) = position(:,:) + position(:,:) - 1.0_real64

    end associate

  end subroutine initialize_position



  subroutine resize_interaction(simulation)

    class(simulation_type), intent(inout) :: simulation


    integer :: stat

    character(len_msg) :: errmsg


    if ( allocated(simulation%interaction) ) then

      deallocate(&!
        simulation%interaction, &!
        stat   = stat, &!
        errmsg = errmsg &!
      )

      call handle_stat(stat, errmsg)

    end if


    allocate( &!
      simulation%interaction( &!
        friend_index : enemy_index                       , &!
        1            : simulation%parameters%num_dancers   &!
      ), &!
      stat   = stat, &!
      errmsg = errmsg &!
    )

    call handle_stat(stat, errmsg)

  end subroutine resize_interaction



  subroutine resize_position(simulation)

    class(simulation_type), intent(inout) :: simulation


    integer :: stat

    character(len_msg) :: errmsg


    real(real64) :: mold


    if ( allocated(simulation%position) ) then

      deallocate(&!
        simulation%position, &!
        stat   = stat, &!
        errmsg = errmsg &!
      )

      call handle_stat(stat, errmsg)

    end if


    mold = ieee_value(0.0_real64, ieee_signaling_nan)


    allocate( &!
      simulation%position( &!
        x_index           : y_index                           , &!
        1                 : simulation%parameters%num_dancers , &!
        initial_now_index : initial_old_index                   &!
      ), &!
      mold   = mold, &!
      stat   = stat, &!
      errmsg = errmsg &!
    )

    call handle_stat(stat, errmsg)

  end subroutine resize_position



  subroutine setup_parameters( &!
    parameters, &!
    num_dancers, &!
    interaction_update_threshold, &!
    position_regularization, &!
    to_origin, &!
    to_enemy, &!
    to_friend &!
  )

    class(parameters_type), intent(out) :: parameters

    integer, intent(in) :: &!
      num_dancers, &!
      interaction_update_threshold

    real(real64), intent(in) :: &!
      position_regularization, &!
      to_origin, &!
      to_enemy, &!
      to_friend


    parameters%num_dancers = num_dancers

    parameters%interaction_update_threshold = &!
    &          interaction_update_threshold

    parameters%position_regularization = &
    &          position_regularization

    parameters%to_origin = to_origin
    parameters%to_enemy  = to_enemy
    parameters%to_friend = to_friend

  end subroutine setup_parameters



  subroutine save_dancer_position(simulation, file)

    class(simulation_type), intent(in) :: simulation

    character(*), intent(in) :: file


    integer :: file_unit

    integer :: iostat

    character(len_msg) :: iomsg


    open( &!
      newunit = file_unit , &!
      file    = file      , &!
      action  = "write"   , &!
      iostat  = iostat    , &!
      iomsg   = iomsg       &!
    )

    call handle_stat(iostat, iomsg)


    write(file_unit, *) simulation


    close( &!
      unit   = file_unit , &!
      iostat = iostat    , &!
      iomsg  = iomsg       &!
    )

    call handle_stat(iostat, iomsg)

  end subroutine save_dancer_position



  subroutine setup_simulation(simulation)

    class(simulation_type), intent(inout) :: simulation


    call simulation%parameters%validate()

    call simulation%resize_interaction()
    call simulation%resize_position()

    call simulation%indexer%initialize()

    call simulation%initialize_interaction()
    call simulation%initialize_position()

  end subroutine setup_simulation



  subroutine update_dancers(simulation)

    class(simulation_type), intent(inout) :: simulation


    call simulation%update_interaction()
    call simulation%update_position()


    call simulation%indexer%exchange()

  end subroutine update_dancers



  subroutine update_interaction(simulation)

    class(simulation_type), intent(inout) :: simulation


    associate( &!
      num_dancers => simulation%parameters%num_dancers                  , &!
      threshold   => simulation%parameters%interaction_update_threshold   &!
    )

      if ( rand_dancer_index(num_dancers) .lt. threshold ) then

        associate( &!
          interaction => &!
            simulation%interaction(:,rand_dancer_index(num_dancers)) &!
        )

            interaction( enemy_index  ) = rand_dancer_index(num_dancers)
            interaction( friend_index ) = rand_dancer_index(num_dancers)

        end associate

      end if

    end associate

  end subroutine update_interaction



  subroutine update_position(simulation)

    class(simulation_type), intent(inout) :: simulation


    integer :: dancer_index


    associate( &!
      old_index => simulation%indexer%old , &!
      now_index => simulation%indexer%now   &!
    )

      do concurrent(dancer_index = 1 : simulation%parameters%num_dancers)

        associate( &!
          old_position => simulation%position(:,:,old_index)     , &!
          now_position => simulation%position(:,:,now_index)     , &!
          interaction  => simulation%interaction(:,dancer_index)   &!
        )

          now_position(:,dancer_index) = &!
            calc_next_position( &!
              parameters      = simulation%parameters                     , &!
              self_position   = old_position(:,dancer_index)              , &!
              enemy_position  = old_position(:,interaction(enemy_index )) , &!
              friend_position = old_position(:,interaction(friend_index))   &!
            )

        end associate

      end do

    end associate

  end subroutine update_position



  subroutine validate_parameters(parameters)

    class(parameters_type), intent(in) :: parameters


    if ( parameters%num_dancers < 3 ) then
      write(error_unit, "(A)") "The number of dancers must be at least 3."
      error stop
    end if


    if ( parameters%interaction_update_threshold < 0 ) then
      write(error_unit, "(A)") "`interaction_update_threshold` must NOT be negative."
      error stop
    end if


    if ( parameters%position_regularization <= 0.0_real64 ) then
      write(error_unit, "(A)") "`position_regularization` must NOT be negative."
      error stop
    end if

  end subroutine validate_parameters



  subroutine write_formatted_simulation(simulation, unit, iotype, vlist, iostat, iomsg)

    class(simulation_type), intent(in) :: simulation

    integer, intent(in) :: unit

    character(*), intent(in) :: iotype

    integer, intent(in), dimension(:) :: vlist

    integer, intent(out) :: iostat

    character(*), intent(inout) :: iomsg


    associate( size_vlist => size(vlist) )

      if ( size(vlist) /= 0 ) then

        iostat = size_vlist

        write(unit=iomsg, fmt="(A)") "size(vlist) must be less than 2."

        return

      end if

    end associate


    if (iotype == "LISTDIRECTED") then

      write( &!
        unit   = unit           , &!
        fmt    = "(2ES25.16E3)" , &!
        iostat = iostat         , &!
        iomsg  = iomsg            &!
      ) &!
        simulation%position(:,:,simulation%indexer%old)

      if ( iostat /= 0 ) return

    else

      iostat = 1

      write(unit=iomsg, fmt="(A)") "Unsupported `iotype` was detected."

    end if

  end subroutine write_formatted_simulation

end module dancing_with_friends_and_enemies
