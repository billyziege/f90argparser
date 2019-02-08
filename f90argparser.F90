module f90argparser
!-------------------------------------------------------------------------------
! Module f90argparser:
! A module for processing cli commands and storing them in an object for
! further processing.  Requires help documentation for the options,
! and provides default value where necessary.  If an option does not
! have a value, than the option value is either '' if not called on the
! cli, or ".true." (the string) if the appropriate cli flag is present. 
! See f90argparser_sample.f90 for an example of the use of this module.
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!type option_s
!   - A data type containing option information (see the type for explanations,
!     and see option_init for default values)
!
!type opt_container
!   - A data type containing the option array and some meta-data.  (see the type for
!     explanaitons and opt_container_init for default values)
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
!INTENTION: PUBLIC SUBROUTINES (the subroutines you should use)
!
!subroutine opt_container_init
!   - A subroutine to initialize the option container that contains programmer
!     data to parse cli arguments.  Used in programming
!     and will be compared to the command line.  The help 
!     functionality is immediately added and should not be in the number N. 
! input:
!   - N = The number of options that will be provided by the programmer.
!     As the help option is automatically initialized, the total number of
!     options is actually N + 1.
! output:
!   - opt_c = The container with all variables initialized and allocated.
! 
!subroutine add_option
!   - Adds an option to the provided option container
! input:
!  REQUIRED:
!   - help = A description of what the option or positional argument
!     is.  This description will appear when the help screen is printed (see 
!     print_help_screen below.)
!  OPTIONAL:
!   - long = The multi-character flag that appears after '--' on the command 
!     line that will indicate this option.  
!     If both long and short are not provided, the option will be assumed to 
!     be a required positional argument.
!   - short = The single-character flag that appears after '-' on the command
!     line that will indicate this option.
!     If both long and short are not provided, the option will be assumed to 
!     be a required positional argument.
!   - has_arg = Specifies if the flag has an argument.  Default is .true.
!     All required positional arguments have has_arg == .true.  The
!     argument can be either applied with a space between the flag or
!     separated from the flag by an '=' sign.
!   - val = The default value of the non-required option.  Is overwritten by the 
!     incoming command line argument if the flag is provided. 
!
!subroutine process_cli
!   - Evaluates the command line arguments, verifies that the program is being 
!     used correctly, and then assigns the values of all provide positional
!     and flagged arguments by the command line arguments.  For flags without
!     arguments, the value of the flag is either '' or '.true.' depending on
!     whether the flag is present.
! input:
!   - opt_c = The option container that contains the information about what is
!     expected on the cli
!
!subroutine print_help_screen
!   - Prints a standard format help screen to provide the user information
!     about how the program works.
! input
!   - opt_c = The option container that contains the information about what is
!     expected on the cli
!
!function pos_args_present
!   - Compares the epected number of positional arguments to the number of
!     positional arguments on the command line.  Returns .true. if they are the
!     same and .false. if they are not.
! input:
!   - opt_c = The option container that contains the information about what is
!     expected on the cli
!
!funciton count_pos_args
!   - Counts the positional arguments on the command line.
! input:
!   - opt_c = The option container that contains the information about what is
!     expected on the cli
!
!subroutine parse_arg
!   - Identifies if an '=' sign is present, and if it is,
!     separates the flag and value.
! input:
!   - arg = The command line arguments without the "-"'s
! output:
!   - has_eqsign = Logical variable telling whether the arg
!     has an equal sign (.true. = yes and .false. = no)
!   - p_arg = The argument prior to the equal sign.
!   - p_val = The argument after the equal sign.  If no
!     equal sign is present, then the empty string.
!
!function arg_has_eqsign
!   - Determines if the command line argument contains an equal
!     sign.  Returns .true. if yest and .false. if no.
! input:
!   - arg = The command line arguments without the "-"'s
!
!function opt_line
!   - Converts the option to a string for printing as part of the help screen.
! input:
!   - opt_c = The option container that contains the information about what is
!     expected on the cli
!-------------------------------------------------------------------------------
!SEARCH SUBROUTINES:
!   - The following subroutines retrieve the correct option index
!     for the option container's option array by the provided option attribute.
! common input:
!   - opt_c = The option container that contains the information about what is
!     expected on the cli
! common output:
!   - ierr = 0 if an option with the provided info is found; elsewise 1.
!   - opt_ind = the index of the found option
!   
!subroutine option_by_long
!   - Uses the option's long attribute as the key.
! specific input:
!   - long = string which will be compared to the options' long attribute until
!     the correct option is found
!   
!subroutine option_by_short
!   - Uses the option's short attribute as the key.
! specific input:
!   - short = character which will be compared to the options' short attribute until
!     the correct option is found
!   
!subroutine option_by_position
!   - Finds the pa_i'th required option.
! specific input:
!   - pa_i = integer which will be used to find the option
!-------------------------------------------------------------------------------
!
!
!INTENTION: PRIVATE SUBROUTINES (the subroutines that should be internal to
!this module)
!
!subroutine option_init
!   - Does the work for add_option.  Initializes the option with default 
!     and provided values
! input:
!  REQUIRED:
!   - help = A description of what the option or positional argument
!     is.  This description will appear when the help screen is printed (see 
!     print_help_screen below.)
!  OPTIONAL:
!   - long = The multi-character flag that appears after '--' on the command 
!     line that will indicate this option.  
!     If both long and short are not provided, the option will be assumed to 
!     be a required positional argument.
!   - short = The single-character flag that appears after '-' on the command
!     line that will indicate this option.
!     If both long and short are not provided, the option will be assumed to 
!     be a required positional argument.
!   - has_arg = Specifies if the flag has an argument.  Default is .true.
!     All required positional arguments have has_arg == .true.  The
!     argument can be either applied with a space between the flag or
!     separated from the flag by an '=' sign.
!   - val = The default value of the non-required option.  Is overwritten by the 
!     incoming command line argument if the flag is provided. 
!
!subroutine process_cli_arg
!   - Process a single command line argument and stores relevant data into opt_c.
! input:
!   - opt_c = The option container that contains the information about what is
!     expected on the cli
!   - arg = the text from the command line to be parsed
! output:
!   - ierr = 0 if everything went well, otherwise 1
!
!
!-------------------------------------------------------------------------------

    implicit none

    ! Portable declaration of stderr, stdin, stdout
#ifdef f2003
    use, intrinsic :: iso_fortran_env, only : input_unit=>stdin, &
        output_unit=>stdout, &
        error_unit=>stderr
#else
#define stdin  5
#define stdout 6
#define stderr 0
#endif

    type option_s
        character(len=80) :: long     ! Long name of the option (specified by '--')
        character         :: short    ! Option's short character (specified by '-')
        logical           :: has_arg  ! Option has an argument (.true./.false.)
        character(len=500):: help     ! Dialog that appears in the help screen.
        character(len=80) :: val      ! Value for option 
        logical           :: required ! Flag indicating that option is really a positional argument 
                                      ! needed for execution (.true./.false.)
    end type option_s

    type opt_container
        integer                                   :: N ! The number of options
        type(option_s), dimension(:), allocatable :: opts  ! Array of options
        ! The rest of the attributes are intended private to this module and should not be accessed externally.
        integer                                   :: current_index ! For adding options
        integer                                   :: pa_index ! For keeping track of positional arguments
        integer                                   :: opt_ind ! for reading options
        integer                                   :: pos_arg_N ! The number of positional arguments
        character(len=500)                        :: description ! Description of main prorgam...What the program does.
    end type opt_container


contains

    ! A subroutine to initialize the option container that contains programmer
    ! data to parse cli arguments.  Used in programming
    ! and will be compared to the command line.  The help 
    ! functionality is immediately added and should not be in the number N. 
    subroutine opt_container_init(opt_c, N)
      type(opt_container), intent(out) :: opt_c
      integer, intent(in) :: N

      opt_c%N = N + 1
      opt_c%current_index = 1
      opt_c%opt_ind = 1
      opt_c%pos_arg_N = 0
      allocate(opt_c%opts(opt_c%N))
      
      ! Nth element is reserved for help flag.
      call option_init(opt_c%opts(opt_c%N),'Prints this help screen.', 'help', 'h', &
                      .false., '')

    end subroutine opt_container_init


    ! A subroutine to fill in the information for a single option. 
    subroutine option_init(option_out, help, long, short, has_arg, val)
      type(option_s), intent(out)              :: option_out
      character(len=*), intent(in)             :: help
      character(len=*), intent(in), optional   :: long
      character, intent(in), optional          :: short
      logical, intent(in), optional            :: has_arg ! Default is .true.
      character(len=*), intent(in), optional   :: val ! Default for opt_arg

      if ( len(help) > 500 ) then 
        write(*,*) 'ARGPARSER ERROR: The help dialog argument needs to be less than 500 characters.' 
        stop
      end if
      option_out%help = help

      if ( present(long) ) then
        if ( len(long) > 80 ) then 
          write(*,*) 'ARGPARSER ERROR: The supplied long option argument needs to be less than 80 characters.' 
          stop
        end if
        option_out%long = long
      else
        option_out%long = ''
      end if

      if ( present(short) ) then
        option_out%short = short
      else
        option_out%short = ''
      end if

      if ( present(has_arg) ) then
        option_out%has_arg = has_arg
      else
        option_out%has_arg = .true.
      end if

      if ( present(val) ) then
        if ( len(long) > 80 ) then 
          write(*,*) 'ARGPARSER ERROR: The supplied val argument needs to be less than 80 characters.' 
          stop
        end if
        option_out%val = val
      else
        option_out%val = ""
      end if

      if ( option_out%short == '' .and. option_out%long == '' ) then
        option_out%required = .true.
      else
        option_out%required = .false.
      end if

    end subroutine option_init

    ! A subroutine to add an option to the container.  
    subroutine add_option(opt_c, help, long, short, has_arg, val)
      type(opt_container), intent(inout)      :: opt_c
      character(len=*), intent(in)            :: help
      character(len=*), intent(in), optional  :: long
      character, intent(in), optional         :: short
      logical, intent(in), optional           :: has_arg ! Default is .true.
      character(len=*), intent(in), optional  :: val ! Default for opt_arg

      if ( opt_c%current_index >= opt_c%N) then
        write (*,*) 'ARGPARSER: Attempting to add more options than allocated.'
        stop
      end if
      call option_init(opt_c%opts(opt_c%current_index), help, long=long, short=short, &
                       has_arg=has_arg, val=val)

      if ( opt_c%opts(opt_c%current_index)%long == '' .and. &
           opt_c%opts(opt_c%current_index)%short == '') then
        opt_c%pos_arg_N = opt_c%pos_arg_N + 1
      end if
      opt_c%current_index = opt_c%current_index + 1
    end subroutine add_option

    subroutine process_cli( opt_c )
        ! arguments
        type(opt_container), intent(inout) :: opt_c

        ! local variables
        character(len=256)                 :: arg
        integer                            :: ierr, cli_count

        cli_count = command_argument_count()
        opt_c%pa_index = 0
        ierr = 0
        if (.not. pos_args_present( opt_c ) ) then
          ierr = 1
        end if

        do while ( opt_c%opt_ind <= cli_count)
          call get_command_argument( opt_c%opt_ind, arg )
          if ( len_trim(arg) == 0 ) then
            exit
          else
            call process_cli_arg( opt_c, arg , ierr)
          end if
        end do
        if ( ierr == 1 ) then
          write(*,*) 'ARGPARSE ERROR: positional aruments not supplied.'
          write(*,*) 'Correct program usage follows:'
          write(*,*) ""
          call print_help_screen( opt_c )
          stop
        end if
    end subroutine process_cli

    subroutine process_cli_arg( opt_c, arg, ierr )
        ! arguments
        type(opt_container),   intent(inout):: opt_c
        character(len=*), intent(in)        :: arg
        integer, intent(out)                :: ierr

        ! local variables
        integer                          :: i = 0, jerr, len_arg, opt_ind
        logical                          :: has_eqsign 
        character(len=80)                :: p_arg, p_val

        len_arg = len_trim(arg)
        has_eqsign = .false.
        ierr = 0

        ! search for matching option
        if ( arg(1:1) /= '-' ) then
          opt_c%pa_index = opt_c%pa_index + 1
          call option_by_position( opt_c, opt_c%pa_index, jerr, opt_ind)
          opt_c%opts(opt_ind)%val = trim(arg)
          opt_c%opt_ind = opt_c%opt_ind + 1
          return
        else if ( arg(1:1) == '-' .and. arg(2:2) /= '-' ) then
          call parse_arg(arg(2:len_arg), has_eqsign, p_arg, p_val )
          call option_by_short( opt_c, p_arg, jerr, opt_ind)
        else 
          call parse_arg(arg(3:len_arg), has_eqsign, p_arg, p_val )
          call option_by_long( opt_c, p_arg, jerr, opt_ind)
        end if
        if (jerr == 1) then
          write(stderr, '(a,a,a)') "ARGPARSER ERROR: Unrecognized option '", arg(1:len_arg), "'"
          ierr = 1
        end if
        
        if ( opt_c%opts(opt_ind)%has_arg ) then
          if (has_eqsign) then ! long option has equal sign between value and option
            if (p_val == '') then !
              write(stderr, '(a,a,a)') "ARGPARSER ERROR: Option '", trim(arg), "' requires a value"
              ierr = 1
              return
            else
              opt_c%opts(opt_ind)%val = trim(p_val)
            endif
            opt_c%opt_ind = opt_c%opt_ind + 1
          else ! has no equal sign between value and option
            if ( opt_c%opt_ind + 1 <= command_argument_count()) then
               call get_command_argument( opt_c%opt_ind + 1, opt_c%opts(opt_ind)%val )
               opt_c%opt_ind = opt_c%opt_ind + 2
            else
              write(stderr, '(a,a,a)') "ERROR: Option '", trim(arg), "' requires a value"
              ierr = 1
              return
            end if
          endif
        else
          opt_c%opts(opt_ind)%val = ".true."
          opt_c%opt_ind = opt_c%opt_ind + 1
        endif
        return
    end subroutine process_cli_arg

    ! A function that returns true if the required (i.e. positional) arguments are provided
    logical function pos_args_present( opt_c )
      type(opt_container), intent(in) :: opt_c
      
      if ( count_pos_args( opt_c ) == opt_c%pos_arg_N ) then
        pos_args_present = .true.
        return
      end if
      pos_args_present = .false.
      return
    end function pos_args_present

    integer function count_pos_args( opt_c )
      type(opt_container), intent(in) :: opt_c
      type(option_s)                  :: option
      character(len=128)              :: arg, p_arg, p_val
      integer                         :: i, ierr, stat, len_arg, opt_ind
      logical                         :: has_eqsign

      i = 1
      count_pos_args = 0
      do while (i <= command_argument_count())
        call get_command_argument( i, value=arg, length=len_arg, status=stat)
        if ( stat /= 0) then
          write(stderr, '(a,a)') "Problem reading cli arguments.  Status of get_command_argument = '", stat
        end if
        if ( arg(1:1) /= '-' ) then
          count_pos_args = count_pos_args + 1
          i = i + 1
          exit
        else if ( arg(1:2) == '--' ) then
          call parse_arg(arg(3:len_arg), has_eqsign, p_arg, p_val )
          call option_by_long( opt_c, p_arg, ierr, opt_ind)
        else if ( arg(1:1) == '-' .and. arg(2:2) /= '-' ) then
          call parse_arg(arg(2:len_arg), has_eqsign, p_arg, p_val )
          call option_by_short( opt_c, p_arg, ierr, opt_ind)
        else 
          write(stderr, '(a,a)') "ARGPARSER ERROR: Arg not recognized: '", arg, '.  Aborting.'
          stop
        end if  

        if ( ierr == 1 ) then
          write(stderr, '(a,a)') "No option named '", p_arg, '.  Aborting.'
          stop
        endif
        if ( opt_c%opts(opt_ind)%has_arg .and. .not. has_eqsign ) then
          i = i + 2
        else
          i = i + 1
        end if
      end do
      
    end function count_pos_args

    subroutine parse_arg( arg, has_eqsign, p_arg, p_val)
      character(*), intent(in)       :: arg ! Without flags, i.e. - or --
      logical, intent(out)           :: has_eqsign ! Tells if the equal sign convention is used.
      character(len=80), intent(out) :: p_arg, p_val ! The parsed argument and optional value (if eqsign used).
      integer                       :: i, len_arg
   
      p_val = ''

      has_eqsign = arg_has_eqsign( arg )
      
      len_arg = len_trim(arg)
 
      if ( has_eqsign ) then
        do i=1, len_arg
            if (arg(i:i) == "=") then
                len_arg = i-1
                exit
            endif
        enddo
        p_val = arg(len_arg + 2:)
        if ( p_val == '' ) then ! no value (len_arg+2 value after "="
          write(stderr, '(a,a,a)') "ARGPARSER ERROR: Option '", trim(arg), "' requires a value"
          stop
        endif
      end if

      p_arg = arg(1:len_arg)
      
    end subroutine parse_arg

    ! Returns .true. if the arg contains an equal sign.
    logical function arg_has_eqsign( arg )
      character(len=128), intent(in) :: arg
      integer                        :: len_arg = 0, j 
      len_arg = len_trim(arg)
      arg_has_eqsign = .false.
      do j=1, len_arg
        if ( arg(j:j) == "=" ) then
          arg_has_eqsign = .true.
          exit
        endif
      enddo 
    end function arg_has_eqsign
 
    !Retrieves the option by long
    subroutine option_by_long( opt_c, long, ierr, opt_ind)
      type(opt_container), intent(in) :: opt_c
      character(len=80), intent(in)   :: long
      integer, intent(out)            :: ierr, opt_ind
      integer                         :: i

      ierr = 0
      do i = 1, opt_c%N
        if ( opt_c%opts(i)%long == long ) then
          opt_ind = i
          return
        end if
      end do

      ierr = 1
    end subroutine option_by_long

    !Retrieves the option by short
    subroutine option_by_short( opt_c, short, ierr, opt_ind)
      type(opt_container), intent(in) :: opt_c
      character, intent(in)           :: short
      integer, intent(out)            :: ierr, opt_ind
      integer                         :: i

      ierr = 0
      do i = 1, opt_c%N
        if ( opt_c%opts(i)%short == short ) then
          opt_ind = i
          return
        end if
      end do

      ierr = 1
    end subroutine option_by_short

    !Retrieves the required positional argument by placement
    subroutine option_by_position( opt_c, pa_i, ierr, opt_ind)
      type(opt_container), intent(in) :: opt_c
      integer, intent(in)             :: pa_i
      integer, intent(out)            :: ierr, opt_ind
      integer                         :: i, pos_count

      ierr = 0
      pos_count = 0
      do i = 1, opt_c%N
        if ( opt_c%opts(i)%required ) then
          pos_count = pos_count + 1
        end if
        if ( pos_count == pa_i ) then
          opt_ind = i
          return
        end if
      end do

      ierr = 1
    end subroutine option_by_position


    ! Automattically prints the help screen.
    subroutine print_help_screen(opt_c) ! 
      type(opt_container), intent(in) :: opt_c
      integer                         :: i, pos_count
      character(len=32)                :: pos_count_str

      write(*,*) "--------------------------------------------------------------------------------"
      write(*,*) ''
      write(*,*) 'Description of program:'
      write(*,*)  '  ' // trim(opt_c%description)
      write(*,*) ''
      write(*,*) "--------------------------------------------------------------------------------"
      write(*,*) ''
      write(*,*) 'REQUIRED POSITIONAL ARGUMENTS:'
      pos_count = 0
      do i = 1, opt_c%N, 1
        if ( opt_c%opts(i)%required ) then 
          pos_count = pos_count + 1
          write(pos_count_str,*) pos_count
          write(*,*)  "  " // trim(adjustl(pos_count_str)) // trim(opt_line(opt_c%opts(i)))
        end if
      end do
      write(*,*) ''
      write(*,*) 'OPTIONAL ARGUMENTS:'
      do i = 1, opt_c%N, 1
        if ( .not. opt_c%opts(i)%required ) then 
          write(*,*)  trim(opt_line(opt_c%opts(i)))
        end if  
      end do
      write(*,*) "--------------------------------------------------------------------------------"
    end subroutine print_help_screen

   ! Obtains a string for the help screen from the data stored in the option.
   pure function opt_line(option_in)
     type(option_s), intent(in) :: option_in
     character(700) :: opt_line
     if (option_in%short == '' .and. option_in%long == '') then
       opt_line = ""
     else if (option_in%short == '') then
       opt_line = ' --' // trim(adjustl(option_in%long))
     else if (option_in%long /= '') then
       opt_line = '  -' // option_in%short
       opt_line = trim(opt_line) // ', --' // trim(adjustl(option_in%long))
     end if
     opt_line = trim(opt_line) // ':  ' // trim(adjustl(option_in%help))
     if ( option_in%has_arg .and. option_in%val /= '' ) then
       opt_line = trim(opt_line) // '  DEFAULT=' // trim(adjustl(option_in%val))
     endif
   end function opt_line

end module f90argparser
