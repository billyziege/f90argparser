program f90argparser_sample
    use f90argparser
    implicit none
    !character:: ch ! Unused: ch=argparser()

    ! START For longopts only
    type(opt_container):: opt_c
    type(option_s):: option
    integer :: i, cli_err
    character(len=32) :: i_str
    call opt_container_init(opt_c,3)
    opt_c%description = 'Test and demonstrate the functionality of f90argparser.'
    call add_option( opt_c, 'This is required.', has_arg = .true. )
    call add_option( opt_c, 'This is alpha.', long = 'alpha', has_arg = .false. )
    call add_option( opt_c, 'This is beta.', long='beta', short='b', has_arg = .true., val = '23.1' )
    ! END longopts

    ! If no options were committed
    if ( .not. pos_args_present( opt_c ) ) then
      write(*,*)  "ERROR: Incorrect usage of program.  Correct usage follows:"
      call print_help_screen(opt_c)
      stop
      !write (*,*) "Available Options: -a. --alpha -b X --beta=X --beta X"
    end if

    ! Process options one by one
    call process_cli( opt_c )

    do i = 1, opt_c%N
      option = opt_c%opts(i)
      if ( option%required ) then
        write(*,*) "Positional argument: " // trim(option%val) // " = " // trim(option%help)
      else
        write(*,*) "-"//option%short // ",--" // trim(option%long) // " = " // trim(option%val) // ":" // trim(option%help)
      end if
    end do
 
end program f90argparser_sample
