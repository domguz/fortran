    program main
        use matrix
        use naivemath
        use bettermath
        use dotmath


        implicit none


        character(len = :), allocatable :: fn
		print *, "kind 4"
        fn = '../res/kind4.dat'
        print *, measure4(fn)
		print *, "kind 8"
        fn = '../res/kind8.dat'
        print *, measure8(fn)
		print *, "kind 16"
        fn = '../res/kind16.dat'
        print *, measure16(fn)
		
		call RANDOM_SEED()

        contains

        function measure4(fn) result (r)
            implicit none
            character(len = :), intent(in), allocatable :: fn
            real(kind = 4), allocatable, dimension(:, :) :: A, B, C
            real :: tstart, tstop, ntime, btime, dtime, mtime
            integer :: n, i,r
            n = 10
            open(unit = 1, file = fn)
            write (1,*) '       N   ', '   naive  ', '   better    ', '   dotmull    ' , '    matmull'
            do i = 1, 7
                n = 2 * n
                A = createMatrix(n)
                B = createMatrix(n)
                call CPU_TIME(tstart)
                C = naive(A, B)
                call CPU_TIME(tstop)
                ntime = tstop-tstart
                call CPU_TIME(tstart)
                C = better(A, B)
                call CPU_TIME(tstop)
                btime = tstop-tstart
                call CPU_TIME(tstart)
                C = dotmull(A, B)
                call CPU_TIME(tstop)
                dtime = tstop-tstart
                call CPU_TIME(tstart)
                C = matmul(A, B)
                call CPU_TIME(tstop)
                mtime = tstop-tstart
                write (1,*) n, ntime, btime, dtime, mtime
            end do
            close(1)
			r=0
        end function measure4

        function measure8(fn) result (r)
            implicit none
            character(len = :), intent(in), allocatable :: fn
            real(kind = 8), allocatable, dimension(:, :) :: A, B, C
            real :: tstart, tstop, ntime, btime, dtime, mtime
            integer :: n, i,r
            n = 10
            open(unit = 1, file = fn)
            write (1,*) '        N   ', '   naive    ', '   better    ', '   dotmull    ' , '    matmull'
            do i = 1, 7
                n = 2 * n
                A = createMatrix(n)
                B = createMatrix(n)
                call CPU_TIME(tstart)
                C = naive(A, B)
                call CPU_TIME(tstop)
                ntime = tstop-tstart
                call CPU_TIME(tstart)
                C = better(A, B)
                call CPU_TIME(tstop)
                btime = tstop-tstart
                call CPU_TIME(tstart)
                C = dotmull(A, B)
                call CPU_TIME(tstop)
                dtime = tstop-tstart
                call CPU_TIME(tstart)
                C = matmul(A, B)
                call CPU_TIME(tstop)
                mtime = tstop-tstart
                write (1,*) n, ntime, btime, dtime, mtime
            end do
            close(1)
			r=0
        end function measure8

        function measure16(fn) result (r)
            implicit none
            character(len = :), intent(in), allocatable :: fn
            real(kind = 16), allocatable, dimension(:, :) :: A, B, C
            real :: tstart, tstop, ntime, btime, dtime, mtime
            integer :: n, i,r
            n = 10
            open(unit = 1, file = fn)
            write (1,*) '        N   ', '   naive    ', '   better    ', '   dotmull    ' , '    matmull'
            do i = 1, 7
                n = 2 * n
                A = createMatrix(n)
                B = createMatrix(n)
                call CPU_TIME(tstart)
                C = naive(A, B)
                call CPU_TIME(tstop)
                ntime = tstop-tstart
                call CPU_TIME(tstart)
                C = better(A, B)
                call CPU_TIME(tstop)
                btime = tstop-tstart
                call CPU_TIME(tstart)
                C = dotmull(A, B)
                call CPU_TIME(tstop)
                dtime = tstop-tstart
                call CPU_TIME(tstart)
                C = matmul(A, B)
                call CPU_TIME(tstop)
                mtime = tstop-tstart
                write (1,*) n, ntime, btime, dtime, mtime
            end do
            close(1)
			r=0
        end function measure16

    end program main

