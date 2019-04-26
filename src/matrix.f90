    module matrix !module for printing and creating Matrix
        implicit none
        private
        public :: printMatrix, createMatrix

        interface printMatrix
            procedure print4, print8, print16
        end interface printMatrix

        contains

        function createMatrix (n) result (mx)
            implicit none
            integer, intent(in) :: n
            real, dimension (n, n) :: mx
            integer :: i, j
            do i = 1, n
                do j = 1, n
                    call RANDOM_NUMBER(mx(i, j))
                end do
            end do
        end function createMatrix


        subroutine print4(mx)
            implicit none
            real(kind = 4), dimension(:,:) :: mx
            integer :: i, j
            write(*,*)
            do i = 1, size(mx, 2)
                write(*,*) (mx(j,i), j = 1, size(mx, 1))
            end do
        end subroutine print4


		
        subroutine print8(mx)
            implicit none
            real(kind = 8), dimension(:,:) :: mx
            integer :: i, j
            write(*,*)
            do i = 1, size(mx, 2)
                write(*,*) (mx(j,i), j = 1, size(mx, 1))
            end do
        end subroutine print8

		
		
        subroutine print16(mx)
            implicit none
            real(kind = 16), dimension(:,:) :: mx
            integer :: i, j
            write(*,*)
            do i = 1, size(mx, 2)
                write(*,*) (mx(j,i), j = 1, size(mx, 1))
            end do
        end subroutine print16

    end module matrix
