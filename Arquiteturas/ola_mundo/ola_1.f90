program ola_1
!$ use omp_lib
        !implicit none
        integer,parameter::r8=kind(1.0d0)
        integer,parameter::n=16
	!Peso 1, sera lido do console
	integer,parameter::p1=5
	!Peso 2, sera lido do console
	integer,parameter::p2=15
        integer::i
	integer::rangeRandom=100
	integer,parameter::sizeRandom=100
        !!!real(kind=r8)::temp,soma
	!Matrizes de multiplicacao
        real,dimension(1:sizeRandom)::vetorPesos
        real(kind=r8),dimension(1:n,1:n)::a,b,c

        !$omp parallel
        !$omp do
            do i=1,sizeRandom
		call random_number( vetorPesos(i) )
            enddo
        !$omp end do
        !$omp end parallel 
	vetorPesos = vetorPesos*rangeRandom

        write(*,*)"*******************************"
        !write(*,*),vetorPesos !sqrt(soma),maxval(b)
        write(*,*)"*******************************"
	call work(p1,p2,vetorPesos(1),a,b,c,n)
        !write(*,*)"*******************************"
        !write(*,*),b !sqrt(soma),maxval(b)
        !write(*,*)"*******************************"

contains
  subroutine work(p1,p2,peso,a,b,c,n)
	integer::p1
	integer::p2
	real::peso
	real(kind=r8),dimension(:,:)::a,b,c
	integer::n

	!$omp parallel
        !$omp do
		do i=1,p1

			do j=1,p2

				

			enddo

		enddo
        !$omp end do
        !$omp end parallel 


    return
  end subroutine work

end program ola_1
