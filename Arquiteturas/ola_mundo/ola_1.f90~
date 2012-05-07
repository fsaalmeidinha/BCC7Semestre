program ola_1
!$ use omp_lib
        !implicit none
        integer,parameter::r8=kind(1.0d0)
        integer,parameter::n=16
	!Peso 1, sera lido do console
	integer,parameter::p1=10
	!Peso 2, sera lido do console
	integer,parameter::p2=50
        integer::i
	integer::rangeRandom=100
	integer,parameter::sizeRandom=100
        !!!real(kind=r8)::temp,soma
	!Matrizes de multiplicacao
        real,dimension(1:sizeRandom)::vetorPesos
        real(kind=r8),dimension(1:n,1:n)::a,b,c

        !$omp parallel
        !$omp do schedule(dynamic)
            do i=1,sizeRandom
		call random_number( vetorPesos(i) )
            enddo
        !$omp end do

	!$omp do schedule(dynamic)
            do i=1,n
		do j=1,n
			call random_number( a(i,j) )
			call random_number( b(i,j) )
            	enddo
            enddo
        !$omp end do
	!&omp single
	vetorPesos = vetorPesos*rangeRandom
	!&omp end single
	!$omp do schedule(dynamic)
	do i=1,p1
		do j=1,p2
			do k=1,sizeRandom
				call work(vetorPesos(k),a,b,c,n)
			enddo
		enddo
	enddo
	!$omp end do
        !$omp end parallel

        write(*,*),c
        
contains
  subroutine work(peso,a,b,c,n)
	real::peso
	real(kind=r8),dimension(:,:)::a,b,c
	integer::n

	do pLinha=1, ifix(peso)
	c=0
		do iLinha=1,n
			do jLinha=1,n
				do kLinha=1,n
					c(iLinha,jLinha) = c(iLinha,jLinha) + (a(iLinha,kLinha) * b(kLinha,jLinha))
				enddo
			enddo
		enddo
	enddo
	
    return
  end subroutine work

end program ola_1
