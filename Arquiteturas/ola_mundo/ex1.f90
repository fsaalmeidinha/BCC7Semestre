program ex1

 	!$ use omp_lib
        !implicit none
        integer,parameter::r8=kind(1.0d0)
        integer,parameter::n=16
	!Peso 1, sera lido do console
	integer,parameter::p1=5
	!Peso 2, sera lido do console
	integer,parameter::p2=15
        integer::i
        integer::j
        !!!real(kind=r8)::temp,soma
	!Matrizes de multiplicacao
        real(kind=r8),dimension(1:n,1:n)::a,b,c


        !$omp parallel
        !$omp do
            do i=1,n
		do j=1,n
			call random_number( a(i,j) )
			call random_number( b(i,j) )
            	enddo
            enddo
        !$omp end do
        !$omp end parallel 

        write(*,*)"*******************************"
        write(*,*),a !sqrt(soma),maxval(b)
        write(*,*)"*******************************"

        write(*,*)"*******************************"
        write(*,*),b !sqrt(soma),maxval(b)
        write(*,*)"*******************************"
        
end program ex1
