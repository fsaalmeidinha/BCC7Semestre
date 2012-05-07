program ola_8_for
        !$ use omp_lib
        implicit none
        integer,parameter::r8=kind(1.0d0)
        integer,parameter::n=16
        !!!integer::i
        !!!real(kind=r8)::temp,soma
!Matrizes de multiplicacao
        real(kind=r8),dimension(1:n,1:n)::a,b,c
        
!Seta os vetores de peso
	!$omp parallel
        !$omp do
            do i=1,n
               temp=a(i)*a(i)
               b(i)=temp
               !$ write(*,*)"threads",omp_get_thread_num()
            enddo
        !$omp end do
        !$omp end parallel 



        !$omp parallel
        !$omp do
            do i=1,n
               temp=a(i)*a(i)
               b(i)=temp
               !$ write(*,*)"threads",omp_get_thread_num()
            enddo
        !$omp end do
        !$omp end parallel 
!        !$omp parallel 
        soma=0.0_r8
        !$omp parallel do
            do i=1,n
               soma=soma+b(i)
            enddo
        !$omp end parallel do
        write(*,*)"*******************************"
        write(*,*),sqrt(soma),maxval(b)
        write(*,*)"*******************************"
        write(*,*)
        do i=1,n
           write(*,*)"i,b(i)",i,b(i)
        enddo
end program ola_8_for
