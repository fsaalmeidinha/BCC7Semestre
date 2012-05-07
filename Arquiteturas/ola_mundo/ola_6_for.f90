program ola_6_for
        !$ use omp_lib
        implicit none
        integer,parameter::r8=kind(1.0d0)
        integer,parameter::n=10
        integer::i
        real(kind=r8)::temp
        real(kind=r8),dimension(1:n)::a,b
        do i=1,n
           a(i)=(i-1)
        enddo
        b=2.0_r8*a
        write(*,*)
        write(*,*)"*******************************"
        write(*,*)
        !$omp parallel do private(temp) 
            do i=1,n
               temp=2.0_r8*a(i)
               call sleep(i) 
               a(i)=temp
            enddo
        !$omp end parallel do 
        write(*,*)
        write(*,*)"*******************************"
        write(*,*),maxval(abs(b-a))
end program ola_6_for
