program ola_7_for
        !$ use omp_lib
        implicit none
        integer,parameter::r8=kind(1.0d0)
        integer,parameter::n=16
        integer::i
        real(kind=r8)::temp
        real(kind=r8),dimension(1:n)::a,b,c
        a=1.0_r8
        b=3.0_r8
        c=0.0_r8
        !$omp parallel do private(temp)
            do i=1,n
               temp=a(i)+b(i)
               c(i)=temp
               !$ write(*,*)"threads",omp_get_thread_num()
            enddo
        !$omp end parallel do
        write(*,*)
        write(*,*)"*******************************"
        write(*,*)
        do i=1,n
           write(*,*)"i,c(i)",i,c(i)
        enddo
end program ola_7_for
