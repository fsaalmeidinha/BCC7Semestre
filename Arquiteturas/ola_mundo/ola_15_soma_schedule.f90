program teste_somavet
        !$ use omp_lib
        implicit none
        integer,parameter::r8=kind(0.0d0)
        integer,parameter::n=100000
        integer,parameter::chunk=10000
!      integer,parameter::chunk=1
        integer,parameter::nmax=10000
        integer::i,k,j
        real(kind=r8),dimension(1:n)::a,b,c
        real(kind=r8)::start,finish
        call cpu_time(start)
        do i=1,n
                a(i)=i*1.0_r8
                b(i)=a(i)
        enddo
        !$omp parallel shared(a,b,c) private(k)
        !$ write(*,*)"inicio thread",omp_get_thread_num()
        call sleep(5)
        i=0
        do
                i=i+1
                if(i>8*nmax) exit
                !$omp do schedule(dynamic,chunk)
                do k=1,n
                        c(k)=a(k)+b(k)
                enddo
                !$omp end do nowait
        enddo
        !$omp end parallel
        write(*,*)"passou"
        call cpu_time(finish)
        write(*,*)"max(c),min(c)",maxval(c),minval(c)
        write(*,*)"Tempo",finish-start
end program teste_somavet

