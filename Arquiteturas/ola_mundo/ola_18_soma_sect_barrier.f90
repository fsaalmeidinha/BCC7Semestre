program teste_somavet
        !$ use omp_lib
        implicit none
        integer,parameter::r8=kind(0.0d0)
        integer,parameter::n=100000
        integer,parameter::nmax=10000
        integer::i,k,j
        integer::num_proc1,num_proc2
        real(kind=r8),dimension(1:n)::a,b,c
        real(kind=r8)::start,finish
        call cpu_time(start)
        do i=1,n
                a(i)=i*1.0_r8
                b(i)=a(i)
        enddo
        !$omp parallel shared(a,b,c,num_proc1,num_proc2) private(k)
        !$omp sections
        !$omp section
        i=0
        do
                i=i+1
                if(i>8*nmax) exit
                do k=1,n/2
                        c(k)=a(k)+b(k)
                enddo
        enddo
        !$      num_proc1=omp_get_thread_num()
        !$      write(*,*)"fim thread     ",num_proc1
        !$omp section
        i=0
        do
                i=i+1
                if(i>2*nmax) exit
                do k=(n/2)+1,n
                        c(k)=a(k)+b(k)
                enddo
        enddo
        !$      num_proc2=omp_get_thread_num()
        !$      write(*,*)"fim thread     ",num_proc2
        !$omp end sections nowait
        !$omp single
        !$      num_proc1=omp_get_thread_num()
        !$      write(*,*)"passou 1 thread",num_proc1
        !$      write(*,*)"***************"
        !$omp end single nowait 
        !$omp single
        !$      num_proc2=omp_get_thread_num()
        !$      write(*,*)"passou 2 thread",num_proc2
        !$omp end single
        !$omp barrier
        !$      write(*,*)"ola thread",omp_get_thread_num()
        !$      write(*,*)"ola thread",omp_get_thread_num()
        !$omp end parallel
        call cpu_time(finish)
        !$      write(*,*)"***************"
        write(*,*)"max(c),min(c)",maxval(c),minval(c)
        write(*,*)"Tempo",finish-start
end program teste_somavet

