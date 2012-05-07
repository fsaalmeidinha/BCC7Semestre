program teste_somavet
        !$ use omp_lib
        implicit none
        integer,parameter::r8=kind(0.0d0)
        integer,parameter::n=100000
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
        !$omp sections
        !$omp section
        i=0
        do
                i=i+1
                do k=1,n/2
                        c(k)=a(k)+b(k)
                enddo
                if(i>8*nmax) exit
        enddo
        !$      write(*,*)"fim thread     ",omp_get_thread_num()
        !$      write(*,*)"fim processador",omp_get_num_procs()
        !$      write(*,*)"max de threads ",OMP_get_max_threads()
        !$      write(*,*)"***************"
        !$omp section
        i=0
        do
                i=i+1
                do k=(n/2)+1,n
                        c(k)=a(k)+b(k)
                enddo
                if(i>2*nmax) exit
        enddo
        !$      write(*,*)"fim thread     ",omp_get_thread_num()
        !$      write(*,*)"fim processador",omp_get_num_procs()
        !$      write(*,*)"max de threads ",OMP_get_max_threads()
        !$      write(*,*)"***************"
        !$omp end sections
        !$omp end parallel
        call cpu_time(finish)
        write(*,*)"max(c),min(c)",maxval(c),minval(c)
        write(*,*)"Tempo",finish-start
end program teste_somavet

