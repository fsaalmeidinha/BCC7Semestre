program ola_20_soma_critical
     !$ use omp_lib
     implicit none
     integer,parameter::r8=kind(0.0d0)
     integer::x,num_proc1,num_proc2
     x=0
     !$OMP PARALLEL SHARED(X) 
     !$OMP CRITICAL 
        x = x + 1
     !$      num_proc1=omp_get_thread_num()
     !$      write(*,*)"fim thread critical  ",num_proc1
     !$OMP END CRITICAL 
     !$OMP END PARALLEL 
     write(*,*)"valor de x=",x
end program ola_20_soma_critical

