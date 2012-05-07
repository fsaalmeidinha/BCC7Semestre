program ola_9_somamat
        !$ use omp_lib
        implicit none
        integer,parameter::r8=kind(1.0d0)
        integer,parameter::n=16
        integer,parameter::nmax=1000
        integer::i,j,k,iglob,ncpu,mycpu,dime,cont
        integer,dimension(1:nmax)::w
!        real(kind=r8),dimension(1:n)::a,b,c,d,e,f,s
        real(kind=r8),dimension(1:n*n,1:n*n)::a,b,s
        real(kind=r8)::start,finish
        real(kind=r8)::aux,t1,t2
        real(kind=r8),dimension(0:7)::tempo
        real(kind=r8),dimension(0:7)::cpuid
        dime=n*n
        ncpu=1
        mycpu=0
        do i = 1, nmax
             call random_number( aux )
             w(i) = int( 100 * aux ) + 1
             write(*,*)i,w(i)
        enddo
        a=1.0_r8
        b=2.0_r8
        s=0.0_r8
        !$omp parallel private(mycpu,i)
        !$  mycpu = omp_get_thread_num()
        !$  ncpu  = omp_get_num_threads()
        call cpu_time(start)
        i=0
        do
            i=i+1
            if(i>nmax) exit
             !$omp do private(j)
              do j=1,dime
                  do cont=1,nmax
                       do k=1,dime
                          s(j,k)=a(j,k)+b(j,k)
                       enddo
                  enddo
             enddo
             !$omp end do
        enddo 
        !$omp end parallel
        call cpu_time(finish)
        write(*,*)"tempo final",finish-start
        write(*,*)"max,min",maxval(s),minval(s)
!       write(*,*)"s",s(1,1)
end program ola_9_somamat
