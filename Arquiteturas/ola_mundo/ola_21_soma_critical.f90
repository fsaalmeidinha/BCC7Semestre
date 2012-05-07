program ola_21_soma_critical
     !$ use omp_lib
     implicit none
     integer,parameter::r8=kind(0.0d0)
     integer,parameter::nmax=16
     integer::iglob,i,n,mycpu
     integer,dimension(1:nmax)::w
     integer,dimension(1:nmax)::cpuid
    integer,dimension(1:nmax)::peso
     real(kind=r8)::aux
     real (kind = r8) :: a(nmax*nmax, nmax*nmax), b(nmax*nmax, nmax*nmax), c(nmax*nmax,nmax*nmax)
     do i = 1, nmax
        call random_number( aux )
        w(i) = int( 100 * aux ) + 1
     end do
     call random_number(a)
     call random_number(b)
     !$OMP PARALLEL private(i,mycpu)
     !$      mycpu=omp_get_thread_num()
     !$omp single
     iglob=0
     !$omp end single
     do
     !$OMP CRITICAL 
       iglob=iglob+1
       i=iglob
     !$OMP END CRITICAL
            if(i>nmax)exit
            call work(i,w,a,b,mycpu,cpuid,peso)
     enddo
     !$OMP END PARALLEL 
     do i=1,nmax
        write(*,*)"cpuid",i,cpuid(i),peso(i)
     enddo
contains
  subroutine work(row,w,a,b,mycpu,cpuid,peso)
    implicit none
    integer, intent(in) :: row
    integer :: i, j, k,mycpu
    integer,dimension(1:nmax)::w
    integer,dimension(1:nmax)::cpuid
    integer,dimension(1:nmax)::peso
    real (kind = r8) :: a(nmax*nmax, nmax*nmax), b(nmax*nmax, nmax*nmax), c(nmax*nmax,nmax*nmax)
    cpuid(row)=mycpu
    peso(row)=w(row)
    do i = 1, w(row)
       c = matmul(a, b)
    end do
    return
  end subroutine work
end program ola_21_soma_critical

