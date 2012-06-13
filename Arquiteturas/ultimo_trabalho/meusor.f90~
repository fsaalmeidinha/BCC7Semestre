Program sor_red_black_serial

  implicit none
include 'mpif.h'

  integer, parameter :: r8 = kind(1.0d0)
  integer, parameter :: maxiter=80000
  real(kind=r8)::pi=4.0d0*atan(1.0_r8)
  real(kind=r8), dimension(:,:), allocatable:: un
  real(kind=r8), dimension(:,:), allocatable:: uo 
  real(kind=r8), dimension(:,:), allocatable:: ue
  real(kind=r8) :: start_time, end_time
  real(kind=r8) :: tol,erro,gerro
  integer ::n,iter,m,mp,abaixo,acima
integer ::k,p,ierr

call mpi_init(ierr)
call mpi_comm_rank(mpi_comm_world, k, ierr)      
call mpi_comm_size(mpi_comm_world, p, ierr)  

if(k==0) then
  write(*,*)'entre com a particao n:'
  read(*,*)n
endif

call mpi_bcast(n,1,mpi_integer,0,mpi_comm_world,ierr)
call vizinho(k,abaixo,acima,p)

  m=n+1
mp=n/p
  tol=0.0000000001
  allocate(uo(0:mp+1,0:m))
  allocate(ue(0:mp+1,0:m))
  uo=0.0
  ue=0.0

  call est_inic(uo,n,mp+1)
  call cond_front(uo,m,mp+1,k,p)
!if(k==1) write(*,*)"uo: ",uo(1,1)
!call sleep(3)
  call u_exata(ue,m,mp+1)
iter=0
 do
  call sor_red_black(uo,m,mp+1)
!if(k==1) write(*,*)"uo: ",uo(1,1)

erro=maxval(dabs(uo-ue))
!write(*,*) "uo: ",uo(1,5),"uo: ",uo(2,5),"uo: ",uo(3,4),"uo: ",uo(3,3)
call mpi_allreduce(erro,gerro,1,mpi_double_precision,mpi_max,  &
                mpi_comm_world,ierr )

       if((gerro.le.tol).or.(iter.ge.maxiter)) exit
       iter=iter+1
       if (k==0) then         
       if(mod(iter,10)==0) write(*,*)"iter:",iter," erro:" ,gerro
       endif
       call atualiza_cf(uo, m, mp, k, abaixo, acima)
 write(*,*),"erro:",erro," k:",k
call sleep(1)
enddo

if(k==0) then
  call escreve_u(uo,m)
endif
  deallocate(uo)
  deallocate(ue)
  call mpi_finalize(ierr)
contains
  !
  subroutine sor_red_black(uo,m,n)

  implicit none
  integer::i,j,n,k,ix,m
  real(kind=r8)::aux,omega,rho
  real(kind=r8),dimension(0:n,0:m)::uo

!write(*,*)"n:",n," m:",m
!call sleep(3)
  k=0
!  erro=maxval(dabs(uo-ue))
  rho=1-(0.5*pi/n)**2

       if(k==0)then
               omega=1.0_r8
       elseif(k==1)then
               omega=1.0_r8/(1.0_r8-0.5*rho**2)
       elseif(k==2)then
               omega=1.0_r8/(1.0_r8-0.25*omega*rho**2)
       else
               omega=1.0_r8/(1.0_r8-0.25*omega**rho**2)
       endif
!        uo=u

!if(k==0) write(*,*)"uo:",uo(2,3)
        do i=1,n-1
                do j=1,m-1
                        aux=0.25_r8*(uo(i-1,j)+uo(i,j-1)+uo(i+1,j)+uo(i,j+1))
                        uo(i,j)=omega*aux+(1.0_r8-omega)*uo(i,j)
                enddo

!ix=2*i-1
!                ix=2*i-1
!                do j=1,m-1
!                        aux=0.25_r8*(uo(ix-1,j)+uo(ix,j-1)+uo(ix+1,j)!+uo(ix,j+1))
!                        uo(i,j)=omega*aux+(1.0_r8-omega)*uo(i,j)
!                enddo
	k=k+1
        enddo
!if(k==0) write(*,*)"uo:",uo(2,3)

!!        erro=maxval(dabs(u-ue))
!        erro=maxval(dabs(u-uo))
!        if((erro.le.tol).or.(k.ge.maxiter)) exit
!        k=k+1
!        write(*,*)k,erro
!        write(19,*)k,erro

!        write(*,*)omega,rho
  return
  end subroutine sor_red_black
  !
  subroutine cond_front(u,m,n,k,p)
  implicit none
  integer::n,i,k,p,m
  real(kind=r8)                   ::h
  real(kind=r8),dimension(0:n,0:m)::u

  h=1.0_r8/n
  u(0,0:n)=0.0_r8
  u(n,0:n)=0.0_r8

if(p == 1) then
  do i=0,n
        u(i,0)=sin(pi*i*h)
        u(i,n)=sin(pi*i*h)*exp(-pi)
  enddo
else
	if (k == 0) then
		do i=0,n
			u(i,0)=sin(pi*i*h)
		enddo
	endif
	if (k == p-1) then
		do i=0,n
			u(i,n)=sin(pi*i*h)*exp(-pi)
		enddo
	endif
endif

  return 
  end subroutine cond_front
  !
  subroutine est_inic(u,n,mp)
  implicit none
  integer::n,mp
  real(kind=r8),dimension(0:mp,0:n+1)::u
  u=0.0_r8
  call random_number(u(1:mp-1,0:n+1))

  return 
  end subroutine est_inic
  !
  subroutine escreve_u(u,n)
  implicit none
  integer::n,i,j
  real(kind=r8)                   ::h
  real(kind=r8),dimension(0:n,0:n)::u
  h=1.0_r8/n
  do i=0,n
        do j=0,n
                write(9,*)i*h,j*h,u(i,j)
        enddo
        write(9,*)
  enddo
  return 
  end subroutine escreve_u
  !
  subroutine u_exata(u,n,mp)
  implicit none
  integer::n,i,j,mp
  real(kind=r8)                   ::h
  real(kind=r8),dimension(0:mp,0:n)::u

  h=1.0_r8/n
  do i=0,n
        do j=0,mp
                u(i,j)=sin(pi*i*h)*exp(-pi*j*h)
!                u(i,j)=0.0_r8
        enddo
  enddo

  return 
  end subroutine u_exata
  !
subroutine vizinho(k,abaixo,acima,p)
    implicit none
    integer::k,abaixo,acima,p
    if(k==0) then
       abaixo= -1
       acima = k+1
    elseif(k==p-1) then
       abaixo= k-1
       acima = -1
    else
       abaixo=k-1
       acima =k+1
    endif
  end subroutine vizinho
  !
  subroutine atualiza_cf(uo, m, mp, k, abaixo, acima)
    implicit none
    integer :: m, mp, k, ierr, abaixo, acima,j
    real(kind=r8), dimension(0:m,0:mp+1) :: uo
    integer status(mpi_status_size)

    if(p>1) then
    if(k.lt.p-1)then
    call mpi_send( uo(0:m,mp), m+1, mpi_double_precision, acima, 0,  &
               mpi_comm_world, ierr)
    call mpi_recv( uo(0:m,mp+1), m+1 , mpi_double_precision, acima, 1,  &
              mpi_comm_world, status, ierr)
    endif
    if(k.gt.0)then
    call mpi_recv( uo(0:m,0), m+1, mpi_double_precision, abaixo, 0,  &
             mpi_comm_world, status, ierr)
    call mpi_send( uo(0:m,1  ), m+1, mpi_double_precision, abaixo, 1,  &
            mpi_comm_world, ierr)
    endif
    endif
    return
    end subroutine atualiza_cf
!
END PROGRAM sor_red_black_serial
