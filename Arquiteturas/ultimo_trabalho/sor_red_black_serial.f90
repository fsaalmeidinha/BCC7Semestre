Program sor_red_black_serial

  implicit none
  integer, parameter :: r8 = kind(1.0d0)
  integer, parameter :: maxiter=80000
  real(kind=r8)::pi=4.0d0*atan(1.0_r8)
  real(kind=r8), dimension(:,:), allocatable:: un
  real(kind=r8), dimension(:,:), allocatable:: uo  
  real(kind=r8), dimension(:,:), allocatable:: ue 
  real(kind=r8), dimension(:,:), allocatable:: er 
  real(kind=r8) :: start_time, end_time
  real(kind=r8) :: tol
  integer ::n,iter,m
  write(*,*)'entre com a particao n:'
  read(*,*)n
  m=n+1
  tol=0.0000000001
  allocate(uo(0:m,0:m))
  allocate(ue(0:m,0:m))
  allocate(er(0:m,0:m))
  uo=0.0
  ue=0.0
  er=0.0
  call est_inic(uo,m)
  call cond_front(uo,m)
  call u_exata(ue,m)
  call cpu_time(start_time) 
  call sor_red_black(uo,ue,m,tol)
  call cpu_time(end_time) 
  write(*,*)'Tempo total de CPU =',end_time - start_time
  er=dabs(uo-ue)
  call escreve_u(uo,m)
  deallocate(uo)
  deallocate(ue)
  deallocate(er)
contains
  !
  subroutine sor_red_black(u,ue,n,tol)
  implicit none
  integer::i,j,n,k,ix
  real(kind=r8)::erro,tol,aux,omega,rho
  real(kind=r8),dimension(0:n,0:n)::u,ue,uo
  k=0
  call cond_front(u,n)
  erro=maxval(dabs(uo-ue))
  write(19,*)k,erro
  write(*,*)k,erro
  rho=1-(0.5*pi/n)**2
  do
       if(k==0)then
               omega=1.0_r8
       elseif(k==1)then
               omega=1.0_r8/(1.0_r8-0.5*rho**2)
       elseif(k==2)then
               omega=1.0_r8/(1.0_r8-0.25*omega*rho**2)
       else
               omega=1.0_r8/(1.0_r8-0.25*omega**rho**2)
       endif
        uo=u
        do i=1,n-1
                ix=2*i-1
                do j=1,n-1
                        aux=0.25_r8*(u(ix-1,j)+u(ix,j-1)+u(ix+1,j)+u(ix,j+1))
                        u(i,j)=omega*aux+(1.0_r8-omega)*u(i,j)
                enddo
        enddo
!        erro=maxval(dabs(u-ue))
        erro=maxval(dabs(u-uo))
        if((erro.le.tol).or.(k.ge.maxiter)) exit
        k=k+1
        write(*,*)k,erro
        write(19,*)k,erro
  enddo
        write(*,*)omega,rho
  return
  end subroutine sor_red_black
  !
  subroutine cond_front(u,n)
  implicit none
  integer::n,i
  real(kind=r8)                   ::h
  real(kind=r8),dimension(0:n,0:n)::u
  h=1.0_r8/n
  u(0,0:n)=0.0_r8
  u(n,0:n)=0.0_r8
  do i=0,n
        u(i,0)=sin(pi*i*h)
        u(i,n)=sin(pi*i*h)*exp(-pi)
!         u(i,0)=0.0_r8
!         u(i,n)=0.0_r8
  enddo
  return 
  end subroutine cond_front
  !
  subroutine est_inic(u,n)
  implicit none
  integer::n
  real(kind=r8),dimension(0:n,0:n)::u
  u=0.0_r8
  call random_number(u(1:n-1,1:n-1))
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
  subroutine u_exata(u,n)
  implicit none
  integer::n,i,j
  real(kind=r8)                   ::h
  real(kind=r8),dimension(0:n,0:n)::u
  h=1.0_r8/n
  do i=0,n
        do j=0,n
                u(i,j)=sin(pi*i*h)*exp(-pi*j*h)
!                u(i,j)=0.0_r8
        enddo
  enddo
  return 
  end subroutine u_exata
  !
END PROGRAM sor_red_black_serial
