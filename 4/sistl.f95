subroutine sistl(n,a,b,u)
    !*********************************************
    !Subrutina de resolucion de un sistema con matriz triangular inferior
    !********************************************

    use mod_clreal

    implicit none

    !argumentos
    integer,intent(in)::n
    real(kind=clreal),intent(out)::u(n)
    real(kind=clreal),intent(in)::a(n,n)
    real(kind=clreal),intent(in)::b(n)

    !variables locales
    integer::i,j
    real(kind=clreal)::aux

    u(1)=b(1)/a(1,1)
    do i=2, n   
        aux=0._clreal
        do j=1,i-1
            aux = aux + a(i,j)*u(j)
        end do
        u(i) = (b(i)-aux)/a(i,i)
    end do

    !        aux=0.
!        do j=1, i-1
!            aux=aux+a(i,j)*u(j)
!        end do
!        u(i)=(b(i)-aux)/a(i,i)
end subroutine sistl