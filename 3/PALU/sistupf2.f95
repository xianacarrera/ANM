subroutine sistupf2(n,a,b,u,ip)

    use mod_clreal

    implicit none

    !Declarar primero argumentos y luego las variables locales
    integer,intent(in)::                                   n !orden del S.E.L.
    real(kind=clreal),dimension(n),intent(out)::           u !solucion del sistema     
    real(kind=clreal),dimension(n,n),intent(in)::          a !matriz del S.E.L.
    real(kind=clreal),dimension(n),intent(inout)::         b !termino independiente del S.E.L.
    integer,dimension(n),intent(in)::                      ip !permutacion de filas

    !variables locales
    integer::i,j
    real(kind=clreal)::aux

    do i=n,1,-1
        aux=0._clreal
        do j=i+1,n
            aux=aux+a(ip(i),j)*u(j)
        end do
        u(i)=(b(i)-aux)/a(ip(i),i)
    end do
        

end subroutine sistupf2