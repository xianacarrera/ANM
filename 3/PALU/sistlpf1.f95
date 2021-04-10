subroutine sistlpf1(n,a,b,u,ip)
    !***********************************
    use mod_clreal

    implicit none

    !argumentos
    integer,intent(in)::                                n !orden del S.E.L.
    real(kind=clreal),dimension(n,n),intent(in)::       a !matriz del S.E.L.
    real(kind=clreal),dimension(n),intent(in)::         b !termino independiente del S.E.L.
    real(kind=clreal),dimension(n),intent(out)::        u !solucion del S.E.L.
    integer,dimension(n),intent(in)::                   ip !permutacion de filas

    !variables locales
    integer::i,j
    real(kind=clreal)::aux

    u(1)=b(ip(1))
    do i=2,n
        aux=0.
        do j=1,i-1
        aux=aux+a(ip(i),j)*u(j)
        end do
        u(i)=b(ip(i))-aux
    end do

end subroutine sistlpf1