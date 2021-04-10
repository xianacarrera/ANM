subroutine sistl(n,a,b,u)

    use mod_clreal

    implicit none

    !Declarar primero argumentos y luego las variables locales
    integer,intent(in)::n
    real(kind=clreal),intent(out)::u(n)
    !real(kind=clreal),intent(in)::a(n,n),b(n)
    real(kind=clreal),intent(in)::a(n,n)
    real(kind=clreal),intent(inout)::b(n)

    !integer::i,j
    !real(kind=clreal)::aux
    integer::i

    !u(1)=b(1)/a(1,1)
    !do i=2, n   
    !    aux=0.
    !    do j=1,i-1
    !        aux = aux + a(i,j)*u(j)
    !    end do
    !    u(i) = (b(i)-aux)/a(i,i)
    !end do

    ! Forma más eficiente:
    do i = 1, n
        u(i) = b(i)/a(i,i)
        ! Ahora que sé u para la columna i, le resto al término independiente de las siguientes filas el elemento de la matriz de cada fila y la columna i 
        b(i+1:n) = b(i+1:n) - a(i+1:n,i)*u(i)
        ! Ahora solo queda dividir b(i+1) por a(i+1,i+1) para obtener u(i+1)
    end do 

end subroutine sistl