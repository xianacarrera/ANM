subroutine sistl1(n, a, b, u)
    ! Resolución de un sistema triangular inferior con matriz diagonal de 1's
    use mod_clreal
    implicit none

    ! Se ha creado esta subrutina en lugar de usar sistl debido a que en la matriz a no disponemos de toda la matriz triangular inferior: no hemos almacenado los 1's de la diagonal.

    !Argumentos
    integer,intent(in)::n
    real(kind=clreal),intent(in)::a(n,n)
    real(kind=clreal),intent(in)::b(n)
    real(kind=clreal),intent(out)::u(n)

    !Variables locales
    integer::i,j
    real(kind=clreal)::aux

    u(1)=b(1)
    do i=2, n   
        aux=0._clreal
        do j=1,i-1
            aux = aux + a(i,j)*u(j)
        end do
        u(i) = (b(i)-aux)
    end do

    ! Forma más eficiente:
    !do i = 1, n
    !    u(i) = b(i)
        ! No divido b(i) por a(i,i), ya que a(i,i) es siempre 1
        ! Ahora que sé u para la columna i, le resto al término independiente de las siguientes filas el elemento de la matriz de cada fila y la columna i 
    !    b(i+1:n) = b(i+1:n) - a(i+1:n,i)*u(i)
        ! Ahora solo queda dividir b(i+1) por a(i+1,i+1) para obtener u(i+1)
    !end do 


end subroutine