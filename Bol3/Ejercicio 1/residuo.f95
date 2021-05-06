subroutine residuo(m,n,a,b,u,r)
    !*****************************************
    !Cálculo del residuo de un sistema R=Au-b
    !*****************************************

    use mod_clreal      !Módulo donde está la clase definida de reales (clreal)
    implicit none

    !Argumentos
    integer,intent(in)::                             m   !Filas de la matriz
    integer,intent(in)::                             n   !Columnas de la matriz
    real(kind=clreal),dimension(n,n),intent(in)::    a   !Matriz del S.E.L.
    real(kind=clreal),dimension(n),intent(in)::      b   !Termino independiente
    real(kind=clreal),dimension(n),intent(in)::      u   !Solucion aproximada
    real(kind=clreal),dimension(n),intent(out)::     r   !Residuo

    !Variables locales
    integer::i,j
    real(kind=clreal)::aux  

    do i=1,m
        aux = 0.
        do j=1,n
            aux = aux + a(i,j)*u(j)
        end do
        r(i) = aux-b(i)
    end do

    !2ª forma posible
    !aux, en este caso, es un vector
    !aux=0._clreal
    !do j=1,n
    !    aux = aux + a(:,j)*u(j)    
        !Columna j-ésima por componente j de la solución
    !end do
    !r = aux - b

    !3ª forma posible
    !La diferencia aquí es que b sería inout, porque trae información desde el programa principal y se devuelve con el resultado
    !do j=1,n
    !   b(:)=b(:)-a(:,j)*u(j)
    !end do
    !b=-b

end subroutine residuo