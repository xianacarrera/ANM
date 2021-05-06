subroutine residuo(m,n,a,b,u,r)
    !*****************************************
    !Calculo del residuo de un sistema R=Au-b
    !*****************************************

    use mod_clreal      !Modulo donde esta la clase definida de reales (clreal)

    implicit none

    !Argumentos
    integer, intent(in)::m        !Numero de filas de la matriz
    integer, intent(in)::n        !Numero de columnas de la matriz
    real(kind=clreal),intent(in)::a(m,n)    !Matriz del S.E.L.
    real(kind=clreal),intent(in)::b(m)      !Termino independiente
    real(kind=clreal),intent(in)::u(n)      !Solucion aproximada
    real(kind=clreal),intent(out)::r(m)     !Residuo

    !Variables locales
    !integer::i,j   !Forma 1
    integer::j      !Forma 2 y 3
    !real(kind=clreal)::aux      !Forma 1
    real(kind=clreal)::aux(m)    !Forma 2 y 3

    !Forma 1 de calcular el residuo
    !do i=1,m
    !    aux = 0._clreal
    !    do j=1,n
    !        aux = aux + a(i,j)*u(j)
    !    end do
    !    r(i) = aux-b(i)
    !end do

    !La forma de residuo2.f95 y residuo3.f95 es preferible, porque opera por columnas

    !Forma 2 de calcular el residuo
    aux=0._clreal       !Todos los elementos se ponen a 0
    do j=1,n
        aux = aux + a(:,j)*u(j)    
        !Columna j-esima por componente j de la solucion
    end do
    r = aux - b

    !Forma 3 de calcular el residuo
    !Se puede eliminar r como argumento de la subrutina y aprovechar b para 
    !almacenar el resultado
    !La diferencia aquí es que b sería inout, porque trae información desde el
    !programa principal y se devuelve con el resultado
    !do j=1,n
    !   b(:)=b(:)-a(:,j)*u(j)
    !end do
    !b=-b

end subroutine residuo