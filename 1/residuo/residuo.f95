subroutine residuo(m,n,a,b,u,r)
    !*****************************************
    !Calculo del residuo de un sistema r=Au-b
    !*****************************************

    use mod_clreal      !Modulo donde esta la clase definida de reales (clreal)

    !Se pueden definir también clases para enteros y números complejos

    !La compilación se realiza de forma normal, no es necesario incluir referencias a mod_clreal (una vez este haya sido compilado y exista el archivo .mod en la carpeta, utilizando la opción -c para ese archivo)

    implicit none

    !Argumentos
    integer, intent(in)::m        !Numero de filas de la matriz
    integer, intent(in)::n        !Numero de columnas de la matriz
    real(kind=clreal),intent(in)::a(m,n)    !Matriz del S.E.L.
    real(kind=clreal),intent(in)::b(m)      !Termino independiente
    real(kind=clreal),intent(in)::u(n)      !Solucion aproximada
    real(kind=clreal),intent(out)::r(m)     !Residuo

    !Variables locales
    integer::i,j
    !integer::j
    real(kind=clreal)::aux
    !real(kind=clreal)::aux(m)

    do i=1,m
        aux = 0._clreal
        do j=1,n
            aux = aux + a(i,j)*u(j)
        end do
        r(i) = aux-b(i)
    end do

    !La forma de residuo2.f95 y residuo3.f95 es preferible, porque opera por columnas

    !residuo2
    !aux=0._clreal
    !do j=1,n
    !    aux = aux + a(:,j)*u(j)    
    !   Columna j-ésima por componente j de la solución
    !end do
    !r = aux - b

    !residuo3
    !Se puede eliminar r como argumento de la subrutina y aprovechar b para almacenar el resultado
    !La diferencia aquí es que b sería inout, porque trae información desde el programa principal y se devuelve con el resultado
    !do j=1,n
    !   b(:)=b(:)-a(:,j)*u(j)
    !end do
    !b=-b

end subroutine residuo