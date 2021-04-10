subroutine residuo(m,n,a,b,u,r)
    !*****************************************
    !Cálculo del residuo de un sistema R=Au-b
    !*****************************************

    use mod_clreal      !Módulo donde está la clase definida de reales (clreal)

    !Se pueden definir también clases para enteros y números complejos

    !La compilación se realiza de forma normal, no es necesario incluir referencias a mod_clreal (una vez este haya sido compilado y exista el archivo .mod en la carpeta, utilizando la opción -c para ese archivo)

    implicit none

    integer,intent(in)::m,n
    real(kind=clreal),intent(in)::a(m,n),b(m),u(n)
    real(kind=clreal),intent(out)::r(m)

    !integer::i,j
    integer::j
    !real::aux
    real(kind=clreal)::aux(m)

    !do i=1,m
    !    aux = 0.
    !    do j=1,n
    !        aux = aux + a(i,j)*u(j)
    !    end do
    !    r(i) = aux-b(i)
    !end do

    !La forma de residuo2.f95 y residuo3.f95 es preferible, porque opera por columnas

    aux=0.
    do j=1,n
        aux = aux + a(:,j)*u(j)    
        !Columna j-ésima por componente j de la solución
    end do
    r = aux - b

    !Se puede eliminar r como argumento de la subrutina y aprovechar b para almacenar el resultado
    !La diferencia aquí es que b sería inout, porque trae información desde el programa principal y se devuelve con el resultado
    !do j=1,n
    !   b(:)=b(:)-a(:,j)*u(j)
    !end do
    !b=-b

end subroutine residuo