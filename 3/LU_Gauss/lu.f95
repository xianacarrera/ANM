subroutine lu(n, a, deter)
    !Obtencion de la factorizacion LU a partir de la eliminacion de Gauss
    use mod_clreal
    implicit none

    !En la subrutina Gauss ya hemos obtenido los coeficientes lij necesarios para obtener la factorizacion LU. Para cada fila, factor es el correspondiente coeficiente lij.

    !Podemos dividir los elementos aik de A por pivote y guardarlos de nuevo en
    !la matriz A.
    !a(i, k) = a(i, k)/piv
    !Asi, en la parte inferior de la matriz A tendremos L.

    !Argumentos
    integer, intent(in)::n
    real(kind=clreal), intent(inout)::a(n,n)   !Matriz del S.E.L.
    real(kind=clreal), intent(out)::deter      !Determinante de la matriz

    !Variables locales
    real(kind=clreal)::piv          !Pivote
    !integer::i, j, k
    integer::j,k


    !Inicializacion del determinante
    deter=1.

    !Etapa k-esima de la eliminacion
    do k=1,n-1
        piv=a(k,k)
        !Comprobacion de que el
        !k-esimo pivote no es nulo
        if(abs(piv)<1.e-12) then
            print*,'pivote nulo en la etapa: ', k
            !No se hacen intercambios de filas. Se para la ejecucion
            stop
        end if

        !Actualización del determinante
        deter = deter*piv

        !Eliminacion
        !do i = k+1, n      !Filas desde k+1 hasta n
            !Para obtener la matriz L de la factorización LU guardamos el resultado de dividir los elementos a(i,k) por el pivote en el propio elemento a(i,k)
        !    a(i,k) = a(i,k) / piv
        
        !    do j = k+1, n
        !        a(i,j) = a(i,j) - a(i,k)*a(k,j)
        !    end do
        !end do

        !Con este codigo, los elementos se guardan ya en la posicion correspondiente
        a(k+1:n,k) = a(k+1:n,k)/piv
        do j=k+1,n
            a(k+1:n,j)=a(k+1:n,j)-a(k+1:n,k)*a(k,j)
        end do

    end do

    !Comprobacion de que el
    !ultimo pivote no es nulo
    if(abs(a(n,n)) < 1.e-12) then
        print*, 'pivote nulo en la etapa: ',n
        stop
    end if

    !Finalizacion del calculo del determinante
    deter = deter*a(n,n)    

end subroutine lu