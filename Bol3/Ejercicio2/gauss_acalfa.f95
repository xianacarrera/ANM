subroutine gauss_acalfa(n, a, c, alfa, b, deter)
    !****************************************************
    !Eliminación de Gauss para la matriz del ejercicio 2 del boletín 3
    !Se transforma el sistema Au=b en uno equivalente con matriz triangular
    !superior y se calcula el determinante de la matriz resultante.
    !****************************************************
    
    use mod_clreal
    implicit none

    !Argumentos
    integer, intent(in)::                                   n !Orden del S.E.L.
    real(kind=clreal), dimension(n), intent(inout)::        a !Diagonal de la matriz A
    real(kind=clreal), dimension(n-1), intent(in)::         c !Vector con la primera fila de la matriz A, exceptuando el elemento a_11
    real(kind=clreal), intent(in)::                         alfa !Elemento a_1n
    real(kind=clreal), dimension(n), intent(inout)::        b !Termino independiente del S.E.L.
    real(kind=clreal), intent(out)::                        deter  !Determinante de la matriz A

    !Variables locales
    integer::k

    !Inicializacion del determinante
    deter=1._clreal

    !Comprobacion de que el primer pivote sea no nulo
    if(abs(a(1)) < 1.e-12) then
        print*,'pivote nulo en la etapa: 1' 
        !No se hacen intercambios de filas. Se para la ejecucion
        stop
    end if    

    !Etapa 1
    b(n) = b(n) - alfa*b(1)/a(1)

    !Etapa k-esima de la eliminacion
    do k=2,n-1
        !Comprobacion de que el
        !k-ésimo pivote no es nulo
        if(abs(a(k)) < 1.e-12) then
            print*,'pivote nulo en la etapa: ', k
            !No se hacen intercambios de filas. Se para la ejecucion
            stop
        end if

        !Actualizacion del determinante
        deter = deter*a(k)

        b(n) = b(n) + alfa*c(k-1)*b(k)/(a(1)*a(k))

    end do

    !Se guardan los cambios sobre la diagonal (elemento a_nn)
    a(n)=a(n) - alfa*c(n-1)/a(1)

    !Comprobacion de que el
    !ultimo elemento diagonal no es nulo
    if(abs(a(n))<1.e-12) then
        print*, 'elemento diagonal', n, 'nulo'
        stop
    end if

    !Finalizacion del calculo del determinante
    deter=deter*a(n)


end subroutine gauss_acalfa