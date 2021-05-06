subroutine gaussh(n, a, b, deter)
    !****************************************************
    !Eliminación de Gauss para una matriz Hesssenberg superior
    !Se transforma el sistema Au=b en uno equivalente con matriz triangular
    !superior y se calcula el determinante de la matriz resultante.
    !****************************************************
    
    use mod_clreal
    implicit none

    !Argumentos
    integer, intent(in)::                                   n !Orden del S.E.L.
    real(kind=clreal), dimension(n,n), intent(inout)::      a !Matriz del S.E.L.
    real(kind=clreal), dimension(n), intent(inout)::        b !Término independiente del S.E.L.
    real(kind=clreal), intent(out)::                        deter  !Determinante de la matriz

    !Variables locales
    real(kind=clreal)::aux          !Variable auxiliar
    integer::k                      !Etapa


    !Inicialización del determinante
    deter=1.

    !Etapa k-ésima de la eliminacion
    do k=1,n-1
        aux=a(k,k)      !Guardamos el pivote en aux
        !Comprobación de que el
        !k-ésimo pivote no es nulo
        if(abs(aux) < 1.e-12) then
            print*,'pivote nulo en la etapa: ', k
            !No se hacen intercambios de filas. Se para la ejecución
            stop
        end if

        !Actualización del determinante
        deter = deter*aux

        !Reutilizamos aux para guardar el factor de la etapa k
        aux = a(k+1,k)/aux

        !Operaciones en la fila k+1
        !No operamos en ninguna fila más, ya que aij = 0 para todo i > j+1
        a(k+1,k+1:n)=a(k+1,k+1:n)-aux*a(k,k+1:n)

        !Transformación del término independiente
        b(k+1)=b(k+1)-aux*b(k)

    end do

    !Comprobacion de que el
    !ultimo pivote no es nulo
    if(abs(a(n,n))<1.e-12) then
        print*, 'pivote nulo en la etapa: ',n
        stop
    end if

    !Finalizacion del calculo del determinante
    deter = deter*a(n,n)  

end subroutine gaussh