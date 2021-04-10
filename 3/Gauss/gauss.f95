subroutine gauss(n, a, b, deter)
    !Subrutina que efectúa la eliminación de Gauss sobre un sistema
    use mod_clreal
    implicit none

    !argumentos (recomendable declararlos en el orden en el que figuran en la subrutina)
    integer, intent(in)::n
    real(kind=clreal), intent(inout)::a(n,n)   !Matriz del S.E.L.
    real(kind=clreal), intent(inout)::b(n)     !Término independiente del S.E.L.
    real(kind=clreal), intent(out)::deter      !Determinante de la matriz

    real(kind=clreal)::piv          !Pivote
    real(kind=clreal)::factor       !Factor
    integer::i, j, k


    !Inicializacion del determinante
    deter=1.

    !Etapa k-ésima de la eliminación
    do k=1,n-1
        piv=a(k,k)
        !Comprobación de que el
        !k-ésimo pivote no es nulo
        if(abs(piv)<1.e-12) then
            print*,'pivote nulo en la etapa: ', k
            !No se hacen intercambios de filas. Se para la ejecución
            stop
        end if

        !Actualización del determinante
        deter = deter*piv

        !Eliminación
        !do i = k+1, n      !Filas desde k+1 hasta n
        !    factor = a(i,k) / piv
        
        !    do j = k+1, n
        !        a(i,j) = a(i,j) - factor*a(k,j)
        !    end do
                
        !    b(i)=b(i)-factor*b(k)
        !end do

        a(k+1:n,k) = a(k+1:n,k)/piv
        do j=k+1,n
            a(k+1:n,j)=a(k+1:n,j)-a(k+1:n,k)*a(k,j)
        end do
        b(k+1:n)=b(k+1:n)-a(k+1:n,k)*b(k)

    end do

    !Comprobación de que el
    !último pivote no es nulo
    if(abs(a(n,n))<1.e-12) then
        print*, 'pivote nulo en la etapa: ',n
        stop
    end if

    !Finalización del cálculo do determinante
    deter = deter*a(n,n)    

end subroutine gauss