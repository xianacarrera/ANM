subroutine chol_BT(a)
    !********************
    ! T
    !Obtencion de la factorizacion de Cholesky A=BB
    ! T
    !B por filas
    !**********************************************************
    !Dpto. de Matematica Aplicada. Facultad de Matematicas. USC
    use mod_clreal
    use mod_formatos
    implicit none
    !argumentos
    real(kind=clreal),dimension(:,:),intent(inout):: a !matriz del S.E.L.
    !variables locales
    integer:: n !orden del S.E.L.
    integer:: i,j,icount1,icount2,irate
    real(kind=clreal):: det,aux
    n=size(a,1)
    call system_clock(icount1,irate)
    !inicializacion del determinante
    !det=1.
    ! T
    !obtencion de la matriz B
    !bucle de filas
    do i=1,n
    ! bucle de columnas
     do j=i,n
     a(i,j)=a(i,j)-sum(a(1:i-1,i)*a(1:i-1,j))
     if(j==i)then
     if(a(i,i)<1.e-12)then
     print*
     print formato5,'** Atencion, elto. diag.: ',a(i,i),' en la fila: ',i
     print formato1,' la matriz del sistema A no es definida positiva **'
     stop
     else
     aux=1./sqrt(a(i,i))
     endif
     end if
     a(i,j)=a(i,j)*aux
     end do
    ! actualizacion del determinante
    ! det=det*a(i,i)
    end do
    !escritura del determinante
    !print*
    !print formato5,'El determinante de la matriz del sistema es: ',det**2
    call system_clock(icount2)
    print*
    print*,"Tiempo de ejecucion en s, chol_BT: ",float(icount2-icount1)/float(irate)
    end subroutine chol_BT