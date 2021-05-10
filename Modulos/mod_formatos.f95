module mod_formatos
    !Modulo con formatos para impresion por pantalla

    implicit none
    character(len=6), parameter :: texto = '(A128)'
    character(len=10), parameter :: decimales = '(100e12.4)'
    character(len=4), parameter :: entero = '(I8)'
    character(len=10), parameter :: enteros = '(100I8)'

end module mod_formatos