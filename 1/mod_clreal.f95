module mod_clreal
    implicit none
    integer,parameter::clreal=selected_real_kind(p=15,r=307)
    !asegura una precision de al menos 15 cifras decimales
    !significativas

    !y un rango de exponente de al menos 10^(-307) a 10^307
    !para la clase clreal

    !Es decir, los números en doble precisión

    !Para usarlo, es necesario tener en la carpeta el archivo .mod que resulta de compilar usando la opción -c
end module mod_clreal