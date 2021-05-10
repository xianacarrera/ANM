function z=f(x)
    %z=[...;...; ... ]; % expresion de la funcion no lineal F(x)
    % el sistema se da en forma de columna (z es un vector columna)
    %z=[x(1)^2-x(2); -x(1)+x(2)^2];
    %z=[x(1)^2-x(2); -x(1)+x(2)^2];
    z=[x(1)^2-x(2)+0.25; -x(1)+x(2)^2+0.25];
    %z=[x(1)^2+x(2)^2-2; -x(1)+2*x(2)^2-1]
return