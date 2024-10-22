
module Clave_module
    
    implicit none

    type :: clave
    character(len = 50) :: tipo
        character(len = 50) :: nombre
        character(len = 50) :: ancho
        character(len = 50) :: alto
        character(len = 50)  :: color1
        character(len = 50) :: color2
        character(len = 50)  :: color3
        character(len = 50)  :: posicion1
        character(len = 50) :: posicion2
        character(len = 50) :: texto
        logical :: encendido
    end type clave


end module Clave_module

module Check_module
    
    implicit none

    type :: check
    character(len = 50) :: tipo
        character(len = 50) :: nombre
        character(len = 50) :: ancho
        character(len = 50) :: alto
        character(len = 50)  :: color1
        character(len = 50) :: color2
        character(len = 50)  :: color3
        character(len = 50)  :: posicion1
        character(len = 50) :: posicion2
        character(len = 50) :: texto
        logical :: encendido
    end type check


end module Check_module



module Etiqueta_module

    implicit none

    type :: etiqueta_atri
        character(len = 50) :: tipo
        character(len = 50) :: nombre
        character(len = 50) :: ancho
        character(len = 50) :: alto
        character(len = 50)  :: color1
        character(len = 50) :: color2
        character(len = 50)  :: color3
        character(len = 50)  :: posicion1
        character(len = 50) :: posicion2
        character(len = 50):: texto
        logical :: encendido
    end type etiqueta_atri
end module Etiqueta_module

module Boton_module

    implicit none

    type :: boton
        character(len = 50) :: tipo
        character(len = 50) :: nombre
        character(len = 50) :: ancho
        character(len = 50) :: alto
        character(len = 50)  :: color1
        character(len = 50) :: color2
        character(len = 50)  :: color3
        character(len = 50)  :: posicion1
        character(len = 50) :: posicion2
        character(len = 50) :: texto
        
        
        logical :: encendido
    end type boton

end module Boton_module

module Texto_module

    implicit none

    type :: texto
        character(len = 50) :: tipo
        character(len = 50) :: nombre
        character(len = 50) :: ancho
        character(len = 50) :: alto
        character(len = 50)  :: color1
        character(len = 50) :: color2
        character(len = 50)  :: color3
        character(len = 50)  :: posicion1
        character(len = 50) :: posicion2
        character(len = 50) :: texto
        logical :: encendido
    end type texto

end module Texto_module

module Contenedor_module
    use Boton_module
    use Clave_module
    use Texto_module
    use Etiqueta_module
    use Check_module
    implicit none

    type :: contenedor_atri
        type(boton), allocatable :: botones_contenedor(:)
        type(etiqueta_atri), allocatable :: etiquetas_contenedor(:)
        type(clave), allocatable :: claves_contenedor(:)
        type(texto), allocatable :: textos_contenedor(:)
        type(check), allocatable :: checks_contenedor(:)
        type(contenedor_atri), allocatable :: contenedores_contenedor(:)
        character(len = 50) :: tipo
        character(len = 50) :: nombre
        character(len = 50) :: ancho
        character(len = 50) :: alto
        character(len = 50)  :: color1
        character(len = 50) :: color2
        character(len = 50)  :: color3
        character(len = 50) :: texto
        character(len = 50)  :: posicion1
        character(len = 50) :: posicion2
        integer :: num_textos_cont
        integer :: num_checks_cont
        integer :: num_etiquetas_cont
        integer :: num_claves_cont
        integer :: num_botones_cont
        integer :: num_contenedores_cont
        logical :: encendido

    end type contenedor_atri


end module Contenedor_module
module Pagina_module
    use Contenedor_module
    implicit none
    type :: pagina
        type(contenedor_atri), allocatable :: contenedores_pag(:)
        integer :: numero_contenedores_pagina
        logical :: encendido

    end type pagina
end module pagina_module

module Token_module
    implicit none

    type :: token
        character(len=50) :: tipo
        character(len=50) :: valor
    end type token

end module Token_module

module errorSintac_module
    implicit none

    type :: errorSintac
        character(len = 50) :: tipo
        integer :: linea
        character(len = 50) :: columna
    end type errorSintac
end module errorSintac_module

module Analizador_Lexico_module
    use Token_module
    implicit none

    type :: analizador_lexico
        type(token), allocatable :: tokens (:) !lista de tokens
        type(token), allocatable :: errores_list(:) !lista de errores
        integer :: num_tokens
        integer :: num_errores
        character(len=:), allocatable :: texto
        integer :: posicion_actual
        integer :: longitud
        logical :: enControles
        logical :: enPropiedades
        logical :: enColocacion
        logical :: enPuntoYcoma
        logical :: existe_error_lexico
    

    contains

    procedure :: analizar
    procedure :: tokenizar_texto
    procedure :: leer_identificadores
    procedure :: leer_numeros
    procedure :: leer_cadena
    procedure :: leer_simbolos
    procedure :: generar_html
    procedure :: generar_html_errores
    procedure :: grabarTokens
    procedure :: imprimirErroresLexicos
    end type analizador_lexico
    
contains


subroutine analizar(self, entrada)
    class(analizador_lexico), intent(inout) :: self
    character(len=*), intent(in) :: entrada
    self%texto = entrada
    self%longitud = len(trim(entrada))
    self%num_tokens = 0
    self%num_errores = 0
    self%posicion_actual = 1
    self%enControles = .false.
    self%enColocacion = .false.
    self%enColocacion = .false.
    self%enPuntoYcoma = .false.
    self%existe_error_lexico = .false.
    allocate(self%tokens(1000)) 
    allocate(self%errores_list(500))
    call tokenizar_texto(self)


end subroutine

subroutine tokenizar_texto(self)
    class(analizador_lexico), intent(inout) :: self
    character(len=1) :: caracter
    integer :: i

    do while (self%posicion_actual <= self%longitud)
        i = self%posicion_actual
        caracter = self%texto(i:i)

        !ignorar espacios en blanco
        if (caracter == ' ' .or. caracter == new_line('A')) then
            self%posicion_actual = i+1
            cycle
        end if

        !detectar identificadores (palabras reservadas etc)
        if (caracter >= 'a' .and. caracter <= 'z' .or. caracter >= 'A' .and. caracter <= "Z") then
            call leer_identificadores(self)
            cycle
        end if

        !detectar numeros enteros
        if (caracter >= '0' .and. caracter <= '9') then
            call leer_numeros (self)
            cycle
        end if

        !detectar cadenas entre comillas
        if (caracter == '"') then
            call leer_cadena (self)
            cycle
        end if

        !detectar comentarios
        if(self%texto(i:i+1) == "//")then

            !avanzamos hasta que sea nueva linea
            do while (self%texto(self%posicion_actual: self%posicion_actual) /= new_line('A'))
                self%posicion_actual = self%posicion_actual+1
            end do
            !una posicion mas para el salto de linea
            self%posicion_actual = self%posicion_actual+1
            cycle
        end if 

        if(self%texto(i:i+1) == "/*")then
            !avanzamos hasta que */
            do while (self%texto(self%posicion_actual: self%posicion_actual + 1) /= "*/")
                self%posicion_actual = self%posicion_actual+1
            end do
            !una posicion mas para el salto de linea
            self%posicion_actual = self%posicion_actual+1
            self%posicion_actual = self%posicion_actual+1

            cycle
        end if

        !detectar simbolos especiales
        if (caracter == '{' .or. caracter == '}' .or. caracter == ':' .or. caracter == ';' .or. caracter == '<' &
            .or. caracter == '>' .or. caracter == '!' .or. caracter =='-' .or. caracter == '/' .or. caracter == '('&
                .or. caracter == ')' .or. caracter == '.' .or. caracter == ',')then
            call leer_simbolos(self,caracter)
            cycle
        end if


        


        !si se llega aqui, el caracter no fue reconocido
        print*, "el caracter no fue reconocido", caracter
        self%posicion_actual = i+1
    end do



end subroutine
!subrutina para obtener el nombre de la grafica

!subrutina para leer identificadores
subroutine leer_identificadores (self)
    class(analizador_lexico), intent(inout) :: self
    character (len=1) :: caracter
    character (len=50) :: lexema
    integer :: i

    lexema = ''
    i = self%posicion_actual
    caracter = self%texto(i:i)

    !leer letras y guines bajos
    do while((caracter >= 'a' .and. caracter <= 'z') .or.(caracter >= 'A' .and. caracter <= 'Z').or. caracter == '_' &
            .or. (caracter >= '0' .and. caracter <= '9'))
        print*, caracter
        lexema = trim(lexema) // caracter
        i = i+1
        if(i > self%longitud) exit
        caracter = self%texto(i:i)

    end do

    if (lexema == 'Contenedor') then
        call agregar_token (self, "RESERVADA_CONTENEDOR",lexema)
        self%posicion_actual = i
    else if (lexema == 'Etiqueta') then
        call agregar_token (self, "RESERVADA_ETIQUETA",lexema)
        self%posicion_actual = i
    else if (lexema == 'Boton') then
        call agregar_token (self, "RESERVADA_BOTON",lexema)
        self%posicion_actual = i
    else if (lexema == 'Texto') then
        call agregar_token (self, "RESERVADA_TEXTO",lexema)
        self%posicion_actual = i
    else if (lexema == 'Clave') then
        call agregar_token (self, "RESERVADA_CLAVE",lexema)
        self%posicion_actual = i
    else if (lexema == 'Controles') then
        call agregar_token (self, "RESERVADA_CONTROLES",lexema)
        self%posicion_actual = i
    else if (lexema == 'Propiedades') then
        call agregar_token (self, "RESERVADA_PROPIEDADES",lexema) 
        self%posicion_actual = i 
    else if (lexema == 'Check') then
        call agregar_token (self, "RESERVADA_CHECK",lexema)
        self%posicion_actual = i
    else if (lexema == 'RadioBoton') then
        call agregar_token (self, "RESERVADA_RADIO_BOTON",lexema)
        self%posicion_actual = i
    else if (lexema == 'AreaTexto') then
        call agregar_token (self, "RESERVADA_AREA_TEXTO",lexema)
        self%posicion_actual = i
    else if (lexema == 'Colocacion') then
        call agregar_token (self, "RESERVADA_COLOCACION",lexema)
        self%posicion_actual = i
    else if (lexema == 'setAncho') then
        call agregar_token (self, "PROPIEDAD_SET_ANCHO",lexema)
        self%posicion_actual = i
    else if (lexema == 'setAlto') then
        call agregar_token (self, "PROPIEDAD_SET_ALTO",lexema)
        self%posicion_actual = i
    else if (lexema == 'setColorFondo') then
        call agregar_token (self, "PROPIEDAD_SET_COLOR_FONDO",lexema)
        self%posicion_actual = i
    else if (lexema == 'setTexto') then
        call agregar_token (self, "PROPIEDAD_SET_TEXTO",lexema)
        self%posicion_actual = i
    else if (lexema == 'add') then
        call agregar_token (self, "PROPIEDAD_ADD",lexema)
        self%posicion_actual = i
    else if (lexema == 'setColorLetra') then
        call agregar_token (self, "PROPIEDAD_SET_COLOR_LETRA",lexema)
        self%posicion_actual = i
    else if (lexema == 'setPosicion') then
        call agregar_token (self, "PROPIEDAD_SET_POSICION",lexema)
        self%posicion_actual = i
    else if(verificarErrorReservado(lexema)) then
        call agregar_error(self,"RESERVADA_MAL_REDACTADA", lexema)
        self%posicion_actual = i
    else
        call agregar_token(self, "IDENTIFICADOR", lexema)
        self%posicion_actual = i
    
    end if

    

end subroutine

logical function verificarErrorReservado(lexema)
    character(len = *) :: lexema
    character(len = len(lexema)) :: lexema_minuscula
    lexema_minuscula = to_lowercase(lexema)
    verificarErrorReservado = .false.
    if(lexema_minuscula == 'contenedor' &
        .or. lexema_minuscula == 'etiqueta' &
        .or. lexema_minuscula == 'propiedades' &
        .or. lexema_minuscula == 'colocacion' &
        .or. lexema_minuscula == 'boton' &
        .or. lexema_minuscula == 'texto' &
        .or. lexema_minuscula == 'clave' &
        .or. lexema_minuscula == 'check' &
        .or. lexema_minuscula == 'radioboton' &
        .or. lexema_minuscula == 'areatexto' &
        .or. lexema_minuscula == 'setancho' &
        .or. lexema_minuscula == 'setalto' &
        .or. lexema_minuscula == 'setcolorfondo' &
        .or. lexema_minuscula == 'setcolorletra' &
        .or. lexema_minuscula == 'setposicion' &
        .or. lexema_minuscula == 'add' &
        .or. lexema_minuscula == 'settexto' &

        )then
        print*, 'error lexico detectado: palabra reservada mal declarada'
        verificarErrorReservado = .true.
    else
        return
    end if

end function verificarErrorReservado

!convertir a minusculas
function to_lowercase(cadena) result(cadena_minuscula)
    implicit none
    character(len=*), intent(in) :: cadena
    character(len=len(cadena)) :: cadena_minuscula
    integer :: i

    cadena_minuscula = cadena
    do i = 1, len(cadena)
        ! Verifica si el caracter es una letra mayúscula
        if (iachar(cadena(i:i)) >= iachar('A') .and. iachar(cadena(i:i)) <= iachar('Z')) then
            ! Convierte a minúscula sumando la diferencia en códigos ASCII
            cadena_minuscula(i:i) = char(iachar(cadena(i:i)) + iachar('a') - iachar('A'))
        endif
    end do
end function
! subrutina para leer numeros enteros
subroutine leer_numeros(self)
    class(analizador_lexico), intent(inout) :: self
    character (len=1) :: caracter
    character (len=50) :: numero
    integer :: i, numero_entero

    numero = ''
    i = self%posicion_actual
    caracter = self%texto(i:i)

    !leer numeros
    do while(caracter >= '0' .and. caracter <= '9')
        print*, caracter
        numero = trim(numero) // caracter
        i = i+1

        if (i > self%longitud) exit
        caracter = self%texto(i:i)

    end do
    read (numero, *) numero_entero
    !agregar el token tipo numero
    call agregar_token(self, "NUMERO", numero)
    self%posicion_actual = i
end subroutine

!subrutina para leer cadenas
subroutine leer_cadena(self)
    class(analizador_lexico), intent(inout) :: self
    character (len = 1) :: caracter
    character (len = 50) :: cadena
    integer :: i

    cadena = ''
    i = self%posicion_actual+1
    caracter = self%texto(i:i)

    !leer cadena
    do while (caracter /= '"')
        print*, caracter
        cadena = trim(cadena) // caracter
        i = i+1

        if(i > self%longitud) exit
        caracter = self%texto(i:i)
    end do

    !agregar el token
    call agregar_token (self, "CADENA", cadena)
    self%posicion_actual = i+1 !nos saltamos la ultima comilla
end subroutine

!subrutina para leer simbolos especiales
subroutine leer_simbolos(self, simbolo)
    class(analizador_lexico), intent(inout) :: self
    character (len = 1), intent(in) :: simbolo
    print*, simbolo
    !agregar el token

    if(simbolo == ";")then
        call agregar_token(self,"PUNTO_Y_COMA",simbolo)
        self%posicion_actual = self%posicion_actual+1
    else if (simbolo == ".")then
        call agregar_token(self,"PUNTO",simbolo)
        self%posicion_actual = self%posicion_actual+1
    else if (simbolo == ",")then
        call agregar_token(self,"COMA",simbolo)
        self%posicion_actual = self%posicion_actual+1
    else if (simbolo == "<")then
        call agregar_token(self,"MENOR_QUE",simbolo)
        self%posicion_actual = self%posicion_actual+1
    else if (simbolo == ">")then
        call agregar_token(self,"MAYOR_QUE",simbolo)
        self%posicion_actual = self%posicion_actual+1
    else if (simbolo == "(")then
        call agregar_token(self,"PARENTESIS_ABIERTO",simbolo)
        self%posicion_actual = self%posicion_actual+1
    else if (simbolo == ")")then
        call agregar_token(self,"PARENTESIS_CERRADO",simbolo)
        self%posicion_actual = self%posicion_actual+1
    else if (simbolo == "!")then
        call agregar_token(self,"EXCLAMACION",simbolo)
        self%posicion_actual = self%posicion_actual+1
    else if (simbolo == ":")then
        call agregar_token(self,"DOS_PUNTOS",simbolo)
        self%posicion_actual = self%posicion_actual+1
    else if (simbolo == "-")then
        call agregar_token(self,"GUION",simbolo)
        self%posicion_actual = self%posicion_actual+1
    else
        call agregar_error(self, "SIMBOLO_NO_RECONOCIDO", simbolo)
        self%posicion_actual = self%posicion_actual+1
    end if
    

end subroutine
!subrutina para agregar tokens
subroutine agregar_token(self, tipo, valor)

    class(analizador_lexico), intent(inout) :: self
    character(len=*), intent(in) :: tipo , valor
    
    self%num_tokens = self%num_tokens +1
    if (self%num_tokens > size(self%tokens)) then
        print*, 'has alcanzado la capacidad macima de tokens permitidos'
        return
    end if

    self%tokens(self%num_tokens)%tipo = trim(tipo)
    self%tokens(self%num_tokens)%valor = trim(valor)
    print*, 'Token agregado', trim(tipo) ,':' ,trim(valor)
    
end subroutine
!subrutina para agregar errores
subroutine agregar_error(self, tipo, valor)

    class(analizador_lexico), intent(inout) :: self
    character (len=*), intent(in) :: tipo, valor

    self%num_errores = self%num_errores + 1

    if (self%num_errores > size(self%errores_list)) then
        print*, 'Has alcanzado la capacidad maxima de errores permitidos'
        return
    end if

    self%errores_list(self%num_errores)%tipo = trim(tipo)
    self%errores_list(self%num_errores)%valor = trim(valor)
    print*, 'Error encontrado: ', trim(tipo), ': ', trim(valor)

end subroutine
subroutine imprimirErroresLexicos(self)
    class(analizador_lexico), intent(in) :: self

    integer :: unidad, i
    open(unit = unidad, file = 'C:\Users\cesar\OneDrive\Documentos\Fortran\Proyecto 2\listaErrores.txt', status = 'replace')

    do i =1, self%num_errores
        write(unidad, '(A , A)') 'Tipo:',self%errores_list(i)%tipo
        write(unidad, '(A , A)') 'linea:',self%errores_list(i)%valor
    end do
    close(unidad)
end subroutine
subroutine generar_html(self, archivo_html)
    class(analizador_lexico), intent(in) :: self
    character(len=*), intent(in) :: archivo_html
    integer :: unidad, i

    ! Abrir el archivo HTML para escritura
    open(unit = unidad, file = archivo_html, status = 'replace')

    ! Escribir el encabezado del HTML
    write(unidad, *) '<html>'
    write(unidad, *) '<head><title>Tokens</title></head>'
    write(unidad, *) '<body>'
    write(unidad, *) '<h1>Lista de Tokens</h1>'
    write(unidad, *) '<table border="1">'
    write(unidad, *) '<tr><th>Tipo</th><th>Nombre</th><th>Valor</th></tr>'  ! Encabezado actualizado

    ! Escribir cada token en una fila de la tabla
    do i = 1, self%num_tokens
        write(unidad, *) '<tr><td>', trim('Token'), '</td><td>', trim(self%tokens(i)%tipo), &
            '</td><td>', trim(self%tokens(i)%valor), '</td></tr>'
    end do

    ! Cerrar la tabla y el documento HTML
    write(unidad, *) '</table>'
    write(unidad, *) '</body>'
    write(unidad, *) '</html>'

    ! Cerrar el archivo
    close(unidad)
end subroutine

subroutine generar_html_errores(self, archivo_html)
    class(analizador_lexico), intent(in) :: self
    character(len=*), intent(in) :: archivo_html
    integer :: unidad, i

    ! Abrir el archivo HTML para escritura
    open(unit = unidad, file = archivo_html, status = 'replace')

    ! Escribir el encabezado del HTML
    write(unidad, *) '<html>'
    write(unidad, *) '<head><title>Tokens</title></head>'
    write(unidad, *) '<body>'
    write(unidad, *) '<h1>Lista de Errores</h1>'
    write(unidad, *) '<table border="1">'
    write(unidad, *) '<tr><th>Tipo</th><th>Error</th><th>Valor</th></tr>'  ! Encabezado actualizado

    ! Escribir cada token en una fila de la tabla
    do i = 1, self%num_errores
        write(unidad, *) '<tr><td>', trim('Token'), '</td><td>', trim(self%errores_list(i)%tipo), &
            '</td><td>', trim(self%errores_list(i)%valor), '</td></tr>'
    end do

    ! Cerrar la tabla y el documento HTML
    write(unidad, *) '</table>'
    write(unidad, *) '</body>'
    write(unidad, *) '</html>'

    ! Cerrar el archivo
    close(unidad)
end subroutine
subroutine grabarTokens(self)
    class(analizador_lexico), intent(inout) :: self
    integer :: unidad, i
    unidad = 10
    open(unit = unidad, file = 'C:\Users\cesar\OneDrive\Documentos\Fortran\Proyecto 2\salidaTokens.txt', status = 'replace')

    do i =1, self%num_tokens
        write(unidad, '(A, A)') 'Tipo: ', self%tokens(i)%tipo
        write(unidad, '(A, A)') 'valor: ', self%tokens(i)%valor
    end do

    close(unidad)
end subroutine grabarTokens

end module Analizador_Lexico_module

module Analizador_sintactico_module
    use Token_module
    use Contenedor_module
    use Etiqueta_module
    use Texto_module
    use Boton_module
    use Clave_module
    use Pagina_module
    use errorSintac_module
    implicit none

    type :: analizador_sintactico
        type(token), allocatable :: tokens_list(:) !lista de tokens
        type(errorSintac), allocatable :: erroresSintacticos(:)
        integer :: posicion_actual, numero_contenedores, numero_botones, numero_etiquetas, numero_textos &
            ,numero_claves, numero_errores
        logical:: existe_error_sintactico
        type(contenedor_atri), allocatable :: contenedores_list(:)
        type(etiqueta_atri), allocatable :: etiquetas_list(:)
        type(texto), allocatable :: textos_list(:)
        type(boton), allocatable :: botones_list(:)
        type(clave), allocatable :: claves_list(:)
        type(pagina) :: main_page
        contains 

        procedure :: verificar_controles
        procedure :: inicializar
        procedure :: parsear
        procedure :: verificar_colocacion
        procedure :: verificar_propiedades
        procedure :: generarHtml
        procedure :: generarCss
        procedure :: enviarErrores

    end type analizador_sintactico
contains

    subroutine inicializar (self, tokens)
        class(analizador_sintactico), intent(inout) :: self
        type(token), allocatable, intent(in) :: tokens(:)
        
        
        ! Alocar memoria para tokens_list
        allocate(self%tokens_list(size(tokens)))
        allocate(self%contenedores_list(100))
        allocate(self%etiquetas_list(100))
        allocate(self%botones_list(100))
        allocate(self%textos_list(100))
        allocate(self%claves_list(100))
        allocate(self%erroresSintacticos(100))
        allocate(self%main_page%contenedores_pag(20))
       
        
        ! Copiar cada token individualmente
        self%tokens_list = tokens
        self%posicion_actual = 1
        self%existe_error_sintactico = .false.

    end subroutine inicializar

    subroutine parsear (self)
        class(analizador_sintactico), intent(inout) :: self
        call verificar_controles(self)
        call verificar_propiedades(self)
        call verificar_colocacion(self)

    
    end subroutine

    subroutine verificar_controles(self)
        class(analizador_sintactico), intent(inout) :: self
        

        if(.not. token_match(self,"MENOR_QUE"))then 
            call agregarErrorSintactico(self, "se esperaba signo menor",self%posicion_actual)
            call modoPanico(self, '>')
            return
        endif
        if(.not. token_match(self, "EXCLAMACION"))then 
            call agregarErrorSintactico(self, "se esperaba signo exclamacion",self%posicion_actual)
            call modoPanico(self, '>')
            return
        endif
        if(.not. token_match(self, "GUION"))then 
            call agregarErrorSintactico(self, "se esperaba guion",self%posicion_actual)
            call modoPanico(self, '>')
            return
        endif
        if(.not. token_match(self, "GUION"))then 
            call agregarErrorSintactico(self, "se esperaba guion",self%posicion_actual)
            call modoPanico(self, '>')
            return
        endif
        if(.not. token_match(self, "RESERVADA_CONTROLES"))then 
            call agregarErrorSintactico(self, "se esperaba reservada controles",self%posicion_actual)
            call modoPanico(self, '>')
            return
        endif

        !llamada a leer el contenido
        call parsear_controles(self)

        if(.not. token_match(self, "RESERVADA_CONTROLES"))then 
            call agregarErrorSintactico(self, "se esperaba reservada controles",self%posicion_actual)
            call modoPanico(self, '>')
        endif
        if(.not. token_match(self, "GUION"))then 
            call agregarErrorSintactico(self, "se esperaba guion",self%posicion_actual)
            call modoPanico(self, '>')
        endif
        if(.not. token_match(self, "GUION"))then 
            call agregarErrorSintactico(self, "se esperaba guion",self%posicion_actual)
            call modoPanico(self, '>')
        endif
        if(.not. token_match(self,"MAYOR_QUE"))then 
            call agregarErrorSintactico(self, "se esperaba mayor que",self%posicion_actual)
            call modoPanico(self, '<')
        endif
        

       

    end subroutine verificar_controles

    subroutine verificar_propiedades(self)
        class(analizador_sintactico), intent(inout) :: self 
        
        if(.not. token_match(self,"MENOR_QUE"))then 
            call agregarErrorSintactico(self, "se esperaba signo menor",self%posicion_actual)
            call modoPanico(self, '>')
            return
        endif
        if(.not. token_match(self, "EXCLAMACION"))then 
            call agregarErrorSintactico(self, "se esperaba signo exclamacion",self%posicion_actual)
            call modoPanico(self, '>')
            return
        endif
        if(.not. token_match(self, "GUION"))then 
            call agregarErrorSintactico(self, "se esperaba guion",self%posicion_actual)
            call modoPanico(self, '>')
            return
        endif
        if(.not. token_match(self, "GUION"))then 
            call agregarErrorSintactico(self, "se esperaba guion",self%posicion_actual)
            call modoPanico(self, '>')
            return
        endif
        if(.not. token_match(self, "RESERVADA_PROPIEDADES"))then 
            call agregarErrorSintactico(self, "se esperaba reservada propiedades",self%posicion_actual)
            call modoPanico(self, '>')
            return
        endif

        !llamada a leer el contenido
        call parsearPropiedades(self)

        if(.not. token_match(self, "RESERVADA_PROPIEDADES"))then 
            call agregarErrorSintactico(self, "se esperaba reservada propiedades",self%posicion_actual)
            call modoPanico(self, '>')
            return
        endif
        if(.not. token_match(self, "GUION"))then 
            call agregarErrorSintactico(self, "se esperaba guion",self%posicion_actual)
            call modoPanico(self, '>')
            return
        endif
        if(.not. token_match(self, "GUION"))then 
            call agregarErrorSintactico(self, "se esperaba guion",self%posicion_actual)
            call modoPanico(self, '>')
            return
        endif
        if(.not. token_match(self,"MAYOR_QUE"))then 
            call agregarErrorSintactico(self, "se esperaba mayor que",self%posicion_actual)
            call modoPanico(self, '<')
            return
        endif
        
            

    end subroutine verificar_propiedades

    subroutine verificar_colocacion(self)
        class(analizador_sintactico), intent(inout) :: self

        if(.not. token_match(self,"MENOR_QUE"))then 
            call agregarErrorSintactico(self, "se esperaba signo menor",self%posicion_actual)
            call modoPanico(self, '>')
            return
        endif
        if(.not. token_match(self, "EXCLAMACION"))then 
            call agregarErrorSintactico(self, "se esperaba signo exclamacion",self%posicion_actual)
            call modoPanico(self, '>')
            return
        endif
        if(.not. token_match(self, "GUION"))then 
            call agregarErrorSintactico(self, "se esperaba guion",self%posicion_actual)
            call modoPanico(self, '>')
            return
        endif
        if(.not. token_match(self, "GUION"))then 
            call agregarErrorSintactico(self, "se esperaba guion",self%posicion_actual)
            call modoPanico(self, '>')
            return
        endif
        if(.not. token_match(self, "RESERVADA_COLOCACION"))then 
            call agregarErrorSintactico(self, "se esperaba reservada colocacion",self%posicion_actual)
            call modoPanico(self, '>')
            return
        endif

        !llamada a leer el contenido
        call parsearColocacion(self)

        if(.not. token_match(self, "RESERVADA_COLOCACION"))then 
            call agregarErrorSintactico(self, "se esperaba reservada colocacion",self%posicion_actual)
            call modoPanico(self, '>')
            return
        endif
        if(.not. token_match(self, "GUION"))then 
            call agregarErrorSintactico(self, "se esperaba guion",self%posicion_actual)
            call modoPanico(self, '>')
            return
        endif
        if(.not. token_match(self, "GUION"))then 
            call agregarErrorSintactico(self, "se esperaba guion",self%posicion_actual)
            call modoPanico(self, '>')
            return
        endif
        if(.not. token_match(self,"MAYOR_QUE"))then 
            call agregarErrorSintactico(self, "se esperaba mayor que",self%posicion_actual)
            call modoPanico(self, '<')
            return
        endif


    end subroutine verificar_colocacion

    recursive subroutine parsearColocacion(self)
        class(analizador_sintactico), intent(inout) :: self
        integer :: posicion_actual
        posicion_actual = self%posicion_actual
        if (verificarIdentificador(self)) call EncenderSwitch(self)
        if(.not. token_match(self,"IDENTIFICADOR"))return
        if(.not. token_match(self,"PUNTO"))return

        if(tipoValidoColocacion(self))then
            call casoColocacion(self)
        end if
        call parsearColocacion(self)


    end subroutine
    subroutine casoColocacion(self)
        class(analizador_sintactico), intent(inout) :: self
        integer :: posicion_actual
        character(len = 50) ::numeroFantasma1, numeroFantasma2
        posicion_actual = self%posicion_actual

        select case(self%tokens_list(posicion_actual)%tipo)
            case("PROPIEDAD_SET_POSICION")
                if(.not. token_match(self,"PROPIEDAD_SET_POSICION"))return
                if(.not. token_match(self,"PARENTESIS_ABIERTO"))then 
                    call agregarErrorSintactico(self, "se esperaba parentesis abierto",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                posicion_actual = self%posicion_actual
                numeroFantasma1 = self%tokens_list(posicion_actual)%valor
                if(.not. token_match(self,"NUMERO"))then 
                    call agregarErrorSintactico(self, "se esperaba un numero",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                if(.not. token_match(self,"COMA"))then 
                    call agregarErrorSintactico(self, "se esperaba coma",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                posicion_actual = self%posicion_actual
                numeroFantasma2 = self%tokens_list(posicion_actual)%valor
                if(.not. token_match(self,"NUMERO"))then 
                    call agregarErrorSintactico(self, "se esperaba un numero",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                if(.not. token_match(self,"PARENTESIS_CERRADO"))then 
                    call agregarErrorSintactico(self, "se esperaba parentesis cerrado",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                if(.not. token_match(self,"PUNTO_Y_COMA"))then 
                    call agregarErrorSintactico(self, "se esperaba punto y coma",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                call setPosicion(self, numeroFantasma1, numeroFantasma2)
            case("PROPIEDAD_ADD")
                if(.not. token_match(self,"PROPIEDAD_ADD"))call modoPanico(self, ';')
                if(.not. token_match(self,"PARENTESIS_ABIERTO"))then 
                    call agregarErrorSintactico(self, "se esperaba parentesis abierto",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                if(.not. token_match(self,"IDENTIFICADOR"))then 
                    call agregarErrorSintactico(self, "se esperaba identificador",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                if(.not. token_match(self,"PARENTESIS_CERRADO"))then 
                    call agregarErrorSintactico(self, "se esperaba parentesis cerrado",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                if(.not. token_match(self,"PUNTO_Y_COMA"))then 
                    call agregarErrorSintactico(self, "se esperaba punto y coma",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                call agregar(self, numeroFantasma1)
            case default
                print *, "error sintactico"
        end select


    end subroutine casoColocacion
    logical function tipoValidoColocacion (self)
        class(analizador_sintactico), intent(inout) :: self
        integer :: posicion_actual
        posicion_actual = self%posicion_actual
        tipoValidoColocacion = .false.
        if(self%tokens_list(posicion_actual)%tipo == "PROPIEDAD_SET_POSICION" &
            .or. self%tokens_list(posicion_actual)%tipo == "PROPIEDAD_ADD") then
            
                tipoValidoColocacion = .true.
        end if 

    end function tipoValidoColocacion

    recursive subroutine parsearPropiedades(self)
        class(analizador_sintactico), intent(inout) :: self
        if (verificarIdentificador(self)) call EncenderSwitch(self)
        if(.not. token_match(self,"IDENTIFICADOR"))return
        if(.not. token_match(self,"PUNTO"))return
        if(tipoValidoPropiedad(self))then
            call casoPropiedad(self)
        end if
        call parsearPropiedades(self)


    end subroutine
    subroutine casoPropiedad(self)
        class(analizador_sintactico), intent(inout) :: self
        integer :: posicion_actual
        character (len=50) :: numeroFantasma1, numeroFantasma2, numeroFantasma3
        posicion_actual = self%posicion_actual

        select case(self%tokens_list(posicion_actual)%tipo)
            case("PROPIEDAD_SET_ALTO")
                if(.not. token_match(self,"PROPIEDAD_SET_ALTO")) then 
                    call agregarErrorSintactico(self, "se esperaba propiedad set",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                if(.not. token_match(self,"PARENTESIS_ABIERTO"))then 
                    call agregarErrorSintactico(self, "se esperaba parentesis abierto",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                posicion_actual = self%posicion_actual
                numeroFantasma1 = self%tokens_list(posicion_actual)%valor
                if(.not. token_match(self,"NUMERO"))then 
                    call agregarErrorSintactico(self, "se esperaba numero",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                if(.not. token_match(self,"PARENTESIS_CERRADO"))then 
                    call agregarErrorSintactico(self, "se esperaba parentesis cerrado",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                if(.not. token_match(self,"PUNTO_Y_COMA"))then 
                    call agregarErrorSintactico(self, "se esperaba punto y coma",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                call setAlto(self, numeroFantasma1)
            case("PROPIEDAD_SET_ANCHO")
                if(.not. token_match(self,"PROPIEDAD_SET_ANCHO"))then 
                    call agregarErrorSintactico(self, "se esperaba propiedad set",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                if(.not. token_match(self,"PARENTESIS_ABIERTO"))then 
                    call agregarErrorSintactico(self, "se esperaba parentesis abierto",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                posicion_actual = self%posicion_actual
                numeroFantasma1 = self%tokens_list(posicion_actual)%valor
                if(.not. token_match(self,"NUMERO"))then 
                    call agregarErrorSintactico(self, "se esperaba numero",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                if(.not. token_match(self,"PARENTESIS_CERRADO"))then 
                    call agregarErrorSintactico(self, "se esperaba parentesis cerrado",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                if(.not. token_match(self,"PUNTO_Y_COMA"))then 
                    call agregarErrorSintactico(self, "se esperaba punto y coma",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                call setAncho(self, numeroFantasma1)
            case("PROPIEDAD_SET_COLOR_FONDO")
                if(.not. token_match(self,"PROPIEDAD_SET_COLOR_FONDO"))then 
                    call agregarErrorSintactico(self, "se esperaba propiedad set",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                if(.not. token_match(self,"PARENTESIS_ABIERTO"))then 
                    call agregarErrorSintactico(self, "se esperaba parentesis abierto",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                posicion_actual = self%posicion_actual
                numeroFantasma1 = self%tokens_list(posicion_actual)%valor
                if(.not. token_match(self,"NUMERO"))then 
                    call agregarErrorSintactico(self, "se esperaba numero",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                if(.not. token_match(self,"COMA"))then 
                    call agregarErrorSintactico(self, "se esperaba coma",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                posicion_actual = self%posicion_actual
                numeroFantasma2 = self%tokens_list(posicion_actual)%valor
                if(.not. token_match(self,"NUMERO"))then 
                    call agregarErrorSintactico(self, "se esperaba numero",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                if(.not. token_match(self,"COMA"))then 
                    call agregarErrorSintactico(self, "se esperaba coma",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                posicion_actual = self%posicion_actual
                numeroFantasma3 = self%tokens_list(posicion_actual)%valor
                if(.not. token_match(self,"NUMERO"))then 
                    call agregarErrorSintactico(self, "se esperaba numero",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                if(.not. token_match(self,"PARENTESIS_CERRADO"))then 
                    call agregarErrorSintactico(self, "se esperaba parentesis cerrado",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                if(.not. token_match(self,"PUNTO_Y_COMA"))then 
                    call agregarErrorSintactico(self, "se esperaba punto y coma",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                call setColorFondo(self, numeroFantasma1, numeroFantasma2, numeroFantasma3)
            case("PROPIEDAD_SET_COLOR_LETRA")
                if(.not. token_match(self,"PROPIEDAD_SET_COLOR_LETRA"))call modoPanico(self, ';')
                if(.not. token_match(self,"PARENTESIS_ABIERTO"))then 
                    call agregarErrorSintactico(self, "se esperaba parentesis abierto",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                posicion_actual = self%posicion_actual
                numeroFantasma1 = self%tokens_list(posicion_actual)%valor
                if(.not. token_match(self,"NUMERO"))then 
                    call agregarErrorSintactico(self, "se esperaba numero",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                if(.not. token_match(self,"COMA"))then 
                    call agregarErrorSintactico(self, "se esperaba coma",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                posicion_actual = self%posicion_actual
                numeroFantasma2 = self%tokens_list(posicion_actual)%valor
                if(.not. token_match(self,"NUMERO"))then 
                    call agregarErrorSintactico(self, "se esperaba numero",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                if(.not. token_match(self,"COMA"))then 
                    call agregarErrorSintactico(self, "se esperaba coma",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                posicion_actual = self%posicion_actual
                numeroFantasma3 = self%tokens_list(posicion_actual)%valor
                if(.not. token_match(self,"NUMERO"))then 
                    call agregarErrorSintactico(self, "se esperaba numero",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                if(.not. token_match(self,"PARENTESIS_CERRADO"))then 
                    call agregarErrorSintactico(self, "se esperaba parentesis cerrado",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                if(.not. token_match(self,"PUNTO_Y_COMA"))then 
                    call agregarErrorSintactico(self, "se esperaba punto y coma",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                call setColorFondo(self, numeroFantasma1, numeroFantasma2, numeroFantasma3)
            case("PROPIEDAD_ADD")
                if(.not. token_match(self,"PROPIEDAD_ADD"))call modoPanico(self, ';')
                if(.not. token_match(self,"PARENTESIS_ABIERTO"))then 
                    call agregarErrorSintactico(self, "se esperaba parentesis abierto",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                if(.not. token_match(self,"IDENTIFICADOR"))then 
                    call agregarErrorSintactico(self, "se esperaba identificador",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                if(.not. token_match(self,"PARENTESIS_CERRADO"))then 
                    call agregarErrorSintactico(self, "se esperaba parentesis cerrado",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                if(.not. token_match(self,"PUNTO_Y_COMA"))then 
                    call agregarErrorSintactico(self, "se esperaba punto y coma",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
            case("PROPIEDAD_SET_TEXTO")
                if(.not. token_match(self,"PROPIEDAD_SET_TEXTO"))call modoPanico(self, ';')
                if(.not. token_match(self,"PARENTESIS_ABIERTO"))then 
                    call agregarErrorSintactico(self, "se esperaba parentesis abierto",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                posicion_actual = self%posicion_actual
                numeroFantasma1 = self%tokens_list(posicion_actual)%valor
                if(.not. token_match(self,"CADENA"))then 
                    call agregarErrorSintactico(self, "se esperaba cadena",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                if(.not. token_match(self,"PARENTESIS_CERRADO"))then 
                    call agregarErrorSintactico(self, "se esperaba parentesis cerrado",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                if(.not. token_match(self,"PUNTO_Y_COMA"))then 
                    call agregarErrorSintactico(self, "se esperaba punto y coma",self%posicion_actual)
                    call modoPanico(self, ';')
                    return
                endif
                call setTexto(self, numeroFantasma1)
            case default
                print*, 'error lexico detectado'
                call modoPanico(self, ';')
        end select

    end subroutine
    logical function tipoValidoPropiedad(self)
        class(analizador_sintactico), intent(inout) :: self
        integer :: posicion_actual
        posicion_actual = self%posicion_actual
        tipoValidoPropiedad = .false.
        if(self%tokens_list(posicion_actual)%tipo == "PROPIEDAD_SET_ALTO"   &
            .or. self%tokens_list(posicion_actual)%tipo == "PROPIEDAD_SET_ANCHO" &
            .or. self%tokens_list(posicion_actual)%tipo == "PROPIEDAD_SET_COLOR_FONDO" &
            .or. self%tokens_list(posicion_actual)%tipo == "PROPIEDAD_SET_TEXTO" &
            .or. self%tokens_list(posicion_actual)%tipo == "PROPIEDAD_SET_COLOR_LETRA" &
            .or. self%tokens_list(posicion_actual)%tipo == "PROPIEDAD_ADD" &
                ) then
            tipoValidoPropiedad = .true.
        end if
    end function
!-------------------------SUBRUTINAS PARA RELACIONAR PROPIEDADES--------------------------------
    subroutine EncenderSwitch(self)
        class(analizador_sintactico), intent(inout) :: self
        integer :: posicion_actual, contenedori, etiquetaj, botonk, clavem , texton
        posicion_actual = self%posicion_actual
        contenedori = 1 
        etiquetaj = 1  
        botonk = 1 
        clavem = 1  
        texton = 1

        !se verifica primero que no sea "this" haciendo referencia a la clase
        if(self%tokens_list(posicion_actual)%valor == 'this') then
            self%main_page%encendido = .true.
        endif

        !verificamos si la variable esta en contenedor
        do while(contenedori <= self%numero_contenedores)
            if(self%contenedores_list(contenedori)%nombre == self%tokens_list(posicion_actual)%valor) then
                self%contenedores_list(contenedori)%encendido = .true.
                return
            endif
            contenedori = contenedori +1
        end do
        !SI NO ESTA VERIFICAMOS SI ESTA EN ETIQUETAS
        do while(etiquetaj <= self%numero_etiquetas)
            if(self%etiquetas_list(etiquetaj)%nombre == self%tokens_list(posicion_actual)%valor) then
                self%etiquetas_list(etiquetaj)%encendido = .true.
                return
            endif
            etiquetaj = etiquetaj +1
        end do
        !si no esta en etiquetas, verificamos botones
        do while(botonk <= self%numero_botones)
            if(self%botones_list(botonk)%nombre == self%tokens_list(posicion_actual)%valor) then
                self%botones_list(botonk)%encendido = .true.
                return
            endif
            botonk = botonk +1
        end do
        !TEXTOS
        do while(texton <= self%numero_claves)
            if(self%textos_list(texton)%nombre == self%tokens_list(posicion_actual)%valor) then
                self%textos_list(texton)%encendido = .true.
                return
            endif
            texton = texton +1
        end do
        !CLAVES
        do while(clavem <= self%numero_claves)
            if(self%claves_list(clavem)%nombre == self%tokens_list(posicion_actual)%valor) then
                self%claves_list(clavem)%encendido = .true.
                return
            endif
            clavem = clavem +1
        end do
        



    end subroutine EncenderSwitch

    subroutine setTexto(self, texto) 
        class(analizador_sintactico), intent(inout) :: self
        integer ::posicion_actual, contenedori, etiquetaj, botonk, clavem , texton
        character (len=*) :: texto
        posicion_actual = self%posicion_actual
        contenedori = 1 
        etiquetaj = 1  
        botonk = 1 
        clavem = 1  
        texton = 1
        !SI NO ESTA VERIFICAMOS SI ESTA EN ETIQUETAS
        do while(etiquetaj <= self%numero_etiquetas)
            if(self%etiquetas_list(etiquetaj)%encendido) then
                self%etiquetas_list(etiquetaj)%texto = texto
                self%etiquetas_list(etiquetaj)%encendido = .false.
                print*, 'texto agregado a: ',self%etiquetas_list(etiquetaj)%nombre,': ',texto
                return
            endif
            etiquetaj = etiquetaj +1
        end do
        !si no esta en etiquetas, verificamos botones
        do while(botonk <= self%numero_botones)
            if(self%botones_list(botonk)%encendido) then
                self%botones_list(botonk)%texto = texto
                self%botones_list(botonk)%encendido = .false.
                print*, 'texto agregado a: ',self%botones_list(botonk)%nombre,': ',texto
                return
            endif
            botonk = botonk +1
        end do
        !TEXTOS
        do while(texton <= self%numero_claves)
            if(self%textos_list(texton)%encendido) then
                self%textos_list(texton)%texto = texto
                self%textos_list(texton)%encendido = .false.
                print*, 'texto agregado a: ',self%textos_list(texton)%nombre,': ',texto
                return
            endif
            texton = texton +1
        end do
        !CLAVES
        do while(clavem <= self%numero_claves)
            if(self%claves_list(clavem)%encendido) then
                self%claves_list(clavem)%texto = texto
                print*, 'texto agregado a: ',self%claves_list(clavem)%nombre,': ',texto
                self%claves_list(clavem)%encendido = .false.
                return
            endif
            clavem = clavem +1
        end do
    end subroutine setTexto

    subroutine setAlto(self, valor) 
        class(analizador_sintactico), intent(inout) :: self
        integer ::posicion_actual, contenedori, etiquetaj, botonk, clavem , texton
        character (len=*) :: valor
        posicion_actual = self%posicion_actual
        contenedori = 1 
        etiquetaj = 1  
        botonk = 1 
        clavem = 1  
        texton = 1
        !verificamos si la variable esta en contenedor
        do while(contenedori <= self%numero_contenedores)
            if(self%contenedores_list(contenedori)%encendido) then
                self%contenedores_list(contenedori)%alto = valor
                self%contenedores_list(contenedori)%encendido = .false.
                print*, "Valor 'alto' agregado"
                return
            endif
            contenedori = contenedori +1
        end do
        !SI NO ESTA VERIFICAMOS SI ESTA EN ETIQUETAS
        do while(etiquetaj <= self%numero_etiquetas)
            if(self%etiquetas_list(etiquetaj)%encendido) then
                self%etiquetas_list(etiquetaj)%alto = valor
                self%etiquetas_list(etiquetaj)%encendido = .false.
                print*, "Valor 'alto' agregado"
                return
            endif
            etiquetaj = etiquetaj +1
        end do
        !si no esta en etiquetas, verificamos botones
        do while(botonk <= self%numero_botones)
            if(self%botones_list(botonk)%encendido) then
                self%botones_list(botonk)%alto = valor
                self%botones_list(botonk)%encendido = .false.
                print*, "Valor 'alto' agregado"
                return
            endif
            botonk = botonk +1
        end do
        !TEXTOS
        do while(texton <= self%numero_claves)
            if(self%textos_list(texton)%encendido) then
                self%textos_list(texton)%alto = valor
                self%textos_list(texton)%encendido = .false.
                print*, "Valor 'alto' agregado"
                return
            endif
            texton = texton +1
        end do
        !CLAVES
        do while(clavem <= self%numero_claves)
            if(self%claves_list(clavem)%encendido) then
                self%claves_list(clavem)%alto = valor
                self%claves_list(clavem)%encendido = .false.
                print*, "Valor 'alto' agregado"
                return
            endif
            clavem = clavem +1
        end do
    end subroutine setAlto

    subroutine setAncho(self, valor) 
        class(analizador_sintactico), intent(inout) :: self
        integer ::posicion_actual, contenedori, etiquetaj, botonk, clavem , texton
        character (len=*) :: valor
        posicion_actual = self%posicion_actual
        contenedori = 1 
        etiquetaj = 1  
        botonk = 1 
        clavem = 1  
        texton = 1
        !verificamos si la variable esta en contenedor
        do while(contenedori <= self%numero_contenedores)
            if(self%contenedores_list(contenedori)%encendido) then
                self%contenedores_list(contenedori)%ancho = valor
                self%contenedores_list(contenedori)%encendido = .false.
                print*, "Valor 'ancho' agregado"
                return
            endif
            contenedori = contenedori +1
        end do
        !SI NO ESTA VERIFICAMOS SI ESTA EN ETIQUETAS
        do while(etiquetaj <= self%numero_etiquetas)
            if(self%etiquetas_list(etiquetaj)%encendido) then
                self%etiquetas_list(etiquetaj)%ancho = valor
                self%etiquetas_list(etiquetaj)%encendido = .false.
                print*, "Valor 'ancho' agregado"
                return
            endif
            etiquetaj = etiquetaj +1
        end do
        !si no esta en etiquetas, verificamos botones
        do while(botonk <= self%numero_botones)
            if(self%botones_list(botonk)%encendido) then
                self%botones_list(botonk)%ancho = valor
                self%botones_list(botonk)%encendido = .false.
                print*, "Valor 'ancho' agregado"
                return
            endif
            botonk = botonk +1
        end do
        !TEXTOS
        do while(texton <= self%numero_claves)
            if(self%textos_list(texton)%encendido) then
                self%textos_list(texton)%ancho = valor
                self%textos_list(texton)%encendido = .false.
                print*, "Valor 'ancho' agregado"
                return
            endif
            texton = texton +1
        end do
        !CLAVES
        do while(clavem <= self%numero_claves)
            if(self%claves_list(clavem)%encendido) then
                self%claves_list(clavem)%ancho = valor
                self%claves_list(clavem)%encendido = .false.
                print*, "Valor 'ancho' agregado"
                return
            endif
            clavem = clavem +1
        end do
    end subroutine setAncho

    subroutine setColorLetra(self, color1, color2, color3) 
        class(analizador_sintactico), intent(inout) :: self
        integer ::  posicion_actual, contenedori, &
            etiquetaj, botonk, clavem , texton
        character (len=*) :: color1,color2,color3
        posicion_actual = self%posicion_actual
        contenedori = 1 
        etiquetaj = 1  
        botonk = 1 
        clavem = 1  
        texton = 1
        !SI NO ESTA VERIFICAMOS SI ESTA EN ETIQUETAS
        do while(etiquetaj <= self%numero_etiquetas)
            if(self%etiquetas_list(etiquetaj)%encendido) then
                self%etiquetas_list(etiquetaj)%color1 = color1
                self%etiquetas_list(etiquetaj)%color2 = color2
                self%etiquetas_list(etiquetaj)%color3 = color3
                self%etiquetas_list(etiquetaj)%encendido = .false.
                print*, "Valor 'color' agregado a:", self%etiquetas_list(etiquetaj)%nombre, ": ",&
                    color1," ",color2," ",color3
                return
            endif
            etiquetaj = etiquetaj +1
        end do
    end subroutine setColorLetra

    subroutine setColorFondo(self, color1, color2, color3) 
        class(analizador_sintactico), intent(inout) :: self
        integer :: posicion_actual, contenedori, &
            etiquetaj, botonk, clavem , texton
        character (len=*) :: color1,color2,color3
        posicion_actual = self%posicion_actual
        contenedori = 1 
        etiquetaj = 1  
        botonk = 1 
        clavem = 1  
        texton = 1
        !verificamos si la variable esta en contenedor
        do while(contenedori <= self%numero_contenedores)
            if(self%contenedores_list(contenedori)%encendido) then
                self%contenedores_list(contenedori)%color1 = color1
                self%contenedores_list(contenedori)%color2 = color2
                self%contenedores_list(contenedori)%color3 = color3
                self%contenedores_list(contenedori)%encendido = .false.
                print*, "Valor 'color' agregado a:", self%contenedores_list(contenedori)%nombre, ": ",&
                    color1," ",color2," ",color3
                return
            endif
            contenedori = contenedori +1
        end do
        
    end subroutine setColorFondo

    subroutine setPosicion(self, valor1, valor2) 
        class(analizador_sintactico), intent(inout) :: self
        integer ::posicion_actual, contenedori, etiquetaj, botonk, clavem , texton
        character (len=*) :: valor1, valor2
        posicion_actual = self%posicion_actual
        contenedori = 1 
        etiquetaj = 1  
        botonk = 1 
        clavem = 1  
        texton = 1
        !verificamos si la variable esta en contenedor
        do while(contenedori <= self%numero_contenedores)
            if(self%contenedores_list(contenedori)%encendido) then
                self%contenedores_list(contenedori)%posicion1= valor1
                self%contenedores_list(contenedori)%posicion2= valor2
                self%contenedores_list(contenedori)%encendido = .false.
                print*, "Valor 'alto' agregado"
                return
            endif
            contenedori = contenedori +1
        end do
        !SI NO ESTA VERIFICAMOS SI ESTA EN ETIQUETAS
        do while(etiquetaj <= self%numero_etiquetas)
            if(self%etiquetas_list(etiquetaj)%encendido) then
                self%etiquetas_list(etiquetaj)%posicion1 = valor1
                self%etiquetas_list(etiquetaj)%posicion2 = valor2
                self%etiquetas_list(etiquetaj)%encendido = .false.
                print*, "Valor 'alto' agregado"
                return
            endif
            etiquetaj = etiquetaj +1
        end do
        !si no esta en etiquetas, verificamos botones
        do while(botonk <= self%numero_botones)
            if(self%botones_list(botonk)%encendido) then
                self%botones_list(botonk)%posicion1 = valor1
                self%botones_list(botonk)%posicion2 = valor2
                self%botones_list(botonk)%encendido = .false.
                print*, "Valor 'alto' agregado"
                return
            endif
            botonk = botonk +1
        end do
        !TEXTOS
        do while(texton <= self%numero_claves)
            if(self%textos_list(texton)%encendido) then
                self%textos_list(texton)%posicion1 = valor1
                self%textos_list(texton)%posicion2 = valor2
                self%textos_list(texton)%encendido = .false.
                print*, "Valor 'alto' agregado"
                return
            endif
            texton = texton +1
        end do
        !CLAVES
        do while(clavem <= self%numero_claves)
            if(self%claves_list(clavem)%encendido) then
                self%claves_list(clavem)%posicion1 = valor1
                self%claves_list(clavem)%posicion2 = valor2
                self%claves_list(clavem)%encendido = .false.
                print*, "Valor 'alto' agregado"
                return
            endif
            clavem = clavem +1
        end do
    end subroutine setPosicion

    subroutine agregar(self, identificador) 
        class(analizador_sintactico), intent(inout) :: self
        integer :: posicion_actual, contenedori, &
            etiquetaj, botonk, clavem , texton, contenedorPagei,contenedorii
        character (len=*) :: identificador
        posicion_actual = self%posicion_actual
        contenedori = 1 
        etiquetaj = 1  
        botonk = 1 
        clavem = 1  
        texton = 1
        contenedorPagei = 1
        contenedorii = 1
         
        !verificamos si no se refiere a "this"
        if(self%main_page%encendido)then
            self%main_page%numero_contenedores_pagina = self%main_page%numero_contenedores_pagina+1
            !buscamos el contenedor con ese nombre
            do while (contenedorPagei <= self%numero_contenedores)
                if(self%contenedores_list(contenedorPagei)%nombre == identificador)then
                    self%main_page%contenedores_pag(self%main_page%numero_contenedores_pagina) = &
                        self%contenedores_list(contenedorPagei)
                endif
                contenedorPagei = contenedorPagei+1
            end do 
            self%main_page%encendido = .false.
            return
        endif
        !verificamos si la variable esta en contenedor
        do while(contenedori <= self%numero_contenedores)
            if(self%contenedores_list(contenedori)%encendido) then

                !verificamos que tipo de variable agregamos al contenedor
                !boton, etiqueta, etc
                !RECORREMOS CONTENEDORES
                do while (contenedorii<= self%numero_contenedores)
                    if(self%contenedores_list(contenedorii)%nombre == identificador)then
                        !aumentamos el numero de contenedores en el contador del contenedor
                        self%contenedores_list(contenedori)%num_contenedores_cont = &
                            self%contenedores_list(contenedori)%num_contenedores_cont +1
                        !agregamos el contenedor al contenedor
                        self%contenedores_list(contenedori)%contenedores_contenedor &
                            (self%contenedores_list(contenedori)%num_contenedores_cont) = &
                                self%contenedores_list(contenedorii)
                        !apagamos
                        self%contenedores_list(contenedori)%encendido = .false.
                        return
                    endif
                    contenedorii = contenedorii +1
                end do
                !RECORREMOS BOTONES
                do while (botonk <= self%numero_botones)
                    if(self%botones_list(botonk)%nombre == identificador)then
                        !aumentamos el numero de botones en el contador del contenedor
                        self%contenedores_list(contenedori)%num_botones_cont = &
                            self%contenedores_list(contenedori)%num_botones_cont +1
                        !agregamos el boton al contenedor
                        self%contenedores_list(contenedori)%botones_contenedor &
                            (self%contenedores_list(contenedori)%num_botones_cont) = &
                                self%botones_list(botonk)
                        !apagamos
                        self%contenedores_list(contenedori)%encendido = .false.
                        return
                    endif
                    botonk = botonk +1
                end do

                !RECORREMOS ETIQUETAS
                do while (etiquetaj <= self%numero_etiquetas)
                    if(self%etiquetas_list(etiquetaj)%nombre == identificador)then
                        !aumentamos el numero de etiquetas en el contador del contenedor
                        self%contenedores_list(contenedori)%num_etiquetas_cont = &
                            self%contenedores_list(contenedori)%num_etiquetas_cont +1
                        !agregamos la etiqueta al contenedor
                        self%contenedores_list(contenedori)%etiquetas_contenedor &
                            (self%contenedores_list(contenedori)%num_etiquetas_cont) = &
                                self%etiquetas_list(etiquetaj)
                        !apagamos
                        self%contenedores_list(contenedori)%encendido = .false.
                        return
                    endif
                    etiquetaj = etiquetaj +1
                end do

                !RECORREMOS CLAVES
                do while (clavem <= self%numero_claves)
                    if(self%claves_list(clavem)%nombre == identificador)then
                        !aumentamos el numero de claves en el contador del contenedor
                        self%contenedores_list(contenedori)%num_claves_cont = &
                            self%contenedores_list(contenedori)%num_claves_cont +1
                        !agregamos la clave al contenedor
                        self%contenedores_list(contenedori)%claves_contenedor &
                            (self%contenedores_list(contenedori)%num_claves_cont) = &
                                self%claves_list(clavem)
                        !apagamos
                        self%contenedores_list(contenedori)%encendido = .false.
                        return
                    endif
                    clavem = clavem +1
                end do

                !RECORREMOS TEXTOS
                do while (texton <= self%numero_textos)
                    if(self%textos_list(texton)%nombre == identificador)then
                        !aumentamos el numero de textos en el contador del contenedor
                        self%contenedores_list(contenedori)%num_textos_cont = &
                            self%contenedores_list(contenedori)%num_textos_cont +1
                        !agregamos el texto al contenedor
                        self%contenedores_list(contenedori)%textos_contenedor &
                            (self%contenedores_list(contenedori)%num_textos_cont) = &
                                self%textos_list(texton)
                        !apagamos
                        self%contenedores_list(contenedori)%encendido = .false.
                        return
                    endif
                    texton = texton +1
                end do
                

                self%contenedores_list(contenedori)%encendido = .false.
                return
            endif
            contenedori = contenedori +1
        end do
        
    end subroutine agregar
!-------------------------SUBRUTINAS PARA AGREGAR CONTROLES----------------------------------------
!CONTENEDOR
    subroutine agregarContenedor (self)

        class(analizador_sintactico), intent(inout) :: self
        character(len=50) :: identificador
        integer:: posicion_actual
        posicion_actual = self%posicion_actual
        identificador = self%tokens_list(posicion_actual)%valor
        self%numero_contenedores = self%numero_contenedores +1

        if(self%numero_contenedores > size(self%contenedores_list))then
            print*, 'capacidad maxima de contenedores alcanzada'
            return
        endif
        !CUANDO SE CREA EL CONTENEDOR, ALLOCATE SUS LISTAS
        allocate(self%contenedores_list(self%numero_contenedores)%botones_contenedor(10))
        allocate(self%contenedores_list(self%numero_contenedores)%etiquetas_contenedor(10))
        allocate(self%contenedores_list(self%numero_contenedores)%textos_contenedor(10))
        allocate(self%contenedores_list(self%numero_contenedores)%claves_contenedor(10))
        allocate(self%contenedores_list(self%numero_contenedores)%checks_contenedor(10))
        allocate(self%contenedores_list(self%numero_contenedores)%contenedores_contenedor(10))
        self%contenedores_list(self%numero_contenedores)%nombre = identificador
        self%contenedores_list(self%numero_contenedores)%texto = ' '
        self%contenedores_list(self%numero_contenedores)%posicion1 = ' '
        self%contenedores_list(self%numero_contenedores)%posicion2 = ' '
        self%contenedores_list(self%numero_contenedores)%color1 = ' '
        self%contenedores_list(self%numero_contenedores)%color2 = ' '
        self%contenedores_list(self%numero_contenedores)%color3 = ' '
        self%contenedores_list(self%numero_contenedores)%alto = ' '
        self%contenedores_list(self%numero_contenedores)%ancho = ' '
        self%contenedores_list(self%numero_contenedores)%encendido = .false.
        self%contenedores_list(self%numero_contenedores)%tipo = 'CONTENEDOR'
        print*, 'contenedor agregado',':', trim(identificador)

    end subroutine agregarContenedor
!ETIQUETA
    subroutine agregarEtiqueta (self)

        class(analizador_sintactico), intent(inout) :: self
        character(len=50) :: identificador
        integer:: posicion_actual
        posicion_actual = self%posicion_actual
        identificador = self%tokens_list(posicion_actual)%valor
        self%numero_etiquetas = self%numero_etiquetas +1

        if(self%numero_etiquetas > size(self%etiquetas_list))then
            print*, 'capacidad maxima de etiquetas alcanzada'
            return
        endif

        self%etiquetas_list(self%numero_etiquetas)%nombre = identificador
        self%etiquetas_list(self%numero_etiquetas)%texto = ' '
        self%etiquetas_list(self%numero_etiquetas)%posicion1 = ' '
        self%etiquetas_list(self%numero_etiquetas)%posicion2 = ' '
        self%etiquetas_list(self%numero_etiquetas)%color1 = ' '
        self%etiquetas_list(self%numero_etiquetas)%color2 = ' '
        self%etiquetas_list(self%numero_etiquetas)%color3 = ' '
        self%etiquetas_list(self%numero_etiquetas)%alto = ' '
        self%etiquetas_list(self%numero_etiquetas)%ancho = ' '
        self%etiquetas_list(self%numero_etiquetas)%encendido = .false.
        self%contenedores_list(self%numero_contenedores)%tipo = 'ETIQUETA'
        print*, 'Etiqueta agregado',':', trim(identificador)

    end subroutine agregarEtiqueta
!BOTON
    subroutine agregarBoton (self)

        class(analizador_sintactico), intent(inout) :: self
        character(len=50) :: identificador
        integer:: posicion_actual
        posicion_actual = self%posicion_actual
        identificador = self%tokens_list(posicion_actual)%valor
        self%numero_botones = self%numero_botones +1

        if(self%numero_botones > size(self%botones_list))then
            print*, 'capacidad maxima de botones alcanzada'
            return
        endif

        self%botones_list(self%numero_botones)%nombre = identificador
        self%botones_list(self%numero_botones)%encendido = .false.
        self%botones_list(self%numero_botones)%texto = ' '
        self%botones_list(self%numero_botones)%posicion1 = ' '
        self%botones_list(self%numero_botones)%posicion2 = ' '
        self%botones_list(self%numero_botones)%color1 = ' '
        self%botones_list(self%numero_botones)%color2 = ' '
        self%botones_list(self%numero_botones)%color3 = ' '
        self%botones_list(self%numero_botones)%alto = ' '
        self%botones_list(self%numero_botones)%ancho = ' '
        self%botones_list(self%numero_botones)%tipo = 'BOTON'
        print*, 'BOTON agregado',':', trim(identificador)

    end subroutine agregarBoton
!CHECK
!TEXTO
    subroutine agregarTexto (self)

        class(analizador_sintactico), intent(inout) :: self
        character(len=50) :: identificador
        integer:: posicion_actual
        posicion_actual = self%posicion_actual
        identificador = self%tokens_list(posicion_actual)%valor
        self%numero_textos = self%numero_textos +1

        if(self%numero_textos > size(self%textos_list))then
            print*, 'capacidad maxima de botones alcanzada'
            return
        endif

        self%textos_list(self%numero_textos)%nombre = identificador
        self%textos_list(self%numero_textos)%texto = ' '
        self%textos_list(self%numero_textos)%posicion1 = ' '
        self%textos_list(self%numero_textos)%posicion2 = ' '
        self%textos_list(self%numero_textos)%color1 = ' '
        self%textos_list(self%numero_textos)%color2 = ' '
        self%textos_list(self%numero_textos)%color3 = ' '
        self%textos_list(self%numero_textos)%alto = ' '
        self%textos_list(self%numero_textos)%ancho = ' '
        self%textos_list(self%numero_textos)%encendido = .false. 
        self%contenedores_list(self%numero_contenedores)%tipo = 'TEXTO'
        print*, 'BOTON agregado',':', trim(identificador)

    end subroutine agregarTexto
!CLAVE
    subroutine agregarClave (self)

        class(analizador_sintactico), intent(inout) :: self
        character(len=50) :: identificador
        integer:: posicion_actual
        posicion_actual = self%posicion_actual
        identificador = self%tokens_list(posicion_actual)%valor
        self%numero_claves = self%numero_claves +1

        if(self%numero_claves > size(self%claves_list))then
            print*, 'capacidad maxima de botones alcanzada'
            return
        endif

        self%claves_list(self%numero_claves)%nombre = identificador
        self%claves_list(self%numero_claves)%texto = ' '
        self%claves_list(self%numero_claves)%posicion1 = ' '
        self%claves_list(self%numero_claves)%posicion2 = ' '
        self%claves_list(self%numero_claves)%color1 = ' '
        self%claves_list(self%numero_claves)%color2 = ' '
        self%claves_list(self%numero_claves)%color3 = ' '
        self%claves_list(self%numero_claves)%alto = ' '
        self%claves_list(self%numero_claves)%ancho = ' '
        self%claves_list(self%numero_claves)%encendido = .false.
        print*, 'CLAVE agregado',':', trim(identificador)

    end subroutine agregarClave
!RADIO_BOTON
!AREA_TEXTO
!---------------------------FIN SUBRUTINAS AGREGAR CONTROLES---------------------------------------------
    
    subroutine verificarTipoControl(self)
        class(analizador_sintactico),intent(inout) :: self
        integer :: posicion_actual
        posicion_actual = self%posicion_actual
        select case(self%tokens_list(posicion_actual)%tipo)
            case("RESERVADA_CONTENEDOR")

                if(.not. token_match(self,"RESERVADA_CONTENEDOR"))return
                !agregamos el contenedor si lo que le sigue es un identificador
                if(verificarIdentificador(self))then
                    call agregarContenedor(self)
                endif

            case("RESERVADA_ETIQUETA")
                if(.not. token_match(self,"RESERVADA_ETIQUETA"))return

                if(verificarIdentificador(self))then
                    call agregarEtiqueta(self)
                endif
            case("RESERVADA_BOTON")
                if(.not. token_match(self,"RESERVADA_BOTON"))return

                if(verificarIdentificador(self))then
                    call agregarBoton(self)
                endif
            case("RESERVADA_TEXTO")
                if(.not. token_match(self,"RESERVADA_TEXTO"))return

                if(verificarIdentificador(self))then
                    call agregarTexto(self)
                endif
            case("RESERVADA_CLAVE")
                if(.not. token_match(self,"RESERVADA_CLAVE"))return

                if(verificarIdentificador(self))then
                    call agregarClave(self)
                endif
            case("RESERVADA_CHECK")
                if(.not. token_match(self,"RESERVADA_CHECK"))return
            case("RESERVADA_RADIO_BOTON")
                if(.not. token_match(self,"RESERVADA_RADIO_BOTON"))return
            case("RESERVADA_AREA_TEXTO")
                if(.not. token_match(self,"RESERVADA_AREA_TEXTO"))return
            case default
                print *, 'control no valido'
        end select
    end subroutine
    logical function tipoValidoControl(self)
        class(analizador_sintactico), intent(inout) :: self
        integer :: posicion_actual
        posicion_actual = self%posicion_actual
            tipoValidoControl = .false.
            if(self%tokens_list(posicion_actual)%tipo == 'RESERVADA_CONTENEDOR'&
                .or. self%tokens_list(posicion_actual)%tipo == 'RESERVADA_ETIQUETA'&
                .or. self%tokens_list(posicion_actual)%tipo =='RESERVADA_BOTON'&
                .or. self%tokens_list(posicion_actual)%tipo =='RESERVADA_TEXTO'&
                .or. self%tokens_list(posicion_actual)%tipo =='RESERVADA_CHECK'&
                .or. self%tokens_list(posicion_actual)%tipo =='RESERVADA_RADIO_BOTON'&
                .or. self%tokens_list(posicion_actual)%tipo =='RESERVADA_AREA_TEXTO'&
                .or. self%tokens_list(posicion_actual)%tipo =='RESERVADA_CLAVE'&
                    ) then
                tipoValidoControl = .true.
            end if

    end function tipoValidoControl

    logical function verificarIdentificador(self)

        class(analizador_sintactico), intent(inout) :: self
        integer :: posicion_actual
        posicion_actual = self%posicion_actual
        verificarIdentificador = .false.

        if(self%tokens_list(posicion_actual)%tipo == 'IDENTIFICADOR')verificarIdentificador = .true.

    end function verificarIdentificador

    subroutine parsearLineaControl(self)
        class(analizador_sintactico), intent(inout) :: self
        integer :: posicion_actual
        posicion_actual = self%posicion_actual

        call verificarTipoControl(self)
        if(.not. token_match(self, "IDENTIFICADOR"))then 
            call agregarErrorSintactico(self, "se esperaba identificador",self%posicion_actual)
            call modoPanico(self, ';')
            return
        endif
        if(.not. token_match(self, "PUNTO_Y_COMA"))then 
            call agregarErrorSintactico(self, "se esperaba punto y coma",self%posicion_actual)
            call modoPanico(self, ';')
            return
        endif

    end subroutine parsearLineaControl

    recursive subroutine parsear_controles(self)
        class(analizador_sintactico), intent(inout) :: self
        if (tipoValidoControl(self))then 
                call parsearLineaControl(self)
                call parsear_controles(self)
        end if

    end subroutine parsear_controles

    logical function token_match(self, tipo_esperado)
        class(analizador_sintactico), intent (inout) :: self
        character(len=*) ,intent(in):: tipo_esperado
        integer :: posicion_actual
        posicion_actual = self%posicion_actual
        token_match = .false.
        if(self%tokens_list(posicion_actual)%tipo == tipo_esperado)then
            token_match = .true.
            self%posicion_actual = posicion_actual +1
        end if

    end function token_match

    function buscar_boton(self, nombre_boton) result(boton_encontrado)
        class(analizador_sintactico), intent(inout) :: self
        type(boton) :: boton_encontrado
        character(len=50) :: boton_busqueda, nombre_boton
        integer :: i
        i = 1
        boton_busqueda = nombre_boton
        boton_encontrado%nombre = 'NONE'
        do while (i <= size(self%botones_list))
            if (self%botones_list(i)%nombre == nombre_boton)then
                boton_encontrado = self%botones_list(i)
                return 
            end if
            i = i+1
        end do

    end function buscar_boton

    subroutine modoPanico(self, tokenSinc)
        class(analizador_sintactico), intent(inout) :: self
        character(len = *) :: tokenSinc
        integer:: posicion_actual
        posicion_actual = self%posicion_actual

        do while(posicion_actual <= size(self%tokens_list) .and. self%tokens_list(posicion_actual)%valor /= tokenSinc)
            posicion_actual = posicion_actual+1
            self%posicion_actual = posicion_actual
        end do

        !cuando el caracter sea encontrado, avanzamos una posicion
        self%posicion_actual = self%posicion_actual+1

    end subroutine modoPanico

    subroutine agregarErrorSintactico(self, tipo, linea)
        class (analizador_sintactico), intent(inout) :: self
        character (len = *) :: tipo
        integer :: linea

        self%numero_errores = self%numero_errores +1

        !agregamos el error
        self%erroresSintacticos(self%numero_errores)%tipo = tipo
        self%erroresSintacticos(self%numero_errores)%linea = linea
    end subroutine

    subroutine enviarErrores(self)
        class(analizador_sintactico), intent(inout) :: self
        integer :: unidad, i
        unidad = 10
        ! Abrir el archivopara escritura
        open(unit = unidad, file = 'C:\Users\cesar\OneDrive\Documentos\Fortran\Proyecto 2\listaErrores.txt',&
             status = 'old', action = 'write', position = 'append')

        !escribimos sobre el archivo de errores
        do i = 1, self%numero_errores
            write(unidad, '(A , A)') 'Tipo:',trim(self%erroresSintacticos(i)%tipo)
            write(unidad, '(A , A)') 'linea:',char(self%erroresSintacticos(i)%linea)
        end do
        close(unidad)
    end subroutine enviarErrores
    subroutine actualizarMainPage(self)
        class(analizador_sintactico), intent(inout) :: self
        integer :: contenedori, contenedorj, contadorSintac
        contenedori = 1
        do while (contenedori <= self%main_page%numero_contenedores_pagina)
            contenedorj = 1
            do while(contenedorj <= self%main_page%contenedores_pag(contenedori)%num_contenedores_cont)
                contadorSintac = 1
                do while(contadorSintac <= self%numero_contenedores)
                    if(self%main_page%contenedores_pag(contenedori)%contenedores_contenedor(contenedorj)%nombre &
                        == self%contenedores_list(contadorSintac)%nombre) then

                        self%main_page%contenedores_pag(contenedori)%contenedores_contenedor(contenedorj) &
                            = self%contenedores_list(contadorSintac)
                        
                        exit
                    end if
                    contadorSintac = contadorSintac +1
                end do
                contenedorj = contenedorj +1
            end do
            contenedori = contenedori+1
        end do

    end subroutine


    subroutine generarHtml(self)
        class(analizador_sintactico), intent(inout) :: self
        integer :: unidad, i, j,k
        unidad = 10
        !actualizamos las instancias de los contenedores en nuestra pagina
        call actualizarMainPage(self)
        i = 1
        ! Abrir el archivo HTML para escritura
        open(unit = unidad, file = 'C:\Users\cesar\OneDrive\Documentos\Fortran\Proyecto 2\pagina.html', status = 'replace')
    
        ! Escribir el encabezado del HTML
        write(unidad, *) '<html>'
        write(unidad, *) '<head> <link href = "style.css" rel = "stylesheet" type = "text/css"/> </head>'
        write(unidad, *) '<body>'
        !agregamos las variables
        do while(i <= self%main_page%numero_contenedores_pagina)
            write(unidad, *) '<div id = "'//trim(self%main_page%contenedores_pag(i)%nombre)//'">'
            !verificamos que ese contenedor no tenga botones, textos, etc, si tiene, los agregamos
            if (allocated(self%main_page%contenedores_pag(i)%contenedores_contenedor)) then
                j = 1
                do while (j <= self%main_page%contenedores_pag(i)%num_contenedores_cont)
                    write(unidad, *) '<div id = "'//trim(self%main_page%contenedores_pag(i)%contenedores_contenedor(j)%nombre)//'">'
                    !botones?
                    if(allocated(self%main_page%contenedores_pag(i)%contenedores_contenedor(j)%botones_contenedor)) then
                        k = 1
                        do while(k <= self%main_page%contenedores_pag(i)%contenedores_contenedor(j)%num_botones_cont)
                            write(unidad, *) '<input type = "submit" value = "', &
                            self%main_page%contenedores_pag(i)%contenedores_contenedor(j)%botones_contenedor(k)%texto &
                                    ,'" style = "text-align:center"/>'
                            k = k+1
                        end do
                    end if
                    !etiquetas?
                    if(allocated(self%main_page%contenedores_pag(i)%contenedores_contenedor(j)%etiquetas_contenedor)) then
                        k = 1
                        do while(k <= self%main_page%contenedores_pag(i)%contenedores_contenedor(j)%num_etiquetas_cont)
                            write(unidad, *) '<label id = "Nombre">', &
                            self%main_page%contenedores_pag(i)%contenedores_contenedor(j)%etiquetas_contenedor(k)%nombre &
                                    ,'</label>'
                            k = k+1
                        end do
                    end if
                    !claves?
                    if(allocated(self%main_page%contenedores_pag(i)%contenedores_contenedor(j)%claves_contenedor)) then
                        k = 1
                        do while(k <= self%main_page%contenedores_pag(i)%contenedores_contenedor(j)%num_claves_cont)
                            write(unidad, *) '<input type = "password" id = "', &
                            self%main_page%contenedores_pag(i)%contenedores_contenedor(j)%claves_contenedor(k)%nombre &
                                    ,'" value = "" style = "text-align:left" />'
                            k = k+1
                        end do
                    end if
                    !textos?
                    if(allocated(self%main_page%contenedores_pag(i)%contenedores_contenedor(j)%textos_contenedor)) then
                        k = 1
                        do while(k <= self%main_page%contenedores_pag(i)%contenedores_contenedor(j)%num_textos_cont)
                            write(unidad, *) '<input type = "text" id = "', &
                            self%main_page%contenedores_pag(i)%contenedores_contenedor(j)%textos_contenedor(k)%nombre &
                                    ,'"value = "" style = "text-align:left" />'
                            k = k+1
                        end do
                    end if
                    write(unidad, *) '</div>'
                    j = j+1
                end do

            endif
            write(unidad, *) '</div>'
            i = i+1
        end do
        
    
        ! Cerrar la tabla y el documento HTML
        write(unidad, *) '</body>'
        write(unidad, *) '</html>'
    
        ! Cerrar el archivo
        close(unidad)
        call generarCss(self)
    end subroutine generarHtml

    subroutine generarCss(self)
        class(analizador_sintactico), intent(inout) :: self
        integer :: unidad, i
        unidad = 10
        !actualizamos las instancias de los contenedores en nuestra pagina
        i = 1
        ! Abrir el archivo CSS para escritura
        open(unit = unidad, file = 'C:\Users\cesar\OneDrive\Documentos\Fortran\Proyecto 2\style.css', status = 'replace')
    
        ! Agregar valores al css
        !Empezamos a escribir los contenedores
        do while(i <= self%numero_contenedores)
            write(unidad, '(A)') '#'//trim(self%contenedores_list(i)%nombre)//'{'
            !posicion?
            if(.not. verificarCampo(self%contenedores_list(i)%posicion1))then
                write(unidad, '(A)') 'position: absolute;'
                write(unidad, '(A)') 'left:'//trim(self%contenedores_list(i)%posicion1)//'px;'
                if(.not. verificarCampo(self%contenedores_list(i)%posicion2))then
                    write(unidad, '(A)') 'top:'//trim(self%contenedores_list(i)%posicion2)//'px;'
                end if
            endif
            !ancho?
            if(.not. verificarCampo(self%contenedores_list(i)%ancho))write(unidad, '(A)') 'width:'&
                //trim(self%contenedores_list(i)%ancho)//'px;'
            !alto?
            if(.not. verificarCampo(self%contenedores_list(i)%alto))write(unidad, '(A)') 'height:'&
                //trim(self%contenedores_list(i)%alto)//'px;'
            !colorFondo?
            if(.not. verificarCampo(self%contenedores_list(i)%color1))then
                write(unidad, '(A)') 'background-color: rgb('//trim(self%contenedores_list(i)%color1)//','&
                    //trim(self%contenedores_list(i)%color2)//','//trim(self%contenedores_list(i)%color3)//');'
            endif
            write(unidad, *)'font-size: 12px;'
            write(unidad, *)'}'
            i = i+1
        end do
        i = 1
        !ahora vamos con etiquetas
        do while(i <= self%numero_etiquetas)
            write(unidad, *) '#'//trim(self%etiquetas_list(i)%nombre)//'{'
            !posicion?
            if(.not. verificarCampo(self%etiquetas_list(i)%posicion1))then
                write(unidad, '(A)') 'position: absolute;'
                write(unidad, '(A)') 'left:'//trim(self%etiquetas_list(i)%posicion1)//'px;'
                if(.not. verificarCampo(self%etiquetas_list(i)%posicion2))then
                    write(unidad, '(A)') 'top:'//trim(self%etiquetas_list(i)%posicion2)//'px;'
                end if
            endif
            !ancho?
            if(.not. verificarCampo(self%etiquetas_list(i)%ancho))write(unidad, '(A)') 'width:'// &
                trim(self%etiquetas_list(i)%ancho)//'px;'
            !alto?
            if(.not. verificarCampo(self%etiquetas_list(i)%alto))write(unidad, '(A)') 'height:'//&
                trim(self%etiquetas_list(i)%alto)//'px;'
            !colorLetra?
            if(.not. verificarCampo(self%etiquetas_list(i)%color1))then
                write(unidad, '(A)') 'color: rgb('//trim(self%etiquetas_list(i)%color1)//','&
                    //trim(self%etiquetas_list(i)%color2)//','//trim(self%etiquetas_list(i)%color3)//');'
            endif
            write(unidad, *)'font-size: 12px;'
            write(unidad, *)'}'
            i = i+1
        end do
        i = 1
        !ahora botones
        do while(i <= self%numero_botones)
            write(unidad, *) '#'//trim(self%botones_list(i)%nombre)//'{'
            !posicion?
            if(.not. verificarCampo(self%botones_list(i)%posicion1))then
                write(unidad, '(A)') 'position: absolute;'
                write(unidad, '(A)') 'left:'//trim(self%botones_list(i)%posicion1)//'px;'
                if(.not. verificarCampo(self%botones_list(i)%posicion2))then
                    write(unidad, '(A)') 'top:'//trim(self%botones_list(i)%posicion2)//'px;'
                end if
            endif
            !ancho?
            if(.not. verificarCampo(self%botones_list(i)%ancho))write(unidad, '(A)') 'width:'// &
                trim(self%botones_list(i)%ancho)//'px;'
            !alto?
            if(.not. verificarCampo(self%botones_list(i)%alto))write(unidad, '(A)') 'height:'// &
                trim(self%botones_list(i)%alto)//'px;'
            write(unidad, *)'font-size: 12px;'
            write(unidad, *)'}'
            i = i+1
        end do
        i = 1
        !textos
        do while(i <= self%numero_textos)
            write(unidad, *) '#'//trim(self%textos_list(i)%nombre)//'{'
            !posicion?
            if(.not. verificarCampo(self%textos_list(i)%posicion1))then
                write(unidad, '(A)') 'position: absolute;'
                write(unidad, '(A)') 'left:'//trim(self%textos_list(i)%posicion1)//'px;'
                if(.not. verificarCampo(self%textos_list(i)%posicion2))then
                    write(unidad, '(A)') 'top:'//trim(self%textos_list(i)%posicion2)//'px;'
                end if
            endif
            !ancho?
            if(.not. verificarCampo(self%textos_list(i)%ancho))write(unidad, '(A)') 'width:'// &
                self%textos_list(i)%ancho//'px;'
            !alto?
            if(.not. verificarCampo(self%textos_list(i)%alto))write(unidad, '(A)') 'height:'//&
                self%textos_list(i)%alto//'px;'
            write(unidad, *)'font-size: 12px;'
            write(unidad, *)'}'
            i = i+1
        end do
        i = 1
        !claves
        
        do while(i <= self%numero_claves)
            write(unidad, *)  '#'//trim(self%claves_list(i)%nombre)//'{'
            !posicion?
            if(.not. verificarCampo(self%claves_list(i)%posicion1))then
                write(unidad, '(A)') 'position: absolute;'
                write(unidad, '(A)') 'left:'//trim(self%claves_list(i)%posicion1)//'px;'
                if(.not. verificarCampo(self%claves_list(i)%posicion2))then
                    write(unidad, '(A)') 'top:'//trim(self%claves_list(i)%posicion2)//'px;'
                end if
            endif
            !ancho?
            if(.not. verificarCampo(self%claves_list(i)%ancho))write(unidad, '(A)') 'width:'//&
                trim(self%claves_list(i)%ancho)//trim('px;')
            !alto?
            if(.not. verificarCampo(self%claves_list(i)%alto))write(unidad, '(A)') 'height:'//&
                trim(self%claves_list(i)%alto)//'px;'
            !colorLetra?
            if(.not. verificarCampo(self%claves_list(i)%color1))then
                write(unidad, '(A)') 'color: rgb('//trim(self%claves_list(i)%color1)//','&
                    //trim(self%claves_list(i)%color2)//','//trim(self%claves_list(i)%color3)//');'
            endif
            write(unidad, *)'font-size: 12px;'
            write(unidad, *)'}'
            i = i+1
        end do
    
        ! Cerrar el archivo
        close(unidad)
    end subroutine generarCss

    logical function verificarCampo(valor)
        character(len = *), intent(in) :: valor

        verificarCampo = .false.
        if(len_trim(valor) == 0 )verificarCampo = .true.

    end function verificarCampo



end module Analizador_sintactico_module

program proyecto_1
    use Analizador_Lexico_module
    use Analizador_sintactico_module
    implicit none

    type (analizador_lexico) :: lexer
    type(analizador_sintactico) :: sintact
    integer :: i, ios
    character (len =5000) :: entrada, linea
    integer :: unit

     ! Inicializar el archivo de entrada
     ! Inicializar el archivo de entrada
    unit = 10
    open(unit=unit, file="entrada.txt",&
             status="old", action="read", iostat=ios)
    if (ios /= 0) then
        print*, "Error al abrir el archivo de entrada."
        stop
    end if

    ! Leer el contenido del archivo
    entrada = ''
    do
        read(unit, '(A)', iostat=ios) linea
        if (ios /= 0) exit   ! Se alcanzó el fin del archivo
        entrada = trim(entrada) // trim(linea) // char(10) ! Concatenar la línea leída al valor de entrada y agregar un salto de línea
    end do
    close(unit)

    ! Llamada al analizador léxico con el texto leído
    call lexer%analizar(trim(entrada))
    call sintact%inicializar(lexer%tokens)
    call lexer%grabarTokens()
    call sintact%parsear()
    call sintact%generarHtml()
    call lexer%imprimirErroresLexicos()
    call sintact%enviarErrores()
    
    ! Mostrar el contenido leído
    print*, "Texto leido desde el archivo:"
    print*, trim(entrada)

    
    ! Mostrar el número de tokens encontrados
    print*, 'Tokens encontrados:', lexer%num_tokens
    do i = 1, lexer%num_tokens
        print*, 'Tipo: ', lexer%tokens(i)%tipo, 'valor: ', lexer%tokens(i)%valor
    end do
    print*, 'errores_encontrados:', lexer%num_errores
    do i = 1, lexer%num_errores
        print*, 'Tipo: ', lexer%errores_list(i)%tipo, 'valor: ', lexer%errores_list(i)%valor
    end do
end program