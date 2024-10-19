from Atomos import Atomos
from Atomo import Atomo

# Dicionario para relacionar a string da palavra reservada
# que irá aparecer no analisador com o valor definido
# para cada atomo correspondente
palavras_reservadas = {
        "begin" : Atomos.BEGIN.value, "boolean" : Atomos.BOOLEAN.value,
        "div" : Atomos.DIV.value, "do" : Atomos.DO.value,
        "else" : Atomos.ELSE.value, "end" : Atomos.END.value, 
        "false" : Atomos.FALSE.value, "if" : Atomos.IF.value,
        "integer" : Atomos.INTEGER.value, "mod" : Atomos.MOD.value, 
        "program" : Atomos.PROGRAM.value, "read" : Atomos.READ.value,
        "then" : Atomos.THEN.value, "true" : Atomos.TRUE.value,
        "not" : Atomos.NOT.value, "var" : Atomos.VAR.value,
        "while" : Atomos.WHILE.value, "write" : Atomos.WRITE.value,
        "or" : Atomos.OR.value, "and" : Atomos.AND.value
        }

# Declaração da classe Analisador_Lexico
class AnalisadorLexico:
    # Declaração do construtor do metodo init
    def __init__(self, buffer):
        self.buffer = buffer + "\0"
        self.linha = 1
        self.index = 0

    # Declaração do metodo retrair para tratar casos em que
    # retornaremos um tipo de Atomo, porém não consumiremos
    # o caracter quando estiver no estado final do AFD
    def retrair(self):
        self.index -= 1

    # Declaração do metodo proximo_char, onde iremos
    # pegar cada caracter do atributo self.buffer
    def proximo_char(self):
        char = self.buffer[self.index]
        self.index += 1
        return char

    # Declaração do metodo proximo_atomo, onde iremos
    # fazer a análise de cada átomo presente em nosso
    # analisador
    def proximo_atomo(self):
        char = self.proximo_char()
        while (char in [" ", "\n", "\t", "\r", "\0"]):
            if (char == "\0"):
                return Atomo(Atomos.EOS.value, "\\0", 0, 0, self.linha)
            elif (char == "\n"):
                self.linha += 1
            char = self.proximo_char()
        if (char.isalpha() or char == "_"):
            return self.tratar_identificador(char)
        elif (char.isdigit()):
            return self.tratar_numero(char)
        elif (char in ["<", "=", ">"]):
            return self.tratar_operador_relacional(char)
        elif (char in ["+", "-"]):
            return self.tratar_operador_de_adicao(char)
        elif (char in ["*", "/"] ):
            if (char == "/" and self.proximo_char() == "/"):
                self.retrair()
                return self.tratar_comentarios_e_parenteses(char)
            elif (char == "*" and self.proximo_char() == ")"):
                self.retrair()
                return self.tratar_comentarios_e_parenteses(char)
            else:
                return self.tratar_operador_de_multiplicacao(char)
        elif (char in [":", ";", ".", ","]):
            return self.tratar_operador_de_atribuicao_e_pontos(char)
        elif (char in ["(", ")", "{", "}"]):
                return self.tratar_comentarios_e_parenteses(char)
        else:
            print(f"Erro léxico: Linha -> {self.linha} : Caractere '{char}")
            return Atomo(Atomos.ERRO.value, "", 0, 0, self.linha)

    # Declaração do metodo tratar_identificador, onde iremos
    # tratar os atomos em caso do retorno no metodo
    # proximo_char ser um caracter ou underline "_"
    def tratar_identificador(self, char: str):
        lexema = char
        char = self.proximo_char()
        estado = 1
        while (True):
            if (len(lexema) <= 20):
                match (estado):
                    case 1:
                        if (char.isdigit() or char.isalpha() or char == "_"):
                            lexema += char
                            estado = 1
                            char = self.proximo_char()
                        else:
                            estado = 2
                    case 2:
                        self.retrair()
                        if (lexema.lower() in palavras_reservadas):
                            return Atomo(palavras_reservadas[lexema.lower()], lexema, 0, 0, self.linha)
                        else:
                            return Atomo(Atomos.IDENTIF.value, lexema, 0, 0, self.linha)
            else:
                return Atomo(Atomos.ERRO.value, lexema, 0, 0, self.linha)

    # Declaração do metodo tratar_numero, onde iremos
    # tratar os atomos em caso do retorno no metodo
    # proximo_char ser um digito
    def tratar_numero(self, char: str):
        lexema = char
        char = self.proximo_char()
        estado = 1
        while (True):
            match estado:
                case 1:
                    if (char.isdigit()):
                        lexema += char
                        estado = 1
                        char = self.proximo_char()
                    else:
                        estado = 2
                case 2:
                    self.retrair()
                    return Atomo(Atomos.NUM.value, lexema, int(lexema), 0, self.linha)

    # Declaração do metodo tratar_operador_relacional, onde iremos
    # tratar os atomos em caso do retorno no metodo 
    # proximo char ser um operador relacional
    def tratar_operador_relacional(self, char: str):
        lexema = char
        # Variavel que ira verificar se o char está entre ["<", "=", ">"]
        char_anterior = char 
        # Variavel que ira verificar qual char será depois do char_anterior
        char = self.proximo_char()
        estado = 0
        while (True):
            match estado:
                case 0:
                    if (char_anterior == "<"):
                        estado = 1
                    elif (char_anterior == ">"):
                        estado = 2
                    else:
                        estado = 3
                case 1:
                    if (char == "="):
                        lexema += char
                        estado = 4
                    elif (char == ">"):
                        lexema += char
                        estado = 5
                    else:
                        estado = 6
                case 2:
                    if (char == "="):
                        lexema += char
                        estado = 7
                    else:
                        estado = 8
                case 3:
                    return Atomo(Atomos.RELOP.value, lexema, 0, Atomos.RELOP_EQ.value, self.linha)
                case 4:
                    return Atomo(Atomos.RELOP.value, lexema, 0, Atomos.RELOP_LE.value, self.linha)
                case 5:
                    return Atomo(Atomos.RELOP.value, lexema, 0, Atomos.RELOP_NE.value, self.linha)
                case 6:
                    return Atomo(Atomos.RELOP.value, lexema, 0, Atomos.RELOP_LT.value, self.linha)
                case 7:
                    return Atomo(Atomos.RELOP.value, lexema, 0, Atomos.RELOP_GE.value, self.linha)
                case 8:
                    return Atomo(Atomos.RELOP.value, lexema, 0, Atomos.RELOP_GT.value, self.linha)

    # Declaração do metodo tratar_operador_de_adicao, onde iremos
    # tratar os atomos em caso do retorno no metodo 
    # proximo char ser um operador de adicao ou subtracao
    def tratar_operador_de_adicao(self, char: str):
        lexema = char
        estado = 0
        while (True):
            match estado:
                case 0:
                    if (char == "+"):
                        estado = 1
                    elif (char == "-"):
                        estado = 2
                case 1:
                    return Atomo(Atomos.ADDOP.value, lexema, 0, Atomos.ADDOP_SOMA.value, self.linha)
                case 2:
                    return Atomo(Atomos.ADDOP.value, lexema, 0, Atomos.ADDOP_SUBT.value, self.linha)

    # Declaração do metodo tratar_operador_de_multiplicacao, onde iremos
    # tratar os atomos em caso do retorno no metodo 
    # proximo char ser um operador de multiplicacao ou divisao
    def tratar_operador_de_multiplicacao(self, char: str):
        lexema = char
        estado = 0
        while (True):
            match estado:
                case 0:
                    if (char == "*"):
                        estado = 1
                    elif (char == "/"):
                        estado = 2
                case 1:
                    return Atomo(Atomos.MULOP.value, lexema, 0, Atomos.MULOP_MULT.value, self.linha)
                case 2:
                    return Atomo(Atomos.MULOP.value, lexema, 0, Atomos.MULOP_DIVI.value, self.linha)

    # Declaração do metodo tratar_operador_de_atribuicao, onde iremos
    # tratar os atomos em caso do retorno no metodo
    # proximo char ser um operador de atribuição,
    # ponto, virgula, dois pontos ou ponto e virgula
    def tratar_operador_de_atribuicao_e_pontos(self, char: str):
        lexema = char
        char_anterior = char
        char = self.proximo_char()
        estado = 0
        while (True):
            match (estado):
                case 0:
                    if (char_anterior == ":"):
                        if (char == "="):
                            lexema += char
                            estado = 5
                        else:
                          estado = 1
                    elif (char_anterior == "."):
                        estado = 2
                    elif (char_anterior == ";"):
                        estado = 3
                    elif (char_anterior == ","):
                        estado = 4
                case 1:
                    self.retrair()
                    return Atomo(Atomos.DOIS_PONTOS.value, lexema, 0, 0, self.linha)
                case 2:
                    self.retrair()
                    return Atomo(Atomos.PONTO.value, lexema, 0, 0, self.linha)
                case 3:
                    self.retrair()
                    return Atomo(Atomos.PONT_VIRG.value, lexema, 0, 0, self.linha)
                case 4:
                    self.retrair()
                    return Atomo(Atomos.VIRGULA.value, lexema, 0, 0, self.linha)
                case 5:
                    return Atomo(Atomos.ATRIB.value, lexema, 0, 0, self.linha)

    # Declaração do metodo tratar_comentarios_e_parenteses,
    # onde iremos tratar os atomos em caso do retorno 
    # no metodo proximo char ser um char que possa indicar
    # começo ou fim de comentários ou parenteses
    def tratar_comentarios_e_parenteses(self, char : str):
        lexema = char
        estado = 0
        while (True):
            match (estado):
                case 0:
                    if (lexema == "{"):
                        estado = 1
                    elif (lexema == "("):
                        char = self.proximo_char()
                        if (char == "*"):
                            estado = 2
                        else:
                            self.retrair()
                            return Atomo(Atomos.PAR_ESQ.value, lexema, 0, 0, self.linha)
                    elif (lexema == "/"):
                        char = self.proximo_char()
                        if (char == "/"):
                            estado = 3
                        else:
                            self.retrair()
                    elif (lexema == ")"):
                        return Atomo(Atomos.PAR_DIR.value, lexema, 0, 0, self.linha)
                case 1:
                    char = self.proximo_char()
                    if (char == "}"):
                        return self.proximo_atomo()
                    elif (char == "\n"):
                        self.linha += 1
                case 2:
                    char = self.proximo_char()
                    if (char == "*" and self.proximo_char() == ")"):
                        return self.proximo_atomo()
                    elif (char == "\n"):
                        self.linha += 1
                case 3:
                    char = self.proximo_char()
                    if (char == "\n" or char == "\0"):
                        self.linha += 1
                        return self.proximo_atomo()
