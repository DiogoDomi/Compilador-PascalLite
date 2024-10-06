from Atomos import Atomos
from AnalisadorLexico import AnalisadorLexico
from Atomo import Atomo
import sys

# Definindo mensages que irão aparecer para cada
# tipo de atomo ao rodar o programa
atomo_message = [
        "ERRO", "EOS", "BEGIN", "BOOLEAN", "DIV", "DO",
        "ELSE", "END", "FALSE", "IF", "INTEGER", "MOD",
        "PROGRAM", "READ", "THEN", "TRUE", "NOT", "VAR",
        "WHILE", "WRITE", "IDENTIF", "NUM", "ATRIB", 
        "PONT_VIRG", "DOIS_PONTOS", "VIRGULA", "PONTO",
        "RELOP", "ADDOP", "MULOP", "OR", "AND", "PAR_ESQ",
        "PAR_DIR", "COMENTARIO"
        ]

class AnalisadorSintatico():
    def __init__(self, analisador_lexico : AnalisadorLexico):
        self.analisador_lexico = analisador_lexico
        self.lookahead = self.analisador_lexico.proximo_atomo()
        self.linhas_processadas = 0

    # Declaração do método que irá consumir o atomo
    # recebido, comparar se o tipo do atomo é o mesmo
    # tipo do atomo recebido no lookahead
    def consome(self, atomo_tipo : int):
        if (atomo_tipo != self.lookahead.tipo):
            print("Erro sintático: ", end="")
            print(f"Linha: {self.analisador_lexico.linha} - ", end="")
            print(f"Esperado: [{atomo_message[atomo_tipo]}] ", end="")
            print(f"Encontrado: [{atomo_message[self.lookahead.tipo]}]")
            sys.exit(1)
        elif (atomo_tipo != Atomos.EOS.value):
            self.imprimir_atomo(self.lookahead)
            self.lookahead = self.analisador_lexico.proximo_atomo()

    # Declaração do método que irá iniciar a
    # análise sintática a partir do método
    # proximo_atomo() do analisador léxico
    def sintatico(self):
        self.programa()
        self.consome(Atomos.EOS.value)

    # Declaração do método que irá imprimir
    # cada átomo que foi analisado e está
    # correto lexicamente e sintáticamente
    def imprimir_atomo(self, atomo : Atomo):
        print(f"Linha: {self.analisador_lexico.linha}", end="")
        print(f" - atomo: {atomo_message[atomo.tipo]}", end="")
        if (atomo.tipo == Atomos.NUM.value):
            print(f"\t\tlexema: {atomo.lexema}", end="")
            print(f"\tvalor: {atomo.valor}")
        else:
            print(f"\t\tlexema: {atomo.lexema}")
        if (atomo.tipo != Atomos.EOS.value):
            self.linhas_processadas = self.analisador_lexico.linha

    # Declaração do método que irá imprimir
    # a quantidade de linhas analisadas
    def imprimir_resultado(self):
        print(f"{self.linhas_processadas} linhas analisadas, programa sintaticamente correto.")

    # Declaração do método que irá validar
    # se a sintaxe do programa está correta
    def programa(self):
        self.consome(Atomos.PROGRAM.value)
        self.consome(Atomos.IDENTIF.value)
        while (self.lookahead.tipo == Atomos.IDENTIF.value):
            self.lista_de_identificadores()
        self.consome(Atomos.PONT_VIRG.value)
        self.bloco()
        self.consome(Atomos.PONTO.value)

    # Declaração do método que irá validar
    # se a sintaxe do bloco está correta
    def bloco(self):
        if (self.lookahead.tipo == Atomos.VAR.value):
            self.declaracao_de_variaveis()
        if (self.lookahead.tipo == Atomos.IDENTIF.value):
            self.declaracao()
            self.consome(Atomos.PONT_VIRG.value)
        self.comando_composto()

    # Declaração do método que irá validar
    # se a sintaxe da declaração de variáveis
    # está correta
    def declaracao_de_variaveis(self):
        self.consome(Atomos.VAR.value)
        self.declaracao()
        while (self.lookahead.tipo == Atomos.IDENTIF.value):
            self.consome(Atomos.PONT_VIRG.value)
            self.declaracao()
        self.consome(Atomos.PONT_VIRG.value)

    # Declaração do método que irá validar
    # se a sintaxe da declaração
    def declaracao(self):
        self.lista_de_identificadores()
        self.consome(Atomos.DOIS_PONTOS.value)
        self.tipo()

    # Declaração do método que irá validar
    # se a sintaxe da lista de identificadores
    # está correta
    def lista_de_identificadores(self):
        self.consome(Atomos.IDENTIF.value)
        while (self.lookahead.tipo == Atomos.VIRGULA.value):
            self.consome(Atomos.VIRGULA.value)
            self.consome(Atomos.IDENTIF.value)

    # Declaração do método que irá validar
    # se a sintaxe do tipo está correta
    def tipo(self):
        match (self.lookahead.tipo):
            case Atomos.INTEGER.value:
                self.consome(Atomos.INTEGER.value)
            case Atomos.BOOLEAN.value:
                self.consome(Atomos.BOOLEAN.value)

    # Declaração do método que irá validar
    # se a sintaxe do comando composto
    # está correta
    def comando_composto(self):
        continuar = True
        self.consome(Atomos.BEGIN.value)
        self.comando()
        while (continuar):
            if (self.lookahead.tipo == Atomos.PONT_VIRG.value):
                self.consome(Atomos.PONT_VIRG.value)
            continuar = self.comando()
        self.consome(Atomos.END.value)

    # Declaração do método que irá validar
    # se a sintaxe do comando está correta
    def comando(self):
        match (self.lookahead.tipo):
            case Atomos.IDENTIF.value:
                self.atribuicao()
                return True
            case Atomos.READ.value:
                self.comando_de_entrada()
                return True
            case Atomos.WRITE.value:
                self.comando_de_saida()
                return True
            case Atomos.IF.value:
                self.comando_if()
                return True
            case Atomos.WHILE.value:
                self.comando_while()
                return True
            case Atomos.BEGIN.value:
                self.comando_composto()
                return True
        return False

    # Declaração do método que irá validar
    # se a sintaxe da atribuição está correta
    def atribuicao(self):
        self.consome(Atomos.IDENTIF.value)
        self.consome(Atomos.ATRIB.value)
        self.expressao()
        if (self.lookahead.tipo != Atomos.PONT_VIRG.value and self.lookahead.tipo == Atomos.ELSE.value):
            pass
        elif (self.lookahead.tipo != Atomos.PONT_VIRG.value and self.lookahead.tipo == Atomos.END.value):
            pass
        else:
            self.consome(Atomos.PONT_VIRG.value)
        
    # Declaração do método que irá validar
    # se a sintaxe do comando IF está correta
    def comando_if(self):
        self.consome(Atomos.IF.value)
        self.expressao()
        self.consome(Atomos.THEN.value)
        self.comando()
        if (self.lookahead.tipo == Atomos.ELSE.value):
            self.consome(Atomos.ELSE.value)
            self.comando()

    # Declaração do método que irá validar
    # se a sintaxe do comando WHILE está correta
    def comando_while(self):
        self.consome(Atomos.WHILE.value)
        self.expressao()
        self.consome(Atomos.DO.value)
        self.comando()

    # Declaração do método que irá validar
    # se a sintaxe do comando READ está correta
    def comando_de_entrada(self):
        self.consome(Atomos.READ.value)
        self.consome(Atomos.PAR_ESQ.value)
        self.lista_de_identificadores()
        self.consome(Atomos.PAR_DIR.value)

    # Declaração do método que irá validar
    # se a sintaxe do comando WRITE está correta
    def comando_de_saida(self):
        self.consome(Atomos.WRITE.value)
        self.consome(Atomos.PAR_ESQ.value)
        self.expressao()
        while (self.lookahead.tipo == Atomos.VIRGULA.value):
            self.consome(Atomos.VIRGULA.value)
            self.expressao()
        self.consome(Atomos.PAR_DIR.value)

    # Declaração do método que irá validar
    # se a sintaxe da expressão está correta
    def expressao(self):
        self.expressao_simples()
        if (self.lookahead.tipo == Atomos.RELOP.value):
            self.operador_relacional()
            self.expressao_simples()

    # Declaração do método que irá validar
    # se a sintaxe do RELOP está correta
    def operador_relacional(self):
        if (self.lookahead.tipo == Atomos.RELOP.value):
            self.consome(Atomos.RELOP.value)

    # Declaração do método que irá validar
    # se a sintaxe do ADDOP está correta
    def operador_de_adicao(self):
        if (self.lookahead.tipo == Atomos.ADDOP.value):
            self.consome(Atomos.ADDOP.value)
        else:
            self.consome(Atomos.OR.value)

    # Declaração do método que irá validar
    # se a sintaxe da expressão simples
    # está correta
    def expressao_simples(self):
        if (self.lookahead.tipo == Atomos.ADDOP.value):
            self.consome(Atomos.ADDOP.value)
        self.termo()
        while (self.lookahead.tipo in
                [Atomos.ADDOP.value, Atomos.OR.value]):
            self.operador_de_adicao()
            self.termo()

    # Declaração do método que irá validar
    # se a sintaxe do termo está correta
    def termo(self):
        self.fator()
        while (self.lookahead.tipo in
                [Atomos.MULOP.value, Atomos.DIV.value,
                 Atomos.MOD.value, Atomos.AND.value]):
            self.operador_de_multiplicacao()
            self.fator()

    # Declaração do método que irá validar
    # se a sintaxe do MULOP está correta
    def operador_de_multiplicacao(self):
        if (self.lookahead.tipo == Atomos.MULOP.value):
            self.consome(Atomos.MULOP.value)
        else:
            match (self.lookahead.tipo):
                case Atomos.DIV.value:
                    self.consome(Atomos.DIV.value)
                case Atomos.MOD.value:
                    self.consome(Atomos.MOD.value)
                case Atomos.AND.value:
                    self.consome(Atomos.AND.value)

    # Declaração do método que irá validar
    # se a sintaxe do fator está correta
    def fator(self):
        match (self.lookahead.tipo):
            case Atomos.IDENTIF.value:
                endereco = self.busca_tabela_simbolo(self.lookahead.lexema)
                print(f"\t CRVL {endereço}")
                self.consome(Atomos.IDENTIF.value)
            case Atomos.NUM.value:
                self.consome(Atomos.NUM.value)
            case Atomos.PAR_ESQ.value:
                self.consome(Atomos.PAR_ESQ.value)
                self.expressao()
                self.consome(Atomos.PAR_DIR.value)
