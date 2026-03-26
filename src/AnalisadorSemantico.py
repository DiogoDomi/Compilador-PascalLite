from .Atomo import Atomo
#import sys

class AnalisadorSemantico:
    def __init__(self):
        self.tabela = {}
        self.endereco = 0
        self.rotulo = 0

    # Método responsável por verificar se
    # o lexema (variável) existe e se está
    # presenta na tabela de símbolos
    def variavel_existe(self, atomo: Atomo):
        return atomo.lexema in self.tabela

    # Método responsável por adicionar a
    # variável na tabela de símbolos, 
    # armazenando o lexema no endereço de 
    # acordo com a ordem de declaração
    def armazenar_variavel(self, atomo: Atomo):
        if (not self.variavel_existe(atomo)):
            self.tabela[atomo.lexema] = self.endereco
            self.endereco += 1
            return True
        return False
        #else:
        #    print(f"Erro semântico: Variável '{atomo.lexema}' já foi declarada anteriormente.")
        #    sys.exit(1)

    # Método responsável por buscar o endereço
    # do lexema (variável) na tabela a partir
    # do valor
    def buscar_endereco(self, atomo: Atomo):
        return self.tabela.get(atomo.lexema)

    def proximo_rotulo(self):
        self.rotulo += 1
        return self.rotulo
