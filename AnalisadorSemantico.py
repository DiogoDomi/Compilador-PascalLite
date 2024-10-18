from Atomo import Atomo

class AnalisadorSemantico:
    def __init__(self):
        self.tabela = {}
        self.endereco = 0

    # Método responsável por verificar se
    # o lexema (variável) existe e se está
    # presenta na tabela de símbolos
    def variavel_existe(self, atomo: Atomo):
        for value in self.tabela.values():
            if (value[0] == atomo.lexema):
                return True
        return False

    # Método responsável por adicionar a
    # variável na tabela de símbolos, 
    # armazenando o lexema no endereço de 
    # acordo com a ordem de declaração
    def armazenar_variavel(self, atomo: Atomo):
        if (not self.variavel_existe(atomo)):
            self.tabela[self.endereco] = atomo.lexema
            self.endereco += 1
        else:
            print(f"Erro semântico: Variável '{atomo.lexema}' já foi declarada anteriormente.")

    # Método responsável por buscar o endereço
    # do lexema (variável) na tabela a partir
    # do valor
    def buscar_endereco(self, atomo: Atomo):
        for key, value in self.tabela.items():
            if (value == atomo.lexema):
                return key
        return None

    def proximo_rotulo(self):
        pass

