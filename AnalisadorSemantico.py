from Atomo import Atomo

class AnalisadorSemantico:
    def __init__(self):
        self.tabela = {}
        self.endereco = 0

    def variavel_existe(self, atomo: Atomo):
        for value in self.tabela.values():
            if (value[0] == atomo.lexema):
                return True
        return False

    def criar_variavel(self, atomo: Atomo):
        if (not self.variavel_existe(atomo)):
            self.tabela[self.endereco] = [atomo.lexema, atomo.tipo]
            self.endereco += 1
        else:
            print(f"Erro semântico: Variável '{atomo.lexema}' já foi declarada anteriormente.")
