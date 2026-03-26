from src.AnalisadorLexico import AnalisadorLexico
from src.AnalisadorSintatico import AnalisadorSintatico
from src.AnalisadorSemantico import AnalisadorSemantico
import sys

def ler_arquivo():
    if len(sys.argv) > 1:
        nome_arquivo = sys.argv[1]
    else:
        nome_arquivo = "examples/teste11.pas"

    arquivo = open(nome_arquivo)
    buffer = arquivo.read()
    arquivo.close()
    return buffer

def main():
    buffer = ler_arquivo()
    analisador_lexico = AnalisadorLexico(buffer)
    analisador_sintatico = AnalisadorSintatico(analisador_lexico, AnalisadorSemantico())
    analisador_sintatico.sintatico()
    analisador_sintatico.imprimir_resultado()

if __name__ == "__main__":
    main()
