from AnalisadorLexico import AnalisadorLexico
from AnalisadorSintatico import AnalisadorSintatico
from AnalisadorSemantico import AnalisadorSemantico
import sys

# Definindo mensagens que irão aparecer para 
# cada tipo de atomo ao rodar o programa
atomo_message = [
        "ERRO", "EOS", "BEGIN", "BOOLEAN", "DIV", "DO",
        "ELSE", "END", "FALSE", "IF", "INTEGER", "MOD",
        "PROGRAM", "READ", "THEN", "TRUE", "NOT", "VAR",
        "WHILE", "WRITE", "IDENTIF", "NUM", "ATRIB", 
        "PONT_VIRG", "DOIS_PONTOS", "VIRGULA", "PONTO",
        "RELOP", "ADDOP", "MULOP", "OR", "AND", "PAR_ESQ",
        "PAR_DIR", "COMENTARIO"
        ]

def ler_arquivo():
    if len(sys.argv) > 1:
        nome_arquivo = sys.argv[1]
    else:
        nome_arquivo = "teste.pas"

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
