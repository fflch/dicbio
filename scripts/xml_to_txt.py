# xml_to_txt.py
# Este código transforma arquivos XML TEI em arquivos TXT contendo apenas o texto puro.

from lxml import etree
from pathlib import Path
import re

# --- CONFIGURAÇÃO ---
# Diretório onde os arquivos XML de entrada estão.
# Assumindo que o script está na raiz do projeto e os XMLs estão em corpus_digital/obras/
INPUT_DIR = Path(__file__).parent.parent / 'corpus_digital' / 'obras'

# Diretório onde os arquivos TXT de saída serão salvos.
# Vamos criar uma nova pasta para manter as coisas organizadas.
OUTPUT_DIR = Path(__file__).parent.parent / 'corpus_digital' / 'obras_txt'
# --- FIM DA CONFIGURAÇÃO ---


def convert_xml_to_txt(input_path, output_path):
    """
    Lê um arquivo XML TEI, extrai o texto puro de dentro do elemento <text>,
    e o salva em um arquivo de texto.
    """
    try:
        # Usar um parser que remove comentários e PIs para limpar o XML
        parser = etree.XMLParser(remove_comments=True, remove_pis=True)
        tree = etree.parse(str(input_path), parser)
        
        # Define o namespace TEI para as buscas XPath
        ns = {'tei': 'http://www.tei-c.org/ns/1.0'}
        
        # Encontra o elemento <text>
        text_element = tree.find('.//tei:text', namespaces=ns)
        
        if text_element is None:
            print(f"  AVISO: Elemento <text> não encontrado em {input_path.name}. Pulando arquivo.")
            return False

        # Extrai todo o texto de dentro do elemento <text> e de seus descendentes
        # O método .itertext() é perfeito para isso, pois junta o texto de todos os nós filhos.
        # Ele retorna um iterador de strings.
        text_parts = text_element.itertext()
        
        # Junta todas as partes de texto com um espaço
        full_text = ' '.join(text_parts)
        
        # Limpeza final do texto com expressões regulares:
        # 1. Substitui múltiplos espaços em branco, quebras de linha e tabs por um único espaço.
        cleaned_text = re.sub(r'\s+', ' ', full_text)
        # 2. Remove espaços no início e no fim da string.
        cleaned_text = cleaned_text.strip()
        
        # Salva o texto limpo no arquivo de saída
        with open(output_path, 'w', encoding='utf-8') as f:
            f.write(cleaned_text)
        
        print(f"  -> Convertido com sucesso para {output_path.name}")
        return True

    except etree.ParseError as e:
        print(f"  ERRO: Falha ao parsear o XML {input_path.name}. Erro: {e}")
        return False
    except Exception as e:
        print(f"  ERRO: Ocorreu um erro inesperado com o arquivo {input_path.name}. Erro: {e}")
        return False

def main():
    """
    Função principal que encontra os arquivos XML e orquestra a conversão.
    """
    print(f"Procurando por arquivos XML em: {INPUT_DIR}")
    
    if not INPUT_DIR.is_dir():
        print(f"ERRO: Diretório de entrada não encontrado: {INPUT_DIR}")
        return

    # Cria o diretório de saída se ele não existir
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    print(f"Arquivos TXT serão salvos em: {OUTPUT_DIR}\n")
    
    # Encontra todos os arquivos .xml no diretório de entrada
    xml_files = list(INPUT_DIR.glob('*.xml'))
    
    if not xml_files:
        print("Nenhum arquivo .xml encontrado.")
        return
        
    success_count = 0
    error_count = 0
    
    for xml_file in xml_files:
        print(f"Processando: {xml_file.name}...")
        
        # Define o nome do arquivo de saída (mesmo nome, extensão .txt)
        output_file_path = OUTPUT_DIR / xml_file.with_suffix('.txt').name
        
        if convert_xml_to_txt(xml_file, output_file_path):
            success_count += 1
        else:
            error_count += 1
            
    print("\n--- Concluído ---")
    print(f"Arquivos processados com sucesso: {success_count}")
    print(f"Arquivos com erro: {error_count}")

# Garante que o script seja executado apenas quando chamado diretamente
if __name__ == '__main__':
    main()