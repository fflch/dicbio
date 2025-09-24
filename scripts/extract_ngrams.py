import xml.etree.ElementTree as ET
import re
from collections import Counter
import os
import csv

# --- Configuração ---
# Lista de stopwords em português. Esta lista é um ponto de partida
# e pode precisar de ajustes para se adequar perfeitamente ao vocabulário
# e à grafia do século XVIII do seu corpus.
# Adicione ou remova palavras conforme a necessidade.
STOPWORDS_PT = set([
    "a", "o", "as", "os", "um", "uma", "uns", "umas",
    "e", "ou", "nem", "mas", "porem", "todavia", "contudo",
    "de", "do", "da", "dos", "das",
    "em", "no", "na", "nos", "nas",
    "por", "para", "com", "sem", "sobre", "sob", "entre", "atras", "adiante",
    "à", "ao", "às", "aos",
    "que", "se", "como", "quando", "onde", "porque", "porquê", "quê",
    "este", "esta", "estes", "estas", "esse", "essa", "esses", "essas",
    "isto", "isso", "aquilo", "este", "esta", "estes", "estas",
    "meu", "minha", "meus", "minhas", "teu", "tua", "teus", "tuas",
    "seu", "sua", "seus", "suas", "nosso", "nossa", "nossos", "nossas",
    "vosso", "vossa", "vossos", "vossas",
    "é", "ser", "foi", "foram", "são", "estar", "está", "estão",
    "ter", "tem", "têm", "haver", "há", "houve",
    "muito", "muita", "muitos", "muitas", "pouco", "pouca", "poucos", "poucas",
    "mais", "menos", "tão", "tanto", "quão", "quanto", "cada", "todo", "toda", "todos", "todas",
    "algum", "alguma", "alguns", "algumas", "nenhum", "nenhuma",
    "outro", "outra", "outros", "outras", "mesmo", "mesma", "mesmos", "mesmas",
    "assim", "logo", "ainda", "já", "depois", "antes", "sempre", "nunca",
    "só", "apenas", "inclusive", "exclusivo", "salvo", "exceto",
    "bem", "mal", "melhor", "pior", "certo", "errado",
    "cujo", "cuja", "cujos", "cujas",
    "um", "dois", "três", "quatro", "cinco", "seis", "sete", "oito", "nove", "dez",
    "primeiro", "segundo", "terceiro",
    # Formas comuns do século XVIII que podem ser consideradas stopwords:
    "naõ", "huma", "hum", "huns", "humas", "coim", "elle", "ella", "sendo", "deste", "desta", "deste", "estas"
])


# --- Funções de Extração e Pré-processamento ---

def extract_portuguese_text_from_tei(xml_file_path):
    """
    Extrai todo o texto em português de um arquivo TEI-XML, ignorando
    elementos com xml:lang="la" e seus descendentes.
    """
    try:
        tree = ET.parse(xml_file_path)
        root = tree.getroot()
        all_portuguese_texts = []

        # Namespace para o atributo xml:lang. ElementTree usa este formato.
        XML_NS = '{http://www.w3.org/XML/1998/namespace}'

        # Função recursiva para percorrer a árvore e coletar texto
        def collect_text(element, is_in_latin_context=False):
            # Verifica se o elemento atual ou um de seus ancestrais definiu o contexto como latim
            current_lang = element.get(f'{XML_NS}lang')
            if current_lang == 'la':
                is_in_latin_context = True # Este elemento e seus filhos são latim

            # Se não estamos em um contexto latino, adiciona o texto do elemento
            if not is_in_latin_context:
                if element.text:
                    all_portuguese_texts.append(element.text)

            # Processa os filhos recursivamente, passando o contexto atual
            for child in element:
                collect_text(child, is_in_latin_context)

            # Adiciona o texto 'tail' do elemento (texto que segue o elemento, mas dentro do pai)
            # O 'tail' também é incluído apenas se o contexto não for latino.
            if not is_in_latin_context:
                if element.tail:
                    all_portuguese_texts.append(element.tail)

        # Inicia a coleta de texto a partir da raiz
        collect_text(root)
        return ' '.join(all_portuguese_texts)
    except ET.ParseError as e:
        print(f"Erro ao analisar o arquivo XML {xml_file_path}: {e}")
        return ""
    except FileNotFoundError:
        print(f"Arquivo não encontrado: {xml_file_path}")
        return ""


def preprocess_text(text):
    """
    Normaliza o texto: minúsculas, remove números, pontuação e retorna uma lista de palavras.
    Preserva caracteres acentuados do português.
    """
    # Substitui hífens por espaços para tratar palavras hifenizadas como separadas
    text = text.replace('-', ' ')
    text = text.lower()
    # Remove números
    text = re.sub(r'\d+', '', text)
    # Remove pontuação, mas mantém caracteres de acentuação do português
    # Regex: [^\w\s] -> qualquer coisa que não seja palavra (letras, números, underscore) ou espaço
    # A adição de 'áéíóúçãõâêôàüäëïöü' permite que esses caracteres sejam tratados como parte de palavras.
    text = re.sub(r'[^\w\sáéíóúçãõâêôàüäëïöü]+', ' ', text)
    # Remove múltiplos espaços e strip
    words = text.split()
    return words


def generate_ngrams(words, n):
    """
    Gera n-gramas a partir de uma lista de palavras.
    """
    ngrams = []
    for i in range(len(words) - n + 1):
        ngrams.append(tuple(words[i : i + n]))
    return Counter(ngrams)


def filter_and_sort_ngrams(ngram_counter, min_freq=2):
    """
    Filtra n-gramas com frequência menor que min_freq e os ordena por frequência.
    """
    filtered_ngrams = {ngram: freq for ngram, freq in ngram_counter.items() if freq >= min_freq}
    sorted_ngrams = sorted(filtered_ngrams.items(), key=lambda item: item[1], reverse=True)
    return sorted_ngrams

def write_ngrams_to_csv(filepath, ngrams_data, title_prefix, min_freq=2):
    """
    Escreve os n-gramas e suas frequências em um arquivo CSV.
    """
    with open(filepath, 'w', newline='', encoding='utf-8') as csvfile:
        writer = csv.writer(csvfile)

        writer.writerow([f"Análise de N-gramas: {title_prefix}"])
        writer.writerow([]) # Linha vazia para espaçamento

        for n_type, counter in ngrams_data.items():
            writer.writerow([f"--- {n_type.capitalize()} ---"])
            writer.writerow(["N-grama", "Frequência"]) # Cabeçalho das colunas

            sorted_ngrams = filter_and_sort_ngrams(counter, min_freq=min_freq)
            if sorted_ngrams:
                for ngram, freq in sorted_ngrams:
                    writer.writerow([' '.join(ngram), freq])
            else:
                writer.writerow([f"Nenhum {n_type[:-1]} encontrado com frequência > {min_freq-1}."])
            writer.writerow([]) # Linha vazia entre os tipos de n-gramas

# --- Função Principal de Análise ---

def analyze_corpus(xml_file_paths, stopwords, output_raw_csv_path, output_filtered_csv_path):
    """
    Processa um conjunto de arquivos XML, extrai texto, gera n-gramas
    com e sem stopwords, e salva os resultados em arquivos CSV.
    """
    all_words_raw = []
    all_words_filtered = []

    for file_path in xml_file_paths:
        print(f"Processando arquivo: {file_path}...")
        portuguese_text = extract_portuguese_text_from_tei(file_path)
        if portuguese_text: # Só processa se houver texto extraído
            words_raw = preprocess_text(portuguese_text)
            all_words_raw.extend(words_raw)

            words_filtered = [word for word in words_raw if word not in stopwords]
            all_words_filtered.extend(words_filtered)
        else:
            print(f"Nenhum texto em português extraído de {file_path} ou houve um erro.")

    print("\n--- Gerando n-gramas (texto original, sem remoção de stopwords) ---")
    ngrams_raw = {
        'bigrams': generate_ngrams(all_words_raw, 2),
        'trigrams': generate_ngrams(all_words_raw, 3),
        'tetragrams': generate_ngrams(all_words_raw, 4),
    }

    print("--- Gerando n-gramas (texto com stopwords removidas) ---")
    ngrams_filtered = {
        'bigrams': generate_ngrams(all_words_filtered, 2),
        'trigrams': generate_ngrams(all_words_filtered, 3),
        'tetragrams': generate_ngrams(all_words_filtered, 4),
    }

    print(f"\nEscrevendo resultados (texto original) para {output_raw_csv_path}")
    write_ngrams_to_csv(output_raw_csv_path, ngrams_raw, "Texto Original")

    print(f"Escrevendo resultados (com stopwords removidas) para {output_filtered_csv_path}")
    write_ngrams_to_csv(output_filtered_csv_path, ngrams_filtered, "Com Stopwords Removidas")

    print("\nProcessamento concluído. Verifique os arquivos CSV na pasta de scripts.")


# --- Exemplo de Uso ---
if __name__ == "__main__":
    # Define o diretório atual do script para calcular caminhos relativos
    SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))

    # Caminho para a pasta das obras
    # partindo do script em dicio_biologia/scripts, subimos um nível (..) e vamos para corpus_digital/obras
    CORPUS_DIR = os.path.join(SCRIPT_DIR, '..', 'corpus_digital', 'obras')

    # Garante que o diretório do corpus exista
    os.makedirs(CORPUS_DIR, exist_ok=True)

    # --- Lista de arquivos XML a serem processados ---
    xml_file_paths = []
    if os.path.exists(CORPUS_DIR):
        for f in os.listdir(CORPUS_DIR):
            if f.endswith('.xml'):
                xml_file_paths.append(os.path.join(CORPUS_DIR, f))
    else:
        print(f"Erro: O diretório do corpus '{CORPUS_DIR}' não foi encontrado.")
        print("Por favor, verifique se a estrutura de pastas está correta:")
        print(f" - dicio_biologia/")
        print(f"   - corpus_digital/")
        print(f"     - obras/ (seus arquivos XML aqui)")
        print(f"   - scripts/")
        print(f"     - extract_ngrams.py (este script)")
        exit() # Sai do script se o diretório principal não existe

    if not xml_file_paths:
        print(f"Nenhum arquivo XML encontrado no diretório: {CORPUS_DIR}")
        print("Verifique se há arquivos XML na pasta 'obras'.")
    
    # Caminhos para os arquivos CSV de saída (na mesma pasta do script)
    output_raw_csv = os.path.join(SCRIPT_DIR, "ngrams_original_text.csv")
    output_filtered_csv = os.path.join(SCRIPT_DIR, "ngrams_stopwords_removed.csv")

    # --- Executar a análise ---
    analyze_corpus(xml_file_paths, STOPWORDS_PT, output_raw_csv, output_filtered_csv)
