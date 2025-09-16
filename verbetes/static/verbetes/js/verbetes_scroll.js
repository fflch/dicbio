// verbetes/static/verbetes/js/verbetes_scroll.js

document.addEventListener('DOMContentLoaded', function() {
    // Encontra a barra lateral que contém a lista de verbetes
    const sidebar = document.querySelector('.verbetes-sidebar');
    
    // Se a barra lateral não existir na página, não faz nada
    if (!sidebar) {
        return;
    }

    // Encontra o link do verbete ativo. Nós o identificamos pela classe 'fw-bold'.
    // Usamos querySelector porque esperamos encontrar apenas um ou nenhum.
    const activeLink = sidebar.querySelector('.fw-bold');

    // Se um link ativo foi encontrado na lista...
    if (activeLink) {
        // scrollIntoView() é o método mágico que faz o scroll.
        // O navegador rolará o contêiner pai (a barra lateral)
        // para que o elemento 'activeLink' se torne visível.
        activeLink.scrollIntoView({
            // 'block: "center"' tenta centralizar o elemento verticalmente na área visível.
            // Outras opções são 'start', 'end', ou 'nearest'. 'center' é geralmente bom.
            block: 'center',
            // 'behavior: "auto"' faz o scroll instantaneamente. 
            // Você poderia usar 'smooth' para uma animação suave, mas 'auto' é melhor
            // para o carregamento da página.
            behavior: 'auto'
        });
    }
});