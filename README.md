FDTMC
=====

Premissas
---------

- Nomes de features possuem apenas 1 caractere
- Não suportamos expressões aritméticas sobre as booleanas nas feature expressions
    (em tese isso seria possível, já que a expressão booleana será convertida em 0 ou 1)
- Sintaxe usada é a mesma do hatt. Ex.: "~(p & (~q | r))"
- Não suportamos a solução parcial de features. Para resolver as feature expressions,
    usaremos toda a feature configuration. No futuro, será possível fazer a aplicação a
    cada feature selecionada, usando cálculo proposicional para simplificar as expressões
    resultantes.


TODO
----

- Remover caminhos inatingíveis ao resolver a variabilidade
- Tratar erros de sintaxe durante o parsing
- Integrar no Hephaestus
    - Fazer parser com tipos esperados pelo HephaestusPL
        - Elaborar um FDTMCModel
        - provavelmente vai precisar ler os arquivos de um diretório
    - Transformação usando todo o InstanceModel para resolver as FE
        - deve usar a função `resolve` do módulo FDTMC


BUGS
----

- O grafo (.dot) gerado perde os atributos globais originais (como o estilo dos nós).
