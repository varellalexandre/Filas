# Tratamento de Dados


Durante o processo de análise de dados de uma fila é necessário tratar os outliers, e fazer toda a estatístia descritiva de chegadas e atendimento. Para isso foi desenvolvido um programa em R para tratamento desses dados.

## Documentação das Funções

#### Ranges Calculator
Nessa função são calculados os intervalos dos elementos totais. Nela é utilizado a regra de Sturges para definir a quantidade de intervalos ideais.

#### Remove Outliers
Na função Remove Outliers, é possível retirar os outliers dos dados. Nela são utilizados o Boxplot stats para retirar os outliers.

#### Plot Correlation
A função Plot Correlation, plota a autocorrelação entre os dados. A autocorrelação dos dados é importante pois mostra se existe alguma tendência entre os elementos ou pode ser considerado um evento aleatório.

#### Data Treatment
A função Data Treatment, realiza a análise de dados esperada para esse tipo de processo, utilizando as funções explicadas para organizar os dados.

