library(foreign)
library(multiplex)

ranges_calculator = function(lista){
  qnt_intervalos_lista = round(1+3.322*log10(length(lista)),0);
  ranges = c(min(lista));
  amplitude = max(lista)-min(lista);
  for(i in 1:qnt_intervalos_lista){
    ranges = c(ranges,ranges[1]+i*(amplitude/qnt_intervalos_lista));
  }
  return(ranges);
}

remove_outliers = function(lista){
  outliers = boxplot.stats(lista)$out
  lista = lista [! lista %in% outliers]
  return(lista)
}

plot_correlation = function(lista){
  x = c()
  y = c()
  for(i in 1:(length(lista)-1)){
    x = c(x,lista[i])
  }
  for(i in 2:(length(lista))){
    y = c(y,lista[i])
  }
  plot(x,y)
}

data_treatment = function(lista){
  i = 2
  len = c(length(lista))
  lista = remove_outliers(lista)
  len = c(len,length(lista))
  while(len[i] != len[i-1]){
    i = i + 1
    lista = remove_outliers(lista)
    len = c(len,length(lista))
  }
  return(lista)
  
}

chegada = c(5,41,3,3,68,1,38,82,82,20,
            7,43,24,51,6,12,9,86,90,120,
            50,7,40,42,40,13,8,19,27,28,
            26,0,4,37,45,11,21,19,55,8,
            54,39,44,34,2,1,32,22,2,1,
            5,20,23,35,31,106,3,2,62,71,
            29,25,30,3,3,24,27,33,66,3,
            3,68,6,3,33,33,81,9,15,14,
            16,50,49,50,49,100,13,17,110,3,
            0,39,15,14,16,40,9,13,17,5
)

atendimento = c(21,47,13,16,12,15,26,22,17,11,
                19,17,12,15,18,10,33,23,21,14,
                43,11,23,25,24,17,16,20,22,20,
                14,32,26,12,20,20,19,21,30,20,
                43,17,20,15,11,11,19,22,20,18,
                12,33,24,26,12,16,34,10,13,36,
                18,25,23,27,14,12,12,17,16,18,
                16,19,36,10,37,12,16,28,18,16,
                37,25,14,16,15,16,19,13,48,10,
                15,12,11,16,11,32,19,14,20,18)

 
media_atendimento = mean(atendimento)
media_chegada = mean(chegada)

desvio_atendimento = sd(atendimento)
desvio_chegada = sd(chegada)

mediana_atendimento = median(atendimento)
mediana_chegada = median(chegada)

amplitude_atendimento = max(atendimento) - min(atendimento)
amplitude_chegada = max(chegada) - min(chegada)

qnt_intervalos_atendimento = sqrt(length(atendimento))
qnt_intervalos_chegada = sqrt(length(chegada))

ranges_atendimento = ranges_calculator(atendimento)
ranges_chegada = ranges_calculator(chegada)

plot_correlation(chegada)
plot_correlation(atendimento)
acf(atendimento)
acf(chegada)


chegada = data_treatment(chegada)
atendimento = data_treatment(atendimento)
print(atendimento)