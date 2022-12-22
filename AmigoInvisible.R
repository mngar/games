# Muestreo Amigo invisible
# solo hay que cambiar los nombres por el de los participantes (puede ser cualquier cantidad, par o impar mayor a 2)
# la segunda ronda no repite pares de amigos 

set.seed(seed = 1234)
v = c("nombre01","nombre02","nombre03","nombre04","nombre05","nombre06","nombre07","nombre08","nombre09","nombre10")

n = length(v)

data <- v
aminv = matrix(NA, 1,n)
colnames(aminv) = v

 for (i in 1:length(data)) {
 x = sample(data[-i])
 datas = na.omit(x)
 datas = datas[1:length(datas)]
 aminv[i] = datas[1]
 data[data==datas[1]] = NA
 }
 aminv

#segunda ronda
data <- v
aminv2 = matrix(NA, 1,n)
colnames(aminv2) = v

 for (i in 1:length(data)) {
 x = sample(data[-i])
 x[x==aminv[i]] = NA
 datas = na.omit(x)
 datas = datas[1:length(datas)]
 aminv2[i] = datas[1]
 data[data==datas[1]] = NA
 }
aminv2



