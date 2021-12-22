skrf <- import("skrf")
plt <- import("matplotlib.pyplot")
paths <- "C:/users/huntdust/desktop/s2p_test_website/s2p_data/CHANNEL_if1_CYCLE_10.s2p"
numFiles <- 1
names <- "53_GHz_dropout_RF_M1_N8.s2p"

net <- skrf$Network(paths)
s12 <- net$s12
db <- s12$s_db
f <-  s12$f
data <- data.frame(f,db)

fig <- plot_ly(data,x=~f,y=~db,type='scatter',mode='lines',name=names[1],hoverinfo = names[1])
cnt<-2

if (numFiles>1) {
  for (f in 2:numFiles) {
    net <- skrf$Network(paths[cnt])
    s12 <- net$s12
    f <-  s12$f
    db <- s12$s_db
    data <- data.frame(f,db)
    data$db <- unlist(data$db)
    
    fig <- fig %>% add_trace(data=data,y=~db,mode='lines', name = names[cnt], hoverinfo = names[cnt])
    cnt <- cnt+1
  }}


fig <- fig %>% layout(title='S12',xaxis=list(title='Frequency (Ghz)'),yaxis=list(title='Magnitude of S12(dB)'),legend = list(orientation="h",y=-0.3)) 
fig