# T Distribution Illustration 
TimeDelay = 1
plotT = function(df, add = FALSE)
{
    f = function(x)
    {
        return(dt(x, df))
    }
    curve(f, xlim = c(-5,5), ylim = c(0, 0.4), add = add, main = "t de Student vs. Normal Estándar",ylab = "f(x)")
    curve(dnorm, add=T, lty = 2, col = "blue")
    legend(x = -6,y=0.5,legend = c("Normal"), lty = 2)
    text(x = 2.5, y = 0.4, labels = paste("Grados de Libertad: ", df, sep = ""))
}

for (i in 1:25)
{
    plotT(i) 
    Sys.sleep(TimeDelay)
}
