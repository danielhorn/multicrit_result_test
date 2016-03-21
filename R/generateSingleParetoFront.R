# c: c > 0: Verschiebung nach links
#    c < 0: Verschiebung nach rechts
# d: d > 0: Verschiebung nach oben
#    d < 0: Verschiebung nach unten
# a: > 0, je groesserer a, desto staerkere Woelbung
# b: > 0, je groesser b, desto hoeher liegt die Woelbung 
# e: Skalierungsfaktor   

library(LambertW)

generateSingleParetoFront = function(a, b, c = 0, d = 0, e = 1) {
  g = function(x) {
    x = x + c
    
    if (b >= 0) {
      y = (exp(-a*x) - b*x - exp(-a) + b) / (1 - exp(-a) + b)
    } else {
      b = abs(b)
      z = (a / b) * exp((1 / b) * a * (b - exp(-a) + 1) * (-b / (b - exp(-a) + 1) + x + exp(-a) / (b - exp(-a) + 1)))
      y = (1 / (a * b)) * (b * W(z) - a * b * x + a * b + exp(-a) * a * x - a * x - exp(-a) * a)
    }
    
    y = y + d
    y = y / e
    
    return(y)
  }
  return(g)
}

