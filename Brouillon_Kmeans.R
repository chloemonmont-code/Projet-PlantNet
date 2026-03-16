# Algorithme des Kmeans du résumé - Entraînement

set.seed(123) # Génération d'un point de départ, à partir duquel faire des tirages de points

# Tirages de points appartenant à des groupes bien définis, l'entraînement
# consistera à retrouver les groupes

# Travail à plat avant de compliquer les choses avec plus de dimensions.
# Soient 3 groupes :

x1 <- matrix(rnorm(60, mean = 0, sd = 0.5), ncol = 2)
X2 <- matrix(rnorm(60, mean = 3, sd = 0.5), ncol = 2)
X3 <- cbind(rnorm(30, mean = 0, sd = 0.4),
            rnorm(30, mean = 4, sd = 0.4))

# Mélange des groupes en mélangeant les groupes entre eux :

X <- rbind(X1, X2, X3) 
colnames(X) <- c("x_ent", "y_ent")

# Fonction de calcul des distance carrées entre un point x et un barycentre mu :

dist <- function(x, mu) {
  sum((x - mu)^2)
}

# On recherche des barycentres pour jauger de la position de groupes potentiels.
# On fait comme si on ne savait pas qu'il y a des groupes et lesquels sont-ils :

cluster <- function(x, barycentres) {
  dist_test <- apply(barycentres,
                     1,
                     function(mu) dist(x, mu))
  which.min(dist_test) # On recherche pour quel barycentre potentiel il y a une distance min entre lui et un point donné
}

