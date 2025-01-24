En gros les pouvoirs sont rangés dans une liste de pouvoir tout comme les briques et power.ml est plus ou moins un copier collé de brick.ml que j'ai quand même pas mal réadapté.
Ce qui marche : - Les pouvoirs sont des blocs créés au même endroit que les briques
- Les pouvoirs ne bougent pas tant que la brique n'est pas cassée
- Quand un pouvoir entre en collision avec la raquette il disparaît(même si ça ne marche pas tout le temps très bien)

  
Il y a deux problèmes qui restent à résoudre : 
- Les pouvoirs ne marchent pas car je n'ai pas encore commencé à les implémenter(mais je ne pense pas que ce soit très dure à faire)
- Quand la brique ou se trouve le pouvoir est cassée, le pouvoir ne descend pas c'est la fonction update_power dans power.ml qui gère ça mais la condition du else if
  permet de modifier la vitesse verticale de la brique n'est jamais vérifiée
