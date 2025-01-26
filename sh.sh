#!/bin/bash
dune clean
dune build
# Se placer dans le répertoire contenant _build/default
cd /home/zacharie/Documents/ENSEEIHT/save/source/_build/default || {
    echo "Erreur : Répertoire introuvable."
    exit 1
}

# Exécuter la commande dune
dune exec bin/newtonoid.exe || {
    echo "Erreur : Échec de l'exécution de newtonoid.exe."
    exit 1
}

echo "Exécution réussie de bin/newtonoid.exe."
