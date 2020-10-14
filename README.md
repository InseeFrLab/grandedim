# EconometrieGrandeDim

Codes correspondant au document de travail "L'économétrie en grande dimension", disponible à cette adresse: https://sites.google.com/site/jeremylhour/research .

Developpés initialement pour la formation "économétrie en grande dimension" (Insee, 2020), qui s'est tenue les 29 et 30 septembre 2020.

## Plan de la formation:

### Jour 1 -- matin:
- Introduction et exemples [Section 1],
- La régression pénalisée [Section 2].

    
### Jour 1 -- après-midi:
- **Application**: [utilisation de glmnet](RidgeLasso-glmnet.ipynb),
- Inférence post-sélection, Leeb and Potscher [Section 3.1],
- **Application**: [simulations](RegularizationBias.ipynb) [Section 3.2].
    
### Jour 2 -- matin:
- Immunisation et double-sélection [Sections 3.3-3.5],
- **Application**: [simulations](DoubleSelection.ipynb),
- **Application empirique**: [l'effet du diplôme sur le salaire dans l'enquête emploi](Exercice_EnqueteEmploi.ipynb), avec des [éléments de correction](ApplicationEnqueteEmploi.ipynb).
    
### Jour 2 -- après-midi:
- Hétérogénéité des effets [Section 4],
- **Application empirique**: [effet du vote sur la propension à économiser l'eau](GenericML-example.R) ou retour sur l'application Enquête Emploi selon le temps restant.


## Autres morceaux de code utiles:
- [Code pour calculer le Lasso avec l'algorithme FISTA](functions/LassoFISTA.R)
- [Code pour calculer le Group Lasso avec l'algorithme FISTA](functions/group_lasso.R)

## Licence

L'ensemble des documents et fichiers disponibles dans ce dépôt sont mis à
disposition sous la Licence Ouverte/Open Licence version 2.0 d'Etalab (voir la
[licence](./LICENCE)).

Cela signifie que vous êtes libres de réutiliser ces informations, gratuitement
et sans restriction d'usage, à la seule condition de citer l'information comme
suit :

```
Jérémy L'Hour, Insee - https://github.com/InseeFrLab/grandedim, 2020
```
