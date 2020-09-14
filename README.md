# EconometrieGrandeDim

Codes pour la formation "économétrie en grande dimension" (Insee, 2020), prévue les 29 et 30 septembre 2020.

Document "L'économétrie en grande dimension" servant de support de cours, disponible à cette adresse: https://sites.google.com/site/jeremylhour/research .

## Plan de la formation:

### Jour 1 -- 9h30-12h:
- Introduction et exemples [Section 1],
- La régression pénalisée [Section 2].

    
### Jour 1 -- 13h30-17h:
- **Application**: [utilisation de glmnet](RidgeLasso-glmnet.ipynb),
- Inférence post-sélection, Leeb and Potscher [Section 3.1],
- **Application**: [simulations](RegularizationBias.ipynb) [Section 3.2].
    
### Jour 2 -- 9h30-12h:
- Immunisation et double-sélection [Sections 3.3-3.5],
- **Application**: [simulations](DoubleSelection.ipynb),
- **Application empirique**: [l'effet du diplôme sur le salaire dans l'enquête emploi](Exercice_EnqueteEmploi.ipynb), avec des [éléments de correction](ApplicationEnqueteEmploi.ipynb).
    
### Jour 2 -- 13h30-17h:
- Hétérogénéité des effets [Section 4],
- **Application empirique**: [effet du vote sur la propension à économiser l'eau](GenericML-example.R) ou retour sur l'application Enquête Emploi selon le temps restant.