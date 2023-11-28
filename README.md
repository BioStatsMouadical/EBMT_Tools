### Main Files:
1.	compute_outcomes.R
2.	syntaxe_mise_cond_prev5.R
3.	variable_classiques.R
4.	secondTX2020.R
5.	survcum.R 
8.	Analysis.QMD: contient les chunks *summary statistics*, et *mise en page cox* (à optimiser sous forme de fonction), + mettre en place 2 templates (Squelette étude comparative, étude prédictive facteurs de risque) 
9.	Dossier Dictionary: Fichiers des Regroupements des Traitments de Conditionnement et Prevention (à optimiser encore plus), + ajouter table correspondance labels.

###	compute_outcomes.R
Le fichier compute_outcomes.R contient 5 fonctions :
a).	La fonction principale est « compute_outcomes » : Cette fonction permet de calculer les différents outcomes nécessaires aux analyses de survie. Elle comporte 4 arguments. Le premier argument est le fichier de données. Le deuxième est « compete_relapse » qui permet de mettre la rechute en compétition des GVH (par défaut en TRUE). Le troisième est « impute_relapse » qui permet d’imputer les rechutes manquantes en « No » (par défaut en FALSE). Le quatrième argument est « compete_consecTX_engneut » qui permet de mettre la greffe consécutive à celle étudiée en compétition des variables de prise de greffe, ENGNEUT et PLAT (par défaut en FALSE).
b).	convertdatespss : Convertie les dates exportées via « read.spss » au format date classique de R. Attention, elle utilise un vecteur de date non exhaustif. Le seul argument est la base de données utilisée.
c).	censure_outcomes : Censure les outcomes classiques (OS, PFS et GVH) à un temps t donné en deuxième argument. Le premier argument est la base de données utilisée.
d).	secondTX : Initialement, elle servait à calculer la consécutive greffe comme évènement. Elle doit être retravaillée.
e).	dcfunction : Calcule les causes de décès de différentes manières. Elle doit être retravaillée.

###	syntaxe_mise_cond_prev5.R
Ce fichier contient deux fonctions :
  a).	prevcond : Le seul argument est la base de données. Cette fonction simplifie les labels des CONDDRUGX et PREVDRUGX. Elle concatène les médicaments du   conditionnement et la TBI. Similairement elle concatène les médicaments de la prévention de la GVH.
  b).	doseconddrug : Le premier argument est la base de données, le deuxième argument est « hypothese_treo », par défaut en FALSE, qui fait l’hypothèse d’un conditionnement myelo-ablatif pour une certaine dose de Treosulfan. Cette fonction calcule et transforme à la même unité les doses des médicaments du conditionnement. Par ailleurs, elle fournit une variable « MYELOABR » corrigé selon les doses des médicaments du conditionnement qui s’appelle « MYELOABRJEG » (attention aux hypothèses).

###	variable_classiques.R
Ce fichier contient une seule fonction : variable_classique :
a).	variable_classique : Cette fonction n’a que la base de données comme argument et calcule quelques variables dont j’ai eu précédemment besoin. Il faudrait que je la mette au propre.

###	secondTX2020.R
Ce fichier contient une seule fonction : sectrans. Attention elle doit être appliquée sur une base de données où les greffes consécutives (ou précédentes) sont présentes en ligne.
a).	sectrans : Cette fonction calcule le nombre de greffe par individu ainsi que le type de greffe, le diagnostic, la rechute, les GVH, l’engraftment des Poly pour chaque individu. Les nouvelles variables sont collées à chaque greffe avec les numéros allant de 1 à 5 correspondants au numéro de la greffe. Attention, j’ai un souci de format de date pour les greffes supérieur à 2. A regler.

###	survcum.R
Ce fichier contient une seule fonction du même nom :
a).	survcum : Le premier argument est la base de données et le deuxième est un vecteur de temps par défaut (t=2). Cette fonction calcule les estimations des différents outcomes aux temps t souhaités. Elle utilise les variables créées par compute_outcome. Un message d’erreur peut s’afficher mais ne bloque pas l’exécution. En effet je ne calcule pas par défaut la GRFS et je dois retravailler l’estimation de « consecutive transplant ». 
**AJOUTER PARAMETRE DAY**
 

