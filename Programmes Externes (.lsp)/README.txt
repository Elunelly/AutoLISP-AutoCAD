Il n'est pas nécessaire de charger l'ensemble des fichiers ci-dessous. Ces programmes ont été écrit par de tierces personnes et partager gratuitement sur Internet.
Il s'agit donc uniquement d'une liste non-exhaustive de programme pouvant répondre à des besoins ponctuels. Seuls les fichiers .lsp sont à charger, les fichiers .dcl sont en lien avec les fichiers .lsp et doivent donc se
trouver dans un dossier approuvé et dont le chemin d'accès est présent dans les chemins de recherche des fichiers de support de travail.
Si vous avez un doute sur le fonctionnement d'un programme ou s'il existe un programme répondant à un besoin, se référer au développeur pour plus d'informations.

PS: Ces fichiers regroupent aussi bien des commandes (pour les utilisateurs) que des fonctions (pour les développeurs) !
RAPPEL: Seuls les fichiers .lsp sont des fichiers de programmation !
RAPPEL: Les fichiers n'ayant pas encore été identifiés dans le fichier ci-dessous se trouvent dans le sous-dossier "_In progress" !

Ci-dessous de brèves explications sur l'utilité de chaque commande :

---{  (gile) - Expressions Régulières.lsp  }---

Ensemble de fonctions utilisant le langage VBScript pour tester le format d'une chaîne de caractères (bien plus complexe d'utilisation que (wcmatch), mais plus précis)
RESERVE AUX DEVELOPPEURS UNIQUEMENT !!

---{  BenchMark - Michael Puckett (2005).lsp  }---

Fonction permettant de tester la vitesse d'exécution d'un ensemble de fonctions, afin de vérifier l'écriture la plus efficiente dans le traitement des données et ainsi limiter le temps d'exécution d'un programme
RESERVE AUX DEVELOPPEURS UNIQUEMENT !!

---{  ChangeBlockBasePointV1-5.lsp  }---

Commandes permettant de modifier l'emplacement du point de base d'une définition de bloc.
Deux commandes sont alors disponibles :
CBPR : Cette commande conserve la position de chaque référence du bloc sélectionné.
       C'est-à-dire que la position visuelle de toutes les géométries de référence du bloc restera inchangée lorsque la position du point de base du bloc sera modifiée.
CBP  : Cette commande conserve les coordonnées du point d'insertion pour toutes les références du bloc sélectionné.
       Ainsi, visuellement, les composants du bloc seront déplacés autour du point d'insertion lorsque la position du point de base sera modifiée.

(cf. http://www.lee-mac.com/changeblockinsertion.html)

---{  circle2lw.lsp  }---

Permet de convertir un cercle en polyligne et de lui affecter une largeur globale. Remplace le cercle par la nouvelle polyligne en conservant ses propriétés initiales communes.

---{  Dyn_Read_Xdata - BonusCAD.lsp  }---

Commande permettant d'afficher de manière dynamique l'ensemble des Xdatas (données étendues) d'un objet situé sous le curseur. Actuellement les objets que l'on utilise ne possèdent pas de Xdatas donc peut efficace.
Le programme créé un objet TEXT ou MTEXT sous le curseur lorsque le curseur est au dessus d'une entité possédant des Xdatas. Ce texte est temporaire et devrait être systématiquement supprimé dans la plupart des cas.

---{  Gef.lsp  }---

Permet de gérer l'état des calques (gelé = Freeze, dégelé = Thaw) des fenêtres de présentation. C'est un programme complexe (anglais/français) pour la gestion d'état des calques sur un ensemble de présentations.
Le programme fonctionne autour d'une boîte de dialogue composée de 2 listes : une liste des fenêtres et une liste des calques.
N'est pas parfait, peut générer des erreurs et les fonctions de filtre sur les présentations ne semble pas fonctionner. Il est accompagné des fichiers "Gef_en.dcl" et "Gef_fr.dcl" pour son bon fonctionnement.

---{  IncArrayV1-8.lsp  }---

Ce programme met en réseau une sélection d'objets, tout en incrémentant automatiquement tout contenu numérique trouvé dans les objets d'annotation de la sélection par une valeur d'incrémentation donnée.
Le programme a deux modes de fonctionnement : standard et dynamique.
La commande standard : IncArray n'affiche pas l'aperçu dynamique, mais s'exécute plus rapidement et de manière plus fluide que la version dynamique.
                       cette différence est particulièrement significative lorsqu'il s'agit de mettre en réseau un grand nombre d'objets.
Le mode dynamique    : IncArrayD affiche un aperçu des objets groupés lorsque la souris est déplacée sur l'écran.
                       Toutefois, en raison de la méthode utilisée pour générer cet aperçu, ce mode ne convient que si vous utilisez le programme pour mettre en réseau un petit nombre d'objets.

(cf. http://www.lee-mac.com/incrementalarray.html)

---{  NumIncV3-9.lsp  }---

En remplacement de GILE_INCR en cas de problème, ce programme (anglais) permet majoritairement l'insertion d'objet en incrémentant leur valeur (dynamique ou non dynamique).
Permet aussi de remplacer les valeurs existantes (se référer à l'aide "About" de la boîte de dialogue).

Incremental Numbering Suite permet à l'utilisateur de placer dynamiquement du texte alphabétique ou numérique incrémentiel dans un dessin, avec une gamme d'utilitaires de positionnement et un préfixe et/ou un suffixe facultatif.
Le texte séquentiel peut être créé à l'aide de blocs de texte, de MText ou de blocs attribués. En outre, le style et le formatage de ces objets peuvent être modifiés directement à partir de la boîte de dialogue principale,
tous les paramètres étant conservés entre les sessions de dessin.
L'utilisateur peut modifier le calque du texte ou du bloc, choisir parmi une liste de styles de texte disponibles dans le dessin, modifier l'alignement du texte ou du texte MT, et également changer la hauteur du texte en saisissant
une valeur arbitraire, en choisissant une valeur dans le dessin ou en utilisant la hauteur définie par le style de texte sélectionné.
Si le type d'objet est défini pour utiliser un bloc attribué, l'utilisateur peut choisir le bloc à utiliser dans une liste de blocs attribués définis dans le dessin, ou sélectionner un objet bloc directement dans le dessin.
L'utilisateur a également le contrôle sur l'attribut qui hébergera la chaîne incrémentale, et sur l'échelle à laquelle le bloc est inséré. Cette valeur d'échelle peut prendre une valeur arbitraire saisie par l'utilisateur ou prélevée
dans le dessin, ou peut dépendre de la valeur actuelle d'une variable système sélectionnée, telle que DIMSCALE.
Si MText est sélectionné, l'utilisateur peut également basculer l'utilisation d'un masque d'arrière-plan MText et contrôler le facteur de décalage et la couleur du masque d'arrière-plan.
L'utilisateur peut saisir un texte optionnel de préfixe, de milieu et de suffixe, et a la possibilité d'incrémenter une ou toutes les sections, avec la possibilité d'incrémenter un texte alphabétique et d'utiliser des décimales et
des zéros en tête. L'utilisateur peut également spécifier tout incrément numérique, positif ou négatif.
Si l'utilisateur a choisi d'utiliser des objets Text ou MText pour loger le texte incrémentiel, il a la possibilité d'entourer ces objets d'une bordure. La bordure peut être circulaire, rectangulaire, fendue ou un polygone à n côtés,
créée sur une couche choisie dans la boîte de dialogue principale.
La taille de la bordure peut être contrôlée en utilisant un facteur de décalage de l'objet Texte ou MText. Le facteur de décalage a un comportement identique à celui du facteur de décalage du masque d'arrière-plan, dans lequel
le décalage dépend de la hauteur du texte :
un facteur de décalage de 1,0 correspond exactement à l'objet Texte ou MText, un facteur de 1,5 étend la bordure de 0,5 fois la hauteur du texte, etc.
L'utilisateur peut également spécifier une taille de bordure fixe, avec la possibilité de choisir l'une ou l'autre dimension dans le dessin.

---{  OutlineObjectsV1-1.lsp  }---

Ce programme permet à l'utilisateur de générer une ou plusieurs polylignes ou régions fermées représentant une silhouette ou un contour de tous les objets d'une sélection.
Ce programme, dont la fonctionnalité est similaire à celle de la commande _SHRINKWRAP offerte par certaines applications verticales telles que Civil 3D,
offre une option alternative aux utilisateurs qui n'ont pas accès à de telles applications.

---{  pline-3d-2d.lsp  }---

Ce programme permet à l'utilisateur de remplacer des polylignes 3D par des polylignes standards 2D. Il semble cependant avoir de nombreux défaut, notamment lorsque les polylignes 3D sont fermées.
Le programme PLINE3Dto2D devrait normalement être plus performant.

---{  RBloc.lsp  }---

Permet de remplacer la commande ExpressTools "Replace block with another block", en permettant de conserver ou non les attributs et/ou les propriétés dynamiques (la commande ExpressTools ne fonctionne pas avec les blocs dynamiques)
pour remplacer un bloc (ou plusieurs) par un autre bloc. Toutes les références de blocs seront donc remplacées par le nouveau bloc en conservant les propriétés de la référence initiale.
Il est accompagné du fichier "Rbloc.dcl" pour son bon fonctionnement.

---{  RIA_Gilles Chanteau.lsp  }---

Permet de limiter la longueur d'une polyligne lors de sa création en dessinant temporairement un cercle correspondant à la longueur restante autorisée.

---{  StripMtext__v5-0d.lsp  }---

Ce programme AutoLISP crée une commande " StripMtext " (raccourci "SMT"), qui permettra à l'utilisateur de supprimer rapidement les codes de formatage dans les Mtext, Lignes de repère, Cotations, Tableaux,
et des attributs multilignes sélectionnés. Voici la liste des formatage pouvant être supprimés :
Alignement, Masque d'arrière-plan, Couleur, Colonnes dynamiques, Champs dynamiques (convertis les champs dynamiques en simple texte), Police, Hauteur de texte, Retour à la ligne, Espaces multiples, Lignes vides,
Séparation des paragraphes, Tabulations, Soulignage, Largeur de texte, ...
Il existe une erreur potentielle depuis AutoCAD 2021 car le programme ne supporte pas les caractères Unicode, dont le support a été apportée par le nouvel IDE d'AutoCAD 2021 et la variable LISPSYS. En cas de soucis,
repasser LISPSYS = 0 (redémarrage d'AutoCAD nécessaire) temporairement pour supprimer le support des caractères Unicode.

---{  SuperFlatten 1.2c.lsp  }---

Permet de d'aplanir entièrement une sélection d'objet, blocs imbriqués y compris. Je n'ai pas eut le temps de vérifier l'utilité des options "Rename" "Explodable blocks", ... donc faire une simple validation à vide pour passer le menu.
Cela peut s'avérer utile lorsqu'on travaille sur des fonds de plans non-nécessaires en 3D pour éviter toutes erreurs d'accrochage objet avec un delta Z (les toitures par exemple).

---{  TabSortV2-2.lsp  }---

Ce programme a pour but de simplifier l'organisation des onglets de présentation (via une boîte de dialogue) en pouvant les déplacer vers le haut (donc à gauche dans la liste des onglets d'AutoCAD) ou vers le bas
(donc à droite dans la liste des onglets d'AutoCAD) afin de les trier manuellement (NB: la commande NAMECART effectue déjà un tri par ordre alphabétique des onglets de présentation à chaque exécution !).
L'utilisateur peut également renommer les onglets (double-clic) et ajouter un préfixe/suffixe à l'ensemble des présentations sélectionnée. Il est aussi possible de trier les présentations selon un ordre alphabétique,
numérique et peut inverser le sens.