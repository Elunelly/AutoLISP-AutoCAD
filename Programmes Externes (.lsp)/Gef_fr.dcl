// =================================================================
//
//  GEF.DCL V3.20
//
//  Copyright (C) Patrick_35
//
// =================================================================

gef : dialog {
  key = "titre";
  fixed_width = true;
  alignment = centered;
  is_cancel = true;
  allow_accept = false;
  : row {
    : column {
      width = 80;
      : list_box {label = "Fenêtres"; key="fen"; height = 30; multiple_select=true;}
      : boxed_column {
	label = "Affichage";
	: row {
	  : toggle {label = "Nom de l'onglet"; key = "nom";}
	  : toggle {label = "Taille fenêtre";  key = "tai";}
	  : toggle {label = "Centre fenêtre";  key = "cen";}
	}
	: row {
	  : edit_box {label = "Noms";     key = "ong"; witdth = 60;}
	  : edit_box {label = "Fenêtres"; key = "ffen";}
	}
	spacer;
      }
      spacer;
      : row {
	ok_cancel;
	: button {label = "Sélection"; key = "sel";}
      }
    }
    : column {
      width = 80;
      : list_box {label = "Calques"; key="cal"; height = 30; multiple_select=true;}
      : boxed_column {
	label = "Filtre";
	:row {
	  : toggle   {label = "Afficher"; key = "gaff";}
	  : toggle   {label = "Mixte";    key = "gmel";}
	  : toggle   {label = "Geler";    key = "ggel";}
	  : toggle   {label = "Xref(s)";  key = "xref";}
	}
	: edit_box {label = "Calques";   key = "calq";}
	spacer;
      }
      spacer;
      : row {
	: button {label = "Afficher";    key = "aff"; width = 20;}
	: button {label = "Geler";       key = "gel"; width = 20;}
	spacer;
	: button {label = "Sauvegarder"; key = "sav"; width = 20;}
	: button {label = "Restaurer";   key = "res"; width = 20;}
      }
    }
  }
}
