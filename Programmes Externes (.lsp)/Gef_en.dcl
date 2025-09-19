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
      : list_box {label = "Layouts"; key="fen"; height = 30; multiple_select=true;}
      : boxed_column {
	label = "View";
	: row {
	  : toggle {label = "Name of layout"; key = "nom";}
	  : toggle {label = "Size of viewport";  key = "tai";}
	  : toggle {label = "Center of viewport";  key = "cen";}
	}
	: row {
	  : edit_box {label = "Names"; key = "ong"; witdth = 60;}
	  : edit_box {label = "Views"; key = "ffen";}
	}
	spacer;
      }
      spacer;
      : row {
	ok_cancel;
	spacer;
	: button {label = "Select"; key = "sel"; width = 10;}
	spacer;
      }
    }
    : column {
      width = 80;
      : list_box {label = "Layers"; key="cal"; height = 30; multiple_select=true;}
      : boxed_column {
	label = "Filter";
	:row {
	  : toggle   {label = "Thaw"; 	key = "gaff";}
	  : toggle   {label = "Joint";  key = "gmel";}
	  : toggle   {label = "Freeze"; key = "ggel";}
	  : toggle   {label = "Xref(s)";key = "xref";}
	}
	: edit_box {label = "Layers";   key = "calq";}
	spacer;
      }
      spacer;
      : row {
	: button {label = "Thaw";    key = "aff"; width = 20;}
	: button {label = "Freeze";  key = "gel"; width = 20;}
	spacer;
	: button {label = "Save";    key = "sav"; width = 20;}
	: button {label = "Restore"; key = "res"; width = 20;}
      }
    }
  }
}
